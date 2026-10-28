/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! The `attr()` arbitrary substitution function.
//! https://drafts.csswg.org/css-values-5/#funcdef-attr

use super::{
    ArbitraryDeclarationValue, AttrTaint, Substitution, SubstitutionParserContext,
    SubstitutionReplacementContext, parse_optional_fallback,
};
use crate::custom_properties::{ComputedValue, Name, VariableValue};
use crate::dom::AttributeTracker;
use crate::properties_and_values::{
    syntax::Descriptor as SyntaxDescriptor,
    value::{
        AllowComputationallyDependent, ComputedValue as ComputedRegisteredValue,
        SpecifiedValue as SpecifiedRegisteredValue,
    },
};
use crate::stylesheets::UrlExtraData;
use crate::values::generics::calc::SortKey as AttrUnit;
use crate::values::specified::ParsedNamespace;
use crate::{Atom, LocalName, Namespace, derives::*};
use cssparser::{Parser, SourcePosition, Token, TokenSerializationType};
use servo_arc::Arc;
use std::borrow::Cow;
use style_traits::{ParseError, StyleParseErrorKind};

impl LocalName {
    #[cfg(feature = "gecko")]
    fn with_name<R>(name: &Name, callback: impl FnOnce(&Self) -> R) -> R {
        callback(Self::cast(name))
    }

    #[cfg(feature = "servo")]
    fn with_name<'a, R>(name: &'a Name, callback: impl FnOnce(&Self) -> R) -> R {
        callback(&name.as_str().into())
    }
}

impl From<Name> for LocalName {
    #[cfg(feature = "gecko")]
    fn from(name: Name) -> Self {
        Self::new(name)
    }

    #[cfg(feature = "servo")]
    fn from(name: Name) -> Self {
        name.as_str().into()
    }
}


#[derive(Clone, Debug, MallocSizeOf, PartialEq, ToShmem, Parse)]
enum AttributeType {
    Invalid,
    None,
    RawString,
    Type(SyntaxDescriptor),
    Unit(AttrUnit),
}

/// Data specific to an `attr()` function.
#[derive(Clone, Debug, MallocSizeOf, PartialEq, ToShmem)]
pub struct AttrFunction {
    /// The referenced attribute name.
    pub name: Name,
    kind: AttributeType,
    namespace: ParsedNamespace,
    /// The fallback for this function, if any.
    pub fallback: Option<ArbitraryDeclarationValue>,
}

impl AttrFunction {
    /// Whether we're an `attr(... type(...))` reference.
    #[inline]
    pub fn has_type(&self) -> bool {
        matches!(self.kind, AttributeType::Type(..))
    }

    /// Attribute values may reference other substitution functions we may need to process.
    /// See step 6: https://drafts.csswg.org/css-values-5/#attr-substitution
    pub fn value_for_cycle_resolution(
        &self,
        url_data: &UrlExtraData,
        attribute_tracker: &mut AttributeTracker,
    ) -> Result<ComputedRegisteredValue, ()> {
        let ParsedNamespace::Known(ref namespace) = self.namespace else {
            return Err(());
        };
        let attr = LocalName::with_name(&self.name, |local_name| {
            attribute_tracker.query(local_name, namespace).ok_or(())
        })?;
        let mut parser = Parser::new(&attr);
        // TODO(Bug 2021110): Support namespaced attributes in chained references.
        let value = VariableValue::parse(&mut parser, None, url_data).map_err(|_| ())?;
        Ok(ComputedRegisteredValue::universal(Arc::new(value)))
    }

    /// Parses the `attr()` function arguments according to its syntax:
    /// - attr() = attr(<attr-name> <attr-type>? , <declaration-value>?)
    pub fn parse(
        input: &mut Parser,
        context: &mut SubstitutionParserContext,
        input_end_position: &mut Option<SourcePosition>,
    ) -> Result<Self, ParseError> {
        let mut namespace = ParsedNamespace::Known(Namespace::default());
        if let Some(namespaces) = context.namespaces
            && let Ok(ns) = input.try_parse(|input| ParsedNamespace::parse(namespaces, input))
        {
            namespace = ns;
            let prev = input.state();
            let next = match *input.next_including_whitespace()? {
                Token::Ident(_) => Ok(()),
                _ => Err(ParseError::unexpected_token()),
            };
            input.reset(&prev);
            next?;
        }
        let name = Atom::from(input.expect_ident()?.as_ref());
        let kind = parse_attr_type(input);
        let fallback = parse_optional_fallback(input, context, input_end_position)?;
        Ok(Self {
            name,
            kind,
            namespace,
            fallback,
        })
    }

    /// Replaces the `attr()` function with its substitution value.
    /// https://drafts.csswg.org/css-values-5/#replace-an-attr-function
    pub fn replace<'a>(
        &'a self,
        css: &'a str,
        url_data: &UrlExtraData,
        context: &mut SubstitutionReplacementContext<'a, '_, '_, '_>,
    ) -> Result<Substitution<'a>, ()> {
        let namespace = match self.namespace {
            ParsedNamespace::Known(ref ns) => Some(ns),
            ParsedNamespace::Unknown => None,
        };
        let substitution = namespace
            .and_then(|namespace| {
                LocalName::with_name(&self.name, |local_name| {
                    context.attribute_tracker.query(local_name, namespace)
                })
            })
            .map_or_else(
                || {
                    // Special case when fallback and <attr-type> are omitted.
                    // See FAILURE: https://drafts.csswg.org/css-values-5/#attr-substitution
                    if self.fallback.is_none() && self.kind == AttributeType::None {
                        Some(simple_attr_substitution(""))
                    } else {
                        None
                    }
                },
                |attr| {
                    let attr = if let AttributeType::Type(_) = &self.kind {
                        // If we're evaluating a container query, we haven't run the cascade
                        // and populated substitution_functions.attributes, so we can't do the
                        // get_attr() lookup here.
                        // TODO: This means chained attr() references will not work reliably in
                        // container style queries:
                        // https://bugzilla.mozilla.org/show_bug.cgi?id=2028861
                        if context.computed_context.in_container_query {
                            attr
                        } else {
                            context
                                .substitution_functions
                                .get_attr(&self.name)
                                .map(|v| v.to_variable_value())?
                                .css
                        }
                    } else {
                        attr
                    };
                    let mut parser = Parser::new(&attr);
                    match &self.kind {
                        AttributeType::Unit(unit) => {
                            let css = {
                                // Verify that attribute data is a <number-token>.
                                parser.expect_number().ok()?;
                                let mut s = attr.clone();
                                s.push_str(unit.as_ref());
                                s
                            };
                            let serialization = match unit {
                                AttrUnit::Number => TokenSerializationType::Number,
                                AttrUnit::Percentage => TokenSerializationType::Percentage,
                                _ => TokenSerializationType::Dimension,
                            };
                            let value =
                                ComputedValue::new(css, url_data, serialization, serialization);
                            Some(Substitution::from_value(
                                value, /* attr_tainted */ true,
                            ))
                        },
                        AttributeType::Type(syntax) => {
                            let value = SpecifiedRegisteredValue::parse(
                                &mut parser,
                                syntax,
                                url_data,
                                None,
                                AllowComputationallyDependent::Yes,
                                AttrTaint::default(),
                                context.property_id,
                            )
                            .ok()?;
                            let value = value.to_variable_value();
                            Some(Substitution::from_value(
                                value, /* attr_tainted */ true,
                            ))
                        },
                        AttributeType::RawString | AttributeType::None => {
                            Some(simple_attr_substitution(&attr))
                        },
                        AttributeType::Invalid => None,
                    }
                },
            );
        match substitution {
            Some(substitution) => Ok(substitution),
            None => context.substitute_declaration_value(
                self.fallback.as_ref().ok_or(())?,
                css,
                url_data,
            ),
        }
    }
}

fn simple_attr_substitution<'a>(s: &str) -> Substitution<'a> {
    let mut css = String::with_capacity(s.len() + 2);
    cssparser::serialize_string(s, &mut css).unwrap();
    Substitution::new(
        Cow::Owned(css),
        TokenSerializationType::Nothing,
        TokenSerializationType::Nothing,
        /* attr_tainted */ true,
    )
}

/// Parse <attr-type> = type( <syntax> ) | raw-string | number | <attr-unit>.
/// https://drafts.csswg.org/css-values-5/#attr-notation
fn parse_attr_type(input: &mut Parser) -> AttributeType {
    input
        .try_parse(|input| {
            Ok(match input.next()? {
                Token::Function(name) if name.eq_ignore_ascii_case("type") => AttributeType::Type(
                    input.parse_nested_block(SyntaxDescriptor::from_css_parser)?,
                ),
                Token::Ident(ident) => {
                    if ident.eq_ignore_ascii_case("raw-string") {
                        AttributeType::RawString
                    } else if let Ok(unit) = AttrUnit::from_ident(ident) {
                        AttributeType::Unit(unit)
                    } else {
                        AttributeType::Invalid
                    }
                },
                Token::Delim('%') => AttributeType::Unit(AttrUnit::Percentage),
                _ => return Err(ParseError::custom(StyleParseErrorKind::UnspecifiedError)),
            })
        })
        .unwrap_or(AttributeType::None)
}
