/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! The `var()` arbitrary substitution function.
//! https://drafts.csswg.org/css-variables-2/#funcdef-var

use super::{
    ArbitraryDeclarationValue, ParsedArgument, Substitution, SubstitutionArgument,
    SubstitutionParserContext, SubstitutionReplacementContext, parse_optional_fallback,
};
use crate::custom_properties::{Name, VariableValue, parse_name};
use crate::parser::Parse;
use crate::{Atom, derives::*};
use cssparser::{Delimiter, Parser};
use style_traits::{ParseError, StyleParseErrorKind};

/// A custom property name, without the leading `--`.
#[derive(Clone, Debug, MallocSizeOf, PartialEq, ToShmem)]
pub struct CustomPropertyName(pub Name);

impl Parse for CustomPropertyName {
    fn parse(_: &crate::parser::ParserContext, input: &mut Parser) -> Result<Self, ParseError> {
        parse_name(input.expect_ident()?.as_ref())
            .map(Atom::from)
            .map(CustomPropertyName)
            .map_err(|_| ParseError::custom(StyleParseErrorKind::UnexpectedIdent))
    }
}

/// Data specific to a `var()` function.
#[derive(Clone, Debug, MallocSizeOf, PartialEq, ToShmem)]
pub struct VarFunction {
    /// The referenced custom property name.
    pub name: SubstitutionArgument<CustomPropertyName>,
    /// The fallback for this function, if any.
    pub fallback: Option<ArbitraryDeclarationValue>,
}

impl VarFunction {
    /// Parses the `var()` function arguments according to its argument grammar and syntax:
    /// - <var-args> = var(<declaration-value>, <declaration-value>?)
    /// - var() = var(<custom-property-name>, <declaration-value>?)
    pub fn parse(
        input: &mut Parser,
        context: &mut SubstitutionParserContext,
    ) -> Result<Self, ParseError> {
        let name = SubstitutionArgument::try_parse(input, context, Delimiter::Comma)?;
        let fallback = parse_optional_fallback(input, context)?;
        Ok(Self { name, fallback })
    }

    /// Replaces the `var()` function with its substitution value.
    /// https://drafts.csswg.org/css-variables-2/#replace-a-var-function
    pub fn replace<'a>(
        &'a self,
        source: &'a VariableValue,
        context: &mut SubstitutionReplacementContext<'a, '_, '_, '_>,
    ) -> Result<Substitution<'a>, ()> {
        let name = self.replace_name(source, context);
        let mut substitution = name
            .value
            .and_then(|name| Self::resolve_custom_property(&name.0, context))
            .ok_or(())
            .or_else(|_| {
                context.substitute_declaration_value(self.fallback.as_ref().ok_or(())?, source)
            })?;
        substitution.attr_tainted |= name.attr_tainted;
        Ok(substitution)
    }

    fn resolve_custom_property<'a>(
        name: &Name,
        context: &mut SubstitutionReplacementContext<'a, '_, '_, '_>,
    ) -> Option<Substitution<'a>> {
        let registration = context.stylist.get_custom_property_registration(name);
        match context.substitution_functions.get_var(registration, name) {
            None => None,
            // If the referenced value is itself still unresolved (i.e. it has references), we
            // are substituting against a partially-resolved map -- this happens while applying
            // a prioritary property in the middle of custom-property resolution. Resolve it
            // recursively, guarding against cycles (which the resolver will also remove, but it
            // may not have gotten to this variable yet).
            Some(v) => match v.as_universal() {
                Some(u) if u.has_references() => {
                    if context.seen.contains(name) {
                        // Cycle: the primary is guaranteed-invalid, so fall through to the
                        // fallback (if any), like any other invalid primary.
                        None
                    } else {
                        context.seen.push(name.clone());
                        let result =
                            context.substitute_variable_value(u, /* attr_taint */ None);
                        context.seen.pop();
                        match result {
                            Ok(mut substitution) => {
                                substitution.attr_tainted |= v.attr_tainted;
                                Some(substitution)
                            },
                            // The primary couldn't be resolved (invalid); use the fallback.
                            Err(()) => None,
                        }
                    }
                },
                _ => Some(Substitution::from_value(
                    v.to_variable_value(),
                    v.attr_tainted,
                )),
            },
        }
    }

    /// Returns the name, substituting and parsing it first if it has references.
    pub fn replace_name<'a>(
        &'a self,
        source: &'a VariableValue,
        context: &mut SubstitutionReplacementContext<'a, '_, '_, '_>,
    ) -> ParsedArgument<'a, CustomPropertyName> {
        self.name.substitute_and_parse(source, context)
    }

    /// Calls `f` for each arbitrary declaration value nested in the function.
    pub fn for_each_unparsed_value<F>(&self, mut f: F)
    where
        F: FnMut(&ArbitraryDeclarationValue),
    {
        if let SubstitutionArgument::Unparsed(arg) = &self.name {
            f(arg);
        }
        if let Some(ref fallback) = self.fallback {
            f(fallback);
        }
    }
}
