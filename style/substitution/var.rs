/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! The `var()` arbitrary substitution function.
//! https://drafts.csswg.org/css-variables-2/#funcdef-var

use super::{
    ArbitraryDeclarationValue, Substitution, SubstitutionParserContext,
    SubstitutionReplacementContext, parse_optional_fallback,
};
use crate::custom_properties::{Name, parse_name};
use crate::stylesheets::UrlExtraData;
use crate::{Atom, derives::*};
use cssparser::{Parser, SourcePosition};
use selectors::parser::SelectorParseErrorKind;
use style_traits::ParseError;

/// Data specific to a `var()` function.
#[derive(Clone, Debug, MallocSizeOf, PartialEq, ToShmem)]
pub struct VarFunction {
    /// The referenced custom property name, without the leading `--`.
    pub name: Name,
    /// The fallback for this function, if any.
    pub fallback: Option<ArbitraryDeclarationValue>,
}

impl VarFunction {
    /// Parses the `var()` function arguments according to its syntax:
    /// - var() = var(<custom-property-name>, <declaration-value>?)
    pub fn parse(
        input: &mut Parser,
        context: &mut SubstitutionParserContext,
        input_end_position: &mut Option<SourcePosition>,
    ) -> Result<Self, ParseError> {
        let ident = input.expect_ident()?;
        let name = parse_name(ident.as_ref())
            .map(Atom::from)
            .map_err(|_| ParseError::custom(SelectorParseErrorKind::UnexpectedIdent))?;
        let fallback = parse_optional_fallback(input, context, input_end_position)?;
        Ok(Self { name, fallback })
    }

    /// Replaces the `var()` function with its substitution value.
    /// https://drafts.csswg.org/css-variables-2/#replace-a-var-function
    pub fn replace<'a>(
        &'a self,
        css: &'a str,
        url_data: &UrlExtraData,
        context: &mut SubstitutionReplacementContext<'a, '_, '_, '_>,
    ) -> Result<Substitution<'a>, ()> {
        let registration = context.stylist.get_custom_property_registration(&self.name);
        let substitution = match context
            .substitution_functions
            .get_var(registration, &self.name)
        {
            None => None,
            // If the referenced value is itself still unresolved (i.e. it has references), we
            // are substituting against a partially-resolved map -- this happens while applying
            // a prioritary property in the middle of custom-property resolution. Resolve it
            // recursively, guarding against cycles (which the resolver will also remove, but it
            // may not have gotten to this variable yet).
            Some(v) => match v.as_universal() {
                Some(u) if u.has_references() => {
                    if context.seen.contains(&&self.name) {
                        // Cycle: the primary is guaranteed-invalid, so fall through to the
                        // fallback (if any), like any other invalid primary.
                        None
                    } else {
                        context.seen.push(&self.name);
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
        };
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
