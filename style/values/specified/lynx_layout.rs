/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Lynx-only layout values.

use crate::derives::*;
use crate::parser::{Parse, ParserContext};
use crate::typed_om::{ToTyped, TypedValue};
use crate::values::computed::{self, Context, ToComputedValue};
use crate::values::specified::Integer;
use cssparser::Parser;
use std::fmt::{self, Write};
use style_traits::{CssWriter, ParseError, SpecifiedValueInfo, ToCss};
use thin_vec::ThinVec;

/// A Lynx relative-layout alignment reference.
///
/// `parent` computes to `0` (`RelativeAlignType::kParent` in Lynx), while
/// positive integers reference sibling `relative-id` values. `None` is the
/// unset sentinel (computed `-1`, `SL_DEFAULT_RELATIVE_ID` in Lynx): it is
/// not parseable from author CSS, only produced from computed values, and
/// serializes as `-1` to match how Lynx reports the sentinel.
#[derive(Clone, Debug, MallocSizeOf, PartialEq, ToShmem)]
pub enum RelativeAlign {
    /// No alignment reference (the initial value; computed `-1`).
    None,
    /// Align against the relative-layout parent edge.
    Parent,
    /// Align against a positive sibling id.
    Id(Integer),
}

impl Parse for RelativeAlign {
    fn parse<'i, 't>(
        context: &ParserContext,
        input: &mut Parser<'i, 't>,
    ) -> Result<Self, ParseError<'i>> {
        if input
            .try_parse(|input| input.expect_ident_matching("parent"))
            .is_ok()
        {
            return Ok(Self::Parent);
        }

        Integer::parse_positive(context, input).map(Self::Id)
    }
}

impl ToComputedValue for RelativeAlign {
    type ComputedValue = computed::lynx_layout::RelativeAlign;

    fn to_computed_value(&self, context: &Context) -> Self::ComputedValue {
        match *self {
            Self::None => -1,
            Self::Parent => 0,
            Self::Id(ref id) => id.to_computed_value(context),
        }
    }

    fn from_computed_value(computed: &Self::ComputedValue) -> Self {
        match *computed {
            0 => Self::Parent,
            id if id > 0 => Self::Id(Integer::from_computed_value(&id)),
            // Negative values are the "unset" sentinel; never a (positive)
            // out-of-grammar `Id`.
            _ => Self::None,
        }
    }
}

impl ToCss for RelativeAlign {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        match *self {
            // The unset sentinel serializes as Lynx reports it.
            Self::None => dest.write_str("-1"),
            Self::Parent => dest.write_str("parent"),
            Self::Id(ref id) => id.to_css(dest),
        }
    }
}

impl ToTyped for RelativeAlign {
    fn to_typed(&self, dest: &mut ThinVec<TypedValue>) -> Result<(), ()> {
        match *self {
            Self::None | Self::Parent => Err(()),
            Self::Id(ref id) => id.to_typed(dest),
        }
    }
}

impl SpecifiedValueInfo for RelativeAlign {}
