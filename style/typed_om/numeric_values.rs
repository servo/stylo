/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Typed OM Numeric Values.

use crate::derives::*;
use crate::values::specified::NoCalcLength;
use crate::values::CSSFloat;

/// A numeric value without a `calc` expression.
#[derive(Clone, ToTyped)]
#[repr(u8)]
#[typed_value(derive_fields)]
pub enum NoCalcNumeric {
    /// A `<length>` value.
    ///
    /// <https://drafts.csswg.org/css-values/#lengths>
    Length(NoCalcLength),
    // TODO: Add other values.
}

impl NoCalcNumeric {
    /// Return the unitless, raw value.
    pub fn unitless_value(&self) -> CSSFloat {
        match *self {
            Self::Length(v) => v.unitless_value(),
        }
    }

    /// Return the unit, as a string.
    ///
    /// TODO: Investigate returning SortKey or adding a new variant for
    /// returning the unit as SortKey. Tracked in
    /// <https://bugzilla.mozilla.org/show_bug.cgi?id=2015863>
    pub fn unit(&self) -> &'static str {
        match *self {
            Self::Length(v) => v.unit(),
        }
    }

    /// Return the canonical unit for this value, if one exists.
    ///
    /// TODO: Investigate returning SortKey. Tracked in
    /// <https://bugzilla.mozilla.org/show_bug.cgi?id=2015863>
    pub fn canonical_unit(&self) -> Option<&'static str> {
        match *self {
            Self::Length(v) => v.canonical_unit(),
        }
    }

    /// Convert this value to the specified unit, if possible.
    ///
    /// TODO: Investigate using SortKey. Tracked in
    /// <https://bugzilla.mozilla.org/show_bug.cgi?id=2015863>
    pub fn to(&self, unit: &str) -> Result<Self, ()> {
        match self {
            Self::Length(v) => Ok(Self::Length(v.to(unit)?)),
        }
    }
}
