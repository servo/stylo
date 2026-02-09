/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Typed OM Numeric Values.

use crate::derives::*;
use crate::values::specified::NoCalcLength;

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
