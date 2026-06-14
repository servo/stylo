/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Typed OM Numeric Type.

use crate::values::generics::grid::FlexUnit;
use crate::values::generics::Optional;
use crate::values::specified::angle::AngleUnit;
use crate::values::specified::frequency::FrequencyUnit;
use crate::values::specified::length::LengthUnit;
use crate::values::specified::resolution::ResolutionUnit;
use crate::values::specified::time::TimeUnit;

/// https://drafts.css-houdini.org/css-typed-om-1/#cssnumericvalue-base-type
#[derive(Clone, Debug)]
#[repr(u8)]
pub enum NumericBaseType {
    /// A `<length>` unit.
    Length,

    /// An `<angle>` unit.
    Angle,

    /// A `<time>` unit.
    Time,

    /// A `<frequency>` unit.
    Frequency,

    /// A `<resolution>` unit.
    Resolution,

    /// The `<flex>` unit.
    Flex,

    /// The percentage unit.
    Percent,
}

#[doc(hidden)] // Need to be public so that cbindgen generates it.
pub const NUMERIC_BASE_TYPE_COUNT: usize = 7;

const_assert!(NumericBaseType::Percent as usize + 1 == NUMERIC_BASE_TYPE_COUNT);

/// https://drafts.css-houdini.org/css-typed-om-1/#numeric-typing
#[derive(Clone, Debug)]
#[repr(C)]
pub struct NumericType {
    exponents: [i32; NUMERIC_BASE_TYPE_COUNT],
    percent_hint: Optional<NumericBaseType>,
}

impl NumericType {
    #[inline]
    fn empty() -> Self {
        Self {
            exponents: [0; NUMERIC_BASE_TYPE_COUNT],
            percent_hint: Optional::None,
        }
    }

    #[inline]
    fn with_base_type(base_type: NumericBaseType) -> Self {
        let mut result = Self::empty();
        result.exponents[base_type as usize] = 1;
        result
    }

    /// <https://drafts.css-houdini.org/css-typed-om-1/#create-a-type-from-a-string>
    pub fn try_from_unit(unit: &str) -> Result<Self, ()> {
        if unit.eq_ignore_ascii_case("number") {
            return Ok(Self::empty());
        }

        if unit.eq_ignore_ascii_case("percent") {
            return Ok(Self::with_base_type(NumericBaseType::Percent));
        }

        if LengthUnit::from_str(unit).is_ok() {
            return Ok(Self::with_base_type(NumericBaseType::Length));
        }

        if AngleUnit::from_str(unit).is_ok() {
            return Ok(Self::with_base_type(NumericBaseType::Angle));
        }

        if TimeUnit::from_str(unit).is_ok() {
            return Ok(Self::with_base_type(NumericBaseType::Time));
        }

        if FrequencyUnit::from_str(unit).is_ok() {
            return Ok(Self::with_base_type(NumericBaseType::Frequency));
        }

        if ResolutionUnit::from_str(unit).is_ok() {
            return Ok(Self::with_base_type(NumericBaseType::Resolution));
        }

        if FlexUnit::matches(unit) {
            return Ok(Self::with_base_type(NumericBaseType::Flex));
        }

        Err(())
    }
}
