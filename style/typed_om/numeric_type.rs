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
#[derive(Clone, Copy, Debug, PartialEq)]
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

/// Every numeric base type in enum-declaration order.
pub const ALL_NUMERIC_BASE_TYPES: [NumericBaseType; NUMERIC_BASE_TYPE_COUNT] = [
    NumericBaseType::Length,
    NumericBaseType::Angle,
    NumericBaseType::Time,
    NumericBaseType::Frequency,
    NumericBaseType::Resolution,
    NumericBaseType::Flex,
    NumericBaseType::Percent,
];

const _ASSERT_ALL_NUMERIC_BASE_TYPES_ORDER: () = {
    let mut i = 0;
    while i < NUMERIC_BASE_TYPE_COUNT {
        assert!(ALL_NUMERIC_BASE_TYPES[i] as u8 == i as u8);
        i += 1;
    }
};

/// https://drafts.css-houdini.org/css-typed-om-1/#numeric-typing
///
/// `non_zero_count` and `non_zero_except_percent_count` are derived fields
/// maintained in sync with `exponents`, allowing O(1) type compatibility
/// checks. They fit without padding into the 2 bytes following `percent_hint`,
/// so the struct remains 32 bytes.
#[derive(Clone, Debug)]
#[repr(C)]
pub struct NumericType {
    exponents: [i32; NUMERIC_BASE_TYPE_COUNT],
    percent_hint: Optional<NumericBaseType>,
    non_zero_count: u8,
    non_zero_except_percent_count: u8,
}

impl NumericType {
    #[inline]
    fn empty() -> Self {
        Self {
            exponents: [0; NUMERIC_BASE_TYPE_COUNT],
            percent_hint: Optional::None,
            non_zero_count: 0,
            non_zero_except_percent_count: 0,
        }
    }

    /// Constructs a numeric type from a single base type.
    ///
    /// Keep Gecko's StyleNumericType::WithBaseType() in sync with this
    /// implementation.
    #[inline]
    fn with_base_type(base_type: NumericBaseType) -> Self {
        let mut result = Self::empty();
        result.exponents[base_type as usize] = 1;
        result.non_zero_count = 1;
        if base_type != NumericBaseType::Percent {
            result.non_zero_except_percent_count = 1;
        }
        result
    }

    /// A numeric type whose exponent map is empty.
    pub fn number() -> Self {
        Self::empty()
    }

    /// A numeric type whose percent exponent is 1.
    pub fn percent() -> Self {
        Self::with_base_type(NumericBaseType::Percent)
    }

    /// A numeric type whose length exponent is 1.
    pub fn length() -> Self {
        Self::with_base_type(NumericBaseType::Length)
    }

    /// A numeric type whose angle exponent is 1.
    pub fn angle() -> Self {
        Self::with_base_type(NumericBaseType::Angle)
    }

    /// A numeric type whose time exponent is 1.
    pub fn time() -> Self {
        Self::with_base_type(NumericBaseType::Time)
    }

    /// A numeric type whose frequency exponent is 1.
    pub fn frequency() -> Self {
        Self::with_base_type(NumericBaseType::Frequency)
    }

    /// A numeric type whose resolution exponent is 1.
    pub fn resolution() -> Self {
        Self::with_base_type(NumericBaseType::Resolution)
    }

    /// A numeric type whose flex exponent is 1.
    pub fn flex() -> Self {
        Self::with_base_type(NumericBaseType::Flex)
    }

    /// <https://drafts.css-houdini.org/css-typed-om-1/#create-a-type-from-a-string>
    pub fn try_from_unit(unit: &str) -> Result<Self, ()> {
        if unit.eq_ignore_ascii_case("number") {
            return Ok(Self::number());
        }

        if unit.eq_ignore_ascii_case("percent") {
            return Ok(Self::percent());
        }

        if LengthUnit::from_str(unit).is_ok() {
            return Ok(Self::length());
        }

        if AngleUnit::from_str(unit).is_ok() {
            return Ok(Self::angle());
        }

        if TimeUnit::from_str(unit).is_ok() {
            return Ok(Self::time());
        }

        if FrequencyUnit::from_str(unit).is_ok() {
            return Ok(Self::frequency());
        }

        if ResolutionUnit::from_str(unit).is_ok() {
            return Ok(Self::resolution());
        }

        if FlexUnit::matches(unit) {
            return Ok(Self::flex());
        }

        Err(())
    }

    /// Creates a numeric type from a previously validated unit string.
    pub fn from_unit_unchecked(unit: &str) -> Self {
        let result = Self::try_from_unit(unit);
        debug_assert!(result.is_ok(), "Expected a valid unit, got {unit:?}");

        result.unwrap_or(Self::number())
    }

    #[allow(dead_code)]
    fn exponent(&self, base_type: NumericBaseType) -> i32 {
        self.exponents[base_type as usize]
    }

    #[allow(dead_code)]
    fn set_exponent(&mut self, base_type: NumericBaseType, new_value: i32) {
        let old_value = self.exponent(base_type);
        self.exponents[base_type as usize] = new_value;
        match (old_value != 0, new_value != 0) {
            (false, true) => {
                self.non_zero_count += 1;
                if base_type != NumericBaseType::Percent {
                    self.non_zero_except_percent_count += 1;
                }
            },
            (true, false) => {
                self.non_zero_count -= 1;
                if base_type != NumericBaseType::Percent {
                    self.non_zero_except_percent_count -= 1;
                }
            },
            _ => {},
        }
    }

    #[allow(dead_code)]
    fn add_exponent(&mut self, base_type: NumericBaseType, delta: i32) {
        self.set_exponent(base_type, self.exponent(base_type) + delta);
    }
}
