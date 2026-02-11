/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Typed OM Sum Value.

use crate::typed_om::numeric_values::NoCalcNumeric;
use std::collections::HashMap;
use style_traits::NumericValue;

type UnitMap = HashMap<String, i32>;

/// <https://drafts.css-houdini.org/css-typed-om-1/#cssnumericvalue-sum-value>
#[derive(Clone, Debug)]
struct SumValueItem {
    value: f32,
    unit_map: UnitMap,
}

/// <https://drafts.css-houdini.org/css-typed-om-1/#cssnumericvalue-sum-value>
#[derive(Clone, Debug)]
pub struct SumValue(Vec<SumValueItem>);

impl SumValue {
    /// <https://drafts.css-houdini.org/css-typed-om-1/#create-a-sum-value>
    pub fn try_from_numeric_value(value: &NumericValue) -> Result<Self, ()> {
        match value {
            // CSSUnitValue
            NumericValue::Unit(unit_value) => {
                // Step 1.
                let mut value = unit_value.value;
                let mut unit = unit_value.unit.to_string();

                // Step2.
                let numeric = NoCalcNumeric::parse_unit_value(value, unit.as_str())?;
                if let Some(canonical_unit) = numeric.canonical_unit() {
                    let canonical = numeric.to(canonical_unit)?;
                    value = canonical.unitless_value();
                    unit = canonical.unit().to_string();
                }

                // Step 3.
                if unit.eq_ignore_ascii_case("number") {
                    return Ok(Self(vec![SumValueItem {
                        value,
                        unit_map: UnitMap::new(),
                    }]));
                }

                // Step 4.
                Ok(Self(vec![SumValueItem {
                    value,
                    unit_map: [(unit, 1)].into_iter().collect::<UnitMap>(),
                }]))
            },

            // CSSMathSum
            NumericValue::Sum(math_sum) => {
                // Step 1.
                let mut values: Vec<SumValueItem> = Vec::new();

                // Step 2.
                for item in &math_sum.values {
                    // Step 2.1.
                    let value = SumValue::try_from_numeric_value(item)?;

                    // Step 2.2.
                    for sub_value in value.0 {
                        // Step 2.2.1.
                        if let Some(item) = values
                            .iter_mut()
                            .find(|item| item.unit_map == sub_value.unit_map)
                        {
                            item.value += sub_value.value;
                            continue;
                        }

                        // Step 2.2.2.
                        values.push(sub_value);
                    }
                }

                // Step 3.

                // TODO: Create a type [1] from the unit map of each item of
                // values, and add [2] all the types together. If the result is
                // failure, return failure.
                //
                // [1] https://drafts.css-houdini.org/css-typed-om-1/#create-a-type-from-a-unit-map
                // [2] https://drafts.css-houdini.org/css-typed-om-1/#cssnumericvalue-add-two-types

                // Step 4.
                Ok(SumValue(values))
            },
        }
    }
}
