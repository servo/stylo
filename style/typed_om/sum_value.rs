/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Typed OM Sum Value.

use crate::typed_om::numeric_values::NoCalcNumeric;
use crate::typed_om::{MathValue, NumericValue, UnitValue};
use itertools::Itertools;
use std::collections::HashMap;
use style_traits::CssString;

type UnitMap = HashMap<String, i32>;

// <https://drafts.css-houdini.org/css-typed-om-1/#product-of-two-unit-maps>
fn product_of_two_unit_maps(s: &UnitMap, other: &UnitMap) -> UnitMap {
    // Step 1.
    let mut result = s.clone();

    // Step 2.
    for (unit, power) in other {
        // Step 2.1 & 2.2.
        *result.entry(unit.clone()).or_insert(0) += power;
    }

    // Step 3.
    result
}

/// <https://drafts.css-houdini.org/css-typed-om-1/#cssnumericvalue-sum-value>
#[derive(Clone, Debug)]
struct SumValueItem {
    value: f32,
    unit_map: UnitMap,
}

impl SumValueItem {
    /// <https://drafts.css-houdini.org/css-typed-om-1/#create-a-cssunitvalue-from-a-sum-value-item>
    fn to_unit_value(&self) -> Result<UnitValue, ()> {
        // Step 1.
        if self.unit_map.len() > 1 {
            return Err(());
        }

        // Step 2.
        if self.unit_map.is_empty() {
            return Ok(UnitValue {
                value: self.value,
                unit: CssString::from("number"),
            });
        }

        // Step 3.
        let (unit, power) = self.unit_map.iter().next().unwrap();
        if *power != 1 {
            return Err(());
        }

        // Step 4.
        Ok(UnitValue {
            value: self.value,
            unit: CssString::from(unit),
        })
    }
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
            NumericValue::Math(MathValue::Sum(math_sum)) => {
                // Step 1.
                let mut values: Vec<SumValueItem> = Vec::new();

                // Step 2.
                for item in math_sum {
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
                Ok(Self(values))
            },

            // CSSMathProduct
            NumericValue::Math(MathValue::Product(math_product)) => {
                // Step 1.
                let mut values = vec![SumValueItem {
                    value: 1.0,
                    unit_map: Default::default(),
                }];

                // Step 2.
                for item in math_product {
                    // Step 2.1 & 2.2.
                    let new_values = SumValue::try_from_numeric_value(item)?;

                    let mut temp = Vec::new();

                    // Step 2.3.
                    for item1 in &values {
                        // Step 2.3.1.
                        for item2 in &new_values.0 {
                            // Step 2.3.1.1.
                            let mut unit_map =
                                product_of_two_unit_maps(&item1.unit_map, &item2.unit_map);
                            unit_map.retain(|_, power| *power != 0);
                            let item = SumValueItem {
                                value: item1.value * item2.value,
                                unit_map,
                            };

                            // Step 2.3.1.2.
                            temp.push(item);
                        }
                    }

                    // Step 2.4.
                    values = temp;
                }

                // Step 3.
                Ok(Self(values))
            },

            // CSSMathNegate
            NumericValue::Math(MathValue::Negate(math_negate)) => {
                // Step 1 & 2.
                let mut values = SumValue::try_from_numeric_value(math_negate)?.0;

                // Step 3.
                for item in &mut values {
                    item.value = -item.value;
                }

                // Step 4.
                Ok(Self(values))
            },

            // CSSMathInvert
            NumericValue::Math(MathValue::Invert(math_invert)) => {
                // Step 1 & 2.
                let mut values = SumValue::try_from_numeric_value(math_invert)?.0;

                // Step 3.
                if values.len() != 1 {
                    return Err(());
                }

                let item = &mut values[0];

                // Step 4.
                item.value = 1.0 / item.value;
                for power in item.unit_map.values_mut() {
                    *power = -*power;
                }

                // Step 5.
                Ok(Self(values))
            },

            // CSSMathMin
            NumericValue::Math(MathValue::Min(math_min)) => {
                // Step 1 & 2.
                let mut args = Vec::new();

                for item in math_min {
                    let values = SumValue::try_from_numeric_value(item)?;

                    if values.0.len() > 1 {
                        return Err(());
                    }

                    args.push(values);
                }

                debug_assert!(!args.is_empty());

                // Step 3.
                if !args.iter().map(|arg| &arg.0[0].unit_map).all_equal() {
                    return Err(());
                }

                // Step 4.
                let min = args
                    .into_iter()
                    .map(|arg| arg.0.into_iter().next().unwrap())
                    .min_by(|a, b| a.value.total_cmp(&b.value))
                    .ok_or(())?;

                Ok(Self(vec![min]))
            },

            // CSSMathMax
            NumericValue::Math(MathValue::Max(math_max)) => {
                // Step 1 & 2.
                let mut args = Vec::new();

                for item in math_max {
                    let values = SumValue::try_from_numeric_value(item)?;

                    if values.0.len() > 1 {
                        return Err(());
                    }

                    args.push(values);
                }
                debug_assert!(!args.is_empty());

                // Step 3.
                if !args.iter().map(|arg| &arg.0[0].unit_map).all_equal() {
                    return Err(());
                }

                // Step 4.
                let max = args
                    .into_iter()
                    .map(|arg| arg.0.into_iter().next().unwrap())
                    .max_by(|a, b| a.value.total_cmp(&b.value))
                    .ok_or(())?;

                Ok(Self(vec![max]))
            },

            // CSSMathClamp
            NumericValue::Math(MathValue::Clamp(_math_clamp)) => {
                // TODO: Implement me!
                Err(())
            },
        }
    }

    /// Step 3 of:
    /// https://drafts.css-houdini.org/css-typed-om-1/#dom-cssnumericvalue-to
    pub fn resolve_to_unit(&self, unit: &str) -> Result<UnitValue, ()> {
        if self.0.len() != 1 {
            return Err(());
        }

        let sole_item = &self.0[0];

        let item = sole_item.to_unit_value()?;

        let item = {
            let numeric =
                NoCalcNumeric::parse_unit_value(item.value, item.unit.to_string().as_str())?;
            let converted = numeric.to(unit)?;

            UnitValue {
                value: converted.unitless_value(),
                unit: CssString::from(converted.unit()),
            }
        };

        Ok(item)
    }
}
