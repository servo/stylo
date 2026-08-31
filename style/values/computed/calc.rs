/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Computed-value calc() leaf types.

use super::{Angle, Length, Number, Percentage, Resolution, Time};
use crate::derives::*;
use crate::typed_om::{NumericBaseType, NumericType};
use crate::values::generics::calc::{
    self, CalcType, GenericCalcPercentageLeaf, SimplificationResult,
};
use crate::values::generics::Optional;
use crate::Zero;
use debug_unreachable::debug_unreachable;
use serde::{Deserialize, Serialize};

/// The value of a percentage leaf node that contains an associated percent hint.
pub type CalcPercentageLeaf = GenericCalcPercentageLeaf<Percentage>;

/// The computed leaf of a calc() expression.
#[derive(
    Clone,
    Debug,
    Deserialize,
    MallocSizeOf,
    PartialEq,
    Serialize,
    ToAnimatedZero,
    ToCss,
    ToResolvedValue,
    ToTyped,
)]
#[allow(missing_docs)]
#[repr(u8)]
pub enum ComputedLeaf {
    Length(Length),
    Percentage(CalcPercentageLeaf),
    Number(Number),
    Angle(Angle),
    Time(Time),
    Resolution(Resolution),
}

impl ComputedLeaf {
    pub(super) fn is_zero_length(&self) -> bool {
        match *self {
            Self::Length(ref l) => l.is_zero(),
            Self::Percentage(..)
            | Self::Number(..)
            | Self::Angle(..)
            | Self::Time(..)
            | Self::Resolution(..) => false,
        }
    }
}

impl calc::CalcNodeLeaf for ComputedLeaf {
    fn numeric_type(&self) -> NumericType {
        match self {
            Self::Length(_) => NumericType::length(),
            Self::Percentage(p) => p.numeric_type(),
            Self::Number(_) => NumericType::number(),
            Self::Angle(_) => NumericType::angle(),
            Self::Time(_) => NumericType::time(),
            Self::Resolution(_) => NumericType::resolution(),
        }
    }

    fn unitless_value(&self) -> Option<f32> {
        Some(match *self {
            Self::Length(ref l) => l.px(),
            Self::Percentage(ref p) => p.get(),
            Self::Number(n) => n,
            Self::Angle(ref a) => a.degrees(),
            Self::Time(ref t) => t.seconds(),
            Self::Resolution(ref r) => r.dppx(),
        })
    }

    fn canonical_value(&self) -> Option<f32> {
        Some(match *self {
            Self::Length(ref l) => l.px(),
            Self::Percentage(ref p) => match p.hint {
                // Percentages that are relative to some other value (indicated by a
                // percent hint other than "percent") cannot yet resolve to a numeric
                // value, as the percentage's basis is not available.
                Optional::Some(NumericBaseType::Percent) => p.get(),
                _ => return None,
            },
            Self::Number(n) => n,
            Self::Angle(ref a) => a.degrees(),
            Self::Time(ref t) => t.seconds(),
            Self::Resolution(ref r) => r.dppx(),
        })
    }

    fn new_number(value: f32) -> Self {
        Self::Number(value)
    }

    fn as_number(&self) -> Option<f32> {
        match *self {
            Self::Length(_)
            | Self::Percentage(_)
            | Self::Angle(_)
            | Self::Time(_)
            | Self::Resolution(_) => None,
            Self::Number(value) => Some(value),
        }
    }

    fn as_percentage(&self) -> Option<(f32, Optional<NumericBaseType>)> {
        match *self {
            Self::Percentage(p) => Some((p.get(), p.hint)),
            _ => None,
        }
    }

    fn as_angle_radians(&self) -> Option<f32> {
        match *self {
            Self::Angle(a) => Some(a.radians()),
            _ => None,
        }
    }

    fn new_angle_from_radians(radians: f32) -> Self {
        Self::Angle(Angle::from_radians(radians))
    }

    fn new_from_typed_value(value: f32, numeric_type: NumericType) -> Result<Self, ()> {
        let calc_type = numeric_type.as_calc_type()?;
        let percent_hint = numeric_type.percent_hint();
        Ok(match calc_type {
            CalcType::Number => Self::new_number(value),
            CalcType::Length => Self::Length(Length::new(value)),
            CalcType::Angle => Self::Angle(Angle::from_degrees(value)),
            CalcType::Time => Self::Time(Time::from_seconds(value)),
            CalcType::Resolution => Self::Resolution(Resolution::from_dppx(value)),
            CalcType::Percentage => Self::Percentage(CalcPercentageLeaf::new(value, percent_hint)),
        })
    }

    fn compare(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use self::ComputedLeaf::*;
        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return None;
        }

        // Percentages that resolve against some other basis value cannot be meaningfully compared.
        if matches!(self, Percentage(p) if p.hint != Optional::Some(NumericBaseType::Percent)) {
            return None;
        }

        let Ok(self_negative) = self.is_negative() else {
            return None;
        };
        let Ok(other_negative) = other.is_negative() else {
            return None;
        };
        if self_negative != other_negative {
            return Some(if self_negative {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Greater
            });
        }

        match (self, other) {
            (Length(one), Length(other)) => one.partial_cmp(other),
            (Percentage(one), Percentage(other)) => one.value.partial_cmp(&other.value),
            (Number(one), Number(other)) => one.partial_cmp(other),
            (Angle(one), Angle(other)) => one.partial_cmp(other),
            (Time(one), Time(other)) => one.partial_cmp(other),
            (Resolution(one), Resolution(other)) => one.partial_cmp(other),
            _ => unsafe {
                match *self {
                    Length(..) | Percentage(..) | Number(..) | Angle(..) | Time(..)
                    | Resolution(..) => {},
                }
                debug_unreachable!("Forgot to handle unit in compare()")
            },
        }
    }

    fn try_sum_in_place(&mut self, other: &Self) -> Result<(), ()> {
        use self::ComputedLeaf::*;

        // 0px plus anything else is equal to the right hand side.
        if self.is_zero_length() {
            *self = other.clone();
            return Ok(());
        }

        if other.is_zero_length() {
            return Ok(());
        }

        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return Err(());
        }

        match (self, other) {
            (&mut Length(ref mut one), Length(other)) => {
                *one += *other;
            },
            (&mut Percentage(ref mut one), Percentage(other)) => {
                *one = CalcPercentageLeaf::new(one.get() + other.get(), one.combined_hint(other));
            },
            (&mut Number(ref mut one), Number(other)) => {
                *one += *other;
            },
            (&mut Angle(ref mut one), Angle(other)) => {
                *one += *other;
            },
            (&mut Time(ref mut one), Time(other)) => {
                *one += *other;
            },
            (&mut Resolution(ref mut one), Resolution(other)) => {
                *one += *other;
            },
            _ => unsafe {
                match *other {
                    Length(..) | Percentage(..) | Number(..) | Angle(..) | Time(..)
                    | Resolution(..) => {},
                }
                debug_unreachable!("Forgot to handle unit in try_sum_in_place()")
            },
        }

        Ok(())
    }

    fn try_product_in_place(&mut self, other: &mut Self) -> bool {
        if let Self::Number(ref mut left) = *self {
            if let Self::Number(ref right) = *other {
                // Both sides are numbers, so we can just modify the left side.
                *left *= *right;
                true
            } else {
                // The right side is not a number, so the result should be in the units of the right
                // side.
                if other.map(|v| v * *left).is_ok() {
                    std::mem::swap(self, other);
                    true
                } else {
                    false
                }
            }
        } else if let Self::Number(ref right) = *other {
            // The left side is not a number, but the right side is, so the result is the left
            // side unit.
            self.map(|v| v * *right).is_ok()
        } else {
            // Neither side is a number, so a product is not possible.
            false
        }
    }

    fn try_op<O>(&self, other: &Self, op: O) -> Result<Self, ()>
    where
        O: Fn(f32, f32) -> f32,
    {
        use self::ComputedLeaf::*;
        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return Err(());
        }
        Ok(match (self, other) {
            (Length(one), Length(other)) => Length(super::Length::new(op(one.px(), other.px()))),
            (Percentage(one), Percentage(other)) => Self::Percentage(CalcPercentageLeaf::new(
                op(one.get(), other.get()),
                one.combined_hint(other),
            )),
            (&Number(one), &Number(other)) => Self::Number(op(one, other)),
            (Angle(one), Angle(other)) => Self::Angle(super::Angle::from_degrees(op(
                one.degrees(),
                other.degrees(),
            ))),
            (Time(one), Time(other)) => Self::Time(super::Time::from_seconds(op(
                one.seconds(),
                other.seconds(),
            ))),
            (Resolution(one), Resolution(other)) => {
                Self::Resolution(super::Resolution::from_dppx(op(one.dppx(), other.dppx())))
            },
            _ => unsafe {
                match *self {
                    Length(..) | Percentage(..) | Number(..) | Angle(..) | Time(..)
                    | Resolution(..) => {},
                }
                debug_unreachable!("Forgot to handle unit in try_op()")
            },
        })
    }

    fn map(&mut self, mut op: impl FnMut(f32) -> f32) -> Result<(), ()> {
        let _: () = match self {
            Self::Length(value) => {
                *value = Length::new(op(value.px()));
            },
            Self::Percentage(p) => {
                *p = CalcPercentageLeaf::new(op(p.get()), p.hint);
            },
            Self::Number(value) => {
                *value = op(*value);
            },
            Self::Angle(value) => {
                *value = Angle::from_degrees(op(value.degrees()));
            },
            Self::Time(value) => {
                *value = Time::from_seconds(op(value.seconds()));
            },
            Self::Resolution(value) => {
                *value = Resolution::from_dppx(op(value.dppx()));
            },
        };
        Ok(())
    }

    fn simplify(&mut self) -> SimplificationResult {
        SimplificationResult::Unchanged
    }

    fn sort_key(&self) -> calc::SortKey {
        match *self {
            Self::Length(..) => calc::SortKey::Px,
            Self::Percentage(..) => calc::SortKey::Percentage,
            Self::Number(..) => calc::SortKey::Number,
            Self::Angle(..) => calc::SortKey::Deg,
            Self::Time(..) => calc::SortKey::S,
            Self::Resolution(..) => calc::SortKey::Dppx,
        }
    }
}
