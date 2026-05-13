/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! `<length-percentage>` computed values, and related ones.
//!
//! The over-all design is a tagged pointer, with the lower bit of the pointer
//! being non-zero if it is a non-calc value. See `tagged_numeric` for the
//! shared implementation details.

use super::{position::AnchorSide, Context, Length, Percentage, ToComputedValue};
use crate::derives::*;
#[cfg(feature = "gecko")]
use crate::gecko_bindings::structs::{AnchorPosOffsetResolutionParams, GeckoFontMetrics};
use crate::logical_geometry::{PhysicalAxis, PhysicalSide};
use crate::values::animated::{
    Animate, Context as AnimatedContext, Procedure, ToAnimatedValue, ToAnimatedZero,
};
use crate::values::computed::position::TryTacticAdjustment;
use crate::values::distance::{ComputeSquaredDistance, SquaredDistance};
use crate::values::generics::calc::{
    CalcUnits, GenericAnchorFunctionFallback, PositivePercentageBasis,
};
#[cfg(feature = "gecko")]
use crate::values::generics::length::AnchorResolutionResult;
use crate::values::generics::position::GenericAnchorSide;
use crate::values::generics::{calc, ClampToNonNegative, NonNegative};
use crate::values::resolved::{Context as ResolvedContext, ToResolvedValue};
use crate::values::specified::length::{EqualsPercentage, FontBaseSize, LineHeightBase};
use crate::values::specified::number::NoCalcNumber;
use crate::values::specified::percentage::NoCalcPercentage;
use crate::values::tagged_numeric::{self as tagged, NumericUnion};
use crate::values::{specified, CSSFloat};
use crate::{Zero, ZeroNoPercent};
use app_units::Au;
use debug_unreachable::debug_unreachable;
use serde::{Deserialize, Serialize};
use std::fmt::{self, Write};
use style_traits::values::specified::AllowedNumericType;
use style_traits::{CssWriter, ToCss, ToTyped, TypedValue};
use thin_vec::ThinVec;

/// The discriminator used for inline LengthPercentage variants.
#[derive(Clone, Copy, Debug, MallocSizeOf, PartialEq, ToShmem)]
#[repr(u8)]
pub enum LengthPercentageTag {
    /// A `<length>` value.
    Length = 0,
    /// A `<percentage>` value.
    Percentage = 1,
}

/// A `<length-percentage>` value. This can be either a `<length>`, a
/// `<percentage>`, or a combination of both via `calc()`.
///
/// https://drafts.csswg.org/css-values-4/#typedef-length-percentage
///
/// cbindgen:derive-eq=false
/// cbindgen:derive-neq=false
#[derive(MallocSizeOf)]
#[repr(C)]
pub struct LengthPercentage(NumericUnion<LengthPercentageTag, f32, CalcLengthPercentage>);

impl ToAnimatedValue for LengthPercentage {
    type AnimatedValue = Self;

    fn to_animated_value(self, context: &AnimatedContext) -> Self::AnimatedValue {
        if context.style.effective_zoom.is_one() {
            return self;
        }
        self.map_lengths(|l| l.to_animated_value(context))
    }

    #[inline]
    fn from_animated_value(value: Self::AnimatedValue) -> Self {
        value
    }
}

impl ToResolvedValue for LengthPercentage {
    type ResolvedValue = Self;

    fn to_resolved_value(self, context: &ResolvedContext) -> Self::ResolvedValue {
        if context.style.effective_zoom.is_one() {
            return self;
        }
        self.map_lengths(|l| l.to_resolved_value(context))
    }

    #[inline]
    fn from_resolved_value(value: Self::ResolvedValue) -> Self {
        value
    }
}

impl EqualsPercentage for LengthPercentage {
    fn equals_percentage(&self, v: CSSFloat) -> bool {
        match self.unpack() {
            Unpacked::Percentage(p) => p.0 == v,
            _ => false,
        }
    }
}

/// An unpacked `<length-percentage>` that borrows the `calc()` variant.
#[derive(Clone, Debug, PartialEq, ToCss, ToTyped)]
pub enum Unpacked<'a> {
    /// A `calc()` value
    Calc(&'a CalcLengthPercentage),
    /// A length value
    Length(Length),
    /// A percentage value
    Percentage(Percentage),
}

/// An unpacked `<length-percentage>` that mutably borrows the `calc()` variant.
enum UnpackedMut<'a> {
    Calc(&'a mut CalcLengthPercentage),
    Length(Length),
    Percentage(Percentage),
}

/// An unpacked `<length-percentage>` that owns the `calc()` variant, for
/// serialization purposes.
#[derive(Deserialize, PartialEq, Serialize)]
enum Serializable {
    Calc(CalcLengthPercentage),
    Length(Length),
    Percentage(Percentage),
}

impl LengthPercentage {
    /// 1px length value for SVG defaults
    #[inline]
    pub fn one() -> Self {
        Self::new_length(Length::new(1.))
    }

    /// 0%
    #[inline]
    pub fn zero_percent() -> Self {
        Self::new_percent(Percentage::zero())
    }

    /// 100%
    #[inline]
    pub fn hundred_percent() -> Self {
        Self::new_percent(Percentage::hundred())
    }

    fn to_calc_node(&self) -> CalcNode {
        match self.unpack() {
            Unpacked::Length(l) => CalcNode::Leaf(CalcLengthPercentageLeaf::Length(l)),
            Unpacked::Percentage(p) => CalcNode::Leaf(CalcLengthPercentageLeaf::Percentage(p)),
            Unpacked::Calc(p) => p.node.clone(),
        }
    }

    fn map_lengths(&self, mut map_fn: impl FnMut(Length) -> Length) -> Self {
        match self.unpack() {
            Unpacked::Length(l) => Self::new_length(map_fn(l)),
            Unpacked::Percentage(p) => Self::new_percent(p),
            Unpacked::Calc(lp) => Self::new_calc_unchecked(Box::new(CalcLengthPercentage {
                clamping_mode: lp.clamping_mode,
                node: lp.node.map_leaves(|leaf| match *leaf {
                    CalcLengthPercentageLeaf::Length(ref l) => {
                        CalcLengthPercentageLeaf::Length(map_fn(*l))
                    },
                    ref l => l.clone(),
                }),
            })),
        }
    }

    /// Constructs a length value.
    #[inline]
    pub fn new_length(length: Length) -> Self {
        Self(NumericUnion::inline(
            LengthPercentageTag::Length,
            length.px(),
        ))
    }

    /// Constructs a percentage value.
    #[inline]
    pub fn new_percent(percentage: Percentage) -> Self {
        Self(NumericUnion::inline(
            LengthPercentageTag::Percentage,
            percentage.0,
        ))
    }

    /// Given a `LengthPercentage` value `v`, construct the value representing
    /// `calc(100% - v)`.
    pub fn hundred_percent_minus(v: Self, clamping_mode: AllowedNumericType) -> Self {
        // TODO: This could in theory take ownership of the calc node in `v` if
        // possible instead of cloning.
        let mut node = v.to_calc_node();
        node.negate();

        let new_node = CalcNode::Sum(
            vec![
                CalcNode::Leaf(CalcLengthPercentageLeaf::Percentage(Percentage::hundred())),
                node,
            ]
            .into(),
        );

        Self::new_calc(new_node, clamping_mode)
    }

    /// Given a list of `LengthPercentage` values, construct the value representing
    /// `calc(100% - the sum of the list)`.
    pub fn hundred_percent_minus_list(list: &[&Self], clamping_mode: AllowedNumericType) -> Self {
        let mut new_list = vec![CalcNode::Leaf(CalcLengthPercentageLeaf::Percentage(
            Percentage::hundred(),
        ))];

        for lp in list.iter() {
            let mut node = lp.to_calc_node();
            node.negate();
            new_list.push(node)
        }

        Self::new_calc(CalcNode::Sum(new_list.into()), clamping_mode)
    }

    /// Constructs a `calc()` value.
    #[inline]
    pub fn new_calc(mut node: CalcNode, clamping_mode: AllowedNumericType) -> Self {
        node.simplify_and_sort();

        match node {
            CalcNode::Leaf(l) => {
                return match l {
                    CalcLengthPercentageLeaf::Length(l) => {
                        Self::new_length(Length::new(clamping_mode.clamp(l.px())).normalized())
                    },
                    CalcLengthPercentageLeaf::Percentage(p) => Self::new_percent(Percentage(
                        clamping_mode.clamp(crate::values::normalize(p.0)),
                    )),
                    CalcLengthPercentageLeaf::Number(number) => {
                        debug_assert!(
                            false,
                            "The final result of a <length-percentage> should never be a number"
                        );
                        Self::new_length(Length::new(number))
                    },
                };
            },
            _ => Self::new_calc_unchecked(Box::new(CalcLengthPercentage {
                clamping_mode,
                node,
            })),
        }
    }

    /// Private version of new_calc() that constructs a calc() variant without
    /// checking.
    fn new_calc_unchecked(calc: Box<CalcLengthPercentage>) -> Self {
        Self(NumericUnion::boxed(calc))
    }

    #[inline]
    fn unpack_mut<'a>(&'a mut self) -> UnpackedMut<'a> {
        match self.0.unpack_mut() {
            tagged::UnpackedMut::Boxed(calc) => UnpackedMut::Calc(calc),
            tagged::UnpackedMut::Inline(t, n) => match *t {
                LengthPercentageTag::Length => UnpackedMut::Length(Length::new(*n)),
                LengthPercentageTag::Percentage => UnpackedMut::Percentage(Percentage(*n)),
            },
        }
    }

    /// Unpack the tagged pointer representation of a length-percentage into an enum
    /// representation with separate tag and value.
    #[inline]
    pub fn unpack<'a>(&'a self) -> Unpacked<'a> {
        match self.0.unpack() {
            tagged::Unpacked::Boxed(calc) => Unpacked::Calc(calc),
            tagged::Unpacked::Inline(LengthPercentageTag::Length, v) => {
                Unpacked::Length(Length::new(v))
            },
            tagged::Unpacked::Inline(LengthPercentageTag::Percentage, v) => {
                Unpacked::Percentage(Percentage(v))
            },
        }
    }

    #[inline]
    fn to_serializable(&self) -> Serializable {
        match self.unpack() {
            Unpacked::Calc(c) => Serializable::Calc(c.clone()),
            Unpacked::Length(l) => Serializable::Length(l),
            Unpacked::Percentage(p) => Serializable::Percentage(p),
        }
    }

    #[inline]
    fn from_serializable(s: Serializable) -> Self {
        match s {
            Serializable::Calc(c) => Self::new_calc_unchecked(Box::new(c)),
            Serializable::Length(l) => Self::new_length(l),
            Serializable::Percentage(p) => Self::new_percent(p),
        }
    }

    /// Resolves the percentage.
    #[inline]
    pub fn resolve(&self, basis: Length) -> Length {
        match self.unpack() {
            Unpacked::Length(l) => l,
            Unpacked::Percentage(p) => (basis * p.0).normalized(),
            Unpacked::Calc(ref c) => c.resolve(basis),
        }
    }

    /// Resolves the percentage. Just an alias of resolve().
    #[inline]
    pub fn percentage_relative_to(&self, basis: Length) -> Length {
        self.resolve(basis)
    }

    /// Return whether there's any percentage in this value.
    #[inline]
    pub fn has_percentage(&self) -> bool {
        match self.unpack() {
            Unpacked::Length(..) => false,
            Unpacked::Percentage(..) | Unpacked::Calc(..) => true,
        }
    }

    /// Converts to a `<length>` if possible.
    pub fn to_length(&self) -> Option<Length> {
        match self.unpack() {
            Unpacked::Length(l) => Some(l),
            Unpacked::Percentage(..) | Unpacked::Calc(..) => {
                debug_assert!(self.has_percentage());
                return None;
            },
        }
    }

    /// Converts to a `<percentage>` if possible.
    #[inline]
    pub fn to_percentage(&self) -> Option<Percentage> {
        match self.unpack() {
            Unpacked::Percentage(p) => Some(p),
            Unpacked::Length(..) | Unpacked::Calc(..) => None,
        }
    }

    /// Converts to a `<percentage>` with given basis. Returns None if the basis is 0.
    #[inline]
    pub fn to_percentage_of(&self, basis: Length) -> Option<Percentage> {
        if basis.px() == 0. {
            return None;
        }
        Some(match self.unpack() {
            Unpacked::Length(l) => Percentage(l.px() / basis.px()),
            Unpacked::Percentage(p) => p,
            Unpacked::Calc(ref c) => Percentage(c.resolve(basis).px() / basis.px()),
        })
    }

    /// Returns the used value.
    #[inline]
    pub fn to_used_value(&self, containing_length: Au) -> Au {
        let length = self.to_pixel_length(containing_length);
        if let Unpacked::Percentage(_) = self.unpack() {
            return Au::from_f32_px_trunc(length.px());
        }
        Au::from(length)
    }

    /// Returns the used value as CSSPixelLength.
    #[inline]
    pub fn to_pixel_length(&self, containing_length: Au) -> Length {
        self.resolve(containing_length.into())
    }

    /// Convert the computed value into used value.
    #[inline]
    pub fn maybe_to_used_value(&self, container_len: Option<Au>) -> Option<Au> {
        self.maybe_percentage_relative_to(container_len.map(Length::from))
            .map(if let Unpacked::Percentage(_) = self.unpack() {
                |length: Length| Au::from_f32_px_trunc(length.px())
            } else {
                Au::from
            })
    }

    /// If there are special rules for computing percentages in a value (e.g.
    /// the height property), they apply whenever a calc() expression contains
    /// percentages.
    pub fn maybe_percentage_relative_to(&self, container_len: Option<Length>) -> Option<Length> {
        if let Unpacked::Length(l) = self.unpack() {
            return Some(l);
        }
        Some(self.resolve(container_len?))
    }
}

impl ClampToNonNegative for LengthPercentage {
    /// Returns the clamped non-negative values.
    #[inline]
    fn clamp_to_non_negative(mut self) -> Self {
        match self.unpack_mut() {
            UnpackedMut::Length(l) => Self::new_length(l.clamp_to_non_negative()),
            UnpackedMut::Percentage(p) => Self::new_percent(p.clamp_to_non_negative()),
            UnpackedMut::Calc(ref mut c) => {
                c.clamping_mode = AllowedNumericType::NonNegative;
                self
            },
        }
    }
}

impl PartialEq for LengthPercentage {
    fn eq(&self, other: &Self) -> bool {
        self.unpack() == other.unpack()
    }
}

impl fmt::Debug for LengthPercentage {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        self.unpack().fmt(formatter)
    }
}

impl ToAnimatedZero for LengthPercentage {
    fn to_animated_zero(&self) -> Result<Self, ()> {
        Ok(match self.unpack() {
            Unpacked::Length(l) => Self::new_length(l.to_animated_zero()?),
            Unpacked::Percentage(p) => Self::new_percent(p.to_animated_zero()?),
            Unpacked::Calc(c) => Self::new_calc_unchecked(Box::new(c.to_animated_zero()?)),
        })
    }
}

impl Clone for LengthPercentage {
    fn clone(&self) -> Self {
        match self.unpack() {
            Unpacked::Length(l) => Self::new_length(l),
            Unpacked::Percentage(p) => Self::new_percent(p),
            Unpacked::Calc(c) => Self::new_calc_unchecked(Box::new(c.clone())),
        }
    }
}

impl ToComputedValue for specified::LengthPercentage {
    type ComputedValue = LengthPercentage;

    fn to_computed_value(&self, context: &Context) -> LengthPercentage {
        match *self {
            specified::LengthPercentage::Length(ref value) => {
                LengthPercentage::new_length(value.to_computed_value(context))
            },
            specified::LengthPercentage::Percentage(value) => {
                LengthPercentage::new_percent(value.to_computed_value(context))
            },
            specified::LengthPercentage::Calc(ref calc) => (**calc).to_computed_value(context),
        }
    }

    fn from_computed_value(computed: &LengthPercentage) -> Self {
        match computed.unpack() {
            Unpacked::Length(ref l) => {
                specified::LengthPercentage::Length(ToComputedValue::from_computed_value(l))
            },
            Unpacked::Percentage(p) => {
                specified::LengthPercentage::Percentage(NoCalcPercentage::new(p.0))
            },
            Unpacked::Calc(c) => {
                // We simplify before constructing the LengthPercentage if
                // needed, so this is always fine.
                specified::LengthPercentage::Calc(Box::new(
                    specified::CalcLengthPercentage::from_computed_value(c),
                ))
            },
        }
    }
}

impl ComputeSquaredDistance for LengthPercentage {
    #[inline]
    fn compute_squared_distance(&self, other: &Self) -> Result<SquaredDistance, ()> {
        // A somewhat arbitrary base, it doesn't really make sense to mix
        // lengths with percentages, but we can't do much better here, and this
        // ensures that the distance between length-only and percentage-only
        // lengths makes sense.
        let basis = Length::new(100.);
        self.resolve(basis)
            .compute_squared_distance(&other.resolve(basis))
    }
}

impl ToCss for LengthPercentage {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        self.unpack().to_css(dest)
    }
}

impl ToTyped for LengthPercentage {
    fn to_typed(&self, dest: &mut ThinVec<TypedValue>) -> Result<(), ()> {
        self.unpack().to_typed(dest)
    }
}

impl Zero for LengthPercentage {
    fn zero() -> Self {
        LengthPercentage::new_length(Length::zero())
    }

    /// Returns true if the computed value is absolute 0 or 0%.
    #[inline]
    fn is_zero(&self) -> bool {
        match self.unpack() {
            Unpacked::Length(l) => l.px() == 0.0,
            Unpacked::Percentage(p) => p.0 == 0.0,
            Unpacked::Calc(..) => false,
        }
    }
}

impl ZeroNoPercent for LengthPercentage {
    #[inline]
    fn is_zero_no_percent(&self) -> bool {
        self.to_length().is_some_and(|l| l.px() == 0.0)
    }
}

impl Serialize for LengthPercentage {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_serializable().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for LengthPercentage {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self::from_serializable(Serializable::deserialize(
            deserializer,
        )?))
    }
}

/// The leaves of a `<length-percentage>` calc expression.
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
pub enum CalcLengthPercentageLeaf {
    Length(Length),
    Percentage(Percentage),
    Number(f32),
}

impl CalcLengthPercentageLeaf {
    fn is_zero_length(&self) -> bool {
        match *self {
            Self::Length(ref l) => l.is_zero(),
            Self::Percentage(..) => false,
            Self::Number(..) => false,
        }
    }
}

impl calc::CalcNodeLeaf for CalcLengthPercentageLeaf {
    fn unit(&self) -> CalcUnits {
        match self {
            Self::Length(_) => CalcUnits::LENGTH,
            Self::Percentage(_) => CalcUnits::PERCENTAGE,
            Self::Number(_) => CalcUnits::empty(),
        }
    }

    fn unitless_value(&self) -> Option<f32> {
        Some(match *self {
            Self::Length(ref l) => l.px(),
            Self::Percentage(ref p) => p.0,
            Self::Number(n) => n,
        })
    }

    fn new_number(value: f32) -> Self {
        Self::Number(value)
    }

    fn as_number(&self) -> Option<f32> {
        match *self {
            Self::Length(_) | Self::Percentage(_) => None,
            Self::Number(value) => Some(value),
        }
    }

    fn compare(&self, other: &Self, basis: PositivePercentageBasis) -> Option<std::cmp::Ordering> {
        use self::CalcLengthPercentageLeaf::*;
        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return None;
        }

        if matches!(self, Percentage(..)) && matches!(basis, PositivePercentageBasis::Unknown) {
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
            (&Length(ref one), &Length(ref other)) => one.partial_cmp(other),
            (&Percentage(ref one), &Percentage(ref other)) => one.partial_cmp(other),
            (&Number(ref one), &Number(ref other)) => one.partial_cmp(other),
            _ => unsafe {
                match *self {
                    Length(..) | Percentage(..) | Number(..) => {},
                }
                debug_unreachable!("Forgot to handle unit in compare()")
            },
        }
    }

    fn try_sum_in_place(&mut self, other: &Self) -> Result<(), ()> {
        use self::CalcLengthPercentageLeaf::*;

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
            (&mut Length(ref mut one), &Length(ref other)) => {
                *one += *other;
            },
            (&mut Percentage(ref mut one), &Percentage(ref other)) => {
                one.0 += other.0;
            },
            (&mut Number(ref mut one), &Number(ref other)) => {
                *one += *other;
            },
            _ => unsafe {
                match *other {
                    Length(..) | Percentage(..) | Number(..) => {},
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
        use self::CalcLengthPercentageLeaf::*;
        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return Err(());
        }
        Ok(match (self, other) {
            (&Length(ref one), &Length(ref other)) => {
                Length(super::Length::new(op(one.px(), other.px())))
            },
            (&Percentage(one), &Percentage(other)) => {
                Self::Percentage(super::Percentage(op(one.0, other.0)))
            },
            (&Number(one), &Number(other)) => Self::Number(op(one, other)),
            _ => unsafe {
                match *self {
                    Length(..) | Percentage(..) | Number(..) => {},
                }
                debug_unreachable!("Forgot to handle unit in try_op()")
            },
        })
    }

    fn map(&mut self, mut op: impl FnMut(f32) -> f32) -> Result<(), ()> {
        Ok(match self {
            Self::Length(value) => {
                *value = Length::new(op(value.px()));
            },
            Self::Percentage(value) => {
                *value = Percentage(op(value.0));
            },
            Self::Number(value) => {
                *value = op(*value);
            },
        })
    }

    fn simplify(&mut self) {}

    fn sort_key(&self) -> calc::SortKey {
        match *self {
            Self::Length(..) => calc::SortKey::Px,
            Self::Percentage(..) => calc::SortKey::Percentage,
            Self::Number(..) => calc::SortKey::Number,
        }
    }
}

/// The computed version of a calc() node for `<length-percentage>` values.
pub type CalcNode = calc::GenericCalcNode<CalcLengthPercentageLeaf>;

/// The representation of a calc() function with mixed lengths and percentages.
#[derive(
    Clone,
    Debug,
    Deserialize,
    MallocSizeOf,
    Serialize,
    ToAnimatedZero,
    ToResolvedValue,
    ToCss,
    ToTyped,
)]
#[repr(C)]
pub struct CalcLengthPercentage {
    #[animation(constant)]
    #[css(skip)]
    clamping_mode: AllowedNumericType,
    node: CalcNode,
}

/// Type for anchor side in `calc()` and other math fucntions.
pub type CalcAnchorSide = GenericAnchorSide<Box<CalcNode>>;

/// Result of resolving `CalcLengthPercentage`
pub struct CalcLengthPercentageResolution {
    /// The resolved length.
    pub result: Length,
    /// Did the resolution of this calc node require resolving percentages?
    pub percentage_used: bool,
}

/// What anchor positioning functions are allowed to resolve in calc percentage
/// values.
#[repr(C)]
#[derive(Clone, Copy)]
pub enum AllowAnchorPosResolutionInCalcPercentage {
    /// Both `anchor()` and `anchor-size()` are valid and should be resolved.
    Both(PhysicalSide),
    /// Only `anchor-size()` is valid and should be resolved.
    AnchorSizeOnly(PhysicalAxis),
}

impl AllowAnchorPosResolutionInCalcPercentage {
    #[cfg(feature = "gecko")]
    /// Get the `anchor-size()` resolution axis.
    pub fn to_axis(&self) -> PhysicalAxis {
        match self {
            Self::AnchorSizeOnly(axis) => *axis,
            Self::Both(side) => {
                if matches!(side, PhysicalSide::Top | PhysicalSide::Bottom) {
                    PhysicalAxis::Vertical
                } else {
                    PhysicalAxis::Horizontal
                }
            },
        }
    }
}

impl From<&CalcAnchorSide> for AnchorSide {
    fn from(value: &CalcAnchorSide) -> Self {
        match value {
            CalcAnchorSide::Keyword(k) => Self::Keyword(*k),
            CalcAnchorSide::Percentage(p) => {
                if let CalcNode::Leaf(CalcLengthPercentageLeaf::Percentage(p)) = **p {
                    Self::Percentage(p)
                } else {
                    unreachable!("Should have parsed simplified percentage.");
                }
            },
        }
    }
}

impl CalcLengthPercentage {
    /// Resolves the percentage.
    #[inline]
    pub fn resolve(&self, basis: Length) -> Length {
        // unwrap() is fine because the conversion below is infallible.
        if let CalcLengthPercentageLeaf::Length(px) = self
            .node
            .resolve_map(|leaf| {
                Ok(if let CalcLengthPercentageLeaf::Percentage(p) = leaf {
                    CalcLengthPercentageLeaf::Length(Length::new(basis.px() * p.0))
                } else {
                    leaf.clone()
                })
            })
            .unwrap()
        {
            Length::new(self.clamping_mode.clamp(px.px())).normalized()
        } else {
            unreachable!("resolve_map should turn percentages to lengths, and parsing should ensure that we don't end up with a number");
        }
    }

    /// Return a clone of this node with all anchor functions computed and replaced with
    /// corresponding values, returning error if the resolution is invalid.
    #[inline]
    #[cfg(feature = "gecko")]
    pub fn resolve_anchor(
        &self,
        allowed: AllowAnchorPosResolutionInCalcPercentage,
        params: &AnchorPosOffsetResolutionParams,
    ) -> Result<(CalcNode, AllowedNumericType), ()> {
        use crate::values::{
            computed::{length::resolve_anchor_size, AnchorFunction},
            generics::{length::GenericAnchorSizeFunction, position::GenericAnchorFunction},
        };

        fn resolve_anchor_function<'a>(
            f: &'a GenericAnchorFunction<
                Box<CalcNode>,
                Box<GenericAnchorFunctionFallback<CalcLengthPercentageLeaf>>,
            >,
            side: PhysicalSide,
            params: &AnchorPosOffsetResolutionParams,
        ) -> AnchorResolutionResult<'a, Box<CalcNode>> {
            let anchor_side: &CalcAnchorSide = &f.side;
            let resolved = if f.valid_for(side, params.mBaseParams.mPosition) {
                AnchorFunction::resolve(&f.target_element, &anchor_side.into(), side, params).ok()
            } else {
                None
            };

            resolved.map_or_else(
                || {
                    let Some(fb) = f.fallback.as_ref() else {
                        return AnchorResolutionResult::Invalid;
                    };
                    let mut node = Box::new(fb.node.clone());
                    let result = node.map_node(|node| {
                        resolve_anchor_functions(
                            node,
                            AllowAnchorPosResolutionInCalcPercentage::Both(side),
                            params,
                        )
                    });
                    if result.is_err() {
                        return AnchorResolutionResult::Invalid;
                    }
                    AnchorResolutionResult::Resolved(node)
                },
                |v| {
                    AnchorResolutionResult::Resolved(Box::new(CalcNode::Leaf(
                        CalcLengthPercentageLeaf::Length(v),
                    )))
                },
            )
        }

        fn resolve_anchor_size_function<'a>(
            f: &'a GenericAnchorSizeFunction<
                Box<GenericAnchorFunctionFallback<CalcLengthPercentageLeaf>>,
            >,
            allowed: AllowAnchorPosResolutionInCalcPercentage,
            params: &AnchorPosOffsetResolutionParams,
        ) -> AnchorResolutionResult<'a, Box<CalcNode>> {
            let axis = allowed.to_axis();
            let resolved = if f.valid_for(params.mBaseParams.mPosition) {
                resolve_anchor_size(&f.target_element, axis, f.size, &params.mBaseParams).ok()
            } else {
                None
            };

            resolved.map_or_else(
                || {
                    let Some(fb) = f.fallback.as_ref() else {
                        return AnchorResolutionResult::Invalid;
                    };
                    let mut node = Box::new(fb.node.clone());
                    let result =
                        node.map_node(|node| resolve_anchor_functions(node, allowed, params));
                    if result.is_err() {
                        return AnchorResolutionResult::Invalid;
                    }
                    AnchorResolutionResult::Resolved(node)
                },
                |v| {
                    AnchorResolutionResult::Resolved(Box::new(CalcNode::Leaf(
                        CalcLengthPercentageLeaf::Length(v),
                    )))
                },
            )
        }

        fn resolve_anchor_functions(
            node: &CalcNode,
            allowed: AllowAnchorPosResolutionInCalcPercentage,
            params: &AnchorPosOffsetResolutionParams,
        ) -> Result<Option<CalcNode>, ()> {
            let resolution = match node {
                CalcNode::Anchor(f) => {
                    let prop_side = match allowed {
                        AllowAnchorPosResolutionInCalcPercentage::Both(side) => side,
                        AllowAnchorPosResolutionInCalcPercentage::AnchorSizeOnly(_) => {
                            unreachable!("anchor() found where disallowed")
                        },
                    };
                    resolve_anchor_function(f, prop_side, params)
                },
                CalcNode::AnchorSize(f) => resolve_anchor_size_function(f, allowed, params),
                _ => return Ok(None),
            };

            match resolution {
                AnchorResolutionResult::Invalid => Err(()),
                AnchorResolutionResult::Fallback(fb) => {
                    // TODO(dshin, bug 1923759): At least for now, fallbacks should not contain any anchor function.
                    Ok(Some(*fb.clone()))
                },
                AnchorResolutionResult::Resolved(v) => Ok(Some(*v.clone())),
            }
        }

        let mut node = self.node.clone();
        node.map_node(|node| resolve_anchor_functions(node, allowed, params))?;
        Ok((node, self.clamping_mode))
    }
}

// NOTE(emilio): We don't compare `clamping_mode` since we want to preserve the
// invariant that `from_computed_value(length).to_computed_value(..) == length`.
//
// Right now for e.g. a non-negative length, we set clamping_mode to `All`
// unconditionally for non-calc values, and to `NonNegative` for calc.
//
// If we determine that it's sound, from_computed_value() can generate an
// absolute length, which then would get `All` as the clamping mode.
//
// We may want to just eagerly-detect whether we can clamp in
// `LengthPercentage::new` and switch to `AllowedNumericType::NonNegative` then,
// maybe.
impl PartialEq for CalcLengthPercentage {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

impl specified::CalcLengthPercentage {
    /// Compute the value, zooming any absolute units by the zoom function.
    fn to_computed_value_with_zoom<F>(
        &self,
        context: &Context,
        zoom_fn: F,
        base_size: FontBaseSize,
        line_height_base: LineHeightBase,
    ) -> LengthPercentage
    where
        F: Fn(Length) -> Length,
    {
        use crate::values::specified::calc::Leaf;

        let node = self.0.node.map_leaves(|leaf| match *leaf {
            Leaf::Percentage(p) => CalcLengthPercentageLeaf::Percentage(Percentage(p.get())),
            Leaf::Length(l) => CalcLengthPercentageLeaf::Length({
                let result =
                    l.to_computed_value_with_base_size(context, base_size, line_height_base);
                if l.should_zoom_text() {
                    zoom_fn(result)
                } else {
                    result
                }
            }),
            Leaf::Number(n) => CalcLengthPercentageLeaf::Number(n.get()),
            Leaf::Angle(..) | Leaf::Time(..) | Leaf::Resolution(..) | Leaf::ColorComponent(..) => {
                unreachable!("Shouldn't have parsed")
            },
        });

        LengthPercentage::new_calc(node, self.0.clamping_mode)
    }

    /// Compute font-size or line-height taking into account text-zoom if necessary.
    pub fn to_computed_value_zoomed(
        &self,
        context: &Context,
        base_size: FontBaseSize,
        line_height_base: LineHeightBase,
    ) -> LengthPercentage {
        self.to_computed_value_with_zoom(
            context,
            |abs| context.maybe_zoom_text(abs),
            base_size,
            line_height_base,
        )
    }

    /// Compute the value into pixel length as CSSFloat without context,
    /// so it returns Err(()) if there is any non-absolute unit.
    pub fn to_computed_pixel_length_without_context(&self) -> Result<CSSFloat, ()> {
        use crate::values::specified::calc::Leaf;

        // Simplification should've turned this into an absolute length,
        // otherwise it wouldn't have been able to.
        match self.0.node {
            calc::CalcNode::Leaf(Leaf::Length(ref l)) => {
                l.to_computed_pixel_length_without_context()
            },
            _ => Err(()),
        }
    }

    /// Compute the value into pixel length as CSSFloat, using the get_font_metrics function
    /// if provided to resolve font-relative dimensions.
    #[cfg(feature = "gecko")]
    pub fn to_computed_pixel_length_with_font_metrics(
        &self,
        get_font_metrics: Option<impl Fn() -> GeckoFontMetrics>,
    ) -> Result<CSSFloat, ()> {
        use crate::values::specified::calc::Leaf;

        match self.0.node {
            calc::CalcNode::Leaf(Leaf::Length(ref l)) => {
                l.to_computed_pixel_length_with_font_metrics(get_font_metrics)
            },
            _ => Err(()),
        }
    }

    /// Compute the calc using the current font-size and line-height. (and without text-zoom).
    pub fn to_computed_value(&self, context: &Context) -> LengthPercentage {
        self.to_computed_value_with_zoom(
            context,
            |abs| abs,
            FontBaseSize::CurrentStyle,
            LineHeightBase::CurrentStyle,
        )
    }

    #[inline]
    fn from_computed_value(computed: &CalcLengthPercentage) -> Self {
        use crate::values::specified::calc::Leaf;
        use crate::values::specified::length::NoCalcLength;

        specified::CalcLengthPercentage(specified::CalcNumeric {
            clamping_mode: computed.clamping_mode,
            node: computed.node.map_leaves(|l| match l {
                CalcLengthPercentageLeaf::Length(ref l) => {
                    Leaf::Length(NoCalcLength::from_px(l.px()))
                },
                CalcLengthPercentageLeaf::Percentage(ref p) => {
                    Leaf::Percentage(NoCalcPercentage::new(p.0))
                },
                CalcLengthPercentageLeaf::Number(n) => Leaf::Number(NoCalcNumber::new(*n)),
            }),
        })
    }
}

/// https://drafts.csswg.org/css-transitions/#animtype-lpcalc
/// https://drafts.csswg.org/css-values-4/#combine-math
/// https://drafts.csswg.org/css-values-4/#combine-mixed
impl Animate for LengthPercentage {
    #[inline]
    fn animate(&self, other: &Self, procedure: Procedure) -> Result<Self, ()> {
        Ok(match (self.unpack(), other.unpack()) {
            (Unpacked::Length(one), Unpacked::Length(other)) => {
                Self::new_length(one.animate(&other, procedure)?)
            },
            (Unpacked::Percentage(one), Unpacked::Percentage(other)) => {
                Self::new_percent(one.animate(&other, procedure)?)
            },
            _ => {
                use calc::CalcNodeLeaf;

                fn product_with(mut node: CalcNode, product: f32) -> CalcNode {
                    let mut number = CalcNode::Leaf(CalcLengthPercentageLeaf::new_number(product));
                    if !node.try_product_in_place(&mut number) {
                        CalcNode::Product(vec![node, number].into())
                    } else {
                        node
                    }
                }

                let (l, r) = procedure.weights();
                let one = product_with(self.to_calc_node(), l as f32);
                let other = product_with(other.to_calc_node(), r as f32);

                Self::new_calc(
                    CalcNode::Sum(vec![one, other].into()),
                    AllowedNumericType::All,
                )
            },
        })
    }
}

/// A wrapper of LengthPercentage, whose value must be >= 0.
pub type NonNegativeLengthPercentage = NonNegative<LengthPercentage>;

impl NonNegativeLengthPercentage {
    /// Returns the used value.
    #[inline]
    pub fn to_used_value(&self, containing_length: Au) -> Au {
        let resolved = self.0.to_used_value(containing_length);
        std::cmp::max(resolved, Au(0))
    }

    /// Convert the computed value into used value.
    #[inline]
    pub fn maybe_to_used_value(&self, containing_length: Option<Au>) -> Option<Au> {
        let resolved = self.0.maybe_to_used_value(containing_length)?;
        Some(std::cmp::max(resolved, Au(0)))
    }
}

impl TryTacticAdjustment for LengthPercentage {
    fn try_tactic_adjustment(&mut self, old_side: PhysicalSide, new_side: PhysicalSide) {
        match self.unpack_mut() {
            UnpackedMut::Calc(calc) => calc.node.try_tactic_adjustment(old_side, new_side),
            UnpackedMut::Percentage(mut p) => {
                p.try_tactic_adjustment(old_side, new_side);
                *self = Self::new_percent(p);
            },
            UnpackedMut::Length(..) => {},
        }
    }
}

impl TryTacticAdjustment for GenericAnchorFunctionFallback<CalcLengthPercentageLeaf> {
    fn try_tactic_adjustment(&mut self, old_side: PhysicalSide, new_side: PhysicalSide) {
        self.node.try_tactic_adjustment(old_side, new_side)
    }
}

impl TryTacticAdjustment for CalcNode {
    fn try_tactic_adjustment(&mut self, old_side: PhysicalSide, new_side: PhysicalSide) {
        self.visit_depth_first(|node| match node {
            Self::Leaf(CalcLengthPercentageLeaf::Percentage(p)) => {
                p.try_tactic_adjustment(old_side, new_side)
            },
            Self::Anchor(a) => a.try_tactic_adjustment(old_side, new_side),
            Self::AnchorSize(a) => a.try_tactic_adjustment(old_side, new_side),
            _ => {},
        });
    }
}
