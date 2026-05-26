/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Typed OM.
//!
//! https://drafts.css-houdini.org/css-typed-om-1/

use crate::{One, Zero};
use app_units::Au;
use servo_arc::Arc;
use style_traits::CssString;
use thin_vec::ThinVec;

pub mod numeric_declaration;
pub mod numeric_values;
pub mod sum_value;

/// A single segment of an unparsed Typed OM value.
///
/// This corresponds to the `CSSUnparsedSegment` union in the Typed OM
/// specification. Unparsed values are represented as a list of string
/// fragments and variable references.
#[derive(Clone, Debug)]
#[repr(C)]
pub enum UnparsedSegment {
    /// A string fragment.
    ///
    /// This corresponds to the string branch of `CSSUnparsedSegment` and is
    /// used for the non-variable parts of a `CSSUnparsedValue`.
    String(CssString),

    /// A `var()` reference segment.
    ///
    /// This corresponds to `CSSVariableReferenceValue` in the Typed OM
    /// specification.
    VariableReference(VariableReferenceValue),
}

/// An unparsed value used by the Typed OM.
///
/// This corresponds to `CSSUnparsedValue` in the Typed OM specification. It
/// is used for values that cannot be reified into a more specific
/// property-agnostic representation and therefore need to preserve their
/// token-like structure as a sequence of string fragments and variable
/// references.
///
/// The underlying list of segments corresponds to the `[[tokens]]` internal
/// slot of `CSSUnparsedValue`.
///
/// This is represented as a type alias over `ThinVec<UnparsedSegment>` rather
/// than a dedicated struct. This avoids the need for additional wrapper types
/// when embedding unparsed values within other structures, while still
/// allowing recursive representations via the segment list.
pub type UnparsedValue = ThinVec<UnparsedSegment>;

/// A variable reference inside an unparsed Typed OM value.
///
/// This corresponds to `CSSVariableReferenceValue` in the Typed OM
/// specification.
#[derive(Clone, Debug)]
#[repr(C)]
pub struct VariableReferenceValue {
    /// The referenced custom property name.
    ///
    /// This corresponds to the `variable` attribute of
    /// `CSSVariableReferenceValue`.
    pub variable: CssString,

    /// The fallback value, if present.
    ///
    /// This corresponds to the `fallback` attribute of
    /// `CSSVariableReferenceValue`. When `has_fallback` is false, this value
    /// must be ignored. When `has_fallback` is true, this contains the
    /// fallback tokens (which may be empty).
    pub fallback: UnparsedValue,

    /// Whether a fallback was explicitly provided.
    ///
    /// This is needed to distinguish between the absence of a fallback
    /// (`var(--a)`) and an explicitly empty fallback (`var(--a,)`), which are
    /// observable via Typed OM.
    pub has_fallback: bool,
}

/// A keyword value used by the Typed OM.
///
/// This corresponds to `CSSKeywordValue` in the Typed OM specification.
/// The keyword is stored as a `CssString` so it can be represented and
/// transferred independently of any specific property (e.g. `"none"`,
/// `"block"`, `"thin"`).
#[derive(Clone, Debug)]
#[repr(C)]
pub struct KeywordValue(pub CssString);

/// A single numeric value with an associated unit.
///
/// This corresponds to `CSSUnitValue` in the Typed OM specification. The
/// numeric component is stored separately from the textual unit identifier.
#[derive(Clone, Debug)]
#[repr(C)]
pub struct UnitValue {
    /// The numeric component of the value.
    pub value: f32,

    /// The textual unit string (e.g. `"px"`, `"em"`, `"%"`, `"deg"`).
    pub unit: CssString,
}

/// A sum of numeric values.
///
/// This corresponds to `CSSMathSum` in the Typed OM specification. A sum
/// value represents an expression such as `10px + 2em`. Each entry is itself
/// a `NumericValue`, allowing nested sums if needed.
#[derive(Clone, Debug)]
#[repr(C)]
pub struct MathSum {
    /// The list of numeric terms that make up the sum.
    pub values: ThinVec<NumericValue>,
}

/// A numeric value used by the Typed OM.
///
/// This corresponds to `CSSNumericValue` and its subclasses in the Typed OM
/// specification. It represents numbers that can appear in CSS values,
/// including both simple unit quantities and composite expressions.
///
/// Unlike the parser-level representation, `NumericValue` is property-agnostic
/// and suitable for conversion to or from the `CSSNumericValue` family of DOM
/// objects.
#[derive(Clone, Debug)]
#[repr(C)]
pub enum NumericValue {
    /// A single numeric value with a concrete unit.
    ///
    /// This corresponds to `CSSUnitValue`.
    Unit(UnitValue),

    /// A sum of numeric values.
    ///
    /// This corresponds to `CSSMathSum`.
    Sum(MathSum),
}

impl NumericValue {
    /// Returns a zero pixel unit value.
    #[inline]
    pub fn zero_px() -> Self {
        Self::Unit(UnitValue {
            value: 0.0,
            unit: CssString::from("px"),
        })
    }
}

impl Zero for NumericValue {
    #[inline]
    fn zero() -> Self {
        Self::Unit(UnitValue {
            value: 0.0,
            unit: CssString::from("number"),
        })
    }

    #[inline]
    fn is_zero(&self) -> bool {
        match *self {
            Self::Unit(ref value) => value.value == 0.0,
            _ => false,
        }
    }
}

impl One for NumericValue {
    #[inline]
    fn one() -> Self {
        Self::Unit(UnitValue {
            value: 1.0,
            unit: CssString::from("number"),
        })
    }

    #[inline]
    fn is_one(&self) -> bool {
        match *self {
            Self::Unit(ref value) => value.value == 1.0,
            _ => false,
        }
    }
}

/// A single transform component used by the Typed OM.
///
/// This corresponds to `CSSTransformComponent` in the Typed OM specification.
/// Each variant represents one concrete transform component subclass.
#[derive(Clone, Debug)]
#[repr(C)]
pub enum TransformComponent {
    /// Temporary marker to satisfy `#[repr(C)]`. This will be replaced by
    /// concrete value kinds as transform component reification support
    /// expands.
    Placeholder(bool),
}

/// A transform value used by the Typed OM.
///
/// This corresponds to `CSSTransformValue` in the Typed OM specification. It
/// represents a `<transform-list>` as an ordered list of transform components.
pub type TransformValue = ThinVec<TransformComponent>;

/// A property-agnostic representation of a value, used by Typed OM.
///
/// `TypedValue` is the internal counterpart of the various `CSSStyleValue`
/// subclasses defined by the Typed OM specification. It captures values that
/// can be represented independently of any particular property.
#[derive(Clone, Debug)]
#[repr(C)]
pub enum TypedValue {
    /// An unparsed value consisting of string fragments and variable
    /// references.
    ///
    /// This corresponds to `CSSUnparsedValue` in the Typed OM specification.
    Unparsed(UnparsedValue),

    /// A keyword value such as `"block"`, `"none"`, or `"thin"`.
    ///
    /// This corresponds to `CSSKeywordValue` in the Typed OM specification.
    /// Keywords are represented as a standalone `KeywordValue` so they can
    /// be carried and compared independently of any particular property.
    Keyword(KeywordValue),

    /// A numeric value such as a length, angle, time, or a sum thereof.
    ///
    /// This corresponds to the `CSSNumericValue` hierarchy in the Typed OM
    /// specification, including `CSSUnitValue` and `CSSMathSum`.
    Numeric(NumericValue),

    /// A transform value such as `translate(10px, 20px)`.
    ///
    /// This corresponds to `CSSTransformValue` in the Typed OM specification.
    Transform(TransformValue),
}

/// A list of property-agnostic values used by the Typed OM.
///
/// `TypedValueList` is the internal counterpart of CSS value lists exposed by
/// Typed OM. It stores one or more [`TypedValue`] items in source order and
/// is used when a value reifies to multiple property-agnostic components.
#[derive(Clone, Debug)]
#[repr(C)]
pub struct TypedValueList {
    /// The list of reified values.
    pub values: ThinVec<TypedValue>,
}

/// Reifies a value into its Typed OM representation.
///
/// This trait is the Typed OM analogue of [`ToCss`]. Instead of serializing
/// values into CSS syntax, it converts them into [`TypedValue`]s that can be
/// exposed to the DOM as `CSSStyleValue` subclasses.
///
/// Most consumers should use [`ToTyped::to_typed_value`] or
/// [`ToTyped::to_typed_value_list`], depending on whether they need a single
/// reified value or the full list of reified values.
///
/// This trait is derivable with `#[derive(ToTyped)]`. The derived
/// implementation currently supports:
///
/// * Keyword enums: Enums whose variants are all unit variants are
///   automatically reified as [`TypedValue::Keyword`], using the same
///   serialization logic as [`ToCss`].
///
/// * Structs and data-carrying variants: By default, the derive attempts to
///   call `.to_typed()` recursively on supported fields or variant payloads,
///   producing [`TypedValue`]s when possible. This recursion can be disabled
///   with `#[typed(skip_derive_fields)]`.
///
/// * Other cases: If no automatic mapping is defined, or recursion is
///   explicitly disabled, the derived implementation falls back to the
///   default method (which returns `Err(())`, and thus `to_typed_value()`
///   returns `None`).
///
/// Over time, the derive may be extended to handle additional CSS value
/// categories such as numeric, color, and transform types.
///
/// Summary of derive attributes recognized by `#[derive(ToTyped)]`:
///
/// * `#[typed(skip_derive_fields)]` on the type disables recursion for
///   structs and data-carrying enum variants.
///
/// * `#[css(skip)]`, `#[typed(skip)]`, or `#[typed(todo)]` on a variant cause
///   that variant to be treated as unsupported (the derived implementation
///   returns `Err(())`).
///
/// * `#[css(skip)]` on a field causes that field to be ignored during
///   reification.
///
/// * `#[css(skip_if = "...")]` / `#[typed(skip_if = "...")]` on a field
///   conditionally disables reification for that field. If the provided
///   function returns `true` for the field value, the field is ignored.
///
/// * `#[css(contextual_skip_if = "...")]` /
///   `#[typed(contextual_skip_if = "...")]` on a field conditionally disables
///   reification for that field. The provided function is called with all
///   fields in the current struct or variant. If it returns `true`, the field
///   is ignored.
///
///   Typed skip annotations override CSS skip annotations when both are
///   present.
///
/// * `#[css(keyword = "...")]` on a unit variant overrides the keyword that
///   would otherwise be derived from the Rust identifier.
///
/// * `#[css(comma)]` on the variant indicates that supported fields may reify
///   to multiple separate values. When this attribute is present, multiple
///   [`TypedValue`] items may be produced, unless
///   `#[typed(no_multiple_values)]` is also present. If multiple values are
///   not allowed and the derived implementation would produce more than one
///   item, it returns `Err(())`.
///
/// * `#[typed(no_multiple_values)]` on a variant prevents it from reifying to
///   multiple [`TypedValue`] items, even if `#[css(comma)]` is present.
///
/// * `#[css(iterable)]` on a field indicates that the field represents a list
///   of values. Each item in the iterable is reified individually by calling
///   `ToTyped::to_typed` on the element type.
///
/// * `#[css(if_empty = "...")]` on an iterable field specifies a keyword
///   value that should be produced when the iterable is empty.
///
/// * `#[css(represents_keyword)]` on a bool field causes the field name to be
///   reified as a keyword when the field is true.
pub trait ToTyped {
    /// Attempt to convert `self` into one or more [`TypedValue`] items.
    ///
    /// Implementations append any resulting values to `dest`. This is the
    /// low-level entry point used by the Typed OM reification infrastructure.
    /// Most callers should prefer [`ToTyped::to_typed_value`] or
    /// [`ToTyped::to_typed_value_list`].
    ///
    /// Returning `Err(())` indicates that the value cannot be represented as
    /// a property-agnostic Typed OM value.
    fn to_typed(&self, _dest: &mut ThinVec<TypedValue>) -> Result<(), ()> {
        Err(())
    }

    /// Attempt to convert `self` into a [`TypedValue`].
    ///
    /// Returns the first reified value as `Some(TypedValue)` if the value can
    /// be reified into a property-agnostic CSSStyleValue subclass. Returns
    /// `None` if the value is unrepresentable, in which case consumers
    /// produce a property-tied CSSStyleValue instead.
    fn to_typed_value(&self) -> Option<TypedValue> {
        let mut dest = ThinVec::new();
        self.to_typed(&mut dest).ok()?;
        dest.into_iter().next()
    }

    /// Attempt to convert `self` into a [`NumericValue`].
    ///
    /// Returns `Some(NumericValue)` if the value reifies to a single
    /// `TypedValue::Numeric` item. Returns `None` otherwise.
    fn to_numeric_value(&self) -> Option<NumericValue> {
        match self.to_typed_value()? {
            TypedValue::Numeric(value) => Some(value),
            _ => None,
        }
    }

    /// Attempt to convert `self` into a [`TypedValueList`].
    ///
    /// Returns `Some(TypedValueList)` if the value can be reified into one or
    /// more property-agnostic Typed OM values. Returns `None` if the value is
    /// unrepresentable, in which case consumers produce a property-tied
    /// `CSSStyleValue` instead.
    fn to_typed_value_list(&self) -> Option<TypedValueList> {
        let mut dest = ThinVec::new();
        self.to_typed(&mut dest).ok()?;
        Some(TypedValueList { values: dest })
    }
}

impl<'a, T> ToTyped for &'a T
where
    T: ToTyped + ?Sized,
{
    fn to_typed(&self, dest: &mut ThinVec<TypedValue>) -> Result<(), ()> {
        (*self).to_typed(dest)
    }
}

impl<T> ToTyped for Box<T>
where
    T: ?Sized + ToTyped,
{
    fn to_typed(&self, dest: &mut ThinVec<TypedValue>) -> Result<(), ()> {
        (**self).to_typed(dest)
    }
}

impl<T> ToTyped for Arc<T>
where
    T: ?Sized + ToTyped,
{
    fn to_typed(&self, dest: &mut ThinVec<TypedValue>) -> Result<(), ()> {
        (**self).to_typed(dest)
    }
}

impl ToTyped for Au {
    fn to_typed(&self, dest: &mut ThinVec<TypedValue>) -> Result<(), ()> {
        let value = self.to_f32_px();
        let unit = CssString::from("px");
        dest.push(TypedValue::Numeric(NumericValue::Unit(UnitValue {
            value,
            unit,
        })));
        Ok(())
    }
}

macro_rules! impl_to_typed_for_predefined_type {
    ($name: ty) => {
        impl<'a> ToTyped for $name {
            fn to_typed(&self, dest: &mut ThinVec<TypedValue>) -> Result<(), ()> {
                dest.push(TypedValue::Numeric(NumericValue::Unit(UnitValue {
                    value: *self as f32,
                    unit: CssString::from("number"),
                })));
                Ok(())
            }
        }
    };
}

impl_to_typed_for_predefined_type!(f32);
impl_to_typed_for_predefined_type!(i8);
impl_to_typed_for_predefined_type!(i32);
