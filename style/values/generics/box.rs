/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Generic types for box properties.

use crate::values::animated::ToAnimatedZero;
use crate::Zero;
use std::fmt::{self, Write};
use style_traits::{CssWriter, ToCss};

#[derive(
    Animate,
    Clone,
    ComputeSquaredDistance,
    Copy,
    Debug,
    FromPrimitive,
    MallocSizeOf,
    Parse,
    PartialEq,
    SpecifiedValueInfo,
    ToAnimatedValue,
    ToComputedValue,
    ToCss,
    ToResolvedValue,
    ToShmem,
)]
#[repr(u8)]
#[allow(missing_docs)]
pub enum VerticalAlignKeyword {
    Baseline,
    Sub,
    Super,
    Top,
    TextTop,
    Middle,
    Bottom,
    TextBottom,
    #[cfg(feature = "gecko")]
    MozMiddleWithBaseline,
}

/// A generic value for the `vertical-align` property.
#[derive(
    Animate,
    Clone,
    ComputeSquaredDistance,
    Copy,
    Debug,
    MallocSizeOf,
    PartialEq,
    SpecifiedValueInfo,
    ToAnimatedValue,
    ToComputedValue,
    ToCss,
    ToResolvedValue,
    ToShmem,
    ToTyped,
)]
#[repr(C, u8)]
pub enum GenericVerticalAlign<LengthPercentage> {
    /// One of the vertical-align keywords.
    Keyword(VerticalAlignKeyword),
    /// `<length-percentage>`
    Length(LengthPercentage),
}

pub use self::GenericVerticalAlign as VerticalAlign;

impl<L> VerticalAlign<L> {
    /// Returns `baseline`.
    #[inline]
    pub fn baseline() -> Self {
        VerticalAlign::Keyword(VerticalAlignKeyword::Baseline)
    }
}

impl<L> ToAnimatedZero for VerticalAlign<L> {
    fn to_animated_zero(&self) -> Result<Self, ()> {
        Err(())
    }
}

/// https://drafts.csswg.org/css-sizing-4/#intrinsic-size-override
#[derive(
    Animate,
    Clone,
    ComputeSquaredDistance,
    Debug,
    MallocSizeOf,
    PartialEq,
    SpecifiedValueInfo,
    ToComputedValue,
    ToAnimatedValue,
    ToAnimatedZero,
    ToResolvedValue,
    ToShmem,
    ToTyped,
)]
#[value_info(other_values = "auto")]
#[repr(C, u8)]
pub enum GenericContainIntrinsicSize<L> {
    /// The keyword `none`.
    None,
    /// The keywords 'auto none',
    AutoNone,
    /// A non-negative length.
    Length(L),
    /// "auto <Length>"
    AutoLength(L),
}

pub use self::GenericContainIntrinsicSize as ContainIntrinsicSize;

impl<L: ToCss> ToCss for ContainIntrinsicSize<L> {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        match *self {
            Self::None => dest.write_str("none"),
            Self::AutoNone => dest.write_str("auto none"),
            Self::Length(ref l) => l.to_css(dest),
            Self::AutoLength(ref l) => {
                dest.write_str("auto ")?;
                l.to_css(dest)
            },
        }
    }
}

/// Note that we only implement -webkit-line-clamp as a single, longhand
/// property for now, but the spec defines line-clamp as a shorthand for
/// separate max-lines, block-ellipsis, and continue properties.
///
/// https://drafts.csswg.org/css-overflow-3/#line-clamp
#[derive(
    Clone,
    ComputeSquaredDistance,
    Copy,
    Debug,
    MallocSizeOf,
    PartialEq,
    SpecifiedValueInfo,
    ToComputedValue,
    ToAnimatedValue,
    ToAnimatedZero,
    ToResolvedValue,
    ToShmem,
    ToTyped,
)]
#[repr(transparent)]
#[value_info(other_values = "none")]
pub struct GenericLineClamp<I>(pub I);

pub use self::GenericLineClamp as LineClamp;

impl<I: Zero> LineClamp<I> {
    /// Returns the `none` value.
    pub fn none() -> Self {
        Self(crate::Zero::zero())
    }

    /// Returns whether we're the `none` value.
    pub fn is_none(&self) -> bool {
        self.0.is_zero()
    }
}

impl<I: Zero + ToCss> ToCss for LineClamp<I> {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        if self.is_none() {
            return dest.write_str("none");
        }
        self.0.to_css(dest)
    }
}

/// A generic value for the `perspective` property.
#[derive(
    Animate,
    Clone,
    ComputeSquaredDistance,
    Copy,
    Debug,
    MallocSizeOf,
    Parse,
    PartialEq,
    SpecifiedValueInfo,
    ToAnimatedValue,
    ToAnimatedZero,
    ToComputedValue,
    ToCss,
    ToResolvedValue,
    ToShmem,
    ToTyped,
)]
#[repr(C, u8)]
pub enum GenericPerspective<NonNegativeLength> {
    /// A non-negative length.
    Length(NonNegativeLength),
    /// The keyword `none`.
    None,
}

pub use self::GenericPerspective as Perspective;

impl<L> Perspective<L> {
    /// Returns `none`.
    #[inline]
    pub fn none() -> Self {
        Perspective::None
    }
}

#[derive(
    Clone,
    Copy,
    Debug,
    MallocSizeOf,
    Parse,
    PartialEq,
    SpecifiedValueInfo,
    ToComputedValue,
    ToCss,
    ToResolvedValue,
    ToShmem,
    ToTyped,
)]
#[repr(u8)]
#[allow(missing_docs)]
pub enum PositionProperty {
    Static = 0,
    Relative,
    Absolute,
    Fixed,
    Sticky,
}

impl PositionProperty {
    /// Is the box absolutely positioned?
    pub fn is_absolutely_positioned(self) -> bool {
        matches!(self, Self::Absolute | Self::Fixed)
    }
}

/// https://drafts.csswg.org/css-overflow-4/#overflow-clip-margin's <visual-box>. Note that the
/// spec has special behavior for the omitted keyword, but that's rather odd, see:
/// https://github.com/w3c/csswg-drafts/issues/13185
#[allow(missing_docs)]
#[derive(
    Clone,
    ComputeSquaredDistance,
    Copy,
    Debug,
    Eq,
    MallocSizeOf,
    PartialEq,
    Parse,
    SpecifiedValueInfo,
    ToAnimatedValue,
    ToComputedValue,
    ToCss,
    ToResolvedValue,
    ToShmem,
    ToTyped,
)]
#[repr(u8)]
pub enum OverflowClipMarginBox {
    ContentBox,
    PaddingBox,
    BorderBox,
}

/// https://drafts.csswg.org/css-overflow-4/#overflow-clip-margin
#[derive(
    Animate,
    Clone,
    ComputeSquaredDistance,
    Copy,
    Debug,
    Eq,
    MallocSizeOf,
    PartialEq,
    SpecifiedValueInfo,
    ToAnimatedValue,
    ToComputedValue,
    ToAnimatedZero,
    ToResolvedValue,
    ToShmem,
    ToTyped,
)]
#[repr(C)]
pub struct GenericOverflowClipMargin<L> {
    /// The offset of the clip.
    pub offset: L,
    /// The box that we're clipping to.
    #[animation(constant)]
    pub visual_box: OverflowClipMarginBox,
}

pub use self::GenericOverflowClipMargin as OverflowClipMargin;

impl<L: Zero> GenericOverflowClipMargin<L> {
    /// Returns the `none` value.
    pub fn zero() -> Self {
        Self {
            offset: Zero::zero(),
            visual_box: OverflowClipMarginBox::PaddingBox,
        }
    }
}

impl<L: Zero + ToCss> ToCss for OverflowClipMargin<L> {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        if self.visual_box == OverflowClipMarginBox::PaddingBox {
            return self.offset.to_css(dest);
        }
        self.visual_box.to_css(dest)?;
        if !self.offset.is_zero() {
            dest.write_char(' ')?;
            self.offset.to_css(dest)?;
        }
        Ok(())
    }
}
