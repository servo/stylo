/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Generic types for box properties.

use crate::derives::*;
use crate::values::animated::ToAnimatedZero;
use crate::values::generics::Optional;
use crate::Zero;
use std::fmt::{self, Write};
use style_traits::values::SequenceWriter;
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
    ToTyped,
)]
#[repr(u8)]
#[allow(missing_docs)]
pub enum BaselineShiftKeyword {
    /// Lower by the offset appropriate for subscripts of the parent’s box. The UA may use the
    /// parent’s font metrics to find this offset; otherwise it defaults to dropping by one
    /// fifth of the parent’s used font-size.
    Sub,
    /// Raise by the offset appropriate for superscripts of the parent’s box. The UA may use the
    /// parent’s font metrics to find this offset; otherwise it defaults to raising by one third
    /// of the parent’s used font-size.
    Super,
    /// Align the line-over edge of the aligned subtree with the line-over edge of the line box.
    Top,
    /// Align the center of the aligned subtree with the center of the line box.
    Center,
    /// Align the line-under edge of the aligned subtree with the line-under edge of the line box.
    Bottom,
}

/// A generic value for the `baseline-shift` property.
/// https://drafts.csswg.org/css-inline-3/#baseline-shift
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
pub enum GenericBaselineShift<LengthPercentage> {
    /// One of the baseline-shift keywords
    Keyword(BaselineShiftKeyword),
    /// Raise (positive value) or lower (negative value) by the specified length or specified percentage of the line-height.
    Length(LengthPercentage),
}

pub use self::GenericBaselineShift as BaselineShift;

impl<L: Zero> BaselineShift<L> {
    /// Returns the initial `0` value.
    #[inline]
    pub fn zero() -> Self {
        BaselineShift::Length(Zero::zero())
    }
}

impl<L> ToAnimatedZero for BaselineShift<L> {
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

/// A block ellipsis for line clamping.
#[derive(
    Clone,
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
)]
#[repr(C, u8)]
pub enum BlockEllipsis {
    /// Display the ellipsis character.
    Ellipsis,
    /// Do not display an ellipsis.
    NoEllipsis,
    /// Display the given string.
    String(crate::values::AtomString),
}

impl BlockEllipsis {
    /// Returns whether this value is `ellipsis`.
    pub fn is_ellipsis(&self) -> bool {
        matches!(self, Self::Ellipsis)
    }
}

/// A keyword for the maximum lines used by `line-clamp`.
#[derive(
    Animate,
    Clone,
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
)]
#[repr(u8)]
pub enum MaxLinesKeyword {
    /// No block-size clamping is applied.
    None,
    /// Clamp using the available block size.
    Auto,
}

impl MaxLinesKeyword {
    /// Returns whether this keyword is `none`.
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

/// A generic value for the maximum lines used by `line-clamp`.
/// <https://drafts.csswg.org/css-overflow-4/#propdef-max-lines>
#[derive(
    Animate,
    Clone,
    ComputeSquaredDistance,
    Debug,
    MallocSizeOf,
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
#[repr(C)]
#[typed(todo_derive_fields)]
pub struct GenericMaxLines<I> {
    /// The maximum number of lines.
    pub lines: Optional<I>,
    /// Whether block-size clamping is applied.
    #[css(skip_if = "MaxLinesKeyword::is_none")]
    #[animation(constant)]
    pub kw: MaxLinesKeyword,
}

pub use self::GenericMaxLines as MaxLines;

impl<I> MaxLines<I> {
    /// Returns a new `none` value.
    pub fn none() -> Self {
        Self {
            lines: Optional::None,
            kw: MaxLinesKeyword::None,
        }
    }

    /// Returns a new `auto` value.
    pub fn auto() -> Self {
        Self {
            lines: Optional::None,
            kw: MaxLinesKeyword::Auto,
        }
    }

    /// Returns a new line-count value, optionally with the `auto` keyword.
    pub fn lines(lines: I, auto: bool) -> Self {
        Self {
            lines: Optional::Some(lines),
            kw: if auto {
                MaxLinesKeyword::Auto
            } else {
                MaxLinesKeyword::None
            },
        }
    }

    /// Returns whether there is no line-count or block-size clamping.
    pub fn is_none(&self) -> bool {
        self.lines.is_none() && matches!(self.kw, MaxLinesKeyword::None)
    }

    /// Returns whether this value is `auto`.
    pub fn is_auto(&self) -> bool {
        self.lines.is_none() && matches!(self.kw, MaxLinesKeyword::Auto)
    }

    /// Returns the line count if exists.
    pub fn lines_value(&self) -> Option<&I> {
        self.lines.as_ref()
    }
}

/// A generic value for the `line-clamp` property.
#[derive(
    Animate,
    Clone,
    ComputeSquaredDistance,
    Debug,
    MallocSizeOf,
    PartialEq,
    SpecifiedValueInfo,
    ToAnimatedValue,
    ToAnimatedZero,
    ToComputedValue,
    ToResolvedValue,
    ToShmem,
    ToTyped,
)]
#[repr(C)]
#[typed(todo_derive_fields)]
#[value_info(other_values = "none")]
pub struct GenericLineClamp<I> {
    /// Maximum `line-count` and `block-size` clamping behavior.
    pub max_lines: GenericMaxLines<I>,
    /// Ellipsis displayed at the clamp point.
    #[animation(constant)]
    pub block_ellipsis: BlockEllipsis,
    /// Whether legacy line clamping behavior is used.
    #[animation(constant)]
    pub webkit_legacy: bool,
    /// Whether the `-webkit-legacy` keyword is printed during serialization.
    #[animation(constant)]
    pub serialize_webkit_legacy: bool,
}

pub use self::GenericLineClamp as LineClamp;

impl<I> LineClamp<I> {
    /// Returns the `none` value.
    pub fn none() -> Self {
        Self {
            max_lines: MaxLines::none(),
            block_ellipsis: BlockEllipsis::Ellipsis,
            webkit_legacy: false,
            serialize_webkit_legacy: false,
        }
    }

    /// Returns whether we are the `none` value or not.
    pub fn is_none(&self) -> bool {
        self.max_lines.is_none() && self.block_ellipsis.is_ellipsis()
    }
}

impl<I: ToCss> ToCss for LineClamp<I> {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        if self.is_none() {
            return dest.write_str("none");
        }

        let mut writer = SequenceWriter::new(dest, " ");
        if !self.max_lines.is_none() {
            writer.item(&self.max_lines)?;
        }
        if !self.block_ellipsis.is_ellipsis() {
            writer.item(&self.block_ellipsis)?;
        }
        if self.webkit_legacy
            && self.serialize_webkit_legacy
            && static_prefs::pref!("layout.css.line-clamp.enabled")
        {
            writer.raw_item("-webkit-legacy")?;
        }
        Ok(())
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
