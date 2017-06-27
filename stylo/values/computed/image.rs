/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! CSS handling for the computed value of
//! [`image`][image]s
//!
//! [image]: https://drafts.csswg.org/css-images/#image-values

use cssparser::RGBA;
use std::f32::consts::PI;
use std::fmt;
use style_traits::ToCss;
use values::{Either, None_};
use values::computed::{Angle, Context, Length, LengthOrPercentage, NumberOrPercentage, ToComputedValue};
use values::computed::position::Position;
use values::generics::image::{CompatMode, ColorStop as GenericColorStop, EndingShape as GenericEndingShape};
use values::generics::image::{Gradient as GenericGradient, GradientItem as GenericGradientItem};
use values::generics::image::{Image as GenericImage, GradientKind as GenericGradientKind};
use values::generics::image::{ImageRect as GenericImageRect, LineDirection as GenericLineDirection};
use values::generics::position::Position as GenericPosition;
use values::specified::image::{Gradient as SpecifiedGradient, LineDirection as SpecifiedLineDirection};
use values::specified::image::{GradientKind as SpecifiedGradientKind};
use values::specified::position::{X, Y};

/// A computed image layer.
pub type ImageLayer = Either<None_, Image>;

/// Computed values for an image according to CSS-IMAGES.
/// https://drafts.csswg.org/css-images/#image-values
pub type Image = GenericImage<Gradient, ImageRect>;

/// Computed values for a CSS gradient.
/// https://drafts.csswg.org/css-images/#gradients
pub type Gradient = GenericGradient<
    LineDirection,
    Length,
    LengthOrPercentage,
    Position,
    RGBA,
>;

/// A computed gradient kind.
pub type GradientKind = GenericGradientKind<
    LineDirection,
    Length,
    LengthOrPercentage,
    Position,
>;

/// A computed gradient line direction.
#[derive(Clone, Copy, Debug, PartialEq)]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub enum LineDirection {
    /// An angle.
    Angle(Angle),
    /// A corner.
    Corner(X, Y),
}

/// A computed radial gradient ending shape.
pub type EndingShape = GenericEndingShape<Length, LengthOrPercentage>;

/// A computed gradient item.
pub type GradientItem = GenericGradientItem<RGBA, LengthOrPercentage>;

/// A computed color stop.
pub type ColorStop = GenericColorStop<RGBA, LengthOrPercentage>;

/// Computed values for ImageRect.
pub type ImageRect = GenericImageRect<NumberOrPercentage>;

impl GenericLineDirection for LineDirection {
    fn points_downwards(&self) -> bool {
        match *self {
            LineDirection::Angle(angle) => angle.radians() == PI,
            LineDirection::Corner(..) => false,
        }
    }

    fn to_css<W>(&self, dest: &mut W, compat_mode: CompatMode) -> fmt::Result
        where W: fmt::Write
    {
        match *self {
            LineDirection::Angle(ref angle) => angle.to_css(dest),
            LineDirection::Corner(x, y) => {
                if compat_mode == CompatMode::Modern {
                    dest.write_str("to ")?;
                }
                x.to_css(dest)?;
                dest.write_str(" ")?;
                y.to_css(dest)
            },
        }
    }
}

impl SpecifiedLineDirection {
    /// Takes a modern linear gradient angle and convert it to Gecko's old coordinate for
    /// webkit-prefixed version
    fn to_gecko_coordinate(modern_angle: f32, _compat_mode: CompatMode) -> f32 {
        #[cfg(feature = "gecko")]
        {
            return match _compat_mode {
                CompatMode::Modern => modern_angle,
                CompatMode::WebKit => -modern_angle + 270.
            }
        }
        #[cfg(feature = "servo")]
        modern_angle
    }

    /// Manually derived to_computed_value
    fn to_computed_value(&self, context: &Context, compat_mode: CompatMode) -> LineDirection {
        match *self {
            SpecifiedLineDirection::Angle(ref angle) => {
                LineDirection::Angle(angle.to_computed_value(context))
            },
            SpecifiedLineDirection::Horizontal(X::Left) => {
                LineDirection::Angle(Angle::Degree(SpecifiedLineDirection::to_gecko_coordinate(270., compat_mode)))
            },
            SpecifiedLineDirection::Horizontal(X::Right) => {
                LineDirection::Angle(Angle::Degree(SpecifiedLineDirection::to_gecko_coordinate(90., compat_mode)))
            },
            SpecifiedLineDirection::Vertical(Y::Top) => {
                LineDirection::Angle(Angle::Degree(SpecifiedLineDirection::to_gecko_coordinate(0., compat_mode)))
            },
            SpecifiedLineDirection::Vertical(Y::Bottom) => {
                LineDirection::Angle(Angle::Degree(SpecifiedLineDirection::to_gecko_coordinate(180., compat_mode)))
            },
            SpecifiedLineDirection::Corner(x, y) => {
                LineDirection::Corner(x, y)
            },
        }
    }

    fn from_computed_value(computed: &LineDirection) -> Self {
        match *computed {
            LineDirection::Angle(ref angle) => {
                SpecifiedLineDirection::Angle(ToComputedValue::from_computed_value(angle))
            },
            LineDirection::Corner(x, y) => {
                SpecifiedLineDirection::Corner(x, y)
            },
        }
    }
}

impl ToComputedValue for SpecifiedGradient {
    type ComputedValue = Gradient;

    fn to_computed_value(&self, context: &Context) -> Self::ComputedValue {
        Self::ComputedValue {
            kind: self.kind.to_computed_value(context, self.compat_mode),
            items: self.items.to_computed_value(context),
            repeating: self.repeating,
            compat_mode: self.compat_mode
        }
    }

    fn from_computed_value(computed: &Self::ComputedValue) -> Self {
        Self {
            kind: SpecifiedGradientKind::from_computed_value(&computed.kind),
            items: ToComputedValue::from_computed_value(&computed.items),
            repeating: computed.repeating,
            compat_mode: computed.compat_mode
        }
    }
}

impl SpecifiedGradientKind {
    /// Manually derived to_computed_value
    pub fn to_computed_value(&self, context: &Context, compat_mode: CompatMode) -> GradientKind {
        match self {
            &GenericGradientKind::Linear(ref line_direction) => {
                GenericGradientKind::Linear(line_direction.to_computed_value(context, compat_mode))
            },
            &GenericGradientKind::Radial(ref ending_shape, ref position) => {
                GenericGradientKind::Radial(ending_shape.to_computed_value(context),
                             position.to_computed_value(context))
            }
        }
    }

    /// Manually derived from_computed_value
    pub fn from_computed_value(computed: &GradientKind) -> SpecifiedGradientKind {
        match *computed {
            GenericGradientKind::Linear(line_direction) => {
                GenericGradientKind::Linear(SpecifiedLineDirection::from_computed_value(&line_direction))
            },
            GenericGradientKind::Radial(ending_shape, position) => {
                GenericGradientKind::Radial(GenericEndingShape::from_computed_value(&ending_shape),
                                            GenericPosition::from_computed_value(&position))
            }
        }
    }
}
