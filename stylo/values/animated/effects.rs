/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! Animated types for CSS values related to effects.

use properties::animated_properties::Animatable;
#[cfg(feature = "gecko")]
use properties::animated_properties::IntermediateColor;
use values::computed::{Angle, Number};
use values::computed::effects::DropShadow as ComputedDropShadow;
use values::computed::effects::Filter as ComputedFilter;
use values::computed::effects::FilterList as ComputedFilterList;
use values::computed::length::Length;
use values::generics::effects::Filter as GenericFilter;
use values::generics::effects::FilterList as GenericFilterList;

/// An animated value for the `filter` property.
pub type FilterList = GenericFilterList<Filter>;

/// An animated value for a single `filter`.
pub type Filter = GenericFilter<
    Angle,
    // FIXME: Should be `NumberOrPercentage`.
    Number,
    Length,
    DropShadow
>;

/// An animated value for the `drop-shadow()` filter.
///
/// Currently unsupported outside of Gecko.
#[cfg(not(feature = "gecko"))]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
#[derive(Clone, Debug, PartialEq)]
pub enum DropShadow {}

/// An animated value for the `drop-shadow()` filter.
///
/// Contrary to the canonical order from the spec, the color is serialised
/// first, like in Gecko and Webkit.
#[cfg(feature = "gecko")]
#[derive(Clone, Debug, PartialEq)]
pub struct DropShadow {
    /// Color.
    pub color: IntermediateColor,
    /// Horizontal radius.
    pub horizontal: Length,
    /// Vertical radius.
    pub vertical: Length,
    /// Blur radius.
    pub blur: Length,
}

impl From<ComputedFilterList> for FilterList {
    #[inline]
    fn from(filters: ComputedFilterList) -> Self {
        filters.0.into_vec().into_iter().map(|f| f.into()).collect::<Vec<_>>().into()
    }
}

impl From<FilterList> for ComputedFilterList {
    #[inline]
    fn from(filters: FilterList) -> Self {
        filters.0.into_vec().into_iter().map(|f| f.into()).collect::<Vec<_>>().into()
    }
}

impl From<ComputedFilter> for Filter {
    #[inline]
    fn from(filter: ComputedFilter) -> Self {
        match filter {
            GenericFilter::Blur(angle) => GenericFilter::Blur(angle),
            GenericFilter::Brightness(factor) => GenericFilter::Brightness(factor),
            GenericFilter::Contrast(factor) => GenericFilter::Contrast(factor),
            GenericFilter::Grayscale(factor) => GenericFilter::Grayscale(factor),
            GenericFilter::HueRotate(factor) => GenericFilter::HueRotate(factor),
            GenericFilter::Invert(factor) => GenericFilter::Invert(factor),
            GenericFilter::Opacity(factor) => GenericFilter::Opacity(factor),
            GenericFilter::Saturate(factor) => GenericFilter::Saturate(factor),
            GenericFilter::Sepia(factor) => GenericFilter::Sepia(factor),
            GenericFilter::DropShadow(shadow) => {
                GenericFilter::DropShadow(shadow.into())
            },
            #[cfg(feature = "gecko")]
            GenericFilter::Url(url) => GenericFilter::Url(url),
        }
    }
}

impl From<Filter> for ComputedFilter {
    #[inline]
    fn from(filter: Filter) -> Self {
        match filter {
            GenericFilter::Blur(angle) => GenericFilter::Blur(angle),
            GenericFilter::Brightness(factor) => GenericFilter::Brightness(factor),
            GenericFilter::Contrast(factor) => GenericFilter::Contrast(factor),
            GenericFilter::Grayscale(factor) => GenericFilter::Grayscale(factor),
            GenericFilter::HueRotate(factor) => GenericFilter::HueRotate(factor),
            GenericFilter::Invert(factor) => GenericFilter::Invert(factor),
            GenericFilter::Opacity(factor) => GenericFilter::Opacity(factor),
            GenericFilter::Saturate(factor) => GenericFilter::Saturate(factor),
            GenericFilter::Sepia(factor) => GenericFilter::Sepia(factor),
            GenericFilter::DropShadow(shadow) => {
                GenericFilter::DropShadow(shadow.into())
            },
            #[cfg(feature = "gecko")]
            GenericFilter::Url(url) => GenericFilter::Url(url.clone())
        }
    }
}

impl From<ComputedDropShadow> for DropShadow {
    #[cfg(not(feature = "gecko"))]
    #[inline]
    fn from(shadow: ComputedDropShadow) -> Self {
        match shadow {}
    }

    #[cfg(feature = "gecko")]
    #[inline]
    fn from(shadow: ComputedDropShadow) -> Self {
        DropShadow {
            color: shadow.color.into(),
            horizontal: shadow.horizontal,
            vertical: shadow.vertical,
            blur: shadow.blur,
        }
    }
}

impl From<DropShadow> for ComputedDropShadow {
    #[cfg(not(feature = "gecko"))]
    #[inline]
    fn from(shadow: DropShadow) -> Self {
        match shadow {}
    }

    #[cfg(feature = "gecko")]
    #[inline]
    fn from(shadow: DropShadow) -> Self {
        ComputedDropShadow {
            color: shadow.color.into(),
            horizontal: shadow.horizontal,
            vertical: shadow.vertical,
            blur: shadow.blur,
        }
    }
}

impl Animatable for DropShadow {
    #[cfg(not(feature = "gecko"))]
    #[inline]
    fn add_weighted(&self, _other: &Self, _self_portion: f64, _other_portion: f64) -> Result<Self, ()> {
        match *self {}
    }

    #[cfg(feature = "gecko")]
    #[inline]
    fn add_weighted(&self, other: &Self, self_portion: f64, other_portion: f64) -> Result<Self, ()> {
        let color = self.color.add_weighted(&other.color, self_portion, other_portion)?;
        let horizontal = self.horizontal.add_weighted(&other.horizontal, self_portion, other_portion)?;
        let vertical = self.vertical.add_weighted(&other.vertical, self_portion, other_portion)?;
        let blur = self.blur.add_weighted(&other.blur, self_portion, other_portion)?;

        Ok(DropShadow {
            color: color,
            horizontal: horizontal,
            vertical: vertical,
            blur: blur,
        })
    }

    #[cfg(not(feature = "gecko"))]
    #[inline]
    fn compute_distance(&self, _other: &Self) -> Result<f64, ()> {
        match *self {}
    }

    #[cfg(feature = "gecko")]
    #[inline]
    fn compute_distance(&self, other: &Self) -> Result<f64, ()> {
        self.compute_squared_distance(other).map(|sd| sd.sqrt())
    }

    #[cfg(not(feature = "gecko"))]
    #[inline]
    fn compute_squared_distance(&self, _other: &Self) -> Result<f64, ()> {
        match *self {}
    }

    #[cfg(feature = "gecko")]
    #[inline]
    fn compute_squared_distance(&self, other: &Self) -> Result<f64, ()> {
        Ok(
            self.color.compute_squared_distance(&other.color)? +
            self.horizontal.compute_squared_distance(&other.horizontal)? +
            self.vertical.compute_squared_distance(&other.vertical)? +
            self.blur.compute_squared_distance(&other.blur)?
        )
    }
}
