/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Common feature values between media and container features.

use crate::derives::*;
use app_units::Au;
use euclid::default::Size2D;

/// The orientation media / container feature.
/// https://drafts.csswg.org/mediaqueries-5/#orientation
/// https://drafts.csswg.org/css-contain-3/#orientation
#[derive(Clone, Copy, Debug, FromPrimitive, Parse, ToCss)]
#[repr(u8)]
#[allow(missing_docs)]
pub enum Orientation {
    Portrait,
    Landscape,
}

impl Orientation {
    /// A helper to evaluate a orientation query given a generic size getter.
    pub fn eval(size: Size2D<Au>, value: Option<Self>) -> bool {
        let query_orientation = match value {
            Some(v) => v,
            None => return true,
        };

        // Per spec, square viewports should be 'portrait'
        let is_landscape = size.width > size.height;
        match query_orientation {
            Self::Landscape => is_landscape,
            Self::Portrait => !is_landscape,
        }
    }
}

/// Values for the prefers-color-scheme media feature.
#[derive(Clone, Copy, Debug, FromPrimitive, Parse, PartialEq, ToCss)]
#[repr(u8)]
#[allow(missing_docs)]
pub enum PrefersColorScheme {
    Light,
    Dark,
}

/// Possible values for prefers-contrast media query.
/// https://drafts.csswg.org/mediaqueries-5/#prefers-contrast
#[derive(Clone, Copy, Debug, FromPrimitive, Parse, PartialEq, ToCss)]
#[repr(u8)]
pub enum PrefersContrast {
    /// More contrast is preferred.
    More,
    /// Low contrast is preferred.
    Less,
    /// Custom (not more, not less).
    Custom,
    /// The default value if neither high or low contrast is enabled.
    NoPreference,
}

/// Values for the prefers-reduced-motion media feature.
#[derive(Clone, Copy, Debug, FromPrimitive, Parse, PartialEq, ToCss)]
#[repr(u8)]
#[allow(missing_docs)]
pub enum PrefersReducedMotion {
    NoPreference,
    Reduce,
}

/// Values for the prefers-reduced-transparency media feature.
#[derive(Clone, Copy, Debug, FromPrimitive, Parse, PartialEq, ToCss)]
#[repr(u8)]
#[allow(missing_docs)]
pub enum PrefersReducedTransparency {
    NoPreference,
    Reduce,
}
