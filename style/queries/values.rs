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

/// The capabilities of the primary (or any) input pointer, used to evaluate
/// the `pointer`, `any-pointer`, `hover` and `any-hover` media features.
/// https://drafts.csswg.org/mediaqueries-4/#mf-interaction
#[derive(Clone, Copy, Debug, PartialEq, Eq, ToShmem)]
pub struct PointerCapabilities(u8);
bitflags! {
    impl PointerCapabilities: u8 {
        /// The pointer is imprecise (e.g. a touchscreen).
        const COARSE = 1 << 0;
        /// The pointer is precise (e.g. a mouse or trackpad).
        const FINE = 1 << 1;
        /// The pointer can hover (e.g. a mouse, but not a touchscreen).
        const HOVER = 1 << 2;
    }
}
