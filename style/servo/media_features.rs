/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Servo's media feature list and evaluator.

use crate::derives::*;
use crate::queries::feature::{AllowsRanges, Evaluator, FeatureFlags, QueryFeatureDescription};
use crate::queries::values::PrefersColorScheme;
use crate::values::computed::{CSSPixelLength, Context, Resolution};
use std::fmt::Debug;

/// https://drafts.csswg.org/mediaqueries-4/#width
fn eval_width(context: &Context) -> CSSPixelLength {
    CSSPixelLength::new(context.device().au_viewport_size().width.to_f32_px())
}

#[derive(Clone, Copy, Debug, FromPrimitive, Parse, ToCss)]
#[repr(u8)]
enum Scan {
    Progressive,
    Interlace,
}

/// https://drafts.csswg.org/mediaqueries-4/#scan
fn eval_scan(_: &Context, _: Option<Scan>) -> bool {
    // Since we doesn't support the 'tv' media type, the 'scan' feature never
    // matches.
    false
}

/// https://drafts.csswg.org/mediaqueries-4/#resolution
fn eval_resolution(context: &Context) -> Resolution {
    Resolution::from_dppx(context.device().device_pixel_ratio().0)
}

/// https://compat.spec.whatwg.org/#css-media-queries-webkit-device-pixel-ratio
fn eval_device_pixel_ratio(context: &Context) -> f32 {
    eval_resolution(context).dppx()
}

fn eval_prefers_color_scheme(context: &Context, query_value: Option<PrefersColorScheme>) -> bool {
    match query_value {
        Some(v) => context.device().color_scheme() == v,
        None => true,
    }
}

/// `pointer` / `any-pointer` keyword values
/// <https://drafts.csswg.org/mediaqueries-4/#pointer>
#[derive(Clone, Copy, Debug, FromPrimitive, Parse, ToCss)]
#[repr(u8)]
enum Pointer {
    None,
    Coarse,
    Fine,
}

/// `hover` / `any-hover` keyword values
/// <https://drafts.csswg.org/mediaqueries-4/#hover>
#[derive(Clone, Copy, Debug, FromPrimitive, Parse, ToCss)]
#[repr(u8)]
enum Hover {
    None,
    Hover,
}

// Servo embedders (Blitz / dioxus-native) target desktop with a real
// mouse, so we hard-code "fine pointer that can hover" as the answer
// for both the primary and any-pointer feature evaluators. Tailwind v4
// emits `@media (hover: hover) { … }` around every `hover:` utility;
// without these features, Stylo treated the queries as unsupported and
// silently dropped the rules — every dropdown / button / link
// `hover:bg-X` was invisible on the native renderer. If/when Servo
// embedders need to support touch, plumb the actual capability through
// `Device` and consult it here instead of returning a constant.
//
// `boolean form` (`(hover)` / `(pointer)`) returns true iff hover /
// pointer is available, per the spec.

/// <https://drafts.csswg.org/mediaqueries-4/#hover>
fn eval_hover(_: &Context, query_value: Option<Hover>) -> bool {
    match query_value {
        // Boolean form `(hover)` matches when hover is available.
        None => true,
        Some(Hover::Hover) => true,
        Some(Hover::None) => false,
    }
}

/// <https://drafts.csswg.org/mediaqueries-4/#descdef-media-any-hover>
fn eval_any_hover(context: &Context, query_value: Option<Hover>) -> bool {
    eval_hover(context, query_value)
}

/// <https://drafts.csswg.org/mediaqueries-4/#pointer>
fn eval_pointer(_: &Context, query_value: Option<Pointer>) -> bool {
    match query_value {
        // Boolean form `(pointer)` matches when any pointer is available.
        None => true,
        Some(Pointer::Fine) => true,
        Some(Pointer::Coarse) => false,
        Some(Pointer::None) => false,
    }
}

/// <https://drafts.csswg.org/mediaqueries-4/#descdef-media-any-pointer>
fn eval_any_pointer(context: &Context, query_value: Option<Pointer>) -> bool {
    eval_pointer(context, query_value)
}

/// A list with all the media features that Servo supports.
pub static MEDIA_FEATURES: [QueryFeatureDescription; 10] = [
    feature!(
        atom!("width"),
        AllowsRanges::Yes,
        Evaluator::Length(eval_width),
        FeatureFlags::empty(),
    ),
    feature!(
        atom!("scan"),
        AllowsRanges::No,
        keyword_evaluator!(eval_scan, Scan),
        FeatureFlags::empty(),
    ),
    feature!(
        atom!("resolution"),
        AllowsRanges::Yes,
        Evaluator::Resolution(eval_resolution),
        FeatureFlags::empty(),
    ),
    feature!(
        atom!("device-pixel-ratio"),
        AllowsRanges::Yes,
        Evaluator::Float(eval_device_pixel_ratio),
        FeatureFlags::WEBKIT_PREFIX,
    ),
    feature!(
        atom!("-moz-device-pixel-ratio"),
        AllowsRanges::Yes,
        Evaluator::Float(eval_device_pixel_ratio),
        FeatureFlags::empty(),
    ),
    feature!(
        atom!("prefers-color-scheme"),
        AllowsRanges::No,
        keyword_evaluator!(eval_prefers_color_scheme, PrefersColorScheme),
        FeatureFlags::empty(),
    ),
    feature!(
        atom!("hover"),
        AllowsRanges::No,
        keyword_evaluator!(eval_hover, Hover),
        FeatureFlags::empty(),
    ),
    feature!(
        atom!("any-hover"),
        AllowsRanges::No,
        keyword_evaluator!(eval_any_hover, Hover),
        FeatureFlags::empty(),
    ),
    feature!(
        atom!("pointer"),
        AllowsRanges::No,
        keyword_evaluator!(eval_pointer, Pointer),
        FeatureFlags::empty(),
    ),
    feature!(
        atom!("any-pointer"),
        AllowsRanges::No,
        keyword_evaluator!(eval_any_pointer, Pointer),
        FeatureFlags::empty(),
    ),
];
