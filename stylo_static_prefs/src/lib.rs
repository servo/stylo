/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

/// Returns the default value for a preference exposed to the style crate.
/// This is what will be used if the embedder has not set the preference.
#[macro_export]
macro_rules! default_value {
    ("layout.css.contrast-color.enabled") => {
        true
    };
    ("layout.css.fit-content-function.enabled") => {
        true
    };
    ("layout.css.font-variations.enabled") => {
        true
    };
    ("layout.css.gradient-color-interpolation-method.enabled") => {
        true
    };
    ("layout.css.marker.restricted") => {
        true
    };
    ("layout.css.outline-offset.snapping") => {
        1
    };
    ("layout.css.properties-and-values.enabled") => {
        true
    };
    ("layout.css.relative-color-syntax.enabled") => {
        true
    };
    ("layout.css.stretch-size-keyword.enabled") => {
        true
    };
    ("layout.css.stylo-local-work-queue.in-main-thread") => {
        32
    };
    ("layout.css.stylo-local-work-queue.in-worker") => {
        0
    };
    ("layout.threads") => {
       // Negative means auto, 0 disables the thread-pool (main-thread styling),
       // other numbers override as specified.
        -1
    };
    ("layout.css.stylo-work-unit-size") => {
        16
    };
    ("layout.css.system-ui.enabled") => {
        true
    };
    ("layout.css.webkit-fill-available.all-size-properties.enabled") => {
        true
    };
    ("layout.css.webkit-fill-available.enabled") => {
        true
    };
    ($string:literal) => {
        false
    };
}

/// Returns the value of a preference exposed to the style crate. If the embedder
/// has not set a value for it, this returns the default value of the preference.
#[macro_export]
macro_rules! pref {
    ($string:tt) => {
        style_config::Getter::get($string, $crate::default_value!($string))
    };
}
