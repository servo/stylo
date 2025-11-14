/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! A list of static preferences exposed to the style crate. These should
//! be kept sync with the preferences used by the style.
#[macro_export]
macro_rules! pref {
    ("layout.css.stylo-local-work-queue.in-main-thread") => {
        32
    };
    ("layout.css.stylo-work-unit-size") => {
        16
    };
    ("layout.css.stylo-local-work-queue.in-worker") => {
        0
    };
    ("layout.css.system-ui.enabled") => {
        true
    };
    ("layout.css.fit-content-function.enabled") => {
        true
    };
    ("layout.css.outline-offset.snapping") => {
        1
    };
    ("layout.css.relative-color-syntax.enabled") => {
        true
    };
    ("layout.css.stretch-size-keyword.enabled") => {
        true
    };
    ("layout.css.marker.restricted") => {
        true
    };
    ("layout.css.webkit-fill-available.enabled") => {
        true
    };
    ("layout.css.webkit-fill-available.all-size-properties.enabled") => {
        true
    };
    ("layout.css.font-variations.enabled") => {
        true
    };
    ($string:literal) => {
        false
    };
}
