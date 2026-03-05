/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use std::collections::HashMap;
use std::sync::{LazyLock, RwLock};

/// Returns the default value for a preference exposed to the style crate.
/// This is what will be used if the embedder has not set the preference.
#[macro_export]
macro_rules! default_value {
    ("layout.columns.enabled") => {
        false
    };
    ("layout.container-queries.enabled") => {
        false
    };
    ("layout.css.anchor-positioning.enabled") => {
        false
    };
    ("layout.css.appearance-base-select.enabled") => {
        false
    };
    ("layout.css.at-scope.enabled") => {
        false
    };
    ("layout.css.attr.enabled") => {
        false
    };
    ("layout.css.basic-shape-shape.enabled") => {
        false
    };
    ("layout.css.color-mix-multi-color.enabled") => {
        false
    };
    ("layout.css.content.alt-text.enabled") => {
        false
    };
    ("layout.css.contrast-color.enabled") => {
        true
    };
    ("layout.css.custom-media.enabled") => {
        false
    };
    ("layout.css.fit-content-function.enabled") => {
        true
    };
    ("layout.css.font-palette.enabled") => {
        false
    };
    ("layout.css.font-tech.enabled") => {
        false
    };
    ("layout.css.font-variations.enabled") => {
        true
    };
    ("layout.css.gradient-color-interpolation-method.enabled") => {
        true
    };
    ("layout.css.light-dark.images.enabled") => {
        false
    };
    ("layout.css.margin-rules.enabled") => {
        false
    };
    ("layout.css.motion-path-url.enabled") => {
        false
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
    ("layout.css.scroll-driven-animations.enabled") => {
        false
    };
    ("layout.css.scroll-state.enabled") => {
        false
    };
    ("layout.css.starting-style-at-rules.enabled") => {
        false
    };
    ("layout.css.stretch-size-keyword.enabled") => {
        true
    };
    ("layout.css.style-queries.enabled") => {
        false
    };
    ("layout.css.stylo-local-work-queue.in-main-thread") => {
        32
    };
    ("layout.css.stylo-local-work-queue.in-worker") => {
        0
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
    ("layout.grid.enabled") => {
        false
    };
    ("layout.threads") => {
        // Negative means auto, 0 disables the thread-pool (main-thread styling),
        // other numbers override as specified.
        -1
    };
    ("layout.unimplemented") => {
        false
    };
    ("layout.variable_fonts.enabled") => {
        false
    };
    ("layout.writing-mode.enabled") => {
        false
    };
}

/// Returns the value of a preference exposed to the style crate. If the embedder
/// has not set a value for it, this returns the default value of the preference.
#[macro_export]
macro_rules! pref {
    ($string:tt) => {
        $crate::Preference::get($string, $crate::default_value!($string))
    };
}

/// Sets a preference to the provided value.
#[macro_export]
macro_rules! set_pref {
    ($string:tt, $($value:expr)+) => {
        let value = $($value)+;
        // This comparison ensures that the provided value belongs to the expected type.
        let _ = $crate::default_value!($string) == value;
        $crate::Preference::set($string, value)
    };
}

static PREFS: LazyLock<Preferences> = LazyLock::new(Preferences::default);

#[derive(Debug, Default)]
struct Preferences {
    bool_prefs: RwLock<HashMap<String, bool>>,
    i32_prefs: RwLock<HashMap<String, i32>>,
}

impl Preferences {
    pub fn get_bool(&self, key: &str, default: bool) -> bool {
        let prefs = self.bool_prefs.read().expect("RwLock is poisoned");
        *prefs.get(key).unwrap_or(&default)
    }

    pub fn get_i32(&self, key: &str, default: i32) -> i32 {
        let prefs = self.i32_prefs.read().expect("RwLock is poisoned");
        *prefs.get(key).unwrap_or(&default)
    }

    pub fn set_bool(&self, key: &str, value: bool) {
        let mut prefs = self.bool_prefs.write().expect("RwLock is poisoned");

        // Avoid cloning the key if it exists.
        if let Some(pref) = prefs.get_mut(key) {
            *pref = value;
        } else {
            prefs.insert(key.to_owned(), value);
        }
    }

    pub fn set_i32(&self, key: &str, value: i32) {
        let mut prefs = self.i32_prefs.write().expect("RwLock is poisoned");

        // Avoid cloning the key if it exists.
        if let Some(pref) = prefs.get_mut(key) {
            *pref = value;
        } else {
            prefs.insert(key.to_owned(), value);
        }
    }
}

pub trait Preference: Sized {
    /// Gets the value of a preference, falling back to the provided default.
    /// Prefer using [`pref!()`] instead, since it ensures that the preference
    /// exists and uses the correct default automatically.
    fn get(key: &str, default: Self) -> Self;

    /// Sets a preference to the provided value. Prefer using [`set_pref!()`]
    /// instead, since it ensures that the preference exists and the value
    /// belongs to the expected type.
    fn set(key: &str, value: Self);
}

impl Preference for bool {
    fn get(key: &str, default: Self) -> Self {
        PREFS.get_bool(key, default)
    }
    fn set(key: &str, value: Self) {
        PREFS.set_bool(key, value)
    }
}

impl Preference for i32 {
    fn get(key: &str, default: Self) -> Self {
        PREFS.get_i32(key, default)
    }
    fn set(key: &str, value: Self) {
        PREFS.set_i32(key, value)
    }
}

#[test]
fn test() {
    let prefs = Preferences::default();

    // We get the default value when the pref is not set.
    assert_eq!(prefs.get_bool("foo", false), false);
    assert_eq!(prefs.get_bool("foo", true), true);
    assert_eq!(prefs.get_i32("bar", 0), 0);
    assert_eq!(prefs.get_i32("bar", 1), 1);
    assert_eq!(prefs.get_i32("bar", 2), 2);

    // Prefs can be set and retrieved.
    prefs.set_bool("foo", true);
    prefs.set_i32("bar", 1);
    assert_eq!(prefs.get_bool("foo", false), true);
    assert_eq!(prefs.get_bool("foo", true), true);
    assert_eq!(prefs.get_i32("bar", 0), 1);
    assert_eq!(prefs.get_i32("bar", 1), 1);
    assert_eq!(prefs.get_i32("bar", 2), 1);
    prefs.set_bool("foo", false);
    prefs.set_i32("bar", 2);
    assert_eq!(prefs.get_bool("foo", false), false);
    assert_eq!(prefs.get_bool("foo", true), false);
    assert_eq!(prefs.get_i32("bar", 0), 2);
    assert_eq!(prefs.get_i32("bar", 1), 2);
    assert_eq!(prefs.get_i32("bar", 2), 2);

    // Each value type currently has an independent namespace.
    prefs.set_i32("foo", 3);
    prefs.set_bool("bar", true);
    assert_eq!(prefs.get_i32("foo", 0), 3);
    assert_eq!(prefs.get_bool("foo", false), false);
    assert_eq!(prefs.get_bool("bar", false), true);
    assert_eq!(prefs.get_i32("bar", 0), 2);
}
