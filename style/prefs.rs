/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Macro helpers to get preference values.

/// Returns the value of a preference exposed to the style crate.
/// It allows overriding the result for either Gecko or Servo, which is
/// useful when the preference only exists for one of them.
#[macro_export]
#[cfg(feature = "gecko")]
macro_rules! pref {
    ($string:tt $(, servo = $_:tt)?) => {
        static_prefs::pref!($string)
    };
    ($string:tt, gecko = $value:tt) => {
        $value
    };
}

/// Returns the value of a preference exposed to the style crate.
/// It allows overriding the result for either Gecko or Servo, which is
/// useful when the preference only exists for one of them.
#[macro_export]
#[cfg(feature = "servo")]
macro_rules! pref {
    ($string:tt $(, gecko = $_:tt)?) => {
        static_prefs::pref!($string)
    };
    ($string:tt, servo = $value:tt) => {
        $value
    };
}
