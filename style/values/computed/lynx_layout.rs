/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Computed Lynx-only layout values.

use crate::values::CSSInteger;

/// Computed `relative-align-*` value.
///
/// `-1` is Lynx's no-reference sentinel, `0` is `parent`, and positive values
/// reference sibling `relative-id` values.
pub type RelativeAlign = CSSInteger;
