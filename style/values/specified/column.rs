/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Specified types for the column properties.

use crate::derives::*;
use crate::values::generics::column::GenericColumnCount;
use crate::values::specified::PositiveInteger;

/// A specified type for `column-count` values.
pub type ColumnCount = GenericColumnCount<PositiveInteger>;

/// https://drafts.csswg.org/css-multicol/#propdef-column-fill
#[allow(missing_docs)]
#[derive(
    Clone,
    Copy,
    Debug,
    Deserialize,
    Eq,
    FromPrimitive,
    Hash,
    MallocSizeOf,
    Parse,
    PartialEq,
    Serialize,
    SpecifiedValueInfo,
    ToComputedValue,
    ToCss,
    ToResolvedValue,
    ToShmem,
    ToTyped,
)]
#[repr(u8)]
pub enum ColumnFill {
    Balance,
    Auto,
}
