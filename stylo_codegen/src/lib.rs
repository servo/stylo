/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

mod data;
mod util;

#[derive(Copy, Clone, PartialEq)]
pub enum Engine {
    Gecko,
    Servo,
}

impl Engine {
    pub fn is_gecko(self) -> bool {
        matches!(self, Self::Gecko)
    }
}
