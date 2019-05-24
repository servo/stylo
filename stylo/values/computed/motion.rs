/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Computed types for CSS values that are related to motion path.

use crate::values::computed::Angle;
use crate::Zero;

/// A computed offset-path. The computed value is as specified value.
///
/// https://drafts.fxtf.org/motion-1/#offset-path-property
pub use crate::values::specified::motion::OffsetPath;

#[inline]
fn is_auto_zero_angle(auto: &bool, angle: &Angle) -> bool {
    *auto && angle.is_zero()
}

/// A computed offset-rotate.
#[derive(Clone, Copy, Debug, MallocSizeOf, PartialEq, ToCss, ToResolvedValue)]
#[repr(C)]
pub struct OffsetRotate {
    /// If auto is false, this is a fixed angle which indicates a
    /// constant clockwise rotation transformation applied to it by this
    /// specified rotation angle. Otherwise, the angle will be added to
    /// the angle of the direction in layout.
    #[css(represents_keyword)]
    pub auto: bool,
    /// The angle value.
    #[css(contextual_skip_if = "is_auto_zero_angle")]
    pub angle: Angle,
}

impl OffsetRotate {
    /// Returns "auto 0deg".
    #[inline]
    pub fn auto() -> Self {
        OffsetRotate {
            auto: true,
            angle: Zero::zero(),
        }
    }
}
