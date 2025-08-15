/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Helpers for computing restyle damage

use crate::computed_values::isolation::T as Isolation;
use crate::computed_values::mix_blend_mode::T as MixBlendMode;
use crate::computed_values::transform_style::T as TransformStyle;
use crate::properties::{
    restyle_damage_rebuild_box, restyle_damage_rebuild_stacking_context, style_structs,
    ComputedValues,
};
use crate::values::computed::basic_shape::ClipPath;
use crate::values::computed::Perspective;
use crate::values::generics::transform::{GenericRotate, GenericScale, GenericTranslate};

/// Returns true if "relayout" damage should be applied
pub fn restyle_damage_layout(old: &ComputedValues, new: &ComputedValues) -> bool {
    let old_box = old.get_box();
    let new_box = new.get_box();
    restyle_damage_rebuild_box(old, new)
        || old_box.original_display != new_box.original_display
        || old_box.has_transform_or_perspective() != new_box.has_transform_or_perspective()
        || old.get_effects().filter.0.is_empty() != new.get_effects().filter.0.is_empty()
}

/// Returns true if "rebuild_stacking_context" damage should be applied
pub fn augmented_restyle_damage_rebuild_stacking_context(
    old: &ComputedValues,
    new: &ComputedValues,
) -> bool {
    restyle_damage_rebuild_stacking_context(old, new)
        || old.guarantees_stacking_context() != new.guarantees_stacking_context()
}

impl ComputedValues {
    /// Some properties establish a stacking context when they are set to a non-initial value.
    /// In that case, the damage is only set to `ServoRestyleDamage::REPAINT` because we don't
    /// need to rebuild stacking contexts when the style changes between different non-initial
    /// values. This function checks whether any of these properties is set to a value that
    /// guarantees a stacking context, so that we only do the work when this changes.
    /// Note that it's still possible to establish a stacking context when this returns false.
    pub fn guarantees_stacking_context(&self) -> bool {
        self.get_effects().opacity != 1.0
            || self.get_effects().mix_blend_mode != MixBlendMode::Normal
            || self.get_svg().clip_path != ClipPath::None
            || self.get_box().isolation == Isolation::Isolate
    }
}

impl style_structs::Box {
    /// Whether there is a non-default transform or perspective style set
    pub fn has_transform_or_perspective(&self) -> bool {
        !self.transform.0.is_empty()
            || self.scale != GenericScale::None
            || self.rotate != GenericRotate::None
            || self.translate != GenericTranslate::None
            || self.perspective != Perspective::None
            || self.transform_style == TransformStyle::Preserve3d
    }
}
