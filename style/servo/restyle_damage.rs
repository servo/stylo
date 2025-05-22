/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! The restyle damage is a hint that tells layout which kind of operations may
//! be needed in presence of incremental style changes.

use crate::computed_values::isolation::T as Isolation;
use crate::computed_values::mix_blend_mode::T as MixBlendMode;
use crate::matching::{StyleChange, StyleDifference};
use crate::properties::ComputedValues;
use crate::values::computed::basic_shape::ClipPath;
use std::fmt;

bitflags! {
    /// Individual layout actions that may be necessary after restyling.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct ServoRestyleDamage: u8 {
        /// Repaint the node itself.
        ///
        /// Propagates both up and down the flow tree.
        const REPAINT = 0b001;

        /// Rebuilds the stacking contexts.
        ///
        /// Propagates both up and down the flow tree.
        const REBUILD_STACKING_CONTEXT = 0b011;

        /// Any other type of damage, which requires rebuilding all layout objects.
        ///
        /// Propagates both up and down the flow tree.
        const REBUILD_BOX = 0b111;
    }
}

malloc_size_of_is_0!(ServoRestyleDamage);

impl ServoRestyleDamage {
    /// Compute the `StyleDifference` (including the appropriate restyle damage)
    /// for a given style change between `old` and `new`.
    pub fn compute_style_difference(old: &ComputedValues, new: &ComputedValues) -> StyleDifference {
        let damage = compute_damage(old, new);
        let change = if damage.is_empty() {
            StyleChange::Unchanged
        } else {
            // FIXME(emilio): Differentiate between reset and inherited
            // properties here, and set `reset_only` appropriately so the
            // optimization to skip the cascade in those cases applies.
            StyleChange::Changed { reset_only: false }
        };
        StyleDifference { damage, change }
    }

    /// Returns a bitmask indicating that the frame needs to be reconstructed.
    pub fn reconstruct() -> ServoRestyleDamage {
        ServoRestyleDamage::REBUILD_BOX
    }
}

impl Default for ServoRestyleDamage {
    fn default() -> Self {
        Self::empty()
    }
}

impl fmt::Display for ServoRestyleDamage {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut first_elem = true;

        let to_iter = [
            (ServoRestyleDamage::REPAINT, "Repaint"),
            (
                ServoRestyleDamage::REBUILD_STACKING_CONTEXT,
                "Rebuild stacking context",
            ),
            (ServoRestyleDamage::REBUILD_BOX, "Rebuild box"),
        ];

        for &(damage, damage_str) in &to_iter {
            if self.contains(damage) {
                if !first_elem {
                    write!(f, " | ")?;
                }
                write!(f, "{}", damage_str)?;
                first_elem = false;
            }
        }

        if first_elem {
            write!(f, "NoDamage")?;
        }

        Ok(())
    }
}

fn compute_damage(old: &ComputedValues, new: &ComputedValues) -> ServoRestyleDamage {
    let mut damage = ServoRestyleDamage::empty();

    // This should check every CSS property, as enumerated in the fields of
    // https://doc.servo.org/style/properties/struct.ComputedValues.html

    // Some properties establish a stacking context when they are set to a non-initial value.
    // In that case, the damage is only set to `ServoRestyleDamage::REPAINT` because we don't
    // need to rebuild stacking contexts when the style changes between different non-initial
    // values. This function checks whether any of these properties is set to a value that
    // guarantees a stacking context, so that we only do the work when this changes.
    // Note that it's still possible to establish a stacking context when this returns false.
    let guarantees_stacking_context = |style: &ComputedValues| {
        style.get_effects().opacity != 1.0 ||
            old.get_effects().mix_blend_mode != MixBlendMode::Normal ||
            old.get_svg().clip_path != ClipPath::None ||
            style.get_box().isolation == Isolation::Isolate
    };

    // This uses short-circuiting boolean OR for its side effects and ignores the result.
    let _ = restyle_damage_rebuild_box!(
        old,
        new,
        damage,
        [ServoRestyleDamage::REBUILD_BOX],
        old.get_box().original_display != new.get_box().original_display
    ) || restyle_damage_rebuild_stacking_context!(
        old,
        new,
        damage,
        [ServoRestyleDamage::REBUILD_STACKING_CONTEXT],
        guarantees_stacking_context(old) != guarantees_stacking_context(new)
    ) || restyle_damage_repaint!(old, new, damage, [ServoRestyleDamage::REPAINT]);

    // Paint worklets may depend on custom properties,
    // so if they have changed we should repaint.
    if !old.custom_properties_equal(new) {
        damage.insert(ServoRestyleDamage::REPAINT);
    }
    damage
}
