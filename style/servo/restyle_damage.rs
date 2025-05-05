/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! The restyle damage is a hint that tells layout which kind of operations may
//! be needed in presence of incremental style changes.

use crate::computed_values::display::T as Display;
use crate::matching::{StyleChange, StyleDifference};
use crate::properties::ComputedValues;
use std::fmt;

bitflags! {
    /// Individual layout actions that may be necessary after restyling.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct ServoRestyleDamage: u8 {
        /// Repaint the node itself.
        ///
        /// Propagates both up and down the flow tree.
        const REPAINT = 0x01;

        /// Any other type of damage, which requires rebuilding all layout objects.
        ///
        /// Propagates both up and down the flow tree.
        const REBUILD = 0x02;
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
        ServoRestyleDamage::REBUILD
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
            (ServoRestyleDamage::REBUILD, "Rebuild"),
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

    // This uses short-circuiting boolean OR for its side effects and ignores the result.
    let _ = restyle_damage_rebuild_and_reflow!(
        old,
        new,
        damage,
        [
            ServoRestyleDamage::REBUILD
        ],
        old.get_box().original_display != new.get_box().original_display
    ) || (new.get_box().display == Display::Inline &&
        restyle_damage_rebuild_and_reflow_inline!(
            old,
            new,
            damage,
            [
                ServoRestyleDamage::REBUILD
            ]
        )) ||
        restyle_damage_reflow!(
            old,
            new,
            damage,
            [
                ServoRestyleDamage::REBUILD
            ]
        ) ||
        restyle_damage_reflow_out_of_flow!(
            old,
            new,
            damage,
            [
                ServoRestyleDamage::REBUILD
            ]
        ) ||
        restyle_damage_repaint!(old, new, damage, [ServoRestyleDamage::REPAINT]);

    // Paint worklets may depend on custom properties,
    // so if they have changed we should repaint.
    if !old.custom_properties_equal(new) {
        damage.insert(ServoRestyleDamage::REPAINT);
    }
    damage
}
