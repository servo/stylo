/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

<%namespace name="helpers" file="/helpers.mako.rs" />

${helpers.predefined_type(
    "content",
    "Content",
    "computed::Content::normal()",
    engines="gecko servo",
    initial_specified_value="specified::Content::normal()",
    animation_type="discrete",
    spec="https://drafts.csswg.org/css-content/#propdef-content",
    servo_restyle_damage="rebuild_box",
    affects="layout",
)}

${helpers.predefined_type(
    "counter-increment",
    "CounterIncrement",
    engines="gecko servo",
    servo_pref="layout.unimplemented",
    initial_value="Default::default()",
    animation_type="discrete",
    spec="https://drafts.csswg.org/css-lists/#propdef-counter-increment",
    servo_restyle_damage="rebuild_box",
    affects="layout",
)}

${helpers.predefined_type(
    "counter-reset",
    "CounterReset",
    engines="gecko servo",
    servo_pref="layout.unimplemented",
    initial_value="Default::default()",
    animation_type="discrete",
    spec="https://drafts.csswg.org/css-lists-3/#propdef-counter-reset",
    servo_restyle_damage="rebuild_box",
    affects="layout",
)}

${helpers.predefined_type(
    "counter-set",
    "CounterSet",
    engines="gecko",
    initial_value="Default::default()",
    animation_type="discrete",
    spec="https://drafts.csswg.org/css-lists-3/#propdef-counter-set",
    servo_restyle_damage="rebuild_box",
    affects="layout",
)}
