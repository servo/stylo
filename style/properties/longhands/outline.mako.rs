/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

<%namespace name="helpers" file="/helpers.mako.rs" />

${helpers.predefined_type(
    "outline-color",
    "Color",
    "computed_value::T::currentcolor()",
    engines="gecko servo",
    initial_specified_value="specified::Color::currentcolor()",
    ignored_when_colors_disabled=True,
    spec="https://drafts.csswg.org/css-ui/#propdef-outline-color",
    servo_restyle_damage="repaint",
    affects="paint",
)}

${helpers.predefined_type(
    "outline-style",
    "OutlineStyle",
    "computed::OutlineStyle::none()",
    engines="gecko servo",
    initial_specified_value="specified::OutlineStyle::none()",
    animation_type="discrete",
    spec="https://drafts.csswg.org/css-ui/#propdef-outline-style",
    servo_restyle_damage="repaint",
    affects="overflow",
)}

${helpers.predefined_type(
    "outline-width",
    "BorderSideWidth",
    "computed::BorderSideWidth::medium()",
    engines="gecko servo",
    initial_specified_value="specified::BorderSideWidth::medium()",
    spec="https://drafts.csswg.org/css-ui/#propdef-outline-width",
    servo_restyle_damage="repaint",
    affects="overflow",
)}

${helpers.predefined_type(
    "outline-offset",
    "BorderSideOffset",
    "app_units::Au(0)",
    engines="gecko servo",
    spec="https://drafts.csswg.org/css-ui/#propdef-outline-offset",
    servo_restyle_damage="repaint",
    affects="overflow",
)}
