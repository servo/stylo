/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

<%namespace name="helpers" file="/helpers.mako.rs" />

${helpers.predefined_type(
    "column-width",
    "length::NonNegativeLengthOrAuto",
    "computed::length::NonNegativeLengthOrAuto::auto()",
    engines="gecko servo",
    initial_specified_value="specified::length::NonNegativeLengthOrAuto::auto()",
    servo_pref="layout.columns.enabled",
    spec="https://drafts.csswg.org/css-multicol/#propdef-column-width",
    servo_restyle_damage="rebuild_box",
    affects="layout",
)}

${helpers.predefined_type(
    "column-count",
    "ColumnCount",
    "computed::ColumnCount::Auto",
    engines="gecko servo",
    initial_specified_value="specified::ColumnCount::Auto",
    servo_pref="layout.columns.enabled",
    spec="https://drafts.csswg.org/css-multicol/#propdef-column-count",
    servo_restyle_damage="rebuild_box",
    affects="layout",
)}

${helpers.single_keyword(
    "column-fill",
    "balance auto",
    engines="gecko",
    animation_type="discrete",
    gecko_enum_prefix="StyleColumnFill",
    spec="https://drafts.csswg.org/css-multicol/#propdef-column-fill",
    affects="layout",
)}

${helpers.predefined_type(
    "column-rule-width",
    "BorderSideWidth",
    "app_units::Au::from_px(3)",
    engines="gecko",
    initial_specified_value="specified::BorderSideWidth::medium()",
    spec="https://drafts.csswg.org/css-multicol/#propdef-column-rule-width",
    affects="layout",
)}

// https://drafts.csswg.org/css-multicol-1/#crc
${helpers.predefined_type(
    "column-rule-color",
    "Color",
    "computed_value::T::currentcolor()",
    engines="gecko",
    initial_specified_value="specified::Color::currentcolor()",
    ignored_when_colors_disabled=True,
    spec="https://drafts.csswg.org/css-multicol/#propdef-column-rule-color",
    affects="paint",
)}

${helpers.single_keyword(
    "column-span",
    "none all",
    engines="gecko servo",
    servo_pref="layout.columns.enabled",
    animation_type="discrete",
    gecko_enum_prefix="StyleColumnSpan",
    spec="https://drafts.csswg.org/css-multicol/#propdef-column-span",
    affects="layout",
)}

${helpers.predefined_type(
    "column-rule-style",
    "BorderStyle",
    "computed::BorderStyle::None",
    engines="gecko",
    initial_specified_value="specified::BorderStyle::None",
    animation_type="discrete",
    spec="https://drafts.csswg.org/css-multicol/#propdef-column-rule-style",
    affects="paint",
)}
