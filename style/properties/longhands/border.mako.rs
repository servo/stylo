/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

<%namespace name="helpers" file="/helpers.mako.rs" />
<% from data import ALL_CORNERS, ALL_SIDES, maybe_moz_logical_alias %>

<%
    def maybe_logical_spec(side, kind):
        if side[1]: # if it is logical
            return "https://drafts.csswg.org/css-logical-props/#propdef-border-%s-%s" % (side[0], kind)
        else:
            return "https://drafts.csswg.org/css-backgrounds/#border-%s-%s" % (side[0], kind)
%>
% for side in ALL_SIDES:
    <%
        side_name = side[0]
        is_logical = side[1]
    %>
    ${helpers.predefined_type(
        "border-%s-color" % side_name, "Color",
        "computed_value::T::currentcolor()",
        engines="gecko servo",
        aliases=maybe_moz_logical_alias(engine, side, "-moz-border-%s-color"),
        spec=maybe_logical_spec(side, "color"),
        logical=is_logical,
        logical_group="border-color",
        allow_quirks="No" if is_logical else "Yes",
        ignored_when_colors_disabled=True,
        affects="paint",
    )}

    ${helpers.predefined_type(
        "border-%s-style" % side_name, "BorderStyle",
        "specified::BorderStyle::None",
        engines="gecko servo",
        aliases=maybe_moz_logical_alias(engine, side, "-moz-border-%s-style"),
        spec=maybe_logical_spec(side, "style"),
        animation_type="discrete" if not is_logical else "none",
        logical=is_logical,
        logical_group="border-style",
        affects="layout",
    )}

    ${helpers.predefined_type(
        "border-%s-width" % side_name,
        "BorderSideWidth",
        "app_units::Au::from_px(3)",
        engines="gecko servo",
        aliases=maybe_moz_logical_alias(engine, side, "-moz-border-%s-width"),
        spec=maybe_logical_spec(side, "width"),
        logical=is_logical,
        logical_group="border-width",
        allow_quirks="No" if is_logical else "Yes",
        servo_restyle_damage="rebuild_box",
        affects="layout",
    )}
% endfor

% for corner in ALL_CORNERS:
    <%
        corner_name = corner[0]
        is_logical = corner[1]
        if is_logical:
            prefixes = None
        else:
            prefixes = "webkit"
    %>
    ${helpers.predefined_type(
        "border-%s-radius" % corner_name,
        "BorderCornerRadius",
        "computed::BorderCornerRadius::zero()",
        "parse",
        engines="gecko servo",
        extra_prefixes=prefixes,
        spec=maybe_logical_spec(corner, "radius"),
        gecko_ffi_name="mBorderRadius.{}".format(corner_name.replace('-', '_')),
        boxed=True,
        logical_group="border-radius",
        logical=is_logical,
        affects="paint",
    )}
% endfor

${helpers.single_keyword(
    "box-decoration-break",
    "slice clone",
    engines="gecko",
    gecko_enum_prefix="StyleBoxDecorationBreak",
    spec="https://drafts.csswg.org/css-break/#propdef-box-decoration-break",
    animation_type="discrete",
    affects="layout",
)}

${helpers.single_keyword(
    "-moz-float-edge",
    "content-box margin-box",
    engines="gecko",
    gecko_ffi_name="mFloatEdge",
    gecko_enum_prefix="StyleFloatEdge",
    spec="Nonstandard (https://developer.mozilla.org/en-US/docs/Web/CSS/-moz-float-edge)",
    animation_type="discrete",
    affects="layout",
)}

${helpers.predefined_type(
    "border-image-source",
    "Image",
    engines="gecko servo",
    initial_value="computed::Image::None",
    initial_specified_value="specified::Image::None",
    spec="https://drafts.csswg.org/css-backgrounds/#the-background-image",
    vector=False,
    animation_type="discrete",
    ignored_when_colors_disabled=True,
    servo_restyle_damage="repaint",
    affects="paint",
)}

${helpers.predefined_type(
    "border-image-outset",
    "NonNegativeLengthOrNumberRect",
    engines="gecko servo",
    initial_value="generics::rect::Rect::all(computed::NonNegativeLengthOrNumber::zero())",
    initial_specified_value="generics::rect::Rect::all(specified::NonNegativeLengthOrNumber::zero())",
    spec="https://drafts.csswg.org/css-backgrounds/#border-image-outset",
    boxed=True,
    servo_restyle_damage="repaint",
    affects="paint",
)}

${helpers.predefined_type(
    "border-image-repeat",
    "BorderImageRepeat",
    "computed::BorderImageRepeat::stretch()",
    engines="gecko servo",
    initial_specified_value="specified::BorderImageRepeat::stretch()",
    animation_type="discrete",
    spec="https://drafts.csswg.org/css-backgrounds/#the-border-image-repeat",
    servo_restyle_damage="repaint",
    affects="paint",
)}

${helpers.predefined_type(
    "border-image-width",
    "BorderImageWidth",
    engines="gecko servo",
    initial_value="computed::BorderImageWidth::all(computed::BorderImageSideWidth::one())",
    initial_specified_value="specified::BorderImageWidth::all(specified::BorderImageSideWidth::one())",
    spec="https://drafts.csswg.org/css-backgrounds/#border-image-width",
    boxed=True,
    servo_restyle_damage="repaint",
    affects="paint",
)}

${helpers.predefined_type(
    "border-image-slice",
    "BorderImageSlice",
    engines="gecko servo",
    initial_value="computed::BorderImageSlice::hundred_percent()",
    initial_specified_value="specified::BorderImageSlice::hundred_percent()",
    spec="https://drafts.csswg.org/css-backgrounds/#border-image-slice",
    boxed=True,
    servo_restyle_damage="repaint",
    affects="paint",
)}
