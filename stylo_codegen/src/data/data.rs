// It is important that the order of these physical / logical variants matches
// the order of the enum variants in logical_geometry.rs

use crate::{util::array_concat, Engine};

pub const PHYSICAL_SIDES: [&str; 4] = ["top", "right", "bottom", "left"];
pub const PHYSICAL_CORNERS: [&str; 4] = ["top-left", "top-right", "bottom-right", "bottom-left"];
pub const PHYSICAL_AXES: [&str; 2] = ["y", "x"];
pub const PHYSICAL_SIZES: [&str; 2] = ["height", "width"];
pub const LOGICAL_SIDES: [&str; 4] = ["block-start", "block-end", "inline-start", "inline-end"];
pub const LOGICAL_CORNERS: [&str; 4] = ["start-start", "start-end", "end-start", "end-end"];
pub const LOGICAL_SIZES: [&str; 2] = ["block-size", "inline-size"];
pub const LOGICAL_AXES: [&str; 2] = ["block", "inline"];

#[derive(Default)]
pub struct RepeatedPropDef {
    pub name: &'static str,
    pub is_logical: bool,
}
impl RepeatedPropDef {
    pub fn physical(name: &'static str) -> Self {
        Self {
            name,
            is_logical: false,
        }
    }
    pub fn logical(name: &'static str) -> Self {
        Self {
            name,
            is_logical: true,
        }
    }

    pub fn maybe_moz_logical_alias(
        &self,
        engine: Engine,
        fmt: impl Fn(&str) -> String,
    ) -> Option<String> {
        if engine.is_gecko() && self.is_logical {
            let mut iter = self.name.split("-");
            let axis = iter.next();
            let dir = iter.next();
            if let (Some("inline"), Some(dir)) = (axis, dir) {
                return Some(fmt(dir));
            }
        }

        return None;
    }
}

// bool is True when logical
pub fn all_sides() -> [RepeatedPropDef; 8] {
    array_concat(
        PHYSICAL_SIDES.map(RepeatedPropDef::physical),
        LOGICAL_SIDES.map(RepeatedPropDef::logical),
    )
}

pub fn all_sizes() -> [RepeatedPropDef; 4] {
    array_concat(
        PHYSICAL_SIZES.map(RepeatedPropDef::physical),
        LOGICAL_SIZES.map(RepeatedPropDef::logical),
    )
}

pub fn all_corners() -> [RepeatedPropDef; 8] {
    array_concat(
        PHYSICAL_CORNERS.map(RepeatedPropDef::physical),
        LOGICAL_CORNERS.map(RepeatedPropDef::logical),
    )
}

pub fn all_axes() -> [RepeatedPropDef; 4] {
    array_concat(
        PHYSICAL_AXES.map(RepeatedPropDef::physical),
        LOGICAL_AXES.map(RepeatedPropDef::logical),
    )
}

pub const SYSTEM_FONT_LONGHANDS: &[&str] = &[
    "font_family",
    "font_size",
    "font_style",
    "font_stretch",
    "font_weight",
];

// TODO: make into a Set?
pub const PRIORITARY_PROPERTIES: &[&str] = &[
    // The writing-mode group has the most priority of all property groups, as
    // sizes like font-size can depend on it.
    "writing-mode",
    "direction",
    "text-orientation",
    // The fonts and colors group has the second priority, as all other lengths
    // and colors depend on them.
    //
    // There are some interdependencies between these, but we fix them up in
    // Cascade::fixup_font_stuff.
    // Needed to properly compute the zoomed font-size.
    "-x-text-scale",
    // Needed to do font-size computation in a language-dependent way.
    "-x-lang",
    // Needed for ruby to respect language-dependent min-font-size
    // preferences properly, see bug 1165538.
    "-moz-min-font-size-ratio",
    // font-size depends on math-depth's computed value.
    "math-depth",
    // Needed to compute the first available font and its used size,
    // in order to compute font-relative units correctly.
    "font-size",
    "font-size-adjust",
    "font-weight",
    "font-stretch",
    "font-style",
    "font-family",
    // color-scheme affects how system colors and light-dark() resolve.
    "color-scheme",
    // forced-color-adjust affects whether colors are adjusted.
    "forced-color-adjust",
    // Zoom affects all absolute lengths.
    "zoom",
    // Line height lengths depend on this.
    "line-height",
];

// TODO: make into a Set?
pub const VISITED_DEPENDENT_PROPERTIES: &[&str] = &[
    "column-rule-color",
    "text-emphasis-color",
    "-webkit-text-fill-color",
    "-webkit-text-stroke-color",
    "text-decoration-color",
    "fill",
    "stroke",
    "caret-color",
    "background-color",
    "border-top-color",
    "border-right-color",
    "border-bottom-color",
    "border-left-color",
    "border-block-start-color",
    "border-inline-end-color",
    "border-block-end-color",
    "border-inline-start-color",
    "outline-color",
    "color",
];

pub const ZOOM_DEPENDENT_PREDEFINED_TYPES: &[&str] = &[
    "BorderSpacing",
    "FontSize",
    "Inset",
    "Length",
    "LengthPercentage",
    "LengthPercentageOrAuto",
    "LetterSpacing",
    "LineHeight",
    "LineWidth",
    "MaxSize",
    "NonNegativeLength",
    "NonNegativeLengthOrAuto",
    "NonNegativeLengthOrNumber",
    "NonNegativeLengthOrNumberRect",
    "NonNegativeLengthPercentage",
    "NonNegativeLengthPercentageOrNormal",
    "Position",
    "PositionOrAuto",
    "SimpleShadow",
    "Size",
    "SVGLength",
    "SVGStrokeDashArray",
    "SVGWidth",
    "TextDecorationLength",
    "TextIndent",
    "WordSpacing",
];

/// Specified types whose implementation in Stylo implements the Rust Copy trait
pub const COPY_SPECIFIED_TYPES: &[&str] = &[
    "AlignContent",
    "AlignItems",
    "AlignSelf",
    "Appearance",
    "AnimationComposition",
    "AnimationDirection",
    "AnimationFillMode",
    "AnimationPlayState",
    "AspectRatio",
    "BaselineSource",
    "BreakBetween",
    "BreakWithin",
    "BackgroundRepeat",
    "BorderImageRepeat",
    "BorderStyle",
    "table::CaptionSide",
    "Clear",
    "ColumnCount",
    "Contain",
    "ContentVisibility",
    "ContainerType",
    "Display",
    "FillRule",
    "Float",
    "FontLanguageOverride",
    "FontSizeAdjust",
    "FontStretch",
    "FontStyle",
    "FontSynthesis",
    "FontSynthesisStyle",
    "FontVariantEastAsian",
    "FontVariantLigatures",
    "FontVariantNumeric",
    "FontWeight",
    "GreaterThanOrEqualToOneNumber",
    "GridAutoFlow",
    "ImageRendering",
    "Inert",
    "InitialLetter",
    "Integer",
    "PositionArea",
    "PositionAreaKeyword",
    "PositionProperty",
    "JustifyContent",
    "JustifyItems",
    "JustifySelf",
    "LineBreak",
    "LineClamp",
    "MasonryAutoFlow",
    "MozTheme",
    "BoolInteger",
    "text::MozControlCharacterVisibility",
    "MathDepth",
    "MozScriptMinSize",
    "MozScriptSizeMultiplier",
    "TransformBox",
    "TextDecorationSkipInk",
    "NonNegativeNumber",
    "OffsetRotate",
    "Opacity",
    "OutlineStyle",
    "Overflow",
    "OverflowAnchor",
    "OverflowClipBox",
    "OverflowWrap",
    "OverscrollBehavior",
    "PageOrientation",
    "Percentage",
    "PointerEvents",
    "PositionTryOrder",
    "PositionVisibility",
    "PrintColorAdjust",
    "ForcedColorAdjust",
    "Resize",
    "RubyPosition",
    "SVGOpacity",
    "SVGPaintOrder",
    "ScrollbarGutter",
    "ScrollSnapAlign",
    "ScrollSnapAxis",
    "ScrollSnapStop",
    "ScrollSnapStrictness",
    "ScrollSnapType",
    "TextAlign",
    "TextAlignLast",
    "TextDecorationLine",
    "TextEmphasisPosition",
    "TextJustify",
    "TextTransform",
    "TextUnderlinePosition",
    "TouchAction",
    "TransformStyle",
    "UserFocus",
    "UserInput",
    "UserSelect",
    "VectorEffect",
    "WordBreak",
    "WritingModeProperty",
    "XSpan",
    "XTextScale",
    "ZIndex",
    "Zoom",
];
