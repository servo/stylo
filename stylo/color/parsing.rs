/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#![deny(missing_docs)]

//! Parsing for CSS colors.

use super::{
    color_function::ColorFunction,
    component::{ColorComponent, ColorComponentType},
    AbsoluteColor,
};
use crate::{
    parser::{Parse, ParserContext},
    values::{
        generics::calc::CalcUnits,
        specified::{
            angle::Angle as SpecifiedAngle, calc::Leaf as SpecifiedLeaf,
            color::Color as SpecifiedColor,
        },
    },
};
use cssparser::{
    color::{parse_hash_color, PredefinedColorSpace, OPAQUE},
    match_ignore_ascii_case, CowRcStr, Parser, Token,
};
use style_traits::{ParseError, StyleParseErrorKind};

/// Returns true if the relative color syntax pref is enabled.
#[inline]
pub fn rcs_enabled() -> bool {
    static_prefs::pref!("layout.css.relative-color-syntax.enabled")
}

/// Represents a channel keyword inside a color.
#[derive(Clone, Copy, Debug, MallocSizeOf, Parse, PartialEq, PartialOrd, ToCss, ToShmem)]
pub enum ChannelKeyword {
    /// alpha
    Alpha,
    /// a
    A,
    /// b, blackness, blue
    B,
    /// chroma
    C,
    /// green
    G,
    /// hue
    H,
    /// lightness
    L,
    /// red
    R,
    /// saturation
    S,
    /// whiteness
    W,
    /// x
    X,
    /// y
    Y,
    /// z
    Z,
}

const RGB_CHANNEL_KEYWORDS: &[ChannelKeyword] = &[
    ChannelKeyword::R,
    ChannelKeyword::G,
    ChannelKeyword::B,
    ChannelKeyword::Alpha,
];

const HSL_CHANNEL_KEYWORDS: &[ChannelKeyword] = &[
    ChannelKeyword::H,
    ChannelKeyword::S,
    ChannelKeyword::L,
    ChannelKeyword::Alpha,
];

const HWB_CHANNEL_KEYWORDS: &[ChannelKeyword] = &[
    ChannelKeyword::H,
    ChannelKeyword::W,
    ChannelKeyword::B,
    ChannelKeyword::Alpha,
];

const LAB_CHANNEL_KEYWORDS: &[ChannelKeyword] = &[
    ChannelKeyword::L,
    ChannelKeyword::A,
    ChannelKeyword::B,
    ChannelKeyword::Alpha,
];

const LCH_CHANNEL_KEYWORDS: &[ChannelKeyword] = &[
    ChannelKeyword::L,
    ChannelKeyword::C,
    ChannelKeyword::H,
    ChannelKeyword::Alpha,
];

const XYZ_CHANNEL_KEYWORDS: &[ChannelKeyword] = &[
    ChannelKeyword::X,
    ChannelKeyword::Y,
    ChannelKeyword::Z,
    ChannelKeyword::Alpha,
];

/// Return the named color with the given name.
///
/// Matching is case-insensitive in the ASCII range.
/// CSS escaping (if relevant) should be resolved before calling this function.
/// (For example, the value of an `Ident` token is fine.)
#[inline]
pub fn parse_color_keyword(ident: &str) -> Result<SpecifiedColor, ()> {
    Ok(match_ignore_ascii_case! { ident,
        "transparent" => {
            SpecifiedColor::from_absolute_color(AbsoluteColor::srgb_legacy(0u8, 0u8, 0u8, 0.0))
        },
        "currentcolor" => SpecifiedColor::CurrentColor,
        _ => {
            let (r, g, b) = cssparser::color::parse_named_color(ident)?;
            SpecifiedColor::from_absolute_color(AbsoluteColor::srgb_legacy(r, g, b, OPAQUE))
        },
    })
}

/// Parse a CSS color using the specified [`ColorParser`] and return a new color
/// value on success.
pub fn parse_color_with<'i, 't>(
    context: &ParserContext,
    input: &mut Parser<'i, 't>,
) -> Result<SpecifiedColor, ParseError<'i>> {
    let location = input.current_source_location();
    let token = input.next()?;
    match *token {
        Token::Hash(ref value) | Token::IDHash(ref value) => parse_hash_color(value.as_bytes())
            .map(|(r, g, b, a)| {
                SpecifiedColor::from_absolute_color(AbsoluteColor::srgb_legacy(r, g, b, a))
            }),
        Token::Ident(ref value) => parse_color_keyword(value),
        Token::Function(ref name) => {
            let name = name.clone();
            return input.parse_nested_block(|arguments| {
                let color_function = parse_color_function(context, name, arguments)?;

                if color_function.has_origin_color() {
                    // Preserve the color as it was parsed.
                    Ok(SpecifiedColor::ColorFunction(Box::new(color_function)))
                } else if let Ok(resolved) = color_function.resolve_to_absolute() {
                    Ok(SpecifiedColor::from_absolute_color(resolved))
                } else {
                    // This will only happen when the parsed color contains errors like calc units
                    // that cannot be resolved at parse time, but will fail when trying to resolve
                    // them, etc. This should be rare, but for now just failing the color value
                    // makes sense.
                    Err(location.new_custom_error(StyleParseErrorKind::UnspecifiedError))
                }
            });
        },
        _ => Err(()),
    }
    .map_err(|()| location.new_unexpected_token_error(token.clone()))
}

/// Parse one of the color functions: rgba(), lab(), color(), etc.
#[inline]
fn parse_color_function<'i, 't>(
    context: &ParserContext,
    name: CowRcStr<'i>,
    arguments: &mut Parser<'i, 't>,
) -> Result<ColorFunction<SpecifiedColor>, ParseError<'i>> {
    let origin_color = parse_origin_color(context, arguments)?;

    let color = match_ignore_ascii_case! { &name,
        "rgb" | "rgba" => parse_rgb(context, arguments, origin_color),
        "hsl" | "hsla" => parse_hsl(context, arguments, origin_color),
        "hwb" => parse_hwb(context, arguments, origin_color),
        "lab" => parse_lab_like(context, arguments, origin_color, ColorFunction::Lab),
        "lch" => parse_lch_like(context, arguments, origin_color, ColorFunction::Lch),
        "oklab" => parse_lab_like(context, arguments, origin_color, ColorFunction::Oklab),
        "oklch" => parse_lch_like(context, arguments, origin_color, ColorFunction::Oklch),
        "color" => parse_color_with_color_space(context, arguments, origin_color),
        _ => return Err(arguments.new_unexpected_token_error(Token::Ident(name))),
    }?;

    arguments.expect_exhausted()?;

    Ok(color)
}

/// Parse the relative color syntax "from" syntax `from <color>`.
fn parse_origin_color<'i, 't>(
    context: &ParserContext,
    arguments: &mut Parser<'i, 't>,
) -> Result<Option<SpecifiedColor>, ParseError<'i>> {
    if !rcs_enabled() {
        return Ok(None);
    }

    // Not finding the from keyword is not an error, it just means we don't
    // have an origin color.
    if arguments
        .try_parse(|p| p.expect_ident_matching("from"))
        .is_err()
    {
        return Ok(None);
    }

    SpecifiedColor::parse(context, arguments).map(Option::Some)
}

#[inline]
fn parse_rgb<'i, 't>(
    context: &ParserContext,
    arguments: &mut Parser<'i, 't>,
    origin_color: Option<SpecifiedColor>,
) -> Result<ColorFunction<SpecifiedColor>, ParseError<'i>> {
    let maybe_red = parse_number_or_percentage(context, arguments, true, RGB_CHANNEL_KEYWORDS)?;

    // If the first component is not "none" and is followed by a comma, then we
    // are parsing the legacy syntax.  Legacy syntax also doesn't support an
    // origin color.
    let is_legacy_syntax = origin_color.is_none() &&
        !maybe_red.is_none() &&
        arguments.try_parse(|p| p.expect_comma()).is_ok();

    Ok(if is_legacy_syntax {
        let (green, blue) = if maybe_red.could_be_percentage() {
            let green = parse_percentage(context, arguments, false, RGB_CHANNEL_KEYWORDS)?;
            arguments.expect_comma()?;
            let blue = parse_percentage(context, arguments, false, RGB_CHANNEL_KEYWORDS)?;
            (green, blue)
        } else {
            let green = parse_number(context, arguments, false, RGB_CHANNEL_KEYWORDS)?;
            arguments.expect_comma()?;
            let blue = parse_number(context, arguments, false, RGB_CHANNEL_KEYWORDS)?;
            (green, blue)
        };

        let alpha = parse_legacy_alpha(context, arguments, RGB_CHANNEL_KEYWORDS)?;

        ColorFunction::Rgb(origin_color, maybe_red, green, blue, alpha)
    } else {
        let green = parse_number_or_percentage(context, arguments, true, RGB_CHANNEL_KEYWORDS)?;
        let blue = parse_number_or_percentage(context, arguments, true, RGB_CHANNEL_KEYWORDS)?;

        let alpha = parse_modern_alpha(context, arguments, RGB_CHANNEL_KEYWORDS)?;

        ColorFunction::Rgb(origin_color, maybe_red, green, blue, alpha)
    })
}

/// Parses hsl syntax.
///
/// <https://drafts.csswg.org/css-color/#the-hsl-notation>
#[inline]
fn parse_hsl<'i, 't>(
    context: &ParserContext,
    arguments: &mut Parser<'i, 't>,
    origin_color: Option<SpecifiedColor>,
) -> Result<ColorFunction<SpecifiedColor>, ParseError<'i>> {
    let hue = parse_number_or_angle(context, arguments, true, HSL_CHANNEL_KEYWORDS)?;

    // If the hue is not "none" and is followed by a comma, then we are parsing
    // the legacy syntax. Legacy syntax also doesn't support an origin color.
    let is_legacy_syntax = origin_color.is_none() &&
        !hue.is_none() &&
        arguments.try_parse(|p| p.expect_comma()).is_ok();

    let (saturation, lightness, alpha) = if is_legacy_syntax {
        let saturation = parse_percentage(context, arguments, false, HSL_CHANNEL_KEYWORDS)?;
        arguments.expect_comma()?;
        let lightness = parse_percentage(context, arguments, false, HSL_CHANNEL_KEYWORDS)?;
        let alpha = parse_legacy_alpha(context, arguments, HSL_CHANNEL_KEYWORDS)?;
        (saturation, lightness, alpha)
    } else {
        let saturation =
            parse_number_or_percentage(context, arguments, true, HSL_CHANNEL_KEYWORDS)?;
        let lightness = parse_number_or_percentage(context, arguments, true, HSL_CHANNEL_KEYWORDS)?;
        let alpha = parse_modern_alpha(context, arguments, HSL_CHANNEL_KEYWORDS)?;
        (saturation, lightness, alpha)
    };

    Ok(ColorFunction::Hsl(
        origin_color,
        hue,
        saturation,
        lightness,
        alpha,
    ))
}

/// Parses hwb syntax.
///
/// <https://drafts.csswg.org/css-color/#the-hbw-notation>
#[inline]
fn parse_hwb<'i, 't>(
    context: &ParserContext,
    arguments: &mut Parser<'i, 't>,
    origin_color: Option<SpecifiedColor>,
) -> Result<ColorFunction<SpecifiedColor>, ParseError<'i>> {
    let hue = parse_number_or_angle(context, arguments, true, HWB_CHANNEL_KEYWORDS)?;
    let whiteness = parse_number_or_percentage(context, arguments, true, HWB_CHANNEL_KEYWORDS)?;
    let blackness = parse_number_or_percentage(context, arguments, true, HWB_CHANNEL_KEYWORDS)?;

    let alpha = parse_modern_alpha(context, arguments, HWB_CHANNEL_KEYWORDS)?;

    Ok(ColorFunction::Hwb(
        origin_color,
        hue,
        whiteness,
        blackness,
        alpha,
    ))
}

type IntoLabFn<Output> = fn(
    origin: Option<SpecifiedColor>,
    l: ColorComponent<NumberOrPercentage>,
    a: ColorComponent<NumberOrPercentage>,
    b: ColorComponent<NumberOrPercentage>,
    alpha: ColorComponent<NumberOrPercentage>,
) -> Output;

#[inline]
fn parse_lab_like<'i, 't>(
    context: &ParserContext,
    arguments: &mut Parser<'i, 't>,
    origin_color: Option<SpecifiedColor>,
    into_color: IntoLabFn<ColorFunction<SpecifiedColor>>,
) -> Result<ColorFunction<SpecifiedColor>, ParseError<'i>> {
    let lightness = parse_number_or_percentage(context, arguments, true, LAB_CHANNEL_KEYWORDS)?;
    let a = parse_number_or_percentage(context, arguments, true, LAB_CHANNEL_KEYWORDS)?;
    let b = parse_number_or_percentage(context, arguments, true, LAB_CHANNEL_KEYWORDS)?;

    let alpha = parse_modern_alpha(context, arguments, LAB_CHANNEL_KEYWORDS)?;

    Ok(into_color(origin_color, lightness, a, b, alpha))
}

type IntoLchFn<Output> = fn(
    origin: Option<SpecifiedColor>,
    l: ColorComponent<NumberOrPercentage>,
    a: ColorComponent<NumberOrPercentage>,
    b: ColorComponent<NumberOrAngle>,
    alpha: ColorComponent<NumberOrPercentage>,
) -> Output;

#[inline]
fn parse_lch_like<'i, 't>(
    context: &ParserContext,
    arguments: &mut Parser<'i, 't>,
    origin_color: Option<SpecifiedColor>,
    into_color: IntoLchFn<ColorFunction<SpecifiedColor>>,
) -> Result<ColorFunction<SpecifiedColor>, ParseError<'i>> {
    let lightness = parse_number_or_percentage(context, arguments, true, LCH_CHANNEL_KEYWORDS)?;
    let chroma = parse_number_or_percentage(context, arguments, true, LCH_CHANNEL_KEYWORDS)?;
    let hue = parse_number_or_angle(context, arguments, true, LCH_CHANNEL_KEYWORDS)?;

    let alpha = parse_modern_alpha(context, arguments, LCH_CHANNEL_KEYWORDS)?;

    Ok(into_color(origin_color, lightness, chroma, hue, alpha))
}

/// Parse the color() function.
#[inline]
fn parse_color_with_color_space<'i, 't>(
    context: &ParserContext,
    arguments: &mut Parser<'i, 't>,
    origin_color: Option<SpecifiedColor>,
) -> Result<ColorFunction<SpecifiedColor>, ParseError<'i>> {
    let color_space = PredefinedColorSpace::parse(arguments)?;

    let allowed_channel_keywords = match color_space {
        PredefinedColorSpace::Srgb |
        PredefinedColorSpace::SrgbLinear |
        PredefinedColorSpace::DisplayP3 |
        PredefinedColorSpace::A98Rgb |
        PredefinedColorSpace::ProphotoRgb |
        PredefinedColorSpace::Rec2020 => RGB_CHANNEL_KEYWORDS,
        PredefinedColorSpace::XyzD50 | PredefinedColorSpace::XyzD65 => XYZ_CHANNEL_KEYWORDS,
    };

    let c1 = parse_number_or_percentage(context, arguments, true, allowed_channel_keywords)?;
    let c2 = parse_number_or_percentage(context, arguments, true, allowed_channel_keywords)?;
    let c3 = parse_number_or_percentage(context, arguments, true, allowed_channel_keywords)?;

    let alpha = parse_modern_alpha(context, arguments, allowed_channel_keywords)?;

    Ok(ColorFunction::Color(
        origin_color,
        c1,
        c2,
        c3,
        alpha,
        color_space.into(),
    ))
}

/// Either a percentage or a number.
#[derive(Clone, Copy, Debug, MallocSizeOf, PartialEq, ToAnimatedValue, ToShmem)]
pub enum NumberOrPercentage {
    /// `<number>`.
    Number(f32),
    /// `<percentage>`
    /// The value as a float, divided by 100 so that the nominal range is 0.0 to 1.0.
    Percentage(f32),
}

impl NumberOrPercentage {
    /// Return the value as a number. Percentages will be adjusted to the range
    /// [0..percent_basis].
    pub fn to_number(&self, percentage_basis: f32) -> f32 {
        match *self {
            Self::Number(value) => value,
            Self::Percentage(unit_value) => unit_value * percentage_basis,
        }
    }
}

impl ColorComponentType for NumberOrPercentage {
    fn from_value(value: f32) -> Self {
        Self::Number(value)
    }

    fn units() -> CalcUnits {
        CalcUnits::PERCENTAGE
    }

    fn try_from_token(token: &Token) -> Result<Self, ()> {
        Ok(match *token {
            Token::Number { value, .. } => Self::Number(value),
            Token::Percentage { unit_value, .. } => Self::Percentage(unit_value),
            _ => {
                return Err(());
            },
        })
    }

    fn try_from_leaf(leaf: &SpecifiedLeaf) -> Result<Self, ()> {
        Ok(match *leaf {
            SpecifiedLeaf::Percentage(unit_value) => Self::Percentage(unit_value),
            SpecifiedLeaf::Number(value) => Self::Number(value),
            _ => return Err(()),
        })
    }
}

/// Either an angle or a number.
#[derive(Clone, Copy, Debug, MallocSizeOf, PartialEq, ToAnimatedValue, ToShmem)]
pub enum NumberOrAngle {
    /// `<number>`.
    Number(f32),
    /// `<angle>`
    /// The value as a number of degrees.
    Angle(f32),
}

impl NumberOrAngle {
    /// Return the angle in degrees. `NumberOrAngle::Number` is returned as
    /// degrees, because it is the canonical unit.
    pub fn degrees(&self) -> f32 {
        match *self {
            Self::Number(value) => value,
            Self::Angle(degrees) => degrees,
        }
    }
}

impl ColorComponentType for NumberOrAngle {
    fn from_value(value: f32) -> Self {
        Self::Number(value)
    }

    fn units() -> CalcUnits {
        CalcUnits::ANGLE
    }

    fn try_from_token(token: &Token) -> Result<Self, ()> {
        Ok(match *token {
            Token::Number { value, .. } => Self::Number(value),
            Token::Dimension {
                value, ref unit, ..
            } => {
                let degrees =
                    SpecifiedAngle::parse_dimension(value, unit, /* from_calc = */ false)
                        .map(|angle| angle.degrees())?;

                NumberOrAngle::Angle(degrees)
            },
            _ => {
                return Err(());
            },
        })
    }

    fn try_from_leaf(leaf: &SpecifiedLeaf) -> Result<Self, ()> {
        Ok(match *leaf {
            SpecifiedLeaf::Angle(angle) => Self::Angle(angle.degrees()),
            SpecifiedLeaf::Number(value) => Self::Number(value),
            _ => return Err(()),
        })
    }
}

/// The raw f32 here is for <number>.
impl ColorComponentType for f32 {
    fn from_value(value: f32) -> Self {
        value
    }

    fn units() -> CalcUnits {
        CalcUnits::empty()
    }

    fn try_from_token(token: &Token) -> Result<Self, ()> {
        if let Token::Number { value, .. } = *token {
            Ok(value)
        } else {
            Err(())
        }
    }

    fn try_from_leaf(leaf: &SpecifiedLeaf) -> Result<Self, ()> {
        if let SpecifiedLeaf::Number(value) = *leaf {
            Ok(value)
        } else {
            Err(())
        }
    }
}

/// Parse an `<number>` or `<angle>` value.
fn parse_number_or_angle<'i, 't>(
    context: &ParserContext,
    input: &mut Parser<'i, 't>,
    allow_none: bool,
    allowed_channel_keywords: &[ChannelKeyword],
) -> Result<ColorComponent<NumberOrAngle>, ParseError<'i>> {
    ColorComponent::parse(context, input, allow_none, allowed_channel_keywords)
}

/// Parse a `<percentage>` value.
fn parse_percentage<'i, 't>(
    context: &ParserContext,
    input: &mut Parser<'i, 't>,
    allow_none: bool,
    allowed_channel_keywords: &[ChannelKeyword],
) -> Result<ColorComponent<NumberOrPercentage>, ParseError<'i>> {
    let location = input.current_source_location();

    let value = ColorComponent::<NumberOrPercentage>::parse(
        context,
        input,
        allow_none,
        allowed_channel_keywords,
    )?;

    if !value.could_be_percentage() {
        return Err(location.new_custom_error(StyleParseErrorKind::UnspecifiedError));
    }

    Ok(value)
}

/// Parse a `<number>` value.
fn parse_number<'i, 't>(
    context: &ParserContext,
    input: &mut Parser<'i, 't>,
    allow_none: bool,
    allowed_channel_keywords: &[ChannelKeyword],
) -> Result<ColorComponent<NumberOrPercentage>, ParseError<'i>> {
    let location = input.current_source_location();

    let value = ColorComponent::<NumberOrPercentage>::parse(
        context,
        input,
        allow_none,
        allowed_channel_keywords,
    )?;

    if !value.could_be_number() {
        return Err(location.new_custom_error(StyleParseErrorKind::UnspecifiedError));
    }

    Ok(value)
}

/// Parse a `<number>` or `<percentage>` value.
fn parse_number_or_percentage<'i, 't>(
    context: &ParserContext,
    input: &mut Parser<'i, 't>,
    allow_none: bool,
    allowed_channel_keywords: &[ChannelKeyword],
) -> Result<ColorComponent<NumberOrPercentage>, ParseError<'i>> {
    ColorComponent::parse(context, input, allow_none, allowed_channel_keywords)
}

fn parse_legacy_alpha<'i, 't>(
    context: &ParserContext,
    arguments: &mut Parser<'i, 't>,
    allowed_channel_keywords: &[ChannelKeyword],
) -> Result<ColorComponent<NumberOrPercentage>, ParseError<'i>> {
    if !arguments.is_exhausted() {
        arguments.expect_comma()?;
        parse_number_or_percentage(context, arguments, false, allowed_channel_keywords)
    } else {
        Ok(ColorComponent::AlphaOmitted)
    }
}

fn parse_modern_alpha<'i, 't>(
    context: &ParserContext,
    arguments: &mut Parser<'i, 't>,
    allowed_channel_keywords: &[ChannelKeyword],
) -> Result<ColorComponent<NumberOrPercentage>, ParseError<'i>> {
    if !arguments.is_exhausted() {
        arguments.expect_delim('/')?;
        parse_number_or_percentage(context, arguments, true, allowed_channel_keywords)
    } else {
        Ok(ColorComponent::AlphaOmitted)
    }
}

impl ColorComponent<NumberOrPercentage> {
    /// Return true if the value contained inside is/can resolve to a percentage.
    /// Also returns false if the node is invalid somehow.
    fn could_be_number(&self) -> bool {
        match self {
            Self::None | Self::AlphaOmitted => true,
            Self::Value(value) => matches!(value, NumberOrPercentage::Number { .. }),
            Self::Calc(node) => {
                if let Ok(unit) = node.unit() {
                    unit.is_empty()
                } else {
                    false
                }
            },
        }
    }

    /// Return true if the value contained inside is/can resolve to a percentage.
    /// Also returns false if the node is invalid somehow.
    fn could_be_percentage(&self) -> bool {
        match self {
            Self::None | Self::AlphaOmitted => true,
            Self::Value(value) => matches!(value, NumberOrPercentage::Percentage { .. }),
            Self::Calc(node) => {
                if let Ok(unit) = node.unit() {
                    unit == CalcUnits::PERCENTAGE
                } else {
                    false
                }
            },
        }
    }
}
