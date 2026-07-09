use cssparser::{Parser as CssParser, ParserInput};
use euclid::{Scale, Size2D};
use style::context::QuirksMode;
use style::custom_properties::AttrTaint;
use style::device::servo::FontMetricsProvider;
use style::font_metrics::FontMetrics;
use style::media_queries::MediaType;
use style::parser::{Parse, ParserContext};
use style::properties::{style_structs, ComputedValues};
use style::queries::values::PrefersColorScheme;
use style::servo::media_features::PointerCapabilities;
use style::stylesheets::{Origin, UrlExtraData};
use style::values::computed::font::GenericFontFamily;
use style::values::computed::{CSSPixelLength, Context, ToComputedValue};
use style::values::specified::font::QueryFontMetricsFlags;
use style::values::specified::length::{Length, LengthUnit, NoCalcLength};
use style_traits::{CSSPixel, DevicePixel, ParsingMode, ToCss};

#[derive(Debug)]
struct TestFontMetricsProvider;

impl FontMetricsProvider for TestFontMetricsProvider {
    fn query_font_metrics(
        &self,
        _vertical: bool,
        _font: &style_structs::Font,
        _base_size: CSSPixelLength,
        _flags: QueryFontMetricsFlags,
    ) -> FontMetrics {
        FontMetrics::default()
    }

    fn base_size_for_generic(
        &self,
        _generic: GenericFontFamily,
    ) -> style::values::computed::Length {
        style::values::computed::Length::new(16.0)
    }
}

fn parse_length(css: &str) -> Result<Length, ()> {
    let url_data = UrlExtraData::from(::url::Url::parse("https://example.com/").unwrap());
    let context = ParserContext::new(
        Origin::Author,
        &url_data,
        None,
        ParsingMode::DEFAULT,
        QuirksMode::NoQuirks,
        Default::default(),
        None,
        None,
        AttrTaint::default(),
    );
    let mut input = ParserInput::new(css);
    let mut parser = CssParser::new(&mut input);
    parser
        .parse_entirely(|input| Length::parse(&context, input))
        .map_err(|_| ())
}

/// A device whose viewport is `width`×667 CSS px at DPR 2 — `rpx` resolves
/// against that viewport width, exactly like `vw`.
fn test_device(width: f32) -> style::device::Device {
    let default_values =
        ComputedValues::initial_values_with_font_override(style_structs::Font::initial_values());
    style::device::Device::new(
        MediaType::screen(),
        QuirksMode::NoQuirks,
        Size2D::<f32, CSSPixel>::new(width, 667.0),
        Size2D::<f32, DevicePixel>::new(width * 2.0, 1334.0),
        Scale::<f32, CSSPixel, DevicePixel>::new(2.0),
        Box::new(TestFontMetricsProvider),
        default_values,
        PrefersColorScheme::Light,
        PointerCapabilities::default(),
        PointerCapabilities::default(),
    )
}

#[test]
fn parses_and_serializes_rpx() {
    let no_calc =
        NoCalcLength::parse_dimension_with_flags(ParsingMode::DEFAULT, false, 100.0, "rpx")
            .unwrap();
    assert_eq!(no_calc.length_unit(), LengthUnit::Rpx);
    assert_eq!(no_calc.unitless_value(), 100.0);

    let parsed = parse_length("100rpx").unwrap();
    assert_eq!(parsed.to_css_string(), "100rpx");
}

#[test]
fn parses_rpx_inside_calc() {
    let parsed = parse_length("calc(2rpx + 10px)").unwrap();

    assert!(parsed.is_calc());
    assert_eq!(parsed.to_css_string(), "calc(10px + 2rpx)");
}

#[test]
fn rejects_invalid_or_removed_units() {
    assert!(parse_length("1rp").is_err());
    assert!(parse_length("1rpxx").is_err());
    assert!(parse_length("calc(1rpx + bad)").is_err());
    // ppx and sp are deliberately not supported.
    assert!(parse_length("2ppx").is_err());
    assert!(parse_length("3sp").is_err());
}

#[test]
fn rpx_resolves_against_the_viewport_width() {
    // viewport width 750 → 1rpx = 1px.
    let device = test_device(750.0);
    Context::for_media_query_evaluation(&device, QuirksMode::NoQuirks, |context| {
        let rpx = NoCalcLength::new(LengthUnit::Rpx, 100.0).to_computed_value(context);
        assert_eq!(rpx.px(), 100.0);
    });

    // viewport width 375 → 1rpx = 0.5px.
    let device = test_device(375.0);
    Context::for_media_query_evaluation(&device, QuirksMode::NoQuirks, |context| {
        let rpx = NoCalcLength::new(LengthUnit::Rpx, 100.0).to_computed_value(context);
        assert_eq!(rpx.px(), 50.0);
    });
}

#[test]
fn rpx_resolves_like_viewport_units() {
    // A non-integral base: viewport width 393 CSS px → 100rpx = 52.4px on the
    // Au grid (393 px = 23580 Au; 23580 * 100 / 750 = 3144 Au = 52.4 px), and
    // 1rpx truncates to whole Au exactly like vw: trunc(31.44) = 31 Au.
    let device = test_device(393.0);
    Context::for_media_query_evaluation(&device, QuirksMode::NoQuirks, |context| {
        let hundred_rpx = NoCalcLength::new(LengthUnit::Rpx, 100.0).to_computed_value(context);
        assert_eq!(hundred_rpx.px(), 52.4);

        let one_rpx = NoCalcLength::new(LengthUnit::Rpx, 1.0).to_computed_value(context);
        #[allow(clippy::cast_possible_truncation)]
        let expected = (31.0f64 / 60.0) as f32;
        assert_eq!(one_rpx.px(), expected);
    });
}
