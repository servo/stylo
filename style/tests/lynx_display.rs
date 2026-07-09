use cssparser::{Parser as CssParser, ParserInput};
use style::context::QuirksMode;
use style::custom_properties::AttrTaint;
use style::parser::{Parse, ParserContext};
use style::stylesheets::{Origin, UrlExtraData};
use style::values::specified::box_::{Display, DisplayInside, DisplayOutside};
use style_traits::{ParsingMode, ToCss};

fn parse_display(css: &str) -> Result<Display, ()> {
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
        .parse_entirely(|input| Display::parse(&context, input))
        .map_err(|_| ())
}

#[test]
fn parses_and_serializes_lynx_display_keywords() {
    let linear = parse_display("linear").unwrap();
    assert_eq!(linear, Display::Linear);
    assert_eq!(linear.to_css_string(), "linear");

    let relative = parse_display("relative").unwrap();
    assert_eq!(relative, Display::LynxRelative);
    assert_eq!(relative.to_css_string(), "relative");
}

#[test]
fn display_inside_serializes_lynx_keywords() {
    // `Display::to_css` has explicit arms for the full keywords, but the
    // derived `ToCss` on `DisplayInside` (reachable via the multi-keyword
    // fallback) must agree — it kebab-cases the variant NAME unless
    // overridden with `#[css(keyword = ...)]`, which would leak
    // "lynx-linear"/"lynx-relative".
    assert_eq!(DisplayInside::LynxLinear.to_css_string(), "linear");
    assert_eq!(DisplayInside::LynxRelative.to_css_string(), "relative");
}

#[test]
fn linear_behaves_like_block_level_flex_container() {
    let display = parse_display("linear").unwrap();

    assert_eq!(display.outside(), DisplayOutside::Block);
    assert_eq!(display.inside(), DisplayInside::LynxLinear);
    assert!(display.is_item_container());
    assert!(!display.is_inline_flow());
    assert_eq!(display.equivalent_block_display(false), Display::Linear);
}

#[test]
fn relative_behaves_like_block_without_becoming_css_block() {
    let display = parse_display("relative").unwrap();

    assert_eq!(display.outside(), DisplayOutside::Block);
    assert_eq!(display.inside(), DisplayInside::LynxRelative);
    assert!(!display.is_item_container());
    assert!(!display.is_inline_flow());
    assert_eq!(
        display.equivalent_block_display(false),
        Display::LynxRelative
    );
}

#[test]
fn lynx_display_keywords_are_single_keyword_values() {
    assert!(parse_display("inline linear").is_err());
    assert!(parse_display("relative list-item").is_err());
}
