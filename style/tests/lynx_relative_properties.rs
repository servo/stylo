use style::context::QuirksMode;
use style::properties::declaration_block::{parse_one_declaration_into, parse_style_attribute};
use style::properties::{
    longhands, LonghandId, PropertyDeclarationId, PropertyId, SourcePropertyDeclaration,
};
use style::stylesheets::{CssRuleType, Origin, UrlExtraData};
use style_traits::ParsingMode;

fn url_data() -> UrlExtraData {
    UrlExtraData::from(::url::Url::parse("https://example.com/").unwrap())
}

fn parse_declaration(name: &str, value: &str) -> Result<SourcePropertyDeclaration, ()> {
    let url_data = url_data();
    let id = PropertyId::parse_unchecked_for_testing(name).unwrap();
    let mut declarations = SourcePropertyDeclaration::default();
    parse_one_declaration_into(
        &mut declarations,
        id,
        value,
        Origin::Author,
        &url_data,
        None,
        ParsingMode::DEFAULT,
        QuirksMode::NoQuirks,
        CssRuleType::Style,
    )?;
    Ok(declarations)
}

fn assert_parses(name: &str, value: &str, id: LonghandId) {
    let declarations = parse_declaration(name, value).unwrap();

    assert_eq!(declarations.declarations.len(), 1);
    assert_eq!(
        declarations.declarations[0].id(),
        PropertyDeclarationId::Longhand(id)
    );
}

fn assert_rejects(name: &str, value: &str) {
    assert!(parse_declaration(name, value).is_err());
}

#[test]
fn parses_relative_integer_references() {
    for (name, id) in [
        ("relative-id", LonghandId::RelativeId),
        ("relative-top-of", LonghandId::RelativeTopOf),
        ("relative-right-of", LonghandId::RelativeRightOf),
        ("relative-bottom-of", LonghandId::RelativeBottomOf),
        ("relative-left-of", LonghandId::RelativeLeftOf),
        (
            "relative-inline-start-of",
            LonghandId::RelativeInlineStartOf,
        ),
        ("relative-inline-end-of", LonghandId::RelativeInlineEndOf),
    ] {
        assert_parses(name, "7", id);
        assert_parses(name, "-1", id);
        assert_rejects(name, "parent");
        assert_rejects(name, "1.5");
        assert_rejects(name, "none");
    }
}

#[test]
fn parses_relative_align_references() {
    for (name, id) in [
        ("relative-align-top", LonghandId::RelativeAlignTop),
        ("relative-align-right", LonghandId::RelativeAlignRight),
        ("relative-align-bottom", LonghandId::RelativeAlignBottom),
        ("relative-align-left", LonghandId::RelativeAlignLeft),
        (
            "relative-align-inline-start",
            LonghandId::RelativeAlignInlineStart,
        ),
        (
            "relative-align-inline-end",
            LonghandId::RelativeAlignInlineEnd,
        ),
    ] {
        assert_parses(name, "parent", id);
        assert_parses(name, "7", id);
        assert_rejects(name, "0");
        assert_rejects(name, "-1");
        assert_rejects(name, "none");
        assert_rejects(name, "1.5");
    }
}

#[test]
fn parses_relative_center_and_layout_once() {
    for value in ["none", "vertical", "horizontal", "both"] {
        assert_parses("relative-center", value, LonghandId::RelativeCenter);
    }

    assert_rejects("relative-center", "center");
    assert_parses(
        "relative-layout-once",
        "true",
        LonghandId::RelativeLayoutOnce,
    );
    assert_parses(
        "relative-layout-once",
        "false",
        LonghandId::RelativeLayoutOnce,
    );
    assert_rejects("relative-layout-once", "yes");
}

#[test]
fn generated_initial_values_match_lynx_defaults() {
    let values = style::properties::ComputedValues::initial_values_with_font_override(
        style::properties::style_structs::Font::initial_values(),
    );

    assert_eq!(values.clone_relative_id(), -1);
    assert_eq!(values.clone_relative_align_top(), -1);
    assert_eq!(values.clone_relative_align_right(), -1);
    assert_eq!(values.clone_relative_align_bottom(), -1);
    assert_eq!(values.clone_relative_align_left(), -1);
    assert_eq!(values.clone_relative_top_of(), -1);
    assert_eq!(values.clone_relative_right_of(), -1);
    assert_eq!(values.clone_relative_bottom_of(), -1);
    assert_eq!(values.clone_relative_left_of(), -1);
    assert_eq!(values.clone_relative_align_inline_start(), -1);
    assert_eq!(values.clone_relative_align_inline_end(), -1);
    assert_eq!(values.clone_relative_inline_start_of(), -1);
    assert_eq!(values.clone_relative_inline_end_of(), -1);
    assert_eq!(
        values.clone_relative_center(),
        longhands::relative_center::computed_value::T::None
    );
    assert_eq!(
        values.clone_relative_layout_once(),
        longhands::relative_layout_once::computed_value::T::True
    );
}

#[test]
fn parses_relative_properties_in_declaration_block() {
    let url_data = url_data();
    let block = parse_style_attribute(
        "relative-id: 1; \
         relative-align-top: parent; \
         relative-align-right: 2; \
         relative-align-bottom: parent; \
         relative-align-left: 3; \
         relative-top-of: 4; \
         relative-right-of: 5; \
         relative-bottom-of: 6; \
         relative-left-of: 7; \
         relative-align-inline-start: parent; \
         relative-align-inline-end: 8; \
         relative-inline-start-of: 9; \
         relative-inline-end-of: 10; \
         relative-center: both; \
         relative-layout-once: false;",
        &url_data,
        None,
        QuirksMode::NoQuirks,
        CssRuleType::Style,
    );

    assert_eq!(block.len(), 15);
    for id in [
        LonghandId::RelativeId,
        LonghandId::RelativeAlignTop,
        LonghandId::RelativeAlignRight,
        LonghandId::RelativeAlignBottom,
        LonghandId::RelativeAlignLeft,
        LonghandId::RelativeTopOf,
        LonghandId::RelativeRightOf,
        LonghandId::RelativeBottomOf,
        LonghandId::RelativeLeftOf,
        LonghandId::RelativeAlignInlineStart,
        LonghandId::RelativeAlignInlineEnd,
        LonghandId::RelativeInlineStartOf,
        LonghandId::RelativeInlineEndOf,
        LonghandId::RelativeCenter,
        LonghandId::RelativeLayoutOnce,
    ] {
        assert!(block
            .declarations()
            .iter()
            .any(|declaration| declaration.id() == PropertyDeclarationId::Longhand(id)));
    }
}

#[test]
fn relative_align_sentinel_round_trips_without_fabricating_ids() {
    use style::values::computed::ToComputedValue;
    use style::values::specified::lynx_layout::RelativeAlign;
    use style_traits::ToCss;

    // Computed `-1` (the unset sentinel / initial) maps to `None`, never to an
    // out-of-grammar negative `Id`.
    assert_eq!(RelativeAlign::from_computed_value(&-1), RelativeAlign::None);
    assert_eq!(
        RelativeAlign::from_computed_value(&0),
        RelativeAlign::Parent
    );
    // The sentinel serializes the way Lynx reports it.
    assert_eq!(RelativeAlign::None.to_css_string(), "-1");
    assert_eq!(RelativeAlign::Parent.to_css_string(), "parent");
}
