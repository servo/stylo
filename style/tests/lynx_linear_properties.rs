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
fn parses_linear_direction_values() {
    for value in ["column", "row", "column-reverse", "row-reverse"] {
        assert_parses("linear-direction", value, LonghandId::LinearDirection);
    }

    assert_rejects("linear-direction", "vertical");
    assert_rejects("linear-direction", "horizontal");
    assert_rejects("linear-direction", "vertical-reverse");
    assert_rejects("linear-direction", "horizontal-reverse");
    assert_rejects("linear-direction", "diagonal");
}

#[test]
fn parses_non_negative_linear_weight_values() {
    assert_parses("linear-weight", "1.5", LonghandId::LinearWeight);
    assert_parses("linear-weight-sum", "3", LonghandId::LinearWeightSum);

    assert_rejects("linear-weight", "-1");
    assert_rejects("linear-weight-sum", "-0.5");
}

#[test]
fn generated_initial_values_match_lynx_defaults() {
    let values = style::properties::ComputedValues::initial_values_with_font_override(
        style::properties::style_structs::Font::initial_values(),
    );

    assert_eq!(
        values.clone_linear_direction(),
        longhands::linear_direction::computed_value::T::Column
    );
    assert_eq!(values.clone_linear_weight().0, 0.0);
    assert_eq!(values.clone_linear_weight_sum().0, 0.0);
}

#[test]
fn parses_linear_properties_in_declaration_block() {
    let url_data = url_data();
    let block = parse_style_attribute(
        "linear-direction: row; \
         linear-weight: 2; \
         linear-weight-sum: 4;",
        &url_data,
        None,
        QuirksMode::NoQuirks,
        CssRuleType::Style,
    );

    assert_eq!(block.len(), 3);
    assert!(block
        .declarations()
        .iter()
        .any(|declaration| declaration.id()
            == PropertyDeclarationId::Longhand(LonghandId::LinearDirection)));
}
