use bitflags::bitflags;
use std::{collections::HashMap, str::FromStr};

/// For enabled_in, the setup is as follows:
///
/// It needs to be one of the four values: ["", "ua", "chrome", "content"]
///  * "chrome" implies "ua", and implies that they're explicitly enabled.
///  * "" implies the property will never be parsed.
///  * "content" implies the property is accessible unconditionally,
///    modulo a pref, set via servo_pref / gecko_pref.
#[derive(Copy, Clone, PartialEq)]
pub enum EnabledIn {
    /// Property is enabled in User Agent stylesheets
    UserAgent,
    /// Property is enabled in User Agent and Chrome (browser ui) stylesheets
    ChromeAndUserAgent,
    /// Property is enabled everywhere, including Content stylesheets (modulo pref settings)
    All,
    /// Property will never be parsed
    None,
}

// Bitfield values for all rule types which can have property declarations.
bitflags! {
  pub struct StyleRuleTypes : u8 {
    const STYLE_RULE = 1 << 0;
    const PAGE_RULE = 1 << 1;
    const KEYFRAME_RULE = 1 << 2;
    const POSITION_TRY_RULE = 1 << 3;

    // Composites
    const ALL_RULES = Self::STYLE_RULE.bits() | Self::PAGE_RULE.bits() | Self::KEYFRAME_RULE.bits();
    const DEFAULT_RULES = Self::STYLE_RULE.bits() | Self::KEYFRAME_RULE.bits();
    const DEFAULT_RULES_AND_PAGE = Self::DEFAULT_RULES.bits() | Self::PAGE_RULE.bits();
    const DEFAULT_RULES_EXCEPT_KEYFRAME = Self::STYLE_RULE.bits();
    const DEFAULT_RULES_AND_POSITION_TRY = Self::DEFAULT_RULES.bits() | Self::POSITION_TRY_RULE.bits();
  }
}
impl FromStr for StyleRuleTypes {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts = s.split(" ");
        let mut rule = Self::empty();

        for part in parts {
            match part {
                "Style" => rule |= Self::STYLE_RULE,
                "Page" => rule |= Self::PAGE_RULE,
                "Keyframe" => rule |= Self::KEYFRAME_RULE,
                "PositionTry" => rule |= Self::POSITION_TRY_RULE,
                _ => {},
            }
        }

        Ok(rule)
    }
}

/// A CSS property
pub struct Property {
    /// Name of the property (e.g. "background-color")
    name: String,
    /// Rust identifier version of name (snake_case + santized to remove keywords)
    ident: String, // to_rust_ident(name)
    /// (Upper? ) camel case version of name
    camel_case: String, // to_camel_case(name)
    /// Specification URL (e.g. "https://drafts.csswg.org/css-backgrounds/#background-color")
    spec: String,
    /// Servo preference which enables/disables this property
    servo_pref: Option<String>,
    /// Gecko preference which enables/disables this property
    gecko_pref: Option<String>,
    /// Which context(s) the property is enabled in
    enabled_in: EnabledIn,
    /// Bitfield values for all rule types which can have this property declaration.
    rule_types_allowed: StyleRuleTypes,
    /// Alternate names for this property
    aliases: Option<HashMap<String, String>>,
    /// Extra prefixes for this property (e.g. `-webkit`)
    extra_prefixes: Option<HashMap<String, String>>,
    /// Property Flags
    flags: Vec<String>, // TODO: use PropertyFlags type from properties/mod.rs
}
