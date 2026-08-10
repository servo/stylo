//! The CSS property database, as types.
//!
//! `style/properties/data.py` builds this model from the TOML files next to it
//! and hands it to Mako. This crate is that model in Rust, so the generator can
//! be an ordinary build script rather than a vendored Python interpreter.
//!
//! # Why `deny_unknown_fields` is the point
//!
//! `data.py` reads these files with attribute lookups and defaults, so a
//! misspelled key is silently ignored: `servo_pre` instead of `servo_pref`
//! yields a property that quietly ships with no preference gate and no error
//! anywhere. Every struct here rejects unknown fields, which turns that class
//! of typo into a parse failure naming the file, the property and the key.
//!
//! The schema was documented in a 52-line comment at the top of
//! `longhands.toml`, which had already drifted from the data it described: it
//! still called `keyword.values` a space separated string where the file uses
//! an array. The doc comments below are that schema, kept where it cannot
//! drift from the parser.

use std::collections::BTreeMap;
use std::path::Path;

use serde::Deserialize;

/// Everything declared across the property TOMLs.
#[derive(Debug, Clone)]
pub struct PropertyDatabase {
    pub longhands: Vec<Longhand>,
    pub shorthands: Vec<Shorthand>,
    pub counter_style_descriptors: Vec<Descriptor>,
    pub font_face_descriptors: Vec<Descriptor>,
    pub view_transition_descriptors: Vec<Descriptor>,
}

/// One CSS longhand property.
///
/// Field order follows the TOML's own documentation rather than alphabetical,
/// so a reader comparing the two can go down both at once.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Longhand {
    /// The property name, from the TOML section header rather than a field.
    #[serde(skip)]
    pub name: String,

    /// The Rust type name, for example `Display` or `Color`.
    pub r#type: Option<String>,
    /// Which style struct this belongs to: `box`, `font`, `position`, and so on.
    #[serde(rename = "struct")]
    pub style_struct: String,
    /// The engine this property is specific to, if it is specific to one.
    pub engine: Option<Engine>,
    /// URL of the specification.
    pub spec: String,
    /// What the property affects.
    pub affects: Affects,

    /// The computed initial value, as a Rust expression.
    ///
    /// A string because it is code, not data. No serialisation format makes an
    /// embedded expression type-safe, and pretending otherwise would only move
    /// the failure from the generator to the parser.
    pub initial: Option<String>,
    /// Initial specified value, when it differs from `initial`.
    pub initial_specified_value: Option<String>,
    pub keyword: Option<Keyword>,
    pub animation_type: Option<AnimationType>,
    /// Parse method name; `parse` when absent.
    pub parse_method: Option<String>,
    #[serde(default)]
    pub allow_quirks: Option<AllowQuirks>,
    #[serde(default)]
    pub boxed: bool,
    pub vector: Option<Vector>,

    pub gecko_pref: Option<String>,
    pub servo_pref: Option<String>,
    /// Gecko FFI name; defaults to `m` plus the CamelCase name.
    pub gecko_ffi_name: Option<String>,
    pub enabled_in: Option<EnabledIn>,
    #[serde(default)]
    pub logical: bool,
    pub logical_group: Option<String>,
    /// Space-separated flags, for example `CAN_ANIMATE_ON_COMPOSITOR`.
    ///
    /// The last list still encoded inside a string. Parsed into a real list by
    /// [`Longhand::flags`] so the rest of the generator never sees the string.
    #[serde(default)]
    flags: Option<String>,
    #[serde(default)]
    pub aliases: Vec<String>,
    /// Additional aliases for Gecko only, not for Servo.
    #[serde(default)]
    pub extra_gecko_aliases: Vec<String>,
    #[serde(default)]
    pub extra_prefixes: Vec<String>,
    #[serde(default)]
    pub ignored_when_colors_disabled: bool,
    #[serde(default)]
    pub has_effect_on_gecko_scrollbars: Option<bool>,
    #[serde(default)]
    pub rule_types_allowed: Option<Vec<RuleType>>,
    pub servo_restyle_damage: Option<RestyleDamage>,
}

impl Longhand {
    /// The flags as a list, splitting the space-separated string the TOML still
    /// carries. Empty when absent.
    pub fn flags(&self) -> Vec<&str> {
        self.flags
            .as_deref()
            .map(|flags| flags.split_whitespace().collect())
            .unwrap_or_default()
    }
}

/// One CSS shorthand property.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Shorthand {
    #[serde(skip)]
    pub name: String,

    pub sub_properties: Vec<String>,
    /// Absent on exactly one shorthand upstream, so it cannot be required.
    pub spec: Option<String>,
    /// The shorthand's parsing shape, for example `four_sides`.
    pub kind: Option<String>,
    pub engine: Option<Engine>,
    pub gecko_pref: Option<String>,
    pub servo_pref: Option<String>,
    #[serde(default)]
    pub allow_quirks: Option<AllowQuirks>,
    #[serde(default)]
    pub derive_serialize: bool,
    #[serde(default)]
    pub derive_value_info: Option<bool>,
    #[serde(default)]
    pub extra_gecko_sub_properties: Vec<String>,
    #[serde(default)]
    pub aliases: Vec<String>,
    #[serde(default)]
    pub extra_gecko_aliases: Vec<String>,
    #[serde(default)]
    pub extra_prefixes: Vec<String>,
    #[serde(default)]
    pub rule_types_allowed: Option<Vec<RuleType>>,
    #[serde(default)]
    flags: Option<String>,
}

impl Shorthand {
    pub fn flags(&self) -> Vec<&str> {
        self.flags
            .as_deref()
            .map(|flags| flags.split_whitespace().collect())
            .unwrap_or_default()
    }
}

/// An at-rule descriptor: `@counter-style`, `@font-face`, `@view-transition`.
///
/// A much smaller schema than a property, and it spells the parse override
/// `parser` where longhands spell it `parse_method`. Kept as upstream has it
/// rather than unified, because renaming a key here would be a change to the
/// data files rather than to this model.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Descriptor {
    #[serde(skip)]
    pub name: String,

    pub r#type: String,
    pub parser: Option<String>,
    pub gecko_pref: Option<String>,
}

/// A keyword-valued property's accepted values.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Keyword {
    pub values: Vec<String>,
    #[serde(default)]
    pub extra_gecko_values: Vec<String>,
    pub gecko_enum_prefix: Option<String>,
    pub gecko_constant_prefix: Option<String>,
    /// Whether the Gecko enum does not cover all keywords.
    #[serde(default)]
    pub gecko_inexhaustive: bool,
    /// Keyword to enum variant overrides.
    #[serde(default)]
    pub custom_consts: BTreeMap<String, String>,
    /// Keyword-level aliases, Gecko only, written as `"alias=target"`.
    ///
    /// The same wart as `flags`: structure encoded inside a string. Split by
    /// [`Keyword::gecko_aliases`] so the generator never parses it again.
    #[serde(default)]
    gecko_aliases: Vec<String>,
}

impl Keyword {
    /// Gecko keyword aliases as `(alias, target)` pairs.
    pub fn gecko_aliases(&self) -> Vec<(&str, &str)> {
        self.gecko_aliases
            .iter()
            .filter_map(|entry| entry.split_once('='))
            .collect()
    }
}

/// A property whose value is a list.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Vector {
    pub animation_type: Option<AnimationType>,
    #[serde(default)]
    pub simple_bindings: Option<bool>,
    #[serde(default)]
    pub need_index: bool,
    /// `Comma` when absent.
    pub separator: Option<String>,
    /// The value a `none` keyword produces, as a Rust expression.
    pub none_value: Option<String>,
}

/// `allow_quirks` is a tri-state upstream, not a boolean: some properties allow
/// quirks only in the shorthand that contains them.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(untagged)]
pub enum AllowQuirks {
    Always(bool),
    Named(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Engine {
    Gecko,
    Servo,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Affects {
    /// Declared as the empty string upstream, meaning nothing observable.
    #[serde(rename = "")]
    Nothing,
    Layout,
    Overflow,
    Paint,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AnimationType {
    None,
    Normal,
    Discrete,
    /// Vector-only variants.
    RepeatableList,
    WithZero,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum EnabledIn {
    #[serde(rename = "")]
    Nothing,
    Content,
    Chrome,
    Ua,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum RuleType {
    Style,
    Page,
    Keyframe,
    PositionTry,
    Scope,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RestyleDamage {
    Repaint,
    RebuildStackingContext,
    RecalculateOverflow,
    RebuildBox,
}

/// A parse failure, carrying enough to find the offending line by hand.
#[derive(Debug)]
pub struct LoadError {
    pub file: String,
    pub source: String,
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}: {}", self.file, self.source)
    }
}

impl std::error::Error for LoadError {}

/// Parse one TOML file of `[name]` sections into a named list.
///
/// The section header carries the property name, which serde cannot see from
/// inside the value, so it is written back onto each record afterwards.
fn load_named<T: for<'de> Deserialize<'de>>(
    path: &Path,
    set_name: fn(&mut T, String),
) -> Result<Vec<T>, LoadError> {
    let fail = |source: String| LoadError {
        file: path.display().to_string(),
        source,
    };
    let text = std::fs::read_to_string(path).map_err(|error| fail(error.to_string()))?;
    let table: BTreeMap<String, T> =
        toml::from_str(&text).map_err(|error| fail(error.to_string()))?;
    Ok(table
        .into_iter()
        .map(|(name, mut value)| {
            set_name(&mut value, name);
            value
        })
        .collect())
}

impl PropertyDatabase {
    /// Load every property TOML from `style/properties`.
    pub fn load(properties_dir: &Path) -> Result<Self, LoadError> {
        Ok(Self {
            longhands: load_named(
                &properties_dir.join("longhands.toml"),
                |value: &mut Longhand, name| value.name = name,
            )?,
            shorthands: load_named(
                &properties_dir.join("shorthands.toml"),
                |value: &mut Shorthand, name| value.name = name,
            )?,
            counter_style_descriptors: load_named(
                &properties_dir.join("counter_style_descriptors.toml"),
                |value: &mut Descriptor, name| value.name = name,
            )?,
            font_face_descriptors: load_named(
                &properties_dir.join("font_face_descriptors.toml"),
                |value: &mut Descriptor, name| value.name = name,
            )?,
            view_transition_descriptors: load_named(
                &properties_dir.join("view_transition_descriptors.toml"),
                |value: &mut Descriptor, name| value.name = name,
            )?,
        })
    }

    /// The longhands an engine actually compiles, which is what every consumer
    /// wants: a property marked for the other engine is not in its build.
    pub fn longhands_for(&self, engine: Engine) -> impl Iterator<Item = &Longhand> {
        self.longhands
            .iter()
            .filter(move |property| property.engine.is_none_or(|only| only == engine))
    }

    pub fn shorthands_for(&self, engine: Engine) -> impl Iterator<Item = &Shorthand> {
        self.shorthands
            .iter()
            .filter(move |property| property.engine.is_none_or(|only| only == engine))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn database() -> PropertyDatabase {
        let dir = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join("style/properties");
        match PropertyDatabase::load(&dir) {
            Ok(database) => database,
            // The whole value of `deny_unknown_fields` is in this message, so
            // do not let a test harness swallow it behind "assertion failed".
            Err(error) => panic!("{error}"),
        }
    }

    /// Every property parses. With `deny_unknown_fields` this also asserts that
    /// no key in any of the five files is one this model does not model.
    #[test]
    fn the_whole_database_parses() {
        let database = database();
        assert_eq!(database.longhands.len(), 429);
        assert_eq!(database.shorthands.len(), 92);
        assert!(!database.counter_style_descriptors.is_empty());
        assert!(!database.font_face_descriptors.is_empty());
        assert!(!database.view_transition_descriptors.is_empty());
    }

    /// Names come from the section headers, not from a field.
    #[test]
    fn properties_keep_their_names() {
        let database = database();
        let transform = database
            .longhands
            .iter()
            .find(|property| property.name == "transform")
            .expect("transform is a longhand");
        assert_eq!(transform.style_struct, "box");
        assert_eq!(transform.affects, Affects::Overflow);
        assert_eq!(transform.flags(), ["CAN_ANIMATE_ON_COMPOSITOR"]);
        assert_eq!(
            transform.servo_restyle_damage,
            Some(RestyleDamage::RecalculateOverflow)
        );
    }

    /// `flags` is the last list still encoded inside a string, so the accessor
    /// splits it. Every value in the database happens to be single today, which
    /// is exactly why the vocabulary is pinned here: a new flag arriving from
    /// upstream should be a failing test rather than a string nothing reads.
    #[test]
    fn flags_parse_and_the_vocabulary_is_known() {
        let database = database();
        let flags: std::collections::BTreeSet<&str> = database
            .longhands
            .iter()
            .flat_map(Longhand::flags)
            .chain(database.shorthands.iter().flat_map(Shorthand::flags))
            .collect();
        assert_eq!(
            flags,
            ["CAN_ANIMATE_ON_COMPOSITOR", "IS_LEGACY_SHORTHAND"]
                .into_iter()
                .collect()
        );
        assert_eq!(
            database
                .longhands
                .iter()
                .filter(|property| property.flags().contains(&"CAN_ANIMATE_ON_COMPOSITOR"))
                .count(),
            11
        );
    }

    /// Gecko keyword aliases are `"alias=target"` pairs inside strings, and the
    /// accessor is the only thing that should ever know that.
    #[test]
    fn gecko_keyword_aliases_split_into_pairs() {
        let database = database();
        let smoothing = database
            .longhands
            .iter()
            .find(|property| property.name == "-moz-osx-font-smoothing")
            .expect("-moz-osx-font-smoothing is a longhand");
        let keyword = smoothing.keyword.as_ref().expect("it is keyword valued");
        assert_eq!(keyword.gecko_aliases(), [("antialiased", "grayscale")]);
    }

    /// A shorthand names longhands that exist. `data.py` resolves these by
    /// lookup and would raise deep inside a template; catching it here names
    /// the shorthand instead.
    #[test]
    fn every_sub_property_resolves_to_a_longhand() {
        let database = database();
        let longhands: std::collections::BTreeSet<&str> = database
            .longhands
            .iter()
            .map(|property| property.name.as_str())
            .collect();
        for shorthand in &database.shorthands {
            for sub_property in &shorthand.sub_properties {
                assert!(
                    longhands.contains(sub_property.as_str()),
                    "shorthand {} names {}, which is not a longhand",
                    shorthand.name,
                    sub_property
                );
            }
        }
    }

    /// An engine's build sees only its own properties plus the shared ones.
    #[test]
    fn engine_filtering_excludes_the_other_engine() {
        let database = database();
        let servo: Vec<&str> = database
            .longhands_for(Engine::Servo)
            .map(|property| property.name.as_str())
            .collect();
        assert!(servo.len() < database.longhands.len());
        assert!(!servo.contains(&"-moz-box-flex"));
    }
}
