/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! The [`@font-feature-values`][font-feature-values] at-rule.
//!
//! [font-feature-values]: https://drafts.csswg.org/css-fonts-3/#at-font-feature-values-rule

use crate::derives::*;
use crate::error_reporting::ContextualParseError;
#[cfg(feature = "gecko")]
use crate::gecko_bindings::bindings::Gecko_AppendFeatureValueHashEntry;
#[cfg(feature = "gecko")]
use crate::gecko_bindings::structs::{self, gfxFontFeatureValueSet};
use crate::parser::{Parse, ParserContext};
use crate::shared_lock::{SharedRwLockReadGuard, ToCssWithGuard};
use crate::stylesheets::CssRuleType;
use crate::values::computed::font::FamilyName;
use crate::values::serialize_atom_identifier;
use crate::Atom;
use cssparser::{
    match_ignore_ascii_case, AtRuleParser, BasicParseErrorKind, CowRcStr, DeclarationParser,
    Parser, ParserState, QualifiedRuleParser, RuleBodyItemParser, RuleBodyParser, SourceLocation,
    Token,
};
use std::fmt::{self, Write};
use style_traits::{CssStringWriter, CssWriter, ParseError, StyleParseErrorKind, ToCss};
#[cfg(feature = "gecko")]
use thin_vec::ThinVec;

/// A @font-feature-values block declaration.
/// It is `<ident>: <integer>+`.
/// This struct can take 3 value types.
/// - `SingleValue` is to keep just one unsigned integer value.
/// - `PairValues` is to keep one or two unsigned integer values.
/// - `VectorValues` is to keep a list of unsigned integer values.
#[derive(Clone, Debug, PartialEq, ToShmem)]
pub struct FFVDeclaration<T> {
    /// An `<ident>` for declaration name.
    pub name: Atom,
    /// An `<integer>+` for declaration value.
    pub value: T,
}

impl<T: ToCss> ToCss for FFVDeclaration<T> {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        serialize_atom_identifier(&self.name, dest)?;
        dest.write_str(": ")?;
        self.value.to_css(dest)?;
        dest.write_char(';')
    }
}

/// A trait for @font-feature-values rule to gecko values conversion.
#[cfg(feature = "gecko")]
pub trait ToGeckoFontFeatureValues {
    /// Sets the equivalent of declaration to gecko `ThinVec<u32>` array.
    fn to_gecko_font_feature_values(&self) -> ThinVec<u32>;
}

/// A @font-feature-values block declaration value that keeps one value.
#[derive(Clone, Debug, PartialEq, ToCss, ToShmem)]
pub struct SingleValue(pub u32);

impl Parse for SingleValue {
    fn parse(_context: &ParserContext, input: &mut Parser) -> Result<SingleValue, ParseError> {
        match *input.next()? {
            Token::Number {
                int_value: Some(v), ..
            } if v >= 0 => Ok(SingleValue(v as u32)),
            _ => Err(ParseError::unexpected_token()),
        }
    }
}

#[cfg(feature = "gecko")]
impl ToGeckoFontFeatureValues for SingleValue {
    fn to_gecko_font_feature_values(&self) -> ThinVec<u32> {
        thin_vec::thin_vec![self.0 as u32]
    }
}

/// A @font-feature-values block declaration value that keeps one or two values.
#[derive(Clone, Debug, PartialEq, ToCss, ToShmem)]
pub struct PairValues(pub u32, pub Option<u32>);

impl Parse for PairValues {
    fn parse(_context: &ParserContext, input: &mut Parser) -> Result<PairValues, ParseError> {
        let first = match *input.next()? {
            Token::Number {
                int_value: Some(a), ..
            } if a >= 0 => a as u32,
            _ => return Err(ParseError::unexpected_token()),
        };
        match input.next() {
            Ok(&Token::Number {
                int_value: Some(b), ..
            }) if b >= 0 => Ok(PairValues(first, Some(b as u32))),
            // It can't be anything other than number.
            Ok(_) => Err(ParseError::unexpected_token()),
            // It can be just one value.
            Err(_) => Ok(PairValues(first, None)),
        }
    }
}

#[cfg(feature = "gecko")]
impl ToGeckoFontFeatureValues for PairValues {
    fn to_gecko_font_feature_values(&self) -> ThinVec<u32> {
        let mut result = thin_vec::thin_vec![self.0 as u32];
        if let Some(second) = self.1 {
            result.push(second as u32);
        }
        result
    }
}

/// A @font-feature-values block declaration value that keeps a list of values.
#[derive(Clone, Debug, PartialEq, ToCss, ToShmem)]
pub struct VectorValues(#[css(iterable)] pub Vec<u32>);

impl Parse for VectorValues {
    fn parse(_context: &ParserContext, input: &mut Parser) -> Result<VectorValues, ParseError> {
        let mut vec = vec![];
        loop {
            match input.next() {
                Ok(&Token::Number {
                    int_value: Some(a), ..
                }) if a >= 0 => {
                    vec.push(a as u32);
                },
                // It can't be anything other than number.
                Ok(_) => return Err(ParseError::unexpected_token()),
                Err(_) => break,
            }
        }

        if vec.len() == 0 {
            return Err(ParseError::from_basic_kind(BasicParseErrorKind::EndOfInput));
        }

        Ok(VectorValues(vec))
    }
}

#[cfg(feature = "gecko")]
impl ToGeckoFontFeatureValues for VectorValues {
    fn to_gecko_font_feature_values(&self) -> ThinVec<u32> {
        self.0.iter().copied().collect()
    }
}

/// Parses a list of `FamilyName`s.
pub fn parse_family_name_list(
    context: &ParserContext,
    input: &mut Parser,
) -> Result<Vec<FamilyName>, ParseError> {
    input
        .parse_comma_separated(|i| FamilyName::parse(context, i))
        .map_err(|e| e.into())
}

/// @font-feature-values inside block parser. Parses a list of `FFVDeclaration`.
/// (`<ident>: <integer>+`)
struct FFVDeclarationsParser<'a, 'b: 'a, T: 'a> {
    context: &'a ParserContext<'b>,
    declarations: &'a mut Vec<FFVDeclaration<T>>,
}

/// Default methods reject all at rules.
impl<'a, 'b, 'i, T> AtRuleParser<'i> for FFVDeclarationsParser<'a, 'b, T> {
    type Prelude = ();
    type AtRule = ();
    type Error = StyleParseErrorKind;
}

impl<'a, 'b, 'i, T> QualifiedRuleParser<'i> for FFVDeclarationsParser<'a, 'b, T> {
    type Prelude = ();
    type QualifiedRule = ();
    type Error = StyleParseErrorKind;
}

impl<'a, 'b, 'i, T> DeclarationParser<'i> for FFVDeclarationsParser<'a, 'b, T>
where
    T: Parse,
{
    type Declaration = ();
    type Error = StyleParseErrorKind;

    fn parse_value(
        &mut self,
        name: CowRcStr<'i>,
        input: &mut Parser<'i, '_>,
        _declaration_start: &ParserState,
    ) -> Result<(), ParseError> {
        let value = input.parse_entirely(|i| T::parse(self.context, i))?;
        let new = FFVDeclaration {
            name: Atom::from(&*name),
            value,
        };
        update_or_push(&mut self.declarations, new);
        Ok(())
    }
}

impl<'a, 'b, 'i, T> RuleBodyItemParser<'i, (), StyleParseErrorKind>
    for FFVDeclarationsParser<'a, 'b, T>
where
    T: Parse,
{
    fn parse_declarations(&self) -> bool {
        true
    }
    fn parse_qualified(&self) -> bool {
        false
    }
}

macro_rules! font_feature_values_blocks {
    (
        blocks = [
            $( #[$doc: meta] $name: tt $ident: ident / $ident_camel: ident / $gecko_enum: ident: $ty: ty, )*
        ]
    ) => {
        /// The [`@font-feature-values`][font-feature-values] at-rule.
        ///
        /// [font-feature-values]: https://drafts.csswg.org/css-fonts-3/#at-font-feature-values-rule
        #[derive(Clone, Debug, PartialEq, ToShmem)]
        pub struct FontFeatureValuesRule {
            /// Font family list for @font-feature-values rule.
            /// Family names cannot contain generic families. FamilyName
            /// also accepts only non-generic names.
            pub family_names: Vec<FamilyName>,
            $(
                #[$doc]
                pub $ident: Vec<FFVDeclaration<$ty>>,
            )*
            /// The line and column of the rule's source code.
            pub source_location: SourceLocation,
        }

        impl FontFeatureValuesRule {
            /// Creates an empty FontFeatureValuesRule with given location and family name list.
            fn new(family_names: Vec<FamilyName>, location: SourceLocation) -> Self {
                FontFeatureValuesRule {
                    family_names: family_names,
                    $(
                        $ident: vec![],
                    )*
                    source_location: location,
                }
            }

            /// Parses a `FontFeatureValuesRule`.
            pub fn parse(
                context: &ParserContext,
                input: &mut Parser,
                family_names: Vec<FamilyName>,
                location: SourceLocation,
            ) -> Self {
                let mut rule = FontFeatureValuesRule::new(family_names, location);
                let mut parser = FontFeatureValuesRuleParser {
                    context,
                    rule: &mut rule,
                };
                let mut iter = RuleBodyParser::new(input, &mut parser);
                while let Some(result) = iter.next() {
                    if let Err((error, slice, location)) = result {
                        let error = ContextualParseError::UnsupportedRule(slice, error);
                        context.log_css_error(location, error);
                    }
                }
                rule
            }

            /// Prints inside of `@font-feature-values` block.
            pub fn value_to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
            where
                W: Write,
            {
                $(
                    if self.$ident.len() > 0 {
                        dest.write_str(concat!("@", $name, " {\n"))?;
                        let iter = self.$ident.iter();
                        for val in iter {
                            val.to_css(dest)?;
                            dest.write_str("\n")?
                        }
                        dest.write_str("}\n")?
                    }
                )*
                Ok(())
            }

            /// Returns length of all at-rules.
            pub fn len(&self) -> usize {
                let mut len = 0;
                $(
                    len += self.$ident.len();
                )*
                len
            }

            /// Convert to Gecko gfxFontFeatureValueSet.
            #[cfg(feature = "gecko")]
            pub fn set_at_rules(&self, dest: *mut gfxFontFeatureValueSet) {
                for ref family in self.family_names.iter() {
                    let family = family.name.to_ascii_lowercase();
                    $(
                        if self.$ident.len() > 0 {
                            for val in self.$ident.iter() {
                                let array = unsafe {
                                    Gecko_AppendFeatureValueHashEntry(
                                        dest,
                                        family.as_ptr(),
                                        structs::$gecko_enum,
                                        val.name.as_ptr()
                                    )
                                };
                                unsafe {
                                    *array = val.value.to_gecko_font_feature_values();
                                }
                            }
                        }
                    )*
                }
            }
        }

        impl ToCssWithGuard for FontFeatureValuesRule {
            fn to_css(&self, _guard: &SharedRwLockReadGuard, dest: &mut CssStringWriter) -> fmt::Result {
                dest.write_str("@font-feature-values ")?;
                self.family_names.to_css(&mut CssWriter::new(dest))?;
                dest.write_str(" {\n")?;
                self.value_to_css(&mut CssWriter::new(dest))?;
                dest.write_char('}')
            }
        }

        /// Updates with new value if same `ident` exists, otherwise pushes to the vector.
        fn update_or_push<T>(vec: &mut Vec<FFVDeclaration<T>>, element: FFVDeclaration<T>) {
            if let Some(item) = vec.iter_mut().find(|item| item.name == element.name) {
                item.value = element.value;
            } else {
                vec.push(element);
            }
        }

        /// Keeps the information about block type like @swash, @styleset etc.
        #[derive(Clone, Copy, Eq, PartialEq)]
        pub enum FontFeatureValuesBlockType {
            $(
                #[$doc]
                $ident_camel,
            )*
        }

        impl FontFeatureValuesBlockType {
            /// Matches the rule type for this name. This does not expect a
            /// leading '@'.
            pub fn from_name(name: &str) -> Option<Self> {
                Some(match_ignore_ascii_case! { name,
                    $( $name => Self::$ident_camel, )*
                    _ => return None,
                })
            }
        }

        /// Parser for `FontFeatureValuesRule`. Parses all blocks
        /// <feature-type> {
        ///   <feature-value-declaration-list>
        /// }
        /// <feature-type> = @stylistic | @historical-forms | @styleset |
        /// @character-variant | @swash | @ornaments | @annotation
        struct FontFeatureValuesRuleParser<'a> {
            context: &'a ParserContext<'a>,
            rule: &'a mut FontFeatureValuesRule,
        }

        /// Default methods reject all qualified rules.
        impl<'a, 'i> QualifiedRuleParser<'i> for FontFeatureValuesRuleParser<'a> {
            type Prelude = ();
            type QualifiedRule = ();
            type Error = StyleParseErrorKind;
        }

        impl<'a, 'i> AtRuleParser<'i> for FontFeatureValuesRuleParser<'a> {
            type Prelude = FontFeatureValuesBlockType;
            type AtRule = ();
            type Error = StyleParseErrorKind;

            fn parse_prelude(
                &mut self,
                name: CowRcStr<'i>,
                _input: &mut Parser<'i, '_>,
            ) -> Result<FontFeatureValuesBlockType, ParseError> {
                FontFeatureValuesBlockType::from_name(&name)
                    .ok_or_else(|| ParseError::from_basic_kind(BasicParseErrorKind::AtRuleBodyInvalid))
            }

            fn parse_block(
                &mut self,
                prelude: FontFeatureValuesBlockType,
                _: &ParserState,
                input: &mut Parser<'i, '_>
            ) -> Result<Self::AtRule, ParseError> {
                debug_assert!(self.context.rule_types().contains(CssRuleType::FontFeatureValues));
                match prelude {
                    $(
                        FontFeatureValuesBlockType::$ident_camel => {
                            let mut parser = FFVDeclarationsParser {
                                context: &self.context,
                                declarations: &mut self.rule.$ident,
                            };

                            let mut iter = RuleBodyParser::new(input, &mut parser);
                            while let Some(declaration) = iter.next() {
                                if let Err((error, slice, location)) = declaration {
                                    // TODO(emilio): Maybe add a more specific error kind for
                                    // font-feature-values descriptors.
                                    let error = ContextualParseError::UnsupportedPropertyDeclaration(slice, error, &[]);
                                    self.context.log_css_error(location, error);
                                }
                            }
                        },
                    )*
                }

                Ok(())
            }
        }

        impl<'a, 'i> DeclarationParser<'i> for FontFeatureValuesRuleParser<'a> {
            type Declaration = ();
            type Error = StyleParseErrorKind;
        }

        impl<'a, 'i> RuleBodyItemParser<'i, (), StyleParseErrorKind> for FontFeatureValuesRuleParser<'a> {
            fn parse_declarations(&self) -> bool { false }
            fn parse_qualified(&self) -> bool { true }
        }
    }
}

font_feature_values_blocks! {
    blocks = [
        #[doc = "A @swash blocksck. \
                 Specifies a feature name that will work with the swash() \
                 functional notation of font-variant-alternates."]
        "swash" swash / Swash / NS_FONT_VARIANT_ALTERNATES_SWASH: SingleValue,

        #[doc = "A @stylistic block. \
                 Specifies a feature name that will work with the annotation() \
                 functional notation of font-variant-alternates."]
        "stylistic" stylistic / Stylistic / NS_FONT_VARIANT_ALTERNATES_STYLISTIC: SingleValue,

        #[doc = "A @ornaments block. \
                 Specifies a feature name that will work with the ornaments() ] \
                 functional notation of font-variant-alternates."]
        "ornaments" ornaments / Ornaments / NS_FONT_VARIANT_ALTERNATES_ORNAMENTS: SingleValue,

        #[doc = "A @annotation block. \
                 Specifies a feature name that will work with the stylistic() \
                 functional notation of font-variant-alternates."]
        "annotation" annotation / Annotation / NS_FONT_VARIANT_ALTERNATES_ANNOTATION: SingleValue,

        #[doc = "A @character-variant block. \
                 Specifies a feature name that will work with the styleset() \
                 functional notation of font-variant-alternates. The value can be a pair."]
        "character-variant" character_variant / CharacterVariant / NS_FONT_VARIANT_ALTERNATES_CHARACTER_VARIANT:
            PairValues,

        #[doc = "A @styleset block. \
                 Specifies a feature name that will work with the character-variant() \
                 functional notation of font-variant-alternates. The value can be a list."]
        "styleset" styleset / Styleset / NS_FONT_VARIANT_ALTERNATES_STYLESET: VectorValues,
    ]
}
