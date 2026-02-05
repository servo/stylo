/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! `list` specified values.

#[cfg(feature = "gecko")]
use crate::counter_style::{CounterStyle, CounterStyleParsingFlags};
use crate::derives::*;
use crate::parser::{Parse, ParserContext};
use cssparser::{Parser, Token};
use style_traits::{ParseError, StyleParseErrorKind};

/// Specified and computed `list-style-type` property.
#[cfg(feature = "gecko")]
#[derive(
    Clone,
    Debug,
    Eq,
    MallocSizeOf,
    PartialEq,
    SpecifiedValueInfo,
    ToComputedValue,
    ToCss,
    ToResolvedValue,
    ToShmem,
    ToTyped,
)]
#[repr(transparent)]
pub struct ListStyleType(pub CounterStyle);

#[cfg(feature = "gecko")]
impl ListStyleType {
    /// Initial specified value for `list-style-type`.
    #[inline]
    pub fn disc() -> Self {
        Self(CounterStyle::disc())
    }

    /// none value.
    #[inline]
    pub fn none() -> Self {
        Self(CounterStyle::None)
    }

    /// Convert from gecko keyword to list-style-type.
    ///
    /// This should only be used for mapping type attribute to list-style-type, and thus only
    /// values possible in that attribute is considered here.
    pub fn from_gecko_keyword(value: u32) -> Self {
        use crate::gecko_bindings::structs;
        use crate::values::CustomIdent;
        let v8 = value as u8;
        if v8 == structs::ListStyle_None {
            return Self::none();
        }

        Self(CounterStyle::Name(CustomIdent(match v8 {
            structs::ListStyle_Disc => atom!("disc"),
            structs::ListStyle_Circle => atom!("circle"),
            structs::ListStyle_Square => atom!("square"),
            structs::ListStyle_Decimal => atom!("decimal"),
            structs::ListStyle_LowerRoman => atom!("lower-roman"),
            structs::ListStyle_UpperRoman => atom!("upper-roman"),
            structs::ListStyle_LowerAlpha => atom!("lower-alpha"),
            structs::ListStyle_UpperAlpha => atom!("upper-alpha"),
            _ => unreachable!("Unknown counter style keyword value"),
        })))
    }

    /// Is this a bullet? (i.e. `list-style-type: disc|circle|square|disclosure-closed|disclosure-open`)
    #[inline]
    pub fn is_bullet(&self) -> bool {
        self.0.is_bullet()
    }
}

#[cfg(feature = "gecko")]
impl Parse for ListStyleType {
    fn parse<'i, 't>(
        context: &ParserContext,
        input: &mut Parser<'i, 't>,
    ) -> Result<Self, ParseError<'i>> {
        let flags = CounterStyleParsingFlags::ALLOW_NONE | CounterStyleParsingFlags::ALLOW_STRING;
        Ok(Self(CounterStyle::parse(context, input, flags)?))
    }
}

/// Specified and computed `list-style-type` property.
#[cfg(feature = "servo")]
#[derive(
    Clone,
    Debug,
    Eq,
    MallocSizeOf,
    Parse,
    PartialEq,
    SpecifiedValueInfo,
    ToCss,
    ToComputedValue,
    ToResolvedValue,
    ToShmem,
    ToTyped,
)]
pub enum ListStyleType {
    /// A filled circle, similar to • U+2022 BULLET.
    /// <https://www.w3.org/TR/css-counter-styles-3/#disc>
    Disc,
    /// The element has no marker string.
    /// <https://www.w3.org/TR/css-lists-3/#valdef-list-style-type-none>
    None,
    /// A hollow circle, similar to ◦ U+25E6 WHITE BULLET.
    /// <https://www.w3.org/TR/css-counter-styles-3/#circle>
    Circle,
    /// A filled square, similar to ▪ U+25AA BLACK SMALL SQUARE.
    /// <https://www.w3.org/TR/css-counter-styles-3/#square>
    Square,
    /// Symbol for indicating an open disclosure widget, such as the HTML `<details>` element.
    /// <https://www.w3.org/TR/css-counter-styles-3/#disclosure-open>
    DisclosureOpen,
    /// Symbol for indicating a closed disclosure widget, such as the HTML `<details>` element.
    /// <https://www.w3.org/TR/css-counter-styles-3/#disclosure-closed>
    DisclosureClosed,
    /// Western decimal numbers (e.g., 1, 2, 3, ..., 98, 99, 100).
    /// <https://www.w3.org/TR/css-counter-styles-3/#decimal>
    Decimal,
    /// Lowercase ASCII letters (e.g., a, b, c, ..., z, aa, ab).
    /// <https://www.w3.org/TR/css-counter-styles-3/#lower-alpha>
    LowerAlpha,
    /// Uppercase ASCII letters (e.g., A, B, C, ..., Z, AA, AB).
    /// <https://www.w3.org/TR/css-counter-styles-3/#upper-alpha>
    UpperAlpha,
    /// Arabic-indic numbering (e.g.، ١، ٢، ٣، ٤، ...، ٩٨، ٩٩، ١٠٠).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-arabic-indic>
    ArabicIndic,
    /// Bengali numbering (e.g., ১, ২, ৩, ..., ৯৮, ৯৯, ১০০).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-bengali>
    Bengali,
    /// Cambodian/Khmer numbering (e.g., ១, ២, ៣, ..., ៩៨, ៩៩, ១០០).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-cambodian>
    Cambodian,
    /// Han decimal numbers (e.g., 一, 二, 三, ..., 九八, 九九, 一〇〇).
    /// <https://www.w3.org/TR/css-counter-styles-3/#cjk-decimal>
    CjkDecimal,
    /// devanagari numbering (e.g., १, २, ३, ..., ९८, ९९, १००).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-devanagari>
    Devanagari,
    /// Gujarati numbering (e.g., ૧, ૨, ૩, ..., ૯૮, ૯૯, ૧૦૦).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-gujarati>
    Gujarati,
    /// Gurmukhi numbering (e.g., ੧, ੨, ੩, ..., ੯੮, ੯੯, ੧੦੦).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-gurmukhi>
    Gurmukhi,
    /// Kannada numbering (e.g., ೧, ೨, ೩, ..., ೯೮, ೯೯, ೧೦೦).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-kannada>
    Kannada,
    /// Cambodian/Khmer numbering (e.g., ១, ២, ៣, ..., ៩៨, ៩៩, ១០០).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-khmer>
    Khmer,
    /// Laotian numbering (e.g., ໑, ໒, ໓, ..., ໙໘, ໙໙, ໑໐໐).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-lao<
    Lao,
    /// Malayalam numbering (e.g., ൧, ൨, ൩, ..., ൯൮, ൯൯, ൧൦൦).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-malayalam>
    Malayalam,
    /// Mongolian numbering (e.g., ᠑, ᠒, ᠓, ..., ᠙᠘, ᠙᠙, ᠑᠐᠐).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-mongolian>
    Mongolian,
    /// Myanmar (Burmese) numbering (e.g., ၁, ၂, ၃, ..., ၉၈, ၉၉, ၁၀၀).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-myanmar>
    Myanmar,
    /// Oriya numbering (e.g., ୧, ୨, ୩, ..., ୯୮, ୯୯, ୧୦୦).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-oriya>
    Oriya,
    /// Persian numbering (e.g., ۱, ۲, ۳, ۴, ..., ۹۸, ۹۹, ۱۰۰).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-persian>
    Persian,
    /// Telugu numbering (e.g., ౧, ౨, ౩, ..., ౯౮, ౯౯, ౧౦౦).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-telugu>
    Telugu,
    /// Thai (Siamese) numbering (e.g., ๑, ๒, ๓, ..., ๙๘, ๙๙, ๑๐๐).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-thai>
    Thai,
    /// Tibetan numbering (e.g., ༡, ༢, ༣, ..., ༩༨, ༩༩, ༡༠༠).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-tibetan>
    Tibetan,
    /// Han "Earthly Branch" ordinals (e.g., 子, 丑, 寅, ..., 亥).
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-cjk-earthly-branch>
    CjkEarthlyBranch,
    /// Han "Heavenly Stem" ordinals (e.g., 甲, 乙, 丙, ..., 癸)
    /// <https://www.w3.org/TR/css-counter-styles-3/#valdef-counter-style-name-cjk-heavenly-stem>
    CjkHeavenlyStem,
    /// Lowercase classical Greek (e.g., α, β, γ, ..., ω, αα, αβ).
    /// <https://www.w3.org/TR/css-counter-styles-3/#lower-greek>
    LowerGreek,
    /// Dictionary-order hiragana lettering (e.g., あ, い, う, ..., ん, ああ, あい).
    /// <https://www.w3.org/TR/css-counter-styles-3/#hiragana>
    Hiragana,
    /// Iroha-order hiragana lettering (e.g., い, ろ, は, ..., す, いい, いろ).
    /// <https://www.w3.org/TR/css-counter-styles-3/#hiragana-iroha>
    HiraganaIroha,
    /// Dictionary-order katakana lettering (e.g., ア, イ, ウ, ..., ン, アア, アイ).
    /// <https://www.w3.org/TR/css-counter-styles-3/#katakana>
    Katakana,
    /// Iroha-order katakana lettering (e.g., イ, ロ, ハ, ..., ス, イイ, イロ)
    /// <https://www.w3.org/TR/css-counter-styles-3/#katakana-iroha>
    KatakanaIroha,
}

#[cfg(feature = "servo")]
impl ListStyleType {
    /// Initial specified value for `list-style-type`.
    #[inline]
    pub fn disc() -> Self {
        Self::Disc
    }

    /// none value.
    #[inline]
    pub fn none() -> Self {
        Self::None
    }
}

/// A quote pair.
#[derive(
    Clone,
    Debug,
    MallocSizeOf,
    PartialEq,
    SpecifiedValueInfo,
    ToComputedValue,
    ToCss,
    ToResolvedValue,
    ToShmem,
)]
#[repr(C)]
pub struct QuotePair {
    /// The opening quote.
    pub opening: crate::OwnedStr,

    /// The closing quote.
    pub closing: crate::OwnedStr,
}

/// List of quote pairs for the specified/computed value of `quotes` property.
#[derive(
    Clone,
    Debug,
    Default,
    MallocSizeOf,
    PartialEq,
    SpecifiedValueInfo,
    ToComputedValue,
    ToCss,
    ToResolvedValue,
    ToShmem,
)]
#[repr(transparent)]
pub struct QuoteList(
    #[css(iterable, if_empty = "none")]
    #[ignore_malloc_size_of = "Arc"]
    pub crate::ArcSlice<QuotePair>,
);

/// Specified and computed `quotes` property: `auto`, `none`, or a list
/// of characters.
#[derive(
    Clone,
    Debug,
    MallocSizeOf,
    PartialEq,
    SpecifiedValueInfo,
    ToComputedValue,
    ToCss,
    ToResolvedValue,
    ToShmem,
    ToTyped,
)]
#[repr(C)]
pub enum Quotes {
    /// list of quote pairs
    QuoteList(QuoteList),
    /// auto (use lang-dependent quote marks)
    Auto,
}

impl Parse for Quotes {
    fn parse<'i, 't>(
        _: &ParserContext,
        input: &mut Parser<'i, 't>,
    ) -> Result<Quotes, ParseError<'i>> {
        if input
            .try_parse(|input| input.expect_ident_matching("auto"))
            .is_ok()
        {
            return Ok(Quotes::Auto);
        }

        if input
            .try_parse(|input| input.expect_ident_matching("none"))
            .is_ok()
        {
            return Ok(Quotes::QuoteList(QuoteList::default()));
        }

        let mut quotes = Vec::new();
        loop {
            let location = input.current_source_location();
            let opening = match input.next() {
                Ok(&Token::QuotedString(ref value)) => value.as_ref().to_owned().into(),
                Ok(t) => return Err(location.new_unexpected_token_error(t.clone())),
                Err(_) => break,
            };

            let closing = input.expect_string()?.as_ref().to_owned().into();
            quotes.push(QuotePair { opening, closing });
        }

        if !quotes.is_empty() {
            Ok(Quotes::QuoteList(QuoteList(crate::ArcSlice::from_iter(
                quotes.into_iter(),
            ))))
        } else {
            Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError))
        }
    }
}
