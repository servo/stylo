/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Parsing for registered custom properties.

use std::fmt::{self, Write};

use super::{
    registry::PropertyRegistrationData,
    syntax::{
        data_type::DataType, Component as SyntaxComponent, ComponentName, Descriptor, Multiplier,
    },
};
use crate::custom_properties::ComputedValue as ComputedPropertyValue;
use crate::parser::{Parse, ParserContext};
use crate::properties;
use crate::stylesheets::{CssRuleType, Origin, UrlExtraData};
use crate::values::{
    animated::{self, Animate, Procedure},
    computed::{self, ToComputedValue},
    specified, CustomIdent,
};
use cssparser::{BasicParseErrorKind, ParseErrorKind, Parser as CSSParser, TokenSerializationType};
use selectors::matching::QuirksMode;
use servo_arc::Arc;
use smallvec::SmallVec;
use style_traits::{
    owned_str::OwnedStr, CssWriter, ParseError as StyleParseError, ParsingMode,
    PropertySyntaxParseError, StyleParseErrorKind, ToCss,
};

/// A single component of the computed value.
pub type ComputedValueComponent = GenericValueComponent<
    computed::Length,
    computed::Number,
    computed::Percentage,
    computed::LengthPercentage,
    computed::Color,
    computed::Image,
    computed::url::ComputedUrl,
    computed::Integer,
    computed::Angle,
    computed::Time,
    computed::Resolution,
    computed::Transform,
>;

/// A single component of the specified value.
pub type SpecifiedValueComponent = GenericValueComponent<
    specified::Length,
    specified::Number,
    specified::Percentage,
    specified::LengthPercentage,
    specified::Color,
    specified::Image,
    specified::url::SpecifiedUrl,
    specified::Integer,
    specified::Angle,
    specified::Time,
    specified::Resolution,
    specified::Transform,
>;

impl<L, N, P, LP, C, Image, U, Integer, A, T, R, Transform>
    GenericValueComponent<L, N, P, LP, C, Image, U, Integer, A, T, R, Transform>
{
    fn serialization_types(&self) -> (TokenSerializationType, TokenSerializationType) {
        let first_token_type = match self {
            Self::Length(_) | Self::Angle(_) | Self::Time(_) | Self::Resolution(_) => {
                TokenSerializationType::Dimension
            },
            Self::Number(_) | Self::Integer(_) => TokenSerializationType::Number,
            Self::Percentage(_) | Self::LengthPercentage(_) => TokenSerializationType::Percentage,
            Self::Color(_) |
            Self::Image(_) |
            Self::Url(_) |
            Self::TransformFunction(_) |
            Self::TransformList(_) => TokenSerializationType::Function,
            Self::CustomIdent(_) => TokenSerializationType::Ident,
            Self::String(_) => TokenSerializationType::Other,
        };
        let last_token_type = if first_token_type == TokenSerializationType::Function {
            TokenSerializationType::Other
        } else {
            first_token_type
        };
        (first_token_type, last_token_type)
    }
}

/// A generic enum used for both specified value components and computed value components.
#[derive(Animate, Clone, ToCss, ToComputedValue, Debug, MallocSizeOf, PartialEq)]
#[animation(no_bound(Image, Url))]
pub enum GenericValueComponent<
    Length,
    Number,
    Percentage,
    LengthPercentage,
    Color,
    Image,
    Url,
    Integer,
    Angle,
    Time,
    Resolution,
    TransformFunction,
> {
    /// A <length> value
    Length(Length),
    /// A <number> value
    Number(Number),
    /// A <percentage> value
    Percentage(Percentage),
    /// A <length-percentage> value
    LengthPercentage(LengthPercentage),
    /// A <color> value
    Color(Color),
    /// An <image> value
    #[animation(error)]
    Image(Image),
    /// A <url> value
    #[animation(error)]
    Url(Url),
    /// An <integer> value
    Integer(Integer),
    /// An <angle> value
    Angle(Angle),
    /// A <time> value
    Time(Time),
    /// A <resolution> value
    Resolution(Resolution),
    /// A <transform-function> value
    TransformFunction(TransformFunction),
    /// A <custom-ident> value
    #[animation(error)]
    CustomIdent(CustomIdent),
    /// A <transform-list> value, equivalent to <transform-function>+
    TransformList(ComponentList<Self>),
    /// A <string> value
    #[animation(error)]
    String(OwnedStr),
}

/// A list of component values, including the list's multiplier.
#[derive(Clone, ToComputedValue, Debug, MallocSizeOf, PartialEq)]
pub struct ComponentList<Component> {
    /// Multiplier
    pub multiplier: Multiplier,
    /// The list of components contained.
    pub components: crate::OwnedSlice<Component>,
}

impl<Component: Animate> Animate for ComponentList<Component> {
    fn animate(&self, other: &Self, procedure: Procedure) -> Result<Self, ()> {
        if self.multiplier != other.multiplier {
            return Err(());
        }
        let components = animated::lists::by_computed_value::animate(&self.components, &other.components, procedure)?;
        Ok(Self {
            multiplier: self.multiplier,
            components,
        })
    }
}

impl<Component: ToCss> ToCss for ComponentList<Component> {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        let mut iter = self.components.iter();
        let Some(first) = iter.next() else {
            return Ok(());
        };
        first.to_css(dest)?;

        // The separator implied by the multiplier for this list.
        let separator = match self.multiplier {
            // <https://drafts.csswg.org/cssom-1/#serialize-a-whitespace-separated-list>
            Multiplier::Space => " ",
            // <https://drafts.csswg.org/cssom-1/#serialize-a-comma-separated-list>
            Multiplier::Comma => ", ",
        };
        for component in iter {
            dest.write_str(separator)?;
            component.to_css(dest)?;
        }
        Ok(())
    }
}

/// A specified registered custom property value.
#[derive(Animate, ToComputedValue, ToCss, Clone, Debug, MallocSizeOf, PartialEq)]
pub enum Value<Component> {
    /// A single specified component value whose syntax descriptor component did not have a
    /// multiplier.
    Component(Component),
    /// A specified value whose syntax descriptor was the universal syntax definition.
    #[animation(error)]
    Universal(#[ignore_malloc_size_of = "Arc"] Arc<ComputedPropertyValue>),
    /// A list of specified component values whose syntax descriptor component had a multiplier.
    List(#[animation(field_bound)] ComponentList<Component>),
}

/// Specified custom property value.
pub type SpecifiedValue = Value<SpecifiedValueComponent>;

/// Computed custom property value.
pub type ComputedValue = Value<ComputedValueComponent>;

impl SpecifiedValue {
    /// Convert a Computed custom property value to a VariableValue.
    pub fn compute<'i, 't>(
        input: &mut CSSParser<'i, 't>,
        registration: &PropertyRegistrationData,
        url_data: &UrlExtraData,
        context: &computed::Context,
        allow_computationally_dependent: AllowComputationallyDependent,
    ) -> Result<ComputedPropertyValue, ()> {
        let value = Self::get_computed_value(
            input,
            registration,
            url_data,
            context,
            allow_computationally_dependent,
        )?;
        Ok(value.to_variable_value(url_data))
    }

    /// Convert a registered custom property to a Computed custom property value, given input and a
    /// property registration.
    fn get_computed_value<'i, 't>(
        input: &mut CSSParser<'i, 't>,
        registration: &PropertyRegistrationData,
        url_data: &UrlExtraData,
        context: &computed::Context,
        allow_computationally_dependent: AllowComputationallyDependent,
    ) -> Result<ComputedValue, ()> {
        debug_assert!(!registration.syntax.is_universal(), "Shouldn't be needed");
        let Ok(value) = Self::parse(
            input,
            &registration.syntax,
            url_data,
            allow_computationally_dependent,
        ) else {
            return Err(());
        };

        Ok(value.to_computed_value(context))
    }

    /// Parse and validate a registered custom property value according to its syntax descriptor,
    /// and check for computational independence.
    pub fn parse<'i, 't>(
        mut input: &mut CSSParser<'i, 't>,
        syntax: &Descriptor,
        url_data: &UrlExtraData,
        allow_computationally_dependent: AllowComputationallyDependent,
    ) -> Result<Self, StyleParseError<'i>> {
        if syntax.is_universal() {
            return Ok(Self::Universal(Arc::new(ComputedPropertyValue::parse(
                &mut input, url_data,
            )?)));
        }

        let mut values = SmallComponentVec::new();
        let mut multiplier = None;
        {
            let mut parser = Parser::new(syntax, &mut values, &mut multiplier);
            parser.parse(&mut input, url_data, allow_computationally_dependent)?;
        }
        let computed_value = if let Some(multiplier) = multiplier {
            Self::List(ComponentList {
                multiplier,
                components: values.to_vec().into(),
            })
        } else {
            Self::Component(values[0].clone())
        };
        Ok(computed_value)
    }
}

impl ComputedValue {
    fn serialization_types(&self) -> (TokenSerializationType, TokenSerializationType) {
        match self {
            Self::Component(component) => component.serialization_types(),
            Self::Universal(_) => unreachable!(),
            Self::List(list) => list
                .components
                .first()
                .map_or(Default::default(), |f| f.serialization_types()),
        }
    }

    fn to_declared_value(&self, url_data: &UrlExtraData) -> Arc<ComputedPropertyValue> {
        if let Self::Universal(var) = self {
            return Arc::clone(var);
        }
        Arc::new(self.to_variable_value(url_data))
    }

    fn to_variable_value(&self, url_data: &UrlExtraData) -> ComputedPropertyValue {
        debug_assert!(!matches!(self, Self::Universal(..)), "Shouldn't be needed");
        // TODO(zrhoffman, 1864736): Preserve the computed type instead of converting back to a
        // string.
        let serialization_types = self.serialization_types();
        ComputedPropertyValue::new(
            self.to_css_string(),
            url_data,
            serialization_types.0,
            serialization_types.1,
        )
    }
}

/// Whether the computed value parsing should allow computationaly dependent values like 3em or
/// var(-foo).
///
/// https://drafts.css-houdini.org/css-properties-values-api-1/#computationally-independent
pub enum AllowComputationallyDependent {
    /// Only computationally independent values are allowed.
    No,
    /// Computationally independent and dependent values are allowed.
    Yes,
}

type SmallComponentVec = SmallVec<[SpecifiedValueComponent; 1]>;

struct Parser<'a> {
    syntax: &'a Descriptor,
    output: &'a mut SmallComponentVec,
    output_multiplier: &'a mut Option<Multiplier>,
}

impl<'a> Parser<'a> {
    fn new(
        syntax: &'a Descriptor,
        output: &'a mut SmallComponentVec,
        output_multiplier: &'a mut Option<Multiplier>,
    ) -> Self {
        Self {
            syntax,
            output,
            output_multiplier,
        }
    }

    fn parse<'i, 't>(
        &mut self,
        input: &mut CSSParser<'i, 't>,
        url_data: &UrlExtraData,
        allow_computationally_dependent: AllowComputationallyDependent,
    ) -> Result<(), StyleParseError<'i>> {
        use self::AllowComputationallyDependent::*;
        let parsing_mode = match allow_computationally_dependent {
            No => ParsingMode::DISALLOW_FONT_RELATIVE,
            Yes => ParsingMode::DEFAULT,
        };
        let ref context = ParserContext::new(
            Origin::Author,
            url_data,
            Some(CssRuleType::Style),
            parsing_mode,
            QuirksMode::NoQuirks,
            /* namespaces = */ Default::default(),
            None,
            None,
        );
        for component in self.syntax.components.iter() {
            let result = input.try_parse(|input| {
                input.parse_entirely(|input| {
                    Self::parse_value(context, input, &component.unpremultiplied())
                })
            });
            let Ok(values) = result else { continue };
            self.output.extend(values);
            *self.output_multiplier = component.multiplier();
            break;
        }
        if self.output.is_empty() {
            return Err(input.new_error(BasicParseErrorKind::EndOfInput));
        }
        Ok(())
    }

    fn parse_value<'i, 't>(
        context: &ParserContext,
        input: &mut CSSParser<'i, 't>,
        component: &SyntaxComponent,
    ) -> Result<SmallComponentVec, StyleParseError<'i>> {
        let mut values = SmallComponentVec::new();
        values.push(Self::parse_component_without_multiplier(
            context, input, component,
        )?);

        if let Some(multiplier) = component.multiplier() {
            loop {
                let result = Self::expect_multiplier(input, &multiplier);
                if Self::expect_multiplier_yielded_eof_error(&result) {
                    break;
                }
                result?;
                values.push(Self::parse_component_without_multiplier(
                    context, input, component,
                )?);
            }
        }
        Ok(values)
    }

    fn parse_component_without_multiplier<'i, 't>(
        context: &ParserContext,
        input: &mut CSSParser<'i, 't>,
        component: &SyntaxComponent,
    ) -> Result<SpecifiedValueComponent, StyleParseError<'i>> {
        let data_type = match component.name() {
            ComponentName::DataType(ty) => ty,
            ComponentName::Ident(ref name) => {
                let ident = CustomIdent::parse(input, &[])?;
                if ident != *name {
                    return Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError));
                }
                return Ok(SpecifiedValueComponent::CustomIdent(ident));
            },
        };

        let value = match data_type {
            DataType::Length => {
                SpecifiedValueComponent::Length(specified::Length::parse(context, input)?)
            },
            DataType::Number => {
                SpecifiedValueComponent::Number(specified::Number::parse(context, input)?)
            },
            DataType::Percentage => {
                SpecifiedValueComponent::Percentage(specified::Percentage::parse(context, input)?)
            },
            DataType::LengthPercentage => SpecifiedValueComponent::LengthPercentage(
                specified::LengthPercentage::parse(context, input)?,
            ),
            DataType::Color => {
                SpecifiedValueComponent::Color(specified::Color::parse(context, input)?)
            },
            DataType::Image => {
                SpecifiedValueComponent::Image(specified::Image::parse(context, input)?)
            },
            DataType::Url => {
                SpecifiedValueComponent::Url(specified::url::SpecifiedUrl::parse(context, input)?)
            },
            DataType::Integer => {
                SpecifiedValueComponent::Integer(specified::Integer::parse(context, input)?)
            },
            DataType::Angle => {
                SpecifiedValueComponent::Angle(specified::Angle::parse(context, input)?)
            },
            DataType::Time => {
                SpecifiedValueComponent::Time(specified::Time::parse(context, input)?)
            },
            DataType::Resolution => {
                SpecifiedValueComponent::Resolution(specified::Resolution::parse(context, input)?)
            },
            DataType::TransformFunction => SpecifiedValueComponent::TransformFunction(
                specified::Transform::parse(context, input)?,
            ),
            DataType::CustomIdent => {
                let name = CustomIdent::parse(input, &[])?;
                SpecifiedValueComponent::CustomIdent(name)
            },
            DataType::TransformList => {
                let mut values = vec![];
                let Some(multiplier) = component.unpremultiplied().multiplier() else {
                    debug_assert!(false, "Unpremultiplied <transform-list> had no multiplier?");
                    return Err(
                        input.new_custom_error(StyleParseErrorKind::PropertySyntaxField(
                            PropertySyntaxParseError::UnexpectedEOF,
                        )),
                    );
                };
                debug_assert_matches!(multiplier, Multiplier::Space);
                loop {
                    values.push(SpecifiedValueComponent::TransformFunction(
                        specified::Transform::parse(context, input)?,
                    ));
                    let result = Self::expect_multiplier(input, &multiplier);
                    if Self::expect_multiplier_yielded_eof_error(&result) {
                        break;
                    }
                    result?;
                }
                let list = ComponentList {
                    multiplier,
                    components: values.into(),
                };
                SpecifiedValueComponent::TransformList(list)
            },
            DataType::String => {
                let string = input.expect_string()?;
                SpecifiedValueComponent::String(string.as_ref().to_owned().into())
            },
        };
        Ok(value)
    }

    fn expect_multiplier_yielded_eof_error<'i>(result: &Result<(), StyleParseError<'i>>) -> bool {
        matches!(
            result,
            Err(StyleParseError {
                kind: ParseErrorKind::Basic(BasicParseErrorKind::EndOfInput),
                ..
            })
        )
    }

    fn expect_multiplier<'i, 't>(
        input: &mut CSSParser<'i, 't>,
        multiplier: &Multiplier,
    ) -> Result<(), StyleParseError<'i>> {
        match multiplier {
            Multiplier::Space => {
                input.expect_whitespace()?;
                if input.is_exhausted() {
                    // If there was trailing whitespace, do not interpret it as a multiplier
                    return Err(input.new_error(BasicParseErrorKind::EndOfInput));
                }
                Ok(())
            },
            Multiplier::Comma => Ok(input.expect_comma()?),
        }
    }
}


/// An animated value for custom property.
#[derive(Clone, Debug, MallocSizeOf, PartialEq)]
pub struct CustomAnimatedValue {
    /// The name of the custom property.
    pub(crate) name: crate::custom_properties::Name,
    /// The computed value of the custom property.
    value: ComputedValue,
    /// The url data where the value came from.
    /// FIXME: This seems like it should not be needed: registered properties don't need it, and
    /// unregistered properties animate discretely. But we need it so far because the computed
    /// value representation isn't typed.
    url_data: UrlExtraData,
}

impl Animate for CustomAnimatedValue {
    fn animate(&self, other: &Self, procedure: Procedure) -> Result<Self, ()> {
        if self.name != other.name {
            return Err(())
        }
        let value = self.value.animate(&other.value, procedure)?;
        Ok(Self {
            name: self.name.clone(),
            value,
            // NOTE: This is sketchy AF, but it's ~fine, since values that can animate (non-universal)
            // don't need it.
            url_data: self.url_data.clone(),
        })
    }
}

impl CustomAnimatedValue {
    pub(crate) fn from_computed(
        name: &crate::custom_properties::Name,
        value: &Arc<ComputedPropertyValue>,
    ) -> Self {
        Self {
            name: name.clone(),
            // FIXME: Should probably preserve type-ness in ComputedPropertyValue.
            value: ComputedValue::Universal(value.clone()),
            url_data: value.url_data.clone(),
        }
    }

    pub(crate) fn from_declaration(
        declaration: &properties::CustomDeclaration,
        context: &mut computed::Context,
        _initial: &properties::ComputedValues,
    ) -> Option<Self> {
        let value = match declaration.value {
            properties::CustomDeclarationValue::Value(ref v) => v,
            // FIXME: This should be made to work to the extent possible like for non-custom
            // properties (using `initial` at least to handle unset / inherit).
            properties::CustomDeclarationValue::CSSWideKeyword(..) => return None,
        };

        debug_assert!(
            context.builder.stylist.is_some(),
            "Need a Stylist to get property registration!"
        );
        let registration =
            context.builder.stylist.unwrap().get_custom_property_registration(&declaration.name);

        // FIXME: Do we need to perform substitution here somehow?
        let computed_value = if registration.syntax.is_universal() {
            None
        } else {
            let mut input = cssparser::ParserInput::new(&value.css);
            let mut input = CSSParser::new(&mut input);
            SpecifiedValue::get_computed_value(
                &mut input,
                registration,
                &value.url_data,
                context,
                AllowComputationallyDependent::Yes,
            ).ok()
        };

        let url_data = value.url_data.clone();
        let value = computed_value.unwrap_or_else(|| ComputedValue::Universal(Arc::clone(value)));
        Some(Self {
            name: declaration.name.clone(),
            url_data,
            value,
        })
    }

    pub(crate) fn to_declaration(&self) -> properties::PropertyDeclaration {
        properties::PropertyDeclaration::Custom(properties::CustomDeclaration {
            name: self.name.clone(),
            value: properties::CustomDeclarationValue::Value(self.value.to_declared_value(&self.url_data)),
        })
    }
}
