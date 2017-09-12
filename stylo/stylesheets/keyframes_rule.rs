/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! Keyframes: https://drafts.csswg.org/css-animations/#keyframes

use cssparser::{AtRuleParser, Parser, QualifiedRuleParser, RuleListParser, ParserInput, CowRcStr};
use cssparser::{DeclarationListParser, DeclarationParser, parse_one_rule, SourceLocation};
use error_reporting::{NullReporter, ContextualParseError, ParseErrorReporter};
use parser::{ParserContext, ParserErrorContext};
use properties::{Importance, PropertyDeclaration, PropertyDeclarationBlock, PropertyId, PropertyParserContext};
use properties::{PropertyDeclarationId, LonghandId, SourcePropertyDeclaration};
use properties::LonghandIdSet;
use properties::animated_properties::AnimatableLonghand;
use properties::longhands::transition_timing_function::single_value::SpecifiedValue as SpecifiedTimingFunction;
use selectors::parser::SelectorParseError;
use servo_arc::Arc;
use shared_lock::{DeepCloneParams, DeepCloneWithLock, SharedRwLock, SharedRwLockReadGuard, Locked, ToCssWithGuard};
use std::fmt;
use style_traits::{PARSING_MODE_DEFAULT, ToCss, ParseError, StyleParseError};
use style_traits::PropertyDeclarationParseError;
use stylesheets::{CssRuleType, StylesheetContents};
use stylesheets::rule_parser::{VendorPrefix, get_location_with_offset};
use values::{KeyframesName, serialize_percentage};

/// A [`@keyframes`][keyframes] rule.
///
/// [keyframes]: https://drafts.csswg.org/css-animations/#keyframes
#[derive(Debug)]
pub struct KeyframesRule {
    /// The name of the current animation.
    pub name: KeyframesName,
    /// The keyframes specified for this CSS rule.
    pub keyframes: Vec<Arc<Locked<Keyframe>>>,
    /// Vendor prefix type the @keyframes has.
    pub vendor_prefix: Option<VendorPrefix>,
    /// The line and column of the rule's source code.
    pub source_location: SourceLocation,
}

impl ToCssWithGuard for KeyframesRule {
    // Serialization of KeyframesRule is not specced.
    fn to_css<W>(&self, guard: &SharedRwLockReadGuard, dest: &mut W) -> fmt::Result
        where W: fmt::Write,
    {
        dest.write_str("@keyframes ")?;
        self.name.to_css(dest)?;
        dest.write_str(" {")?;
        let iter = self.keyframes.iter();
        for lock in iter {
            dest.write_str("\n")?;
            let keyframe = lock.read_with(&guard);
            keyframe.to_css(guard, dest)?;
        }
        dest.write_str("\n}")
    }
}

impl KeyframesRule {
    /// Returns the index of the last keyframe that matches the given selector.
    /// If the selector is not valid, or no keyframe is found, returns None.
    ///
    /// Related spec:
    /// https://drafts.csswg.org/css-animations-1/#interface-csskeyframesrule-findrule
    pub fn find_rule(&self, guard: &SharedRwLockReadGuard, selector: &str) -> Option<usize> {
        let mut input = ParserInput::new(selector);
        if let Ok(selector) = Parser::new(&mut input).parse_entirely(KeyframeSelector::parse) {
            for (i, keyframe) in self.keyframes.iter().enumerate().rev() {
                if keyframe.read_with(guard).selector == selector {
                    return Some(i);
                }
            }
        }
        None
    }
}

impl DeepCloneWithLock for KeyframesRule {
    fn deep_clone_with_lock(
        &self,
        lock: &SharedRwLock,
        guard: &SharedRwLockReadGuard,
        params: &DeepCloneParams,
    ) -> Self {
        KeyframesRule {
            name: self.name.clone(),
            keyframes: self.keyframes.iter()
                .map(|x| {
                    Arc::new(lock.wrap(
                        x.read_with(guard).deep_clone_with_lock(lock, guard, params)
                    ))
                })
                .collect(),
            vendor_prefix: self.vendor_prefix.clone(),
            source_location: self.source_location.clone(),
        }
    }
}

/// A number from 0 to 1, indicating the percentage of the animation when this
/// keyframe should run.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "gecko", derive(MallocSizeOf))]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub struct KeyframePercentage(pub f32);

impl ::std::cmp::Ord for KeyframePercentage {
    #[inline]
    fn cmp(&self, other: &Self) -> ::std::cmp::Ordering {
        // We know we have a number from 0 to 1, so unwrap() here is safe.
        self.0.partial_cmp(&other.0).unwrap()
    }
}

impl ::std::cmp::Eq for KeyframePercentage { }

impl ToCss for KeyframePercentage {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
        serialize_percentage(self.0, dest)
    }
}

impl KeyframePercentage {
    /// Trivially constructs a new `KeyframePercentage`.
    #[inline]
    pub fn new(value: f32) -> KeyframePercentage {
        debug_assert!(value >= 0. && value <= 1.);
        KeyframePercentage(value)
    }

    fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<KeyframePercentage, ParseError<'i>> {
        let percentage = if input.try(|input| input.expect_ident_matching("from")).is_ok() {
            KeyframePercentage::new(0.)
        } else if input.try(|input| input.expect_ident_matching("to")).is_ok() {
            KeyframePercentage::new(1.)
        } else {
            let percentage = input.expect_percentage()?;
            if percentage >= 0. && percentage <= 1. {
                KeyframePercentage::new(percentage)
            } else {
                return Err(StyleParseError::UnspecifiedError.into());
            }
        };

        Ok(percentage)
    }
}

/// A keyframes selector is a list of percentages or from/to symbols, which are
/// converted at parse time to percentages.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct KeyframeSelector(Vec<KeyframePercentage>);

impl ToCss for KeyframeSelector {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
        let mut iter = self.0.iter();
        iter.next().unwrap().to_css(dest)?;
        for percentage in iter {
            dest.write_str(", ")?;
            percentage.to_css(dest)?;
        }
        Ok(())
    }
}

impl KeyframeSelector {
    /// Return the list of percentages this selector contains.
    #[inline]
    pub fn percentages(&self) -> &[KeyframePercentage] {
        &self.0
    }

    /// A dummy public function so we can write a unit test for this.
    pub fn new_for_unit_testing(percentages: Vec<KeyframePercentage>) -> KeyframeSelector {
        KeyframeSelector(percentages)
    }

    /// Parse a keyframe selector from CSS input.
    pub fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i>> {
        input.parse_comma_separated(KeyframePercentage::parse)
             .map(KeyframeSelector)
    }
}

/// A keyframe.
#[derive(Debug)]
pub struct Keyframe {
    /// The selector this keyframe was specified from.
    pub selector: KeyframeSelector,

    /// The declaration block that was declared inside this keyframe.
    ///
    /// Note that `!important` rules in keyframes don't apply, but we keep this
    /// `Arc` just for convenience.
    pub block: Arc<Locked<PropertyDeclarationBlock>>,

    /// The line and column of the rule's source code.
    pub source_location: SourceLocation,
}

impl ToCssWithGuard for Keyframe {
    fn to_css<W>(&self, guard: &SharedRwLockReadGuard, dest: &mut W) -> fmt::Result
    where W: fmt::Write {
        self.selector.to_css(dest)?;
        dest.write_str(" { ")?;
        self.block.read_with(guard).to_css(dest)?;
        dest.write_str(" }")?;
        Ok(())
    }
}

impl Keyframe {
    /// Parse a CSS keyframe.
    pub fn parse<'i>(
        css: &'i str,
        parent_stylesheet_contents: &StylesheetContents,
        lock: &SharedRwLock,
    ) -> Result<Arc<Locked<Self>>, ParseError<'i>> {
        let url_data = parent_stylesheet_contents.url_data.read();
        let error_reporter = NullReporter;
        let namespaces = parent_stylesheet_contents.namespaces.read();
        let mut context = ParserContext::new(
            parent_stylesheet_contents.origin,
            &url_data,
            Some(CssRuleType::Keyframe),
            PARSING_MODE_DEFAULT,
            parent_stylesheet_contents.quirks_mode
        );
        let error_context = ParserErrorContext { error_reporter: &error_reporter };
        context.namespaces = Some(&*namespaces);
        let mut input = ParserInput::new(css);
        let mut input = Parser::new(&mut input);

        let mut declarations = SourcePropertyDeclaration::new();
        let mut rule_parser = KeyframeListParser {
            context: &context,
            error_context: &error_context,
            shared_lock: &lock,
            declarations: &mut declarations,
        };
        parse_one_rule(&mut input, &mut rule_parser)
    }
}

impl DeepCloneWithLock for Keyframe {
    /// Deep clones this Keyframe.
    fn deep_clone_with_lock(
        &self,
        lock: &SharedRwLock,
        guard: &SharedRwLockReadGuard,
        _params: &DeepCloneParams,
    ) -> Keyframe {
        Keyframe {
            selector: self.selector.clone(),
            block: Arc::new(lock.wrap(self.block.read_with(guard).clone())),
            source_location: self.source_location.clone(),
        }
    }
}

/// A keyframes step value. This can be a synthetised keyframes animation, that
/// is, one autogenerated from the current computed values, or a list of
/// declarations to apply.
///
/// TODO: Find a better name for this?
#[derive(Debug)]
#[cfg_attr(feature = "gecko", derive(MallocSizeOf))]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub enum KeyframesStepValue {
    /// A step formed by a declaration block specified by the CSS.
    Declarations {
        /// The declaration block per se.
        #[cfg_attr(feature = "gecko",
                   ignore_malloc_size_of = "XXX: Primary ref, measure if DMD says it's worthwhile")]
        #[cfg_attr(feature = "servo", ignore_heap_size_of = "Arc")]
        block: Arc<Locked<PropertyDeclarationBlock>>
    },
    /// A synthetic step computed from the current computed values at the time
    /// of the animation.
    ComputedValues,
}

/// A single step from a keyframe animation.
#[derive(Debug)]
#[cfg_attr(feature = "gecko", derive(MallocSizeOf))]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub struct KeyframesStep {
    /// The percentage of the animation duration when this step starts.
    pub start_percentage: KeyframePercentage,
    /// Declarations that will determine the final style during the step, or
    /// `ComputedValues` if this is an autogenerated step.
    pub value: KeyframesStepValue,
    /// Wether a animation-timing-function declaration exists in the list of
    /// declarations.
    ///
    /// This is used to know when to override the keyframe animation style.
    pub declared_timing_function: bool,
}

impl KeyframesStep {
    #[inline]
    fn new(percentage: KeyframePercentage,
           value: KeyframesStepValue,
           guard: &SharedRwLockReadGuard) -> Self {
        let declared_timing_function = match value {
            KeyframesStepValue::Declarations { ref block } => {
                block.read_with(guard).declarations().iter().any(|prop_decl| {
                    match *prop_decl {
                        PropertyDeclaration::AnimationTimingFunction(..) => true,
                        _ => false,
                    }
                })
            }
            _ => false,
        };

        KeyframesStep {
            start_percentage: percentage,
            value: value,
            declared_timing_function: declared_timing_function,
        }
    }

    /// Return specified TransitionTimingFunction if this KeyframesSteps has 'animation-timing-function'.
    pub fn get_animation_timing_function(&self, guard: &SharedRwLockReadGuard)
                                         -> Option<SpecifiedTimingFunction> {
        if !self.declared_timing_function {
            return None;
        }
        match self.value {
            KeyframesStepValue::Declarations { ref block } => {
                let guard = block.read_with(guard);
                let (declaration, _) =
                    guard.get(PropertyDeclarationId::Longhand(LonghandId::AnimationTimingFunction)).unwrap();
                match *declaration {
                    PropertyDeclaration::AnimationTimingFunction(ref value) => {
                        // Use the first value.
                        Some(value.0[0])
                    },
                    PropertyDeclaration::CSSWideKeyword(..) => None,
                    PropertyDeclaration::WithVariables(..) => None,
                    _ => panic!(),
                }
            },
            KeyframesStepValue::ComputedValues => {
                panic!("Shouldn't happen to set animation-timing-function in missing keyframes")
            },
        }
    }
}

/// This structure represents a list of animation steps computed from the list
/// of keyframes, in order.
///
/// It only takes into account animable properties.
#[derive(Debug)]
#[cfg_attr(feature = "gecko", derive(MallocSizeOf))]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub struct KeyframesAnimation {
    /// The difference steps of the animation.
    pub steps: Vec<KeyframesStep>,
    /// The properties that change in this animation.
    pub properties_changed: Vec<AnimatableLonghand>,
    /// Vendor prefix type the @keyframes has.
    pub vendor_prefix: Option<VendorPrefix>,
}

/// Get all the animated properties in a keyframes animation.
fn get_animated_properties(keyframes: &[Arc<Locked<Keyframe>>], guard: &SharedRwLockReadGuard)
                           -> Vec<AnimatableLonghand> {
    let mut ret = vec![];
    let mut seen = LonghandIdSet::new();
    // NB: declarations are already deduplicated, so we don't have to check for
    // it here.
    for keyframe in keyframes {
        let keyframe = keyframe.read_with(&guard);
        let block = keyframe.block.read_with(guard);
        for (declaration, importance) in block.declaration_importance_iter() {
            assert!(!importance.important());

            if let Some(property) = AnimatableLonghand::from_declaration(declaration) {
                // Skip the 'display' property because although it is animatable from SMIL,
                // it should not be animatable from CSS Animations or Web Animations.
                if property != AnimatableLonghand::Display &&
                   !seen.has_animatable_longhand_bit(&property) {
                    seen.set_animatable_longhand_bit(&property);
                    ret.push(property);
                }
            }
        }
    }

    ret
}

impl KeyframesAnimation {
    /// Create a keyframes animation from a given list of keyframes.
    ///
    /// This will return a keyframe animation with empty steps and
    /// properties_changed if the list of keyframes is empty, or there are no
    /// animated properties obtained from the keyframes.
    ///
    /// Otherwise, this will compute and sort the steps used for the animation,
    /// and return the animation object.
    pub fn from_keyframes(keyframes: &[Arc<Locked<Keyframe>>],
                          vendor_prefix: Option<VendorPrefix>,
                          guard: &SharedRwLockReadGuard)
                          -> Self {
        let mut result = KeyframesAnimation {
            steps: vec![],
            properties_changed: vec![],
            vendor_prefix: vendor_prefix,
        };

        if keyframes.is_empty() {
            return result;
        }

        result.properties_changed = get_animated_properties(keyframes, guard);
        if result.properties_changed.is_empty() {
            return result;
        }

        for keyframe in keyframes {
            let keyframe = keyframe.read_with(&guard);
            for percentage in keyframe.selector.0.iter() {
                result.steps.push(KeyframesStep::new(*percentage, KeyframesStepValue::Declarations {
                    block: keyframe.block.clone(),
                }, guard));
            }
        }

        // Sort by the start percentage, so we can easily find a frame.
        result.steps.sort_by_key(|step| step.start_percentage);

        // Prepend autogenerated keyframes if appropriate.
        if result.steps[0].start_percentage.0 != 0. {
            result.steps.insert(0, KeyframesStep::new(KeyframePercentage::new(0.),
                                                      KeyframesStepValue::ComputedValues,
                                                      guard));
        }

        if result.steps.last().unwrap().start_percentage.0 != 1. {
            result.steps.push(KeyframesStep::new(KeyframePercentage::new(1.),
                                                 KeyframesStepValue::ComputedValues,
                                                 guard));
        }

        result
    }
}

/// Parses a keyframes list, like:
/// 0%, 50% {
///     width: 50%;
/// }
///
/// 40%, 60%, 100% {
///     width: 100%;
/// }
struct KeyframeListParser<'a, R: 'a> {
    context: &'a ParserContext<'a>,
    error_context: &'a ParserErrorContext<'a, R>,
    shared_lock: &'a SharedRwLock,
    declarations: &'a mut SourcePropertyDeclaration,
}

/// Parses a keyframe list from CSS input.
pub fn parse_keyframe_list<R>(
    context: &ParserContext,
    error_context: &ParserErrorContext<R>,
    input: &mut Parser,
    shared_lock: &SharedRwLock
) -> Vec<Arc<Locked<Keyframe>>>
    where R: ParseErrorReporter
{
    debug_assert!(context.namespaces.is_some(),
                  "Parsing a keyframe list from a context without namespaces?");

    let mut declarations = SourcePropertyDeclaration::new();
    RuleListParser::new_for_nested_rule(input, KeyframeListParser {
        context: context,
        error_context: error_context,
        shared_lock: shared_lock,
        declarations: &mut declarations,
    }).filter_map(Result::ok).collect()
}

impl<'a, 'i, R> AtRuleParser<'i> for KeyframeListParser<'a, R> {
    type PreludeNoBlock = ();
    type PreludeBlock = ();
    type AtRule = Arc<Locked<Keyframe>>;
    type Error = SelectorParseError<'i, StyleParseError<'i>>;
}

/// A wrapper to wraps the KeyframeSelector with its source location
struct KeyframeSelectorParserPrelude {
    selector: KeyframeSelector,
    source_location: SourceLocation,
}

impl<'a, 'i, R: ParseErrorReporter> QualifiedRuleParser<'i> for KeyframeListParser<'a, R> {
    type Prelude = KeyframeSelectorParserPrelude;
    type QualifiedRule = Arc<Locked<Keyframe>>;
    type Error = SelectorParseError<'i, StyleParseError<'i>>;

    fn parse_prelude<'t>(&mut self, input: &mut Parser<'i, 't>) -> Result<Self::Prelude, ParseError<'i>> {
        let start_position = input.position();
        let start_location = input.current_source_location();
        let location = get_location_with_offset(start_location);
        match KeyframeSelector::parse(input) {
            Ok(sel) => {
                Ok(KeyframeSelectorParserPrelude {
                    selector: sel,
                    source_location: location,
                })
            },
            Err(e) => {
                let error = ContextualParseError::InvalidKeyframeRule(input.slice_from(start_position), e.clone());
                self.context.log_css_error(self.error_context, start_location, error);
                Err(e)
            }
        }
    }

    fn parse_block<'t>(&mut self, prelude: Self::Prelude, input: &mut Parser<'i, 't>)
                       -> Result<Self::QualifiedRule, ParseError<'i>> {
        let context =
            ParserContext::new_with_rule_type(
                self.context,
                CssRuleType::Keyframe,
                self.context.namespaces.unwrap(),
            );

        let parser = KeyframeDeclarationParser {
            context: &context,
            declarations: self.declarations,
        };
        let mut iter = DeclarationListParser::new(input, parser);
        let mut block = PropertyDeclarationBlock::new();
        while let Some(declaration) = iter.next() {
            match declaration {
                Ok(()) => {
                    block.extend(iter.parser.declarations.drain(), Importance::Normal);
                }
                Err(err) => {
                    iter.parser.declarations.clear();
                    let error = ContextualParseError::UnsupportedKeyframePropertyDeclaration(err.slice, err.error);
                    context.log_css_error(self.error_context, err.location, error);
                }
            }
            // `parse_important` is not called here, `!important` is not allowed in keyframe blocks.
        }
        Ok(Arc::new(self.shared_lock.wrap(Keyframe {
            selector: prelude.selector,
            block: Arc::new(self.shared_lock.wrap(block)),
            source_location: prelude.source_location,
        })))
    }
}

struct KeyframeDeclarationParser<'a, 'b: 'a> {
    context: &'a ParserContext<'b>,
    declarations: &'a mut SourcePropertyDeclaration,
}

/// Default methods reject all at rules.
impl<'a, 'b, 'i> AtRuleParser<'i> for KeyframeDeclarationParser<'a, 'b> {
    type PreludeNoBlock = ();
    type PreludeBlock = ();
    type AtRule = ();
    type Error = SelectorParseError<'i, StyleParseError<'i>>;
}

impl<'a, 'b, 'i> DeclarationParser<'i> for KeyframeDeclarationParser<'a, 'b> {
    type Declaration = ();
    type Error = SelectorParseError<'i, StyleParseError<'i>>;

    fn parse_value<'t>(&mut self, name: CowRcStr<'i>, input: &mut Parser<'i, 't>)
                       -> Result<(), ParseError<'i>> {
        let property_context = PropertyParserContext::new(self.context);

        let id = PropertyId::parse(&name, Some(&property_context))
            .map_err(|()| PropertyDeclarationParseError::UnknownProperty(name.clone()))?;
        match PropertyDeclaration::parse_into(self.declarations, id, name, self.context, input) {
            Ok(()) => {
                // In case there is still unparsed text in the declaration, we should roll back.
                input.expect_exhausted().map_err(|e| e.into())
            }
            Err(_e) => Err(StyleParseError::UnspecifiedError.into())
        }
    }
}
