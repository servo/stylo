/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

#![deny(missing_docs)]

//! Servo's selector parser.

use crate::attr::{AttrIdentifier, AttrValue};
use crate::computed_value_flags::ComputedValueFlags;
use crate::dom::{OpaqueNode, TElement, TNode};
use crate::invalidation::element::document_state::InvalidationMatchingData;
use crate::invalidation::element::element_wrapper::ElementSnapshot;
use crate::properties::longhands::display::computed_value::T as Display;
use crate::properties::{ComputedValues, PropertyFlags};
use crate::selector_parser::AttrValue as SelectorAttrValue;
use crate::selector_parser::{PseudoElementCascadeType, SelectorParser};
use crate::values::{AtomIdent, AtomString};
use crate::{Atom, CaseSensitivityExt, LocalName, Namespace, Prefix};
use cssparser::{serialize_identifier, CowRcStr, Parser as CssParser, SourceLocation, ToCss};
use dom::{DocumentState, ElementState};
use fxhash::FxHashMap;
use selectors::attr::{AttrSelectorOperation, CaseSensitivity, NamespaceConstraint};
use selectors::parser::SelectorParseErrorKind;
use selectors::visitor::SelectorVisitor;
use std::fmt;
use std::mem;
use std::ops::{Deref, DerefMut};
use style_traits::{ParseError, StyleParseErrorKind};

/// A pseudo-element, both public and private.
///
/// NB: If you add to this list, be sure to update `each_simple_pseudo_element` too.
#[derive(
    Clone, Copy, Debug, Deserialize, Eq, Hash, MallocSizeOf, PartialEq, Serialize, ToShmem,
)]
#[allow(missing_docs)]
#[repr(usize)]
pub enum PseudoElement {
    // Eager pseudos. Keep these first so that eager_index() works.
    After = 0,
    Before,
    Selection,
    // If/when :first-letter is added, update is_first_letter accordingly.

    // If/when :first-line is added, update is_first_line accordingly.

    // If/when ::first-letter, ::first-line, or ::placeholder are added, adjust
    // our property_restriction implementation to do property filtering for
    // them.  Also, make sure the UA sheet has the !important rules some of the
    // APPLIES_TO_PLACEHOLDER properties expect!

    // Non-eager pseudos.
    Backdrop,
    DetailsSummary,
    DetailsContent,
    Marker,

    // Implemented pseudos. These pseudo elements are representing the
    // elements within an UA shadow DOM, and matching the elements with
    // their appropriate styles.
    ColorSwatch,
    Placeholder,

    // Private, Servo-specific implemented pseudos. Only matchable in UA sheet.
    ServoTextControlInnerContainer,
    ServoTextControlInnerEditor,

    // Other Servo-specific pseudos.
    ServoAnonymousBox,
    ServoAnonymousTable,
    ServoAnonymousTableCell,
    ServoAnonymousTableRow,
    ServoTableGrid,
    ServoTableWrapper,
}

/// The count of all pseudo-elements.
pub const PSEUDO_COUNT: usize = PseudoElement::ServoTableWrapper as usize + 1;

impl ToCss for PseudoElement {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        use self::PseudoElement::*;
        dest.write_str(match *self {
            After => "::after",
            Before => "::before",
            Selection => "::selection",
            Backdrop => "::backdrop",
            DetailsSummary => "::-servo-details-summary",
            DetailsContent => "::-servo-details-content",
            Marker => "::marker",
            ColorSwatch => "::color-swatch",
            Placeholder => "::placeholder",
            ServoTextControlInnerContainer => "::-servo-text-control-inner-container",
            ServoTextControlInnerEditor => "::-servo-text-control-inner-editor",
            ServoAnonymousBox => "::-servo-anonymous-box",
            ServoAnonymousTable => "::-servo-anonymous-table",
            ServoAnonymousTableCell => "::-servo-anonymous-table-cell",
            ServoAnonymousTableRow => "::-servo-anonymous-table-row",
            ServoTableGrid => "::-servo-table-grid",
            ServoTableWrapper => "::-servo-table-wrapper",
        })
    }
}

impl ::selectors::parser::PseudoElement for PseudoElement {
    type Impl = SelectorImpl;
}

/// The number of eager pseudo-elements. Keep this in sync with cascade_type.
pub const EAGER_PSEUDO_COUNT: usize = 3;

impl PseudoElement {
    /// Gets the canonical index of this eagerly-cascaded pseudo-element.
    #[inline]
    pub fn eager_index(&self) -> usize {
        debug_assert!(self.is_eager());
        self.clone() as usize
    }

    /// An index for this pseudo-element to be indexed in an enumerated array.
    #[inline]
    pub fn index(&self) -> usize {
        self.clone() as usize
    }

    /// An array of `None`, one per pseudo-element.
    pub fn pseudo_none_array<T>() -> [Option<T>; PSEUDO_COUNT] {
        Default::default()
    }

    /// Creates a pseudo-element from an eager index.
    #[inline]
    pub fn from_eager_index(i: usize) -> Self {
        assert!(i < EAGER_PSEUDO_COUNT);
        let result: PseudoElement = unsafe { mem::transmute(i) };
        debug_assert!(result.is_eager());
        result
    }

    /// Whether the current pseudo element is ::before or ::after.
    #[inline]
    pub fn is_before_or_after(&self) -> bool {
        self.is_before() || self.is_after()
    }

    /// Whether this is an unknown ::-webkit- pseudo-element.
    #[inline]
    pub fn is_unknown_webkit_pseudo_element(&self) -> bool {
        false
    }

    /// Whether this pseudo-element is the ::marker pseudo.
    #[inline]
    pub fn is_marker(&self) -> bool {
        *self == PseudoElement::Marker
    }

    /// Whether this pseudo-element is the ::selection pseudo.
    #[inline]
    pub fn is_selection(&self) -> bool {
        *self == PseudoElement::Selection
    }

    /// Whether this pseudo-element is the ::before pseudo.
    #[inline]
    pub fn is_before(&self) -> bool {
        *self == PseudoElement::Before
    }

    /// Whether this pseudo-element is the ::after pseudo.
    #[inline]
    pub fn is_after(&self) -> bool {
        *self == PseudoElement::After
    }

    /// Whether the current pseudo element is :first-letter
    #[inline]
    pub fn is_first_letter(&self) -> bool {
        false
    }

    /// Whether the current pseudo element is :first-line
    #[inline]
    pub fn is_first_line(&self) -> bool {
        false
    }

    /// Whether this pseudo-element is representing the color swatch
    /// inside an `<input>` element.
    #[inline]
    pub fn is_color_swatch(&self) -> bool {
        *self == PseudoElement::ColorSwatch
    }

    /// Whether this pseudo-element is eagerly-cascaded.
    #[inline]
    pub fn is_eager(&self) -> bool {
        self.cascade_type() == PseudoElementCascadeType::Eager
    }

    /// Whether this pseudo-element is lazily-cascaded.
    #[inline]
    pub fn is_lazy(&self) -> bool {
        self.cascade_type() == PseudoElementCascadeType::Lazy
    }

    /// Whether this pseudo-element is for an anonymous box.
    pub fn is_anon_box(&self) -> bool {
        self.is_precomputed()
    }

    /// Whether this pseudo-element skips flex/grid container display-based
    /// fixup.
    #[inline]
    pub fn skip_item_display_fixup(&self) -> bool {
        !self.is_before_or_after()
    }

    /// Whether this pseudo-element is precomputed.
    #[inline]
    pub fn is_precomputed(&self) -> bool {
        self.cascade_type() == PseudoElementCascadeType::Precomputed
    }

    /// Returns which kind of cascade type has this pseudo.
    ///
    /// See the documentation for `PseudoElementCascadeType` for how we choose
    /// which cascade type to use.
    ///
    /// Note: Keep eager pseudos in sync with `EAGER_PSEUDO_COUNT` and
    /// `EMPTY_PSEUDO_ARRAY` in `style/data.rs`
    #[inline]
    pub fn cascade_type(&self) -> PseudoElementCascadeType {
        match *self {
            PseudoElement::After | PseudoElement::Before | PseudoElement::Selection => {
                PseudoElementCascadeType::Eager
            },
            PseudoElement::Backdrop |
            PseudoElement::ColorSwatch |
            PseudoElement::DetailsSummary |
            PseudoElement::Marker |
            PseudoElement::Placeholder |
            PseudoElement::ServoTextControlInnerContainer |
            PseudoElement::ServoTextControlInnerEditor => PseudoElementCascadeType::Lazy,
            PseudoElement::DetailsContent |
            PseudoElement::ServoAnonymousBox |
            PseudoElement::ServoAnonymousTable |
            PseudoElement::ServoAnonymousTableCell |
            PseudoElement::ServoAnonymousTableRow |
            PseudoElement::ServoTableGrid |
            PseudoElement::ServoTableWrapper => PseudoElementCascadeType::Precomputed,
        }
    }

    /// Covert non-canonical pseudo-element to canonical one, and keep a
    /// canonical one as it is.
    pub fn canonical(&self) -> PseudoElement {
        self.clone()
    }

    /// Stub, only Gecko needs this
    pub fn pseudo_info(&self) {
        ()
    }

    /// Property flag that properties must have to apply to this pseudo-element.
    #[inline]
    pub fn property_restriction(&self) -> Option<PropertyFlags> {
        None
    }

    /// Whether this pseudo-element should actually exist if it has
    /// the given styles.
    pub fn should_exist(&self, style: &ComputedValues) -> bool {
        let display = style.get_box().clone_display();
        if display == Display::None {
            return false;
        }
        if self.is_before_or_after() && style.ineffective_content_property() {
            return false;
        }

        true
    }
}

/// The type used for storing `:lang` arguments.
pub type Lang = Box<str>;

/// The type used to store the state argument to the `:state` pseudo-class.
#[derive(Clone, Debug, Eq, Hash, MallocSizeOf, PartialEq, ToCss, ToShmem)]
pub struct CustomState(pub AtomIdent);

/// A non tree-structural pseudo-class.
/// See https://drafts.csswg.org/selectors-4/#structural-pseudos
#[derive(Clone, Debug, Eq, Hash, MallocSizeOf, PartialEq, ToShmem)]
#[allow(missing_docs)]
pub enum NonTSPseudoClass {
    Active,
    AnyLink,
    Autofill,
    Checked,
    /// The :state` pseudo-class.
    CustomState(CustomState),
    Default,
    Defined,
    Disabled,
    Enabled,
    Focus,
    FocusWithin,
    FocusVisible,
    Fullscreen,
    Hover,
    InRange,
    Indeterminate,
    Invalid,
    Lang(Lang),
    Link,
    Modal,
    MozMeterOptimum,
    MozMeterSubOptimum,
    MozMeterSubSubOptimum,
    Optional,
    OutOfRange,
    PlaceholderShown,
    PopoverOpen,
    ReadOnly,
    ReadWrite,
    Required,
    ServoNonZeroBorder,
    Target,
    UserInvalid,
    UserValid,
    Valid,
    Visited,
}

impl ::selectors::parser::NonTSPseudoClass for NonTSPseudoClass {
    type Impl = SelectorImpl;

    #[inline]
    fn is_active_or_hover(&self) -> bool {
        matches!(*self, NonTSPseudoClass::Active | NonTSPseudoClass::Hover)
    }

    #[inline]
    fn is_user_action_state(&self) -> bool {
        matches!(
            *self,
            NonTSPseudoClass::Active | NonTSPseudoClass::Hover | NonTSPseudoClass::Focus
        )
    }

    fn visit<V>(&self, _: &mut V) -> bool
    where
        V: SelectorVisitor<Impl = Self::Impl>,
    {
        true
    }
}

impl ToCss for NonTSPseudoClass {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        use self::NonTSPseudoClass::*;
        if let Lang(ref lang) = *self {
            dest.write_str(":lang(")?;
            serialize_identifier(lang, dest)?;
            return dest.write_char(')');
        }

        dest.write_str(match *self {
            Self::Active => ":active",
            Self::AnyLink => ":any-link",
            Self::Autofill => ":autofill",
            Self::Checked => ":checked",
            Self::Default => ":default",
            Self::Defined => ":defined",
            Self::Disabled => ":disabled",
            Self::Enabled => ":enabled",
            Self::Focus => ":focus",
            Self::FocusVisible => ":focus-visible",
            Self::FocusWithin => ":focus-within",
            Self::Fullscreen => ":fullscreen",
            Self::Hover => ":hover",
            Self::InRange => ":in-range",
            Self::Indeterminate => ":indeterminate",
            Self::Invalid => ":invalid",
            Self::Link => ":link",
            Self::Modal => ":modal",
            Self::MozMeterOptimum => ":-moz-meter-optimum",
            Self::MozMeterSubOptimum => ":-moz-meter-sub-optimum",
            Self::MozMeterSubSubOptimum => ":-moz-meter-sub-sub-optimum",
            Self::Optional => ":optional",
            Self::OutOfRange => ":out-of-range",
            Self::PlaceholderShown => ":placeholder-shown",
            Self::PopoverOpen => ":popover-open",
            Self::ReadOnly => ":read-only",
            Self::ReadWrite => ":read-write",
            Self::Required => ":required",
            Self::ServoNonZeroBorder => ":-servo-nonzero-border",
            Self::Target => ":target",
            Self::UserInvalid => ":user-invalid",
            Self::UserValid => ":user-valid",
            Self::Valid => ":valid",
            Self::Visited => ":visited",
            Self::Lang(_) | Self::CustomState(_) => unreachable!(),
        })
    }
}

impl NonTSPseudoClass {
    /// Gets a given state flag for this pseudo-class. This is used to do
    /// selector matching, and it's set from the DOM.
    pub fn state_flag(&self) -> ElementState {
        match *self {
            Self::Active => ElementState::ACTIVE,
            Self::AnyLink => ElementState::VISITED_OR_UNVISITED,
            Self::Autofill => ElementState::AUTOFILL,
            Self::Checked => ElementState::CHECKED,
            Self::Default => ElementState::DEFAULT,
            Self::Defined => ElementState::DEFINED,
            Self::Disabled => ElementState::DISABLED,
            Self::Enabled => ElementState::ENABLED,
            Self::Focus => ElementState::FOCUS,
            Self::FocusVisible => ElementState::FOCUSRING,
            Self::FocusWithin => ElementState::FOCUS_WITHIN,
            Self::Fullscreen => ElementState::FULLSCREEN,
            Self::Hover => ElementState::HOVER,
            Self::InRange => ElementState::INRANGE,
            Self::Indeterminate => ElementState::INDETERMINATE,
            Self::Invalid => ElementState::INVALID,
            Self::Link => ElementState::UNVISITED,
            Self::Modal => ElementState::MODAL,
            Self::MozMeterOptimum => ElementState::OPTIMUM,
            Self::MozMeterSubOptimum => ElementState::SUB_OPTIMUM,
            Self::MozMeterSubSubOptimum => ElementState::SUB_SUB_OPTIMUM,
            Self::Optional => ElementState::OPTIONAL_,
            Self::OutOfRange => ElementState::OUTOFRANGE,
            Self::PlaceholderShown => ElementState::PLACEHOLDER_SHOWN,
            Self::PopoverOpen => ElementState::POPOVER_OPEN,
            Self::ReadOnly => ElementState::READONLY,
            Self::ReadWrite => ElementState::READWRITE,
            Self::Required => ElementState::REQUIRED,
            Self::Target => ElementState::URLTARGET,
            Self::UserInvalid => ElementState::USER_INVALID,
            Self::UserValid => ElementState::USER_VALID,
            Self::Valid => ElementState::VALID,
            Self::Visited => ElementState::VISITED,
            Self::CustomState(_) | Self::Lang(_) | Self::ServoNonZeroBorder => ElementState::empty(),
        }
    }

    /// Get the document state flag associated with a pseudo-class, if any.
    pub fn document_state_flag(&self) -> DocumentState {
        DocumentState::empty()
    }

    /// Returns true if the given pseudoclass should trigger style sharing cache revalidation.
    pub fn needs_cache_revalidation(&self) -> bool {
        self.state_flag().is_empty()
    }
}

/// The abstract struct we implement the selector parser implementation on top
/// of.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "servo", derive(MallocSizeOf))]
pub struct SelectorImpl;

/// A set of extra data to carry along with the matching context, either for
/// selector-matching or invalidation.
#[derive(Debug, Default)]
pub struct ExtraMatchingData<'a> {
    /// The invalidation data to invalidate doc-state pseudo-classes correctly.
    pub invalidation_data: InvalidationMatchingData,

    /// The invalidation bits from matching container queries. These are here
    /// just for convenience mostly.
    pub cascade_input_flags: ComputedValueFlags,

    /// The style of the originating element in order to evaluate @container
    /// size queries affecting pseudo-elements.
    pub originating_element_style: Option<&'a ComputedValues>,
}

impl ::selectors::SelectorImpl for SelectorImpl {
    type PseudoElement = PseudoElement;
    type NonTSPseudoClass = NonTSPseudoClass;

    type ExtraMatchingData<'a> = ExtraMatchingData<'a>;
    type AttrValue = AtomString;
    type Identifier = AtomIdent;
    type LocalName = LocalName;
    type NamespacePrefix = Prefix;
    type NamespaceUrl = Namespace;
    type BorrowedLocalName = web_atoms::LocalName;
    type BorrowedNamespaceUrl = web_atoms::Namespace;
}

impl<'a, 'i> ::selectors::Parser<'i> for SelectorParser<'a> {
    type Impl = SelectorImpl;
    type Error = StyleParseErrorKind<'i>;

    #[inline]
    fn parse_nth_child_of(&self) -> bool {
        false
    }

    #[inline]
    fn parse_is_and_where(&self) -> bool {
        true
    }

    #[inline]
    fn parse_has(&self) -> bool {
        false
    }

    #[inline]
    fn parse_parent_selector(&self) -> bool {
        true
    }

    #[inline]
    fn parse_part(&self) -> bool {
        true
    }

    #[inline]
    fn allow_forgiving_selectors(&self) -> bool {
        !self.for_supports_rule
    }

    fn parse_non_ts_pseudo_class(
        &self,
        location: SourceLocation,
        name: CowRcStr<'i>,
    ) -> Result<NonTSPseudoClass, ParseError<'i>> {
        let pseudo_class = match_ignore_ascii_case! { &name,
            "active" => NonTSPseudoClass::Active,
            "any-link" => NonTSPseudoClass::AnyLink,
            "autofill" => NonTSPseudoClass::Autofill,
            "checked" => NonTSPseudoClass::Checked,
            "default" => NonTSPseudoClass::Default,
            "defined" => NonTSPseudoClass::Defined,
            "disabled" => NonTSPseudoClass::Disabled,
            "enabled" => NonTSPseudoClass::Enabled,
            "focus" => NonTSPseudoClass::Focus,
            "focus-visible" => NonTSPseudoClass::FocusVisible,
            "focus-within" => NonTSPseudoClass::FocusWithin,
            "fullscreen" => NonTSPseudoClass::Fullscreen,
            "hover" => NonTSPseudoClass::Hover,
            "indeterminate" => NonTSPseudoClass::Indeterminate,
            "invalid" => NonTSPseudoClass::Invalid,
            "link" => NonTSPseudoClass::Link,
            "optional" => NonTSPseudoClass::Optional,
            "out-of-range" => NonTSPseudoClass::OutOfRange,
            "placeholder-shown" => NonTSPseudoClass::PlaceholderShown,
            "popover-open" => NonTSPseudoClass::PopoverOpen,
            "read-only" => NonTSPseudoClass::ReadOnly,
            "read-write" => NonTSPseudoClass::ReadWrite,
            "required" => NonTSPseudoClass::Required,
            "target" => NonTSPseudoClass::Target,
            "user-invalid" => NonTSPseudoClass::UserInvalid,
            "user-valid" => NonTSPseudoClass::UserValid,
            "valid" => NonTSPseudoClass::Valid,
            "visited" => NonTSPseudoClass::Visited,
            "-moz-meter-optimum" => NonTSPseudoClass::MozMeterOptimum,
            "-moz-meter-sub-optimum" => NonTSPseudoClass::MozMeterSubOptimum,
            "-moz-meter-sub-sub-optimum" => NonTSPseudoClass::MozMeterSubSubOptimum,
            "-servo-nonzero-border" => {
                if !self.in_user_agent_stylesheet() {
                    return Err(location.new_custom_error(
                        SelectorParseErrorKind::UnexpectedIdent("-servo-nonzero-border".into())
                    ))
                }
                NonTSPseudoClass::ServoNonZeroBorder
            },
            _ => return Err(location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone()))),
        };

        Ok(pseudo_class)
    }

    fn parse_non_ts_functional_pseudo_class<'t>(
        &self,
        name: CowRcStr<'i>,
        parser: &mut CssParser<'i, 't>,
        after_part: bool,
    ) -> Result<NonTSPseudoClass, ParseError<'i>> {
        use self::NonTSPseudoClass::*;
        let pseudo_class = match_ignore_ascii_case! { &name,
            "lang" if !after_part => {
                Lang(parser.expect_ident_or_string()?.as_ref().into())
            },
            _ => return Err(parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone()))),
        };

        Ok(pseudo_class)
    }

    fn parse_pseudo_element(
        &self,
        location: SourceLocation,
        name: CowRcStr<'i>,
    ) -> Result<PseudoElement, ParseError<'i>> {
        use self::PseudoElement::*;
        let pseudo_element = match_ignore_ascii_case! { &name,
            "before" => Before,
            "after" => After,
            "backdrop" => Backdrop,
            "selection" => Selection,
            "marker" => Marker,
            "-servo-details-summary" => {
                if !self.in_user_agent_stylesheet() {
                    return Err(location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone())))
                }
                DetailsSummary
            },
            "-servo-details-content" => {
                if !self.in_user_agent_stylesheet() {
                    return Err(location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone())))
                }
                DetailsContent
            },
            "color-swatch" => ColorSwatch,
            "placeholder" => {
                if !self.in_user_agent_stylesheet() {
                    return Err(location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone())))
                }
                Placeholder
            },
            "-servo-text-control-inner-container" => {
                if !self.in_user_agent_stylesheet() {
                    return Err(location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone())))
                }
                ServoTextControlInnerContainer
            },
            "-servo-text-control-inner-editor" => {
                if !self.in_user_agent_stylesheet() {
                    return Err(location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone())))
                }
                ServoTextControlInnerEditor
            },
            "-servo-anonymous-box" => {
                if !self.in_user_agent_stylesheet() {
                    return Err(location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone())))
                }
                ServoAnonymousBox
            },
            "-servo-anonymous-table" => {
                if !self.in_user_agent_stylesheet() {
                    return Err(location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone())))
                }
                ServoAnonymousTable
            },
            "-servo-anonymous-table-row" => {
                if !self.in_user_agent_stylesheet() {
                    return Err(location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone())))
                }
                ServoAnonymousTableRow
            },
            "-servo-anonymous-table-cell" => {
                if !self.in_user_agent_stylesheet() {
                    return Err(location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone())))
                }
                ServoAnonymousTableCell
            },
            "-servo-table-grid" => {
                if !self.in_user_agent_stylesheet() {
                    return Err(location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone())))
                }
                ServoTableGrid
            },
            "-servo-table-wrapper" => {
                if !self.in_user_agent_stylesheet() {
                    return Err(location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone())))
                }
                ServoTableWrapper
            },
            _ => return Err(location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(name.clone())))

        };

        Ok(pseudo_element)
    }

    fn default_namespace(&self) -> Option<Namespace> {
        self.namespaces.default.as_ref().map(|ns| ns.clone())
    }

    fn namespace_for_prefix(&self, prefix: &Prefix) -> Option<Namespace> {
        self.namespaces.prefixes.get(prefix).cloned()
    }

    fn parse_host(&self) -> bool {
        true
    }

    fn parse_slotted(&self) -> bool {
        true
    }
}

impl SelectorImpl {
    /// A helper to traverse each eagerly cascaded pseudo-element, executing
    /// `fun` on it.
    #[inline]
    pub fn each_eagerly_cascaded_pseudo_element<F>(mut fun: F)
    where
        F: FnMut(PseudoElement),
    {
        for i in 0..EAGER_PSEUDO_COUNT {
            fun(PseudoElement::from_eager_index(i));
        }
    }
}

/// A map from elements to snapshots for the Servo style back-end.
#[derive(Debug)]
pub struct SnapshotMap(FxHashMap<OpaqueNode, ServoElementSnapshot>);

impl SnapshotMap {
    /// Create a new empty `SnapshotMap`.
    pub fn new() -> Self {
        SnapshotMap(FxHashMap::default())
    }

    /// Get a snapshot given an element.
    pub fn get<T: TElement>(&self, el: &T) -> Option<&ServoElementSnapshot> {
        self.0.get(&el.as_node().opaque())
    }
}

impl Deref for SnapshotMap {
    type Target = FxHashMap<OpaqueNode, ServoElementSnapshot>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for SnapshotMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Servo's version of an element snapshot.
#[derive(Debug, Default, MallocSizeOf)]
pub struct ServoElementSnapshot {
    /// The stored state of the element.
    pub state: Option<ElementState>,
    /// The set of stored attributes and its values.
    pub attrs: Option<Vec<(AttrIdentifier, AttrValue)>>,
    /// The set of changed attributes and its values.
    pub changed_attrs: Vec<LocalName>,
    /// Whether the class attribute changed or not.
    pub class_changed: bool,
    /// Whether the id attribute changed or not.
    pub id_changed: bool,
    /// Whether other attributes other than id or class changed or not.
    pub other_attributes_changed: bool,
}

impl ServoElementSnapshot {
    /// Create an empty element snapshot.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns whether the id attribute changed or not.
    pub fn id_changed(&self) -> bool {
        self.id_changed
    }

    /// Returns whether the class attribute changed or not.
    pub fn class_changed(&self) -> bool {
        self.class_changed
    }

    /// Returns whether other attributes other than id or class changed or not.
    pub fn other_attr_changed(&self) -> bool {
        self.other_attributes_changed
    }

    fn get_attr(&self, namespace: &Namespace, name: &LocalName) -> Option<&AttrValue> {
        self.attrs
            .as_ref()
            .unwrap()
            .iter()
            .find(|&&(ref ident, _)| ident.local_name == *name && ident.namespace == *namespace)
            .map(|&(_, ref v)| v)
    }

    /// Executes the callback once for each attribute that changed.
    #[inline]
    pub fn each_attr_changed<F>(&self, mut callback: F)
    where
        F: FnMut(&LocalName),
    {
        for name in &self.changed_attrs {
            callback(name)
        }
    }

    fn any_attr_ignore_ns<F>(&self, name: &LocalName, mut f: F) -> bool
    where
        F: FnMut(&AttrValue) -> bool,
    {
        self.attrs
            .as_ref()
            .unwrap()
            .iter()
            .any(|&(ref ident, ref v)| ident.local_name == *name && f(v))
    }
}

impl ElementSnapshot for ServoElementSnapshot {
    fn state(&self) -> Option<ElementState> {
        self.state.clone()
    }

    fn has_attrs(&self) -> bool {
        self.attrs.is_some()
    }

    fn id_attr(&self) -> Option<&Atom> {
        self.get_attr(&ns!(), &local_name!("id"))
            .map(|v| v.as_atom())
    }

    fn is_part(&self, part_name: &AtomIdent) -> bool {
        self.get_attr(&ns!(), &local_name!("part"))
            .is_some_and(|v| {
                v.as_tokens()
                    .iter()
                    .any(|atom| CaseSensitivity::CaseSensitive.eq_atom(atom, part_name))
            })
    }

    fn imported_part(&self, _: &AtomIdent) -> Option<AtomIdent> {
        None
    }

    fn has_class(&self, name: &AtomIdent, case_sensitivity: CaseSensitivity) -> bool {
        self.get_attr(&ns!(), &local_name!("class"))
            .map_or(false, |v| {
                v.as_tokens()
                    .iter()
                    .any(|atom| case_sensitivity.eq_atom(atom, name))
            })
    }

    fn each_class<F>(&self, mut callback: F)
    where
        F: FnMut(&AtomIdent),
    {
        if let Some(v) = self.get_attr(&ns!(), &local_name!("class")) {
            for class in v.as_tokens() {
                callback(AtomIdent::cast(class));
            }
        }
    }

    fn lang_attr(&self) -> Option<SelectorAttrValue> {
        self.get_attr(&ns!(xml), &local_name!("lang"))
            .or_else(|| self.get_attr(&ns!(), &local_name!("lang")))
            .map(|v| SelectorAttrValue::from(v as &str))
    }

    /// Returns true if the snapshot has stored state for custom states
    #[inline]
    fn has_custom_states(&self) -> bool {
        false
    }

    /// Returns true if the snapshot has a given CustomState
    #[inline]
    fn has_custom_state(&self, _state: &AtomIdent) -> bool {
        false
    }

    #[inline]
    fn each_custom_state<F>(&self, mut _callback: F)
    where
        F: FnMut(&AtomIdent),
    {
    }
}

impl ServoElementSnapshot {
    /// selectors::Element::attr_matches
    pub fn attr_matches(
        &self,
        ns: &NamespaceConstraint<&Namespace>,
        local_name: &LocalName,
        operation: &AttrSelectorOperation<&AtomString>,
    ) -> bool {
        match *ns {
            NamespaceConstraint::Specific(ref ns) => self
                .get_attr(ns, local_name)
                .map_or(false, |value| value.eval_selector(operation)),
            NamespaceConstraint::Any => {
                self.any_attr_ignore_ns(local_name, |value| value.eval_selector(operation))
            },
        }
    }
}

/// Returns whether the language is matched, as defined by
/// [RFC 4647](https://tools.ietf.org/html/rfc4647#section-3.3.2).
pub fn extended_filtering(tag: &str, range: &str) -> bool {
    range.split(',').any(|lang_range| {
        // step 1
        let mut range_subtags = lang_range.split('\x2d');
        let mut tag_subtags = tag.split('\x2d');

        // step 2
        // Note: [Level-4 spec](https://drafts.csswg.org/selectors/#lang-pseudo) check for wild card
        if let (Some(range_subtag), Some(tag_subtag)) = (range_subtags.next(), tag_subtags.next()) {
            if !(range_subtag.eq_ignore_ascii_case(tag_subtag) ||
                range_subtag.eq_ignore_ascii_case("*"))
            {
                return false;
            }
        }

        let mut current_tag_subtag = tag_subtags.next();

        // step 3
        for range_subtag in range_subtags {
            // step 3a
            if range_subtag == "*" {
                continue;
            }
            match current_tag_subtag.clone() {
                Some(tag_subtag) => {
                    // step 3c
                    if range_subtag.eq_ignore_ascii_case(tag_subtag) {
                        current_tag_subtag = tag_subtags.next();
                        continue;
                    }
                    // step 3d
                    if tag_subtag.len() == 1 {
                        return false;
                    }
                    // else step 3e - continue with loop
                    current_tag_subtag = tag_subtags.next();
                    if current_tag_subtag.is_none() {
                        return false;
                    }
                },
                // step 3b
                None => {
                    return false;
                },
            }
        }
        // step 4
        true
    })
}
