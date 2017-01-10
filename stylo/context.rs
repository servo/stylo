/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! The context within which style is calculated.
#![deny(missing_docs)]

use animation::Animation;
use app_units::Au;
use bloom::StyleBloom;
use data::ElementData;
use dom::{OpaqueNode, TNode, TElement};
use error_reporting::ParseErrorReporter;
use euclid::Size2D;
use matching::StyleSharingCandidateCache;
use parking_lot::RwLock;
use properties::ComputedValues;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::sync::mpsc::Sender;
use stylist::Stylist;
use timer::Timer;

/// This structure is used to create a local style context from a shared one.
pub struct ThreadLocalStyleContextCreationInfo {
    new_animations_sender: Sender<Animation>,
}

impl ThreadLocalStyleContextCreationInfo {
    /// Trivially constructs a `ThreadLocalStyleContextCreationInfo`.
    pub fn new(animations_sender: Sender<Animation>) -> Self {
        ThreadLocalStyleContextCreationInfo {
            new_animations_sender: animations_sender,
        }
    }
}

/// Which quirks mode is this document in.
///
/// See: https://quirks.spec.whatwg.org/
#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub enum QuirksMode {
    /// Quirks mode.
    Quirks,
    /// Limited quirks mode.
    LimitedQuirks,
    /// No quirks mode.
    NoQuirks,
}

/// A shared style context.
///
/// There's exactly one of these during a given restyle traversal, and it's
/// shared among the worker threads.
pub struct SharedStyleContext {
    /// The current viewport size.
    pub viewport_size: Size2D<Au>,

    /// Screen sized changed?
    pub screen_size_changed: bool,

    /// The CSS selector stylist.
    pub stylist: Arc<Stylist>,

    /// Why is this reflow occurring
    pub goal: ReflowGoal,

    /// The animations that are currently running.
    pub running_animations: Arc<RwLock<HashMap<OpaqueNode, Vec<Animation>>>>,

    /// The list of animations that have expired since the last style recalculation.
    pub expired_animations: Arc<RwLock<HashMap<OpaqueNode, Vec<Animation>>>>,

    ///The CSS error reporter for all CSS loaded in this layout thread
    pub error_reporter: Box<ParseErrorReporter + Sync>,

    /// Data needed to create the thread-local style context from the shared one.
    pub local_context_creation_data: Mutex<ThreadLocalStyleContextCreationInfo>,

    /// The current timer for transitions and animations. This is needed to test
    /// them.
    pub timer: Timer,

    /// The QuirksMode state which the document needs to be rendered with
    pub quirks_mode: QuirksMode,

    /// The default computed values to use for elements with no rules
    /// applying to them.
    pub default_computed_values: Arc<ComputedValues>,
}

/// Information about the current element being processed. We group this together
/// into a single struct within ThreadLocalStyleContext so that we can instantiate
/// and destroy it easily at the beginning and end of element processing.
struct CurrentElementInfo {
    /// The element being processed. Currently we use an OpaqueNode since we only
    /// use this for identity checks, but we could use SendElement if there were
    /// a good reason to.
    element: OpaqueNode,
    /// Whether the element is being styled for the first time.
    is_initial_style: bool,
}

/// A thread-local style context.
///
/// This context contains data that needs to be used during restyling, but is
/// not required to be unique among worker threads, so we create one per worker
/// thread in order to be able to mutate it without locking.
pub struct ThreadLocalStyleContext<E: TElement> {
    /// A cache to share style among siblings.
    pub style_sharing_candidate_cache: StyleSharingCandidateCache<E>,
    /// The bloom filter used to fast-reject selector-matching.
    pub bloom_filter: StyleBloom<E>,
    /// A channel on which new animations that have been triggered by style
    /// recalculation can be sent.
    pub new_animations_sender: Sender<Animation>,
    /// Information related to the current element, non-None during processing.
    current_element_info: Option<CurrentElementInfo>,
}

impl<E: TElement> ThreadLocalStyleContext<E> {
    /// Creates a new `ThreadLocalStyleContext` from a shared one.
    pub fn new(shared: &SharedStyleContext) -> Self {
        ThreadLocalStyleContext {
            style_sharing_candidate_cache: StyleSharingCandidateCache::new(),
            bloom_filter: StyleBloom::new(),
            new_animations_sender: shared.local_context_creation_data.lock().unwrap().new_animations_sender.clone(),
            current_element_info: None,
        }
    }

    /// Notes when the style system starts traversing an element.
    pub fn begin_element(&mut self, element: E, data: &ElementData) {
        debug_assert!(self.current_element_info.is_none());
        self.current_element_info = Some(CurrentElementInfo {
            element: element.as_node().opaque(),
            is_initial_style: !data.has_styles(),
        });
    }

    /// Notes when the style system finishes traversing an element.
    pub fn end_element(&mut self, element: E) {
        debug_assert!(self.current_element_info.is_some());
        debug_assert!(self.current_element_info.as_ref().unwrap().element ==
                      element.as_node().opaque());
        self.current_element_info = None;
    }

    /// Returns true if the current element being traversed is being styled for
    /// the first time.
    ///
    /// Panics if called while no element is being traversed.
    pub fn is_initial_style(&self) -> bool {
        self.current_element_info.as_ref().unwrap().is_initial_style
    }
}

#[cfg(debug_assertions)]
impl<E: TElement> Drop for ThreadLocalStyleContext<E> {
    fn drop(&mut self) {
        debug_assert!(self.current_element_info.is_none());
    }
}

/// A `StyleContext` is just a simple container for a immutable reference to a
/// shared style context, and a mutable reference to a local one.
pub struct StyleContext<'a, E: TElement + 'a> {
    /// The shared style context reference.
    pub shared: &'a SharedStyleContext,
    /// The thread-local style context (mutable) reference.
    pub thread_local: &'a mut ThreadLocalStyleContext<E>,
}

/// Why we're doing reflow.
#[derive(PartialEq, Copy, Clone, Debug)]
pub enum ReflowGoal {
    /// We're reflowing in order to send a display list to the screen.
    ForDisplay,
    /// We're reflowing in order to satisfy a script query. No display list will be created.
    ForScriptQuery,
}
