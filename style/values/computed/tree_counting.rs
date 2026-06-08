/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Computed types for CSS tree-counting functions.
//! https://drafts.csswg.org/css-values-5/#tree-counting

use crate::dom::TElement;
use crate::values::computed::Context;

/// Holds the resolved sibling-index() and sibling-count() values for an element.
#[derive(Clone, Copy, Debug)]
pub struct TreeCountingResult {
    /// The total number of siblings of the element, including itself.
    pub sibling_count: u32,
    /// The 1-based index of the element among its siblings.
    pub sibling_index: u32,
}

/// Information needed to evaluate sibling-index() and sibling-count() for an element.
pub enum TreeCountingInfo<'a> {
    /// The values have not yet been computed; holds a closure to do so.
    NotEvaluated(Box<dyn Fn() -> TreeCountingResult + 'a>),
    /// The values have already been computed and cached.
    Evaluated(TreeCountingResult),
}

impl<'a> TreeCountingInfo<'a> {
    /// Creates a not-yet-evaluated instance that will query `element` on first resolution,
    /// or returns `None` if the tree-counting-functions pref is disabled.
    pub fn for_element<E>(element: E) -> Option<Self>
    where
        E: TElement + 'a,
    {
        if !static_prefs::pref!("layout.css.tree-counting-functions.enabled") {
            return None;
        }

        // TODO(Bug 2045140) - This closure is being allocated for every element, even if
        // the element does not use sibling-count() or sibling-index(), which is wasteful.
        Some(Self::NotEvaluated(Box::new(move || {
            let (sibling_index, sibling_count) = element.tree_counting_info();
            TreeCountingResult {
                sibling_count,
                sibling_index,
            }
        })))
    }

    /// Resolves and caches the result, returning it.
    pub fn resolve(&mut self, context: &Context) -> TreeCountingResult {
        // https://drafts.csswg.org/css-values-5/#tree-counting
        // > A tree-counting function is a type of loosely-matched tree-scoped reference.
        //
        // As a loosely-matched reference, the tree-counting function can only match if
        // the declaration is in the same or descendant shadow tree of the element. It
        // does not match if the declaration is in a containing tree, so it must return 0.
        if context
            .current_scope()
            .shadow_order()
            .is_in_same_or_containing_tree()
        {
            return TreeCountingResult {
                sibling_count: 0,
                sibling_index: 0,
            };
        }

        match self {
            TreeCountingInfo::NotEvaluated(lookup) => {
                let info = lookup();
                *self = TreeCountingInfo::Evaluated(info);
                info
            },
            TreeCountingInfo::Evaluated(info) => *info,
        }
    }
}
