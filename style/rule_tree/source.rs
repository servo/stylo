/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

#![deny(unsafe_code)]

use crate::derives::*;
use crate::properties::PropertyDeclarationBlock;
use crate::shared_lock::{Locked, SharedRwLockReadGuard};
use servo_arc::{Arc, ArcBorrow};
use std::io::Write;
use std::ptr;

/// A style source for the rule node. It is a declaration block that may come from either a style
/// rule or a standalone block like animations / transitions / smil / preshints / style attr...
///
/// Keeping the style rule around would provide more debugability, but also causes more
/// pointer-chasing in the common code-path, which is undesired. If needed, we could keep it around
/// in debug builds or something along those lines.
#[derive(Clone, Debug)]
pub struct StyleSource(Arc<Locked<PropertyDeclarationBlock>>);

/// A borrowed version of `StyleSource`.
///
/// This is what we use during selector matching, so that we only need to touch the refcount of
/// the declarations when we actually insert them in the rule tree.
#[derive(Clone, Copy, Debug, MallocSizeOf)]
pub struct StyleSourceBorrow<'a>(ArcBorrow<'a, Locked<PropertyDeclarationBlock>>);

// The declaration block is measured as part of the stylesheet (or as part of whatever object owns
// it, like the element for the style attribute).
malloc_size_of::malloc_size_of_is_0!(StyleSource);

impl PartialEq for StyleSource {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl<'a> PartialEq for StyleSourceBorrow<'a> {
    fn eq(&self, other: &Self) -> bool {
        ArcBorrow::ptr_eq(&self.0, &other.0)
    }
}

impl<'a> StyleSourceBorrow<'a> {
    /// Creates a borrowed style source from a borrowed declaration block.
    #[inline]
    pub fn from_declarations(decls: ArcBorrow<'a, Locked<PropertyDeclarationBlock>>) -> Self {
        Self(decls)
    }

    /// Creates an owned style source from this borrow, bumping the refcount.
    #[inline]
    pub fn to_owned(&self) -> StyleSource {
        StyleSource(self.0.clone_arc())
    }

    #[inline]
    pub(super) fn key(&self) -> ptr::NonNull<()> {
        ptr::NonNull::from(self.0.get()).cast()
    }

    /// Read the style source guard, and obtain thus read access to the underlying property
    /// declaration block.
    #[inline]
    pub fn read<'b>(&self, guard: &'b SharedRwLockReadGuard) -> &'b PropertyDeclarationBlock
    where
        'a: 'b,
    {
        self.0.get().read_with(guard)
    }
}

impl StyleSource {
    #[inline]
    pub(super) fn key(&self) -> ptr::NonNull<()> {
        ptr::NonNull::from(&*self.0).cast()
    }

    /// Borrows this style source.
    #[inline]
    pub fn borrow(&self) -> StyleSourceBorrow<'_> {
        StyleSourceBorrow(self.0.borrow_arc())
    }

    /// Creates a StyleSource from a PropertyDeclarationBlock.
    #[inline]
    pub fn from_declarations(decls: Arc<Locked<PropertyDeclarationBlock>>) -> Self {
        Self(decls)
    }

    pub(super) fn dump<W: Write>(&self, guard: &SharedRwLockReadGuard, writer: &mut W) {
        let _ = write!(writer, "  -> {:?}", self.read(guard).declarations());
    }

    /// Read the style source guard, and obtain thus read access to the
    /// underlying property declaration block.
    #[inline]
    pub fn read<'a>(&'a self, guard: &'a SharedRwLockReadGuard) -> &'a PropertyDeclarationBlock {
        self.0.read_with(guard)
    }

    /// Returns the declaration block if applicable, otherwise None.
    #[inline]
    pub fn get(&self) -> &Arc<Locked<PropertyDeclarationBlock>> {
        &self.0
    }

    /// Marks this block as part of the rule tree.
    #[inline]
    pub fn mark_in_rule_tree(&self) {
        use std::sync::atomic::Ordering;
        if self.0.is_static() {
            // For static pointers, it doesn't matter whether we might be in the rule tree or not,
            // because those are not mutable.
            return;
        }
        // SAFETY: We're only accessing a relaxed atomic inside the locked object, which we know
        // is alive. In theory, we could/should track that boolean outside of the
        // Locked<PropertyDeclarationBlock>, but that is kind of a PITA.
        #[allow(unsafe_code)]
        unsafe {
            // Load before storing: the flag is never cleared, so after the first mark every
            // store is redundant, and an unconditional store would still take the cache line
            // exclusive and invalidate it for every other thread reading this block.
            let immutable = &self.0.read_unchecked().immutable;
            if !immutable.load(Ordering::Relaxed) {
                immutable.store(true, Ordering::Relaxed);
            }
        }
    }
}
