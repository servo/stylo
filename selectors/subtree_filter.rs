/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Hashes for the DOM-maintained filter of local names, classes and
//! null-namespace attribute names in an element's DOM subtree, including itself.
//! Shadow-tree contents are not included. This is separate from the ancestor
//! bloom and the wider, lazily constructed `RelativeSelectorFilterMap`.
//!
//! Required hashes can be OR'd together: `(filter & hashes) == hashes` means
//! that a match remains possible. Zero never rejects. Removed names may leave
//! stale bits, so a hit cannot prove that a selector matches.

// Bit 0 is the DOM's inline-filter tag. Match AttrArray::HashForBloomFilter.
#[cfg(target_pointer_width = "32")]
const BLOOM_BITS: u32 = 31;

#[cfg(target_pointer_width = "64")]
const BLOOM_BITS: u32 = 63;

/// Turn a 32-bit atom hash into the bits it occupies in the subtree filter
/// (k = 2 hash functions).
///
/// This must match `AttrArray::HashForBloomFilter` exactly, or lookups will
/// reject subtrees that do contain the name we're looking for.
#[inline]
pub fn hash_for_subtree_filter(hash: u32) -> u64 {
    let mut filter = 1u64;
    filter |= 1u64 << (1 + (hash % BLOOM_BITS));
    filter |= 1u64 << (1 + ((hash >> 6) % BLOOM_BITS));
    filter
}
