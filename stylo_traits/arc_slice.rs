/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! A thin atomically-reference-counted slice.

use servo_arc::ThinArc;
use std::{iter, mem};
use std::ops::Deref;
use std::ptr::NonNull;

/// A canary that we stash in ArcSlices.
///
/// Given we cannot use a zero-sized-type for the header, since well, C++
/// doesn't have zsts, and we want to use cbindgen for this type, we may as well
/// assert some sanity at runtime.
const ARC_SLICE_CANARY: u32 = 0xf3f3f3f3;

/// A wrapper type for a refcounted slice using ThinArc.
///
/// cbindgen:derive-eq=false
/// cbindgen:derive-neq=false
#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq, ToShmem)]
pub struct ArcSlice<T>(#[shmem(field_bound)] ThinArc<u32, T>);

impl<T> Deref for ArcSlice<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        debug_assert_eq!(self.0.header.header, ARC_SLICE_CANARY);
        &self.0.slice
    }
}

lazy_static! {
    // ThinArc doesn't support alignments greater than align_of::<u64>.
    static ref EMPTY_ARC_SLICE: ArcSlice<u64> = {
        ArcSlice(ThinArc::from_header_and_iter(ARC_SLICE_CANARY, iter::empty()))
    };
}

impl<T> Default for ArcSlice<T> {
    #[allow(unsafe_code)]
    fn default() -> Self {
        debug_assert!(
            mem::align_of::<T>() <= mem::align_of::<u64>(),
            "Need to increase the alignment of EMPTY_ARC_SLICE"
        );
        unsafe {
            let empty: ArcSlice<_> = EMPTY_ARC_SLICE.clone();
            mem::transmute(empty)
        }
    }
}

impl<T> ArcSlice<T> {
    /// Creates an Arc for a slice using the given iterator to generate the
    /// slice.
    #[inline]
    pub fn from_iter<I>(items: I) -> Self
    where
        I: Iterator<Item = T> + ExactSizeIterator,
    {
        if items.len() == 0 {
            return Self::default();
        }
        ArcSlice(ThinArc::from_header_and_iter(ARC_SLICE_CANARY, items))
    }

    /// Creates a value that can be passed via FFI, and forgets this value
    /// altogether.
    #[inline]
    #[allow(unsafe_code)]
    pub fn forget(self) -> ForgottenArcSlicePtr<T> {
        let ret = unsafe {
            ForgottenArcSlicePtr(NonNull::new_unchecked(self.0.ptr() as *const _ as *mut _))
        };
        mem::forget(self);
        ret
    }
}

/// The inner pointer of an ArcSlice<T>, to be sent via FFI.
/// The type of the pointer is a bit of a lie, we just want to preserve the type
/// but these pointers cannot be constructed outside of this crate, so we're
/// good.
#[repr(C)]
pub struct ForgottenArcSlicePtr<T>(NonNull<T>);
