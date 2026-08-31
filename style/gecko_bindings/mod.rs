/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Gecko's C++ bindings, along with some rust helpers to ease its use.

// FIXME: We allow `improper_ctypes` (for now), because the lint doesn't allow
// foreign structs to have `PhantomData`. We should remove this once the lint
// ignores this case.

#[allow(unknown_lints)]
#[allow(
    dead_code,
    improper_ctypes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    missing_docs,
    unnecessary_transmutes,
    unsafe_op_in_unsafe_fn,
    clippy::doc_overindented_list_items,
    clippy::missing_safety_doc,
    clippy::ptr_offset_with_cast,
    clippy::too_many_arguments,
    clippy::transmute_int_to_bool,
    clippy::useless_transmute
)]
pub mod structs {
    include!(concat!(env!("OUT_DIR"), "/gecko/structs.rs"));
}

pub use self::structs as bindings;

pub mod sugar;
