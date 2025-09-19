/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use proc_macro2::TokenStream;
use syn::DeriveInput;

pub fn derive(input: DeriveInput) -> TokenStream {
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let name = &input.ident;

    quote! {
        impl #impl_generics style_traits::ToTyped for #name #ty_generics #where_clause {}
    }
}
