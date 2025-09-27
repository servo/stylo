/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DataEnum, DeriveInput, Fields};

/// Derive implementation of the `ToTyped` trait.
///
/// This currently supports enums that are made up entirely of unit variants
/// (pure keyword enums). In that case we can optimize by serializing the
/// entire value with `ToCss` once, and wrapping the result in
/// `TypedValue::Keyword`.
///
/// For other types (mixed enums, structs, etc.), we don’t yet generate a
/// `to_typed` implementation, the default `None` from the trait is used.
pub fn derive(input: DeriveInput) -> TokenStream {
    // Split the input type’s generics into pieces we can use for impl.
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let name = &input.ident;

    // Track whether this is an enum with *only* unit variants.
    let mut all_unit = false;

    // Only enums are interesting for this optimization.
    if let Data::Enum(DataEnum { variants, .. }) = &input.data {
        // `all_unit = true` if every variant has no fields.
        all_unit = variants.iter().all(|v| matches!(v.fields, Fields::Unit));
    }

    // If it’s all unit variants, implement `to_typed` by serializing with
    // `ToCss` and wrapping as a Keyword.
    let body = if all_unit {
        quote! {
            fn to_typed(&self) -> Option<style_traits::TypedValue> {
                let s = style_traits::ToCss::to_css_cssstring(self);
                Some(style_traits::TypedValue::Keyword(s))
            }
        }
    } else {
        // Otherwise, don’t emit any `to_typed` method body. The default
        // implementation (returning `None`) will apply.
        quote! {}
    };

    // Put it all together into the impl block.
    quote! {
        impl #impl_generics style_traits::ToTyped for #name #ty_generics #where_clause {
            #body
        }
    }
}
