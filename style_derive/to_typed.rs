/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use crate::cg;
use crate::to_css::CssVariantAttrs;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DataEnum, DeriveInput, Fields};
use synstructure::Structure;

/// Derive implementation of the `ToTyped` trait.
///
/// This derive currently supports two cases:
///
/// * Pure keyword enums: Enums made up entirely of unit variants (e.g.
///   `enum Visibility { Visible, Hidden, Collapse }`). In this case, the
///   derive optimizes by delegating to `ToCss` once for the whole value, then
///   wrapping the result in `TypedValue::Keyword`.
///
/// * Mixed enums with unit variants: Enums that contain some unit variants
///   (keywords) and some non-unit variants (values with fields). The derive
///   generates a `match` implementation where each unit variant is reified as
///   a `TypedValue::Keyword`, and all other variants currently return `None`.
///
/// At present, unit variants are mapped to keywords using their Rust
/// identifier converted via `to_css_identifier`. Attributes like
/// `#[css(keyword = "...")]` are not yet supported in this path.
///
/// For other kinds of types (structs, non-enums, or enums where no variant is
/// is a unit), no `to_typed` method is generated; the default implementation
/// is used, which always returns `None`.
///
/// This allows keyword enums to be reified automatically into
///`CSSKeywordValue` objects, while leaving more complex value types to be
/// implemented incrementally as Typed OM support expands.
pub fn derive(input: DeriveInput) -> TokenStream {
    // Split the input type’s generics into pieces we can use for impl.
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let name = &input.ident;

    // Only enums are currently supported.
    let body = if let Data::Enum(DataEnum { variants, .. }) = &input.data {
        // Check if this enum consists entirely of unit variants (no fields).
        let all_unit = variants.iter().all(|v| matches!(v.fields, Fields::Unit));

        if all_unit {
            // Optimization: for all-unit enums, reuse `ToCss` once and wrap
            // the result in a `TypedValue::Keyword`, instead of generating
            // a full match. This avoids code bloat while producing the same
            // runtime behavior.
            quote! {
                fn to_typed(&self) -> Option<style_traits::TypedValue> {
                  let s = style_traits::ToCss::to_css_cssstring(self);
                  Some(style_traits::TypedValue::Keyword(s))
                }
            }
        } else {
            // Mixed enums: generate a `match` where unit variants map to
            // `TypedValue::Keyword` and all other variants return `None`.
            // This is more verbose in code size, but allows selective handling
            // of individual variants.
            let s = Structure::new(&input);
            let match_body = s.each_variant(|variant| derive_variant_arm(variant));

            quote! {
                fn to_typed(&self) -> Option<style_traits::TypedValue> {
                    match *self {
                        #match_body
                    }
                }
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

/// Generate the `to_typed` body for a single enum variant.
///
/// * Unit variants are reified into `TypedValue::Keyword`, using the variant’s
///   identifier converted with `cg::to_css_identifier`.
/// * Variants marked with `#[css(skip)]` return `None`.
/// * Variants with fields (non-unit) are not yet supported and return `None`.
///
/// Note: `#[css(keyword = "...")]` overrides are not handled in this
/// `derive_variant_arm` path. This is fine for now because all existing cases
/// that use such overrides (e.g. `#[css(keyword = "preserve-3d")]`) occur in
/// pure keyword enums, which are covered by the all-unit `ToCss` path. Support
/// will need to be added here once mixed enums with keyword overrides start
/// implementing `ToTyped`.
fn derive_variant_arm(variant: &synstructure::VariantInfo) -> TokenStream {
    // Get the underlying syn AST node for this variant.
    let ast = variant.ast();
    let identifier = &ast.ident;

    // Parse any #[css(...)] attributes attached to this variant.
    let variant_attrs = cg::parse_variant_attrs_from_ast::<CssVariantAttrs>(&ast);

    // If the variant is explicitly marked #[css(skip)], don’t generate
    // anything for it, always return None.
    if variant_attrs.skip {
        return quote!(None);
    }

    assert!(
        variant_attrs.keyword.is_none(),
        "Unhandled keyword attribute"
    );

    // If the variant has no fields (a unit variant), treat it as a keyword.
    if ast.fields.is_empty() {
        // Convert the Rust variant name into its CSS identifier form
        // (e.g. AvoidColumn -> "avoid-column").
        let keyword = cg::to_css_identifier(&identifier.to_string());

        // Emit code to wrap this keyword into a TypedValue.
        quote! {
            Some(style_traits::TypedValue::Keyword(
                style_traits::CssString::from(#keyword)
            ))
        }
    } else {
        // Variants with fields are not yet supported by ToTyped derive.
        // For now, generate code that always returns None.
        quote! {
            None
        }
    }
}
