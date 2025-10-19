/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use crate::cg;
use crate::to_css::{CssFieldAttrs, CssInputAttrs, CssVariantAttrs};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DataEnum, DeriveInput, Fields, WhereClause};
use synstructure::{BindingInfo, Structure};

/// Derive implementation of the `ToTyped` trait.
///
/// This derive supports both enums and structs:
///
/// * Enums
///   * Pure keyword enums: Enums made up entirely of unit variants
///     (e.g. `enum Visibility { Visible, Hidden, Collapse }`). In this case,
///     the derive optimizes by delegating to `ToCss` once for the whole value,
///     then wrapping the result in `TypedValue::Keyword`.
///
///   * Mixed enums with unit variants: Enums that contain both unit
///     variants (keywords) and data-carrying variants. The derive generates a
///     `match` implementation where unit variants are reified as
///     `TypedValue::Keyword`, and fielded variants may be reified by calling
///     `.to_typed()` on their inner values when the `derive_fields` attribute
///     is enabled. Otherwise, those variants return `None`.
///
/// * Structs
///   * Structs are handled similarly to data-carrying variants in mixed enums.
///     When `derive_fields` is enabled and the type is not marked as a
///     bitflags type, `.to_typed()` may be generated for their inner value;
///     otherwise, they return `None`.
///
/// Unit variants are mapped to keywords using their Rust identifier converted
/// via `to_css_identifier`. Attributes like `#[css(keyword = "...")]` are not
/// yet supported in this path (see bug 1995187).
///
/// For other kinds of types (e.g. unions), no `to_typed` method is generated;
/// the default implementation applies, which always returns `None`.
///
/// This allows keywords to be reified automatically into `CSSKeywordValue`
/// objects, while leaving more complex value types to be implemented
/// incrementally as Typed OM support expands.
pub fn derive(mut input: DeriveInput) -> TokenStream {
    // The mutable `where_clause` is passed down to helper functions so they
    // can append trait bounds only when necessary. In particular, a bound of
    // the form `T: ToTyped` is added only if the generated code actually calls
    // `.to_typed()` on that inner value. This avoids forcing unrelated types
    // to implement `ToTyped` prematurely, keeping compilation requirements
    // minimal and isolated to cases where reification recursion is explicitly
    // performed.
    let mut where_clause = input.generics.where_clause.take();

    let css_input_attrs = cg::parse_input_attrs::<CssInputAttrs>(&input);

    let input_attrs = cg::parse_input_attrs::<TypedValueInputAttrs>(&input);

    let body = match &input.data {
        // Handle enums.
        Data::Enum(DataEnum { variants, .. }) => {
            // Check if this enum consists entirely of unit variants (no fields).
            let all_unit = variants.iter().all(|v| matches!(v.fields, Fields::Unit));

            if all_unit {
                // Optimization: for all-unit enums, reuse `ToCss` once and
                // wrap the result in a `TypedValue::Keyword`, instead of
                // generating a full match. This avoids code bloat while
                // producing the same runtime behavior.
                quote! {
                    fn to_typed(&self) -> Option<style_traits::TypedValue> {
                      let s = style_traits::ToCss::to_css_cssstring(self);
                      Some(style_traits::TypedValue::Keyword(s))
                    }
                }
            } else {
                // Mixed enums: generate a `match` where unit variants map to
                // `TypedValue::Keyword` and all other variants return `None`.
                // This is more verbose in code size, but allows selective
                // handling of individual variants.
                let s = Structure::new(&input);
                let match_body = s.each_variant(|variant| {
                    derive_variant_arm(variant, input_attrs.derive_fields, &mut where_clause)
                });

                quote! {
                    fn to_typed(&self) -> Option<style_traits::TypedValue> {
                        match *self {
                            #match_body
                        }
                    }
                }
            }
        },

        // Handle structs that are not bitflags.
        Data::Struct(_) => {
            if css_input_attrs.bitflags.is_none() {
                let s = Structure::new(&input);
                let match_body = s.each_variant(|variant| {
                    derive_variant_arm(variant, input_attrs.derive_fields, &mut where_clause)
                });

                quote! {
                    fn to_typed(&self) -> Option<style_traits::TypedValue> {
                        match *self {
                            #match_body
                        }
                    }
                }
            } else {
                quote! {}
            }
        },

        // Otherwise, don’t emit any `to_typed` method body. The default
        // implementation (returning `None`) will apply.
        _ => quote! {},
    };

    input.generics.where_clause = where_clause;

    let name = &input.ident;

    // Split the input type’s generics into pieces we can use for impl.
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // Put it all together into the impl block.
    quote! {
        impl #impl_generics style_traits::ToTyped for #name #ty_generics #where_clause {
            #body
        }
    }
}

/// Generate the match arm expression for a struct or enum variant in a derived
/// `ToTyped` implementation.
///
/// * Unit variants are reified into `TypedValue::Keyword`, using the variant’s
///   identifier converted with `cg::to_css_identifier`.
/// * Variants marked with `#[css(skip)]` or `#[typed_value(skip)]` or
///   `#[typed(todo)]` return `None`.
/// * Variants with fields delegate to `derive_variant_fields_expr()` when
///   `derive_fields` is enabled; otherwise they return `None`.
///
/// Note: `#[css(keyword = "...")]` overrides are not handled in this
/// `derive_variant_arm` path. This is fine for now because all existing cases
/// that use such overrides (e.g. `#[css(keyword = "preserve-3d")]`) occur in
/// pure keyword enums, which are covered by the all-unit `ToCss` path. Support
/// will need to be added here once mixed enums with keyword overrides start
/// implementing `ToTyped`.
fn derive_variant_arm(
    variant: &synstructure::VariantInfo,
    derive_fields: bool,
    where_clause: &mut Option<WhereClause>,
) -> TokenStream {
    let bindings = variant.bindings();
    // Get the underlying syn AST node for this variant.
    let ast = variant.ast();
    let identifier = &ast.ident;

    // Parse any #[css(...)] attributes attached to this variant.
    let css_variant_attrs = cg::parse_variant_attrs_from_ast::<CssVariantAttrs>(&ast);

    // Parse any #[typed_value(...)] attributes attached to this variant.
    let variant_attrs = cg::parse_variant_attrs_from_ast::<TypedValueVariantAttrs>(&ast);

    // If the variant is explicitly marked #[css(skip)], don’t generate
    // anything for it, always return None.
    if css_variant_attrs.skip {
        return quote!(None);
    }

    assert!(
        css_variant_attrs.keyword.is_none(),
        "Unhandled keyword attribute"
    );

    // If the variant is explicitly marked #[typed_value(skip)] or
    // #[typed_value(todo)], don’t generate anything for it, always return
    // None.
    if variant_attrs.skip || variant_attrs.todo {
        return quote!(None);
    }

    // If the variant has no bindings (i.e. no data fields), treat it as a unit
    // variant and reify it as a keyword.
    if bindings.is_empty() {
        // Convert the Rust variant name into its CSS identifier form
        // (e.g. AvoidColumn -> "avoid-column").
        let keyword = cg::to_css_identifier(&identifier.to_string());

        // Emit code to wrap this keyword into a TypedValue.
        quote! {
            Some(style_traits::TypedValue::Keyword(
                style_traits::CssString::from(#keyword)
            ))
        }
    } else if derive_fields {
        derive_variant_fields_expr(bindings, where_clause)
    } else {
        // This variant has one or more fields, but field reification is
        // disabled. Without `derive_fields`, this variant simply returns
        // `None`.
        quote! {
            None
        }
    }
}

/// Generate the match arm expression for fields of a struct or enum variant
/// in a derived `ToTyped` implementation.
///
/// This helper examines the variant’s fields and determines whether it can
/// safely generate a `.to_typed()` call for a single inner value. If the
/// variant has exactly one non-skipped, non-iterable field, it emits a call to
/// that field’s `ToTyped` implementation and adds the corresponding trait
/// bound (e.g. `T: ToTyped`) to the `where` clause.
///
/// Variants with multiple usable fields or iterable fields are not yet
/// supported and simply return `None`.
fn derive_variant_fields_expr(
    bindings: &[BindingInfo],
    where_clause: &mut Option<WhereClause>,
) -> TokenStream {
    // Filter out fields marked with #[css(skip)] so they are ignored during
    // reification.
    let mut iter = bindings
        .iter()
        .filter_map(|binding| {
            let css_field_attrs = cg::parse_field_attrs::<CssFieldAttrs>(&binding.ast());
            if css_field_attrs.skip {
                return None;
            }
            Some((binding, css_field_attrs))
        })
        .peekable();

    // If no usable fields remain, generate code that just returns None.
    let (first, css_field_attrs) = match iter.next() {
        Some(pair) => pair,
        None => return quote! { None },
    };

    // Handle the simple case of exactly one non-iterable field. Add a trait
    // bound `T: ToTyped` to ensure the field type implements the required
    // conversion, and emit a call to its `.to_typed()` method.
    if !css_field_attrs.iterable && iter.peek().is_none() {
        let ty = &first.ast().ty;
        cg::add_predicate(where_clause, parse_quote!(#ty: style_traits::ToTyped));

        return quote! { style_traits::ToTyped::to_typed(#first) };
    }

    // Complex cases (multiple fields, iterable fields, etc.) are not yet
    // supported for automatic reification.
    quote! {
        None
    }
}

#[derive(Default, FromDeriveInput)]
#[darling(attributes(typed_value), default)]
pub struct TypedValueInputAttrs {
    /// Enables field-level recursion when deriving `ToTyped`.
    ///
    /// When set, the derive will attempt to call `.to_typed()` on inner
    /// values (for example, struct fields or data-carrying enum variants)
    /// instead of always returning `None`.
    ///
    /// This is intentionally opt-in: blindly enabling recursion would require
    /// many types to implement `ToTyped` even when they don’t need to. Once
    /// reification coverage is complete, this attribute may be replaced by
    /// an opposite flag (see bug 1995184).
    pub derive_fields: bool,
}

#[derive(Default, FromVariant)]
#[darling(attributes(typed_value), default)]
pub struct TypedValueVariantAttrs {
    /// Same as the top-level `derive_fields`, but included here because
    /// struct variants are represented as both a variant and a type
    /// definition.
    pub derive_fields: bool,

    /// If present, this variant is excluded from generated reification code.
    /// `to_typed()` will always return `None` for it.
    pub skip: bool,

    /// Marks this variant as a placeholder for a future implementation.
    /// Behavior is the same as `skip`, but used to indicate that reification
    /// is intentionally left unimplemented for now.
    pub todo: bool,
}
