use heck::*;
use std::collections::HashMap;

pub(crate) fn array_concat<T, const A: usize, const B: usize, const C: usize>(
    a: [T; A],
    b: [T; B],
) -> [T; C]
where
    T: Default,
{
    assert_eq!(A + B, C);
    let mut ary: [T; C] = std::array::from_fn(|_| Default::default());
    for (idx, val) in a.into_iter().chain(b.into_iter()).enumerate() {
        ary[idx] = val;
    }
    ary
}

pub(crate) fn to_rust_ident(name: &str) -> String {
    let mut name = name.replace("-", "_").to_string();
    // Rust keywords
    if matches!(&*name, "static" | "super" | "box" | "move") {
        name.push('_');
    }
    return name;
}

// https://drafts.csswg.org/cssom/#css-property-to-idl-attribute
pub(crate) fn to_idl_name(ident: &str) -> String {
    ident.to_lower_camel_case()
}

/// Parses values of the for "key1=value1 key2=value2"
pub(crate) fn parse_aliases(values: &str) -> HashMap<String, String> {
    let mut aliases = HashMap::new();
    for pair in values.split_whitespace() {
        if let Some((alias, value)) = pair.split_once('=') {
            aliases.insert(alias.to_string(), value.to_string());
        }
    }
    aliases
}
