use std::collections::HashMap;

pub struct Keyword {
    name: String,
    values: Vec<String>, // TODO: Value type
    extra_gecko_values: Vec<String>,
    extra_servo_values: Vec<String>,

    gecko_aliases: Option<HashMap<String, String>>,
    servo_aliases: Option<HashMap<String, String>>,

    gecko_consts_map: Option<String>, // custom_consts
    gecko_enum_prefix: Option<String>,
    gecko_constant_prefix: Option<String>,
    gecko_strip_moz_prefix: Option<String>,
    gecko_inexhaustive: bool,
}
