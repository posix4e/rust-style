use std::cmp;
use std::default::Default;
use toml::{self, Value};

// the following types are for debugging display, to end users
static STYLE_TOML_TYPES: &'static [(&'static str, &'static str)] = &[
    ("column_limit", "u32"),
    ("indent_width", "u32"),
    ("tab_width", "u32"),
    ("continuation_indent_width", "u32"),
    ("method_chain_indent_width", "u32"),
    ("use_tabs", "\"Never\"|\"Always\"|\"ForIndentation\""),
    ("max_empty_lines_to_keep", "u32"),
    ("penalty_excess_character", "u64"),
];

pub type Penalty = u64;

#[derive(Copy, Clone)]
pub enum UseTabs {
    Never,
    Always,
    ForIndentation,
}

#[derive(Clone)]
pub struct FormatStyle {
    pub column_limit: u32,
    pub indent_width: u32,
    pub tab_width: u32,
    pub continuation_indent_width: u32,
    pub method_chain_indent_width: u32,
    pub use_tabs: UseTabs,
    pub max_empty_lines_to_keep: u32,
    pub penalty_excess_character: Penalty,
}

impl Default for FormatStyle {
    fn default() -> FormatStyle {
        FormatStyle {
            column_limit: 99,
            indent_width: 4,
            tab_width: 4,
            continuation_indent_width: 4,
            method_chain_indent_width: 4,
            use_tabs: UseTabs::Never,
            max_empty_lines_to_keep: 1,
            penalty_excess_character: 1000000,
        }
    }
}

impl FormatStyle {
    pub fn from_toml_str(toml_text: &str) -> Result<FormatStyle, StyleParseError> {
        let mut parser = toml::Parser::new(toml_text);
        let parse_result = parser.parse();

        if parse_result == None {
            return Err(StyleParseError::ParseError(parser.errors));
        }

        let parse_result = parse_result.unwrap();

        let mut style = FormatStyle::default();

        for (key, value) in &parse_result {
            try!(FormatStyle::process_field(&mut style, key.as_ref(), value));
        }

        Ok(style)
    }


    fn process_field(style: &mut FormatStyle, key: &str, value: &Value) -> Result<(), StyleParseError> {
        match (key, value) {
            ("column_limit",              &Value::Integer(integer)) => style.column_limit              = integer as u32,
            ("indent_width",              &Value::Integer(integer)) => style.indent_width              = integer as u32,
            ("tab_width",                 &Value::Integer(integer)) => style.tab_width                 = integer as u32,
            ("continuation_indent_width", &Value::Integer(integer)) => style.continuation_indent_width = integer as u32,
            ("method_chain_indent_width", &Value::Integer(integer)) => style.method_chain_indent_width = integer as u32,
            ("max_empty_lines_to_keep",   &Value::Integer(integer)) => style.max_empty_lines_to_keep   = integer as u32,
            ("penalty_excess_character",  &Value::Integer(integer)) => style.penalty_excess_character  = integer as u64,
            ("use_tabs",                  &Value::String(ref tabs)) => {
                // TODO: replace when string.to_lowercase() is stable
                let mut lower_tabs = String::with_capacity(tabs.len());
                lower_tabs.extend(tabs[..].chars().flat_map(|c| c.to_lowercase()));

                style.use_tabs = match lower_tabs.as_ref() {
                    "never"          => UseTabs::Never,
                    "always"         => UseTabs::Always,
                    "forindentation" => UseTabs::ForIndentation,
                    _ => return Err(StyleParseError::InvalidValue(format!("{}", key), format!("{}", value),
                                    format!("{}", FormatStyle::get_field_type(key).unwrap()))),
                };
            },
            (key, value) => {
                match FormatStyle::get_field_type(key) {
                    None =>            return Err(StyleParseError::InvalidKey(format!("{}", key), format!("{}", value))),
                    Some(ref vtype) => return Err(StyleParseError::InvalidValueType(format!("{}", key),
                                            format!("{}", value), format!("{}", vtype))),
                }
            },
        }
        Ok(())
    }

    fn get_field_type(key: &str) -> Option<&'static str> {
        for &(_, vtype) in STYLE_TOML_TYPES.iter()
                                   .filter(|&p| p.0 == key)
                                   .next() {
            return Some(vtype);
        }
        None
    }
}

#[derive(Debug)]
pub enum StyleParseError {
    ParseError(Vec<toml::ParserError>),
    InvalidKey(String, String),
    InvalidValue(String, String, String),
    InvalidValueType(String, String, String),
}

#[derive(Clone, Copy)]
pub enum LineEnding {
    CRLF,
    LF,
}

impl LineEnding {
    pub fn derive_from_source(src: &str) -> LineEnding {
        let crlf_count = src.chars().filter(|&c| c == '\r').count() * 2;
        let lf_count = src.chars().filter(|&c| c == '\n').count();
        if crlf_count > lf_count { LineEnding::CRLF } else { LineEnding::LF }
    }
}

pub struct LineRanges {
    ranges: Vec<(u32, u32)>,
}

impl LineRanges {
    pub fn new_from_tuples(lines: &[(u32, u32)]) -> LineRanges {
        let ranges = lines.iter()
                        .map(|&(start, end)| (cmp::min(start, end), cmp::max(start, end)))
                        .collect();

        LineRanges { ranges: ranges }
    }

    pub fn in_ranges(&self, line: u32) -> bool {
        self.ranges.iter().any(|&(low, high)| line >= low && line <= high)
    }
}
