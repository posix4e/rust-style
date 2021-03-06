use rustc_serialize::{Encodable, Encoder};
use std::default::Default;
use std::ops::Range;
use toml::{self, Value, Table};

pub type Penalty = u64;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum UseTabs {
    Never,
    Always,
    ForIndentation,
}

impl Encodable for UseTabs {
    fn encode<S: Encoder>(&self, s: &mut S) -> Result<(), S::Error> {
        let value = match *self {
            UseTabs::Never => "Never",
            UseTabs::Always => "Always",
            UseTabs::ForIndentation => "ForIndentation",
        };
        s.emit_str(value)
    }
}

#[derive(Clone, Eq, PartialEq, Debug, RustcEncodable)]
pub struct FormatStyle {
    pub bin_pack_arguments: bool,
    pub bin_pack_parameters: bool,
    pub bin_pack_patterns: bool,
    pub column_limit: u32,
    pub indent_width: u32,
    pub continuation_indent_width: u32,
    pub tab_width: u32,
    pub use_tabs: UseTabs,
    pub max_empty_lines_to_keep: u32,
    pub penalty_excess_character: Penalty,
}

impl Default for FormatStyle {
    fn default() -> FormatStyle {
        FormatStyle {
            bin_pack_arguments: true,
            bin_pack_parameters: false,
            bin_pack_patterns: false,
            column_limit: 99,
            indent_width: 4,
            continuation_indent_width: 4,
            tab_width: 4,
            use_tabs: UseTabs::Never,
            max_empty_lines_to_keep: 1,
            penalty_excess_character: 1000000,
        }
    }
}

macro_rules! process_fields {
    ($default:ident, $result:ident, {$($field: ident),+}) => (
        FormatStyle {
            $(
                $field: try!(process_field($default.$field, stringify!($field), &mut $result)),
            )+
        }
    )
}

impl FormatStyle {
    pub fn from_toml_str(toml_text: &str) -> Result<FormatStyle, StyleParseError> {
        let default_style = FormatStyle::default();
        let mut parser = toml::Parser::new(toml_text);
        let mut parser_result = match parser.parse() {
            None => return Err(StyleParseError::ParseError(parser.errors)),
            Some(result) => result,
        };

        let style = process_fields!(default_style, parser_result, {
            bin_pack_arguments,
            bin_pack_parameters,
            bin_pack_patterns,
            column_limit,
            indent_width,
            continuation_indent_width,
            tab_width,
            use_tabs,
            max_empty_lines_to_keep,
            penalty_excess_character
        });

        // Any values which were not removed have an invalid key
        for (key, value) in &parser_result {
           return Err(StyleParseError::InvalidKey(key.to_string(), value.to_string()));
        }

        Ok(style)
    }

    pub fn to_toml_str(&self) -> String {
        toml::encode_str(self)
    }
}

fn process_field<T: FromValue>(default: T, key: &str, result: &mut Table) -> Result<T, StyleParseError> {
    match result.remove(key) {
        None => Ok(default),
        Some(value) => FromValue::from_value(key, &value),
    }
}

trait FromValue {
    fn from_value(key: &str, value: &Value) -> Result<Self, StyleParseError>;
}

impl FromValue for u32 {
    fn from_value(key: &str, value: &Value) -> Result<u32, StyleParseError> {
        match value.as_integer() {
            Some(value) => Ok(value as u32),
            None => Err(StyleParseError::InvalidValueType(key.to_string(), value.to_string(),
                                                          "u32".to_string()))
        }
    }
}

impl FromValue for u64 {
    fn from_value(key: &str, value: &Value) -> Result<u64, StyleParseError> {
        match value.as_integer() {
            Some(value) => Ok(value as u64),
            None => Err(StyleParseError::InvalidValueType(key.to_string(), value.to_string(),
                                                          "u64".to_string()))
        }
    }
}

impl FromValue for bool {
    fn from_value(key: &str, value: &Value) -> Result<bool, StyleParseError> {
        match value.as_bool() {
            Some(value) => Ok(value),
            None => Err(StyleParseError::InvalidValueType(key.to_string(), value.to_string(),
                                                          "bool".to_string()))
        }
    }
}

impl FromValue for UseTabs {
    fn from_value(key: &str, value: &Value) -> Result<UseTabs, StyleParseError> {
        match value.as_str() {
            Some("Never") => Ok(UseTabs::Never),
            Some("Always") => Ok(UseTabs::Always),
            Some("ForIndentation") => Ok(UseTabs::ForIndentation),
            Some(..) =>
                Err(StyleParseError::InvalidValue(key.to_string(), value.to_string(),
                                                  "Never|Always|ForIndentation".to_string())),
            None => Err(StyleParseError::InvalidValueType(key.to_string(), value.to_string(),
                                                          "u64".to_string())),
        }
    }
}

#[derive(Debug)]
pub enum StyleParseError {
    ParseError(Vec<toml::ParserError>),
    InvalidKey(/*key:*/ String, /*value :*/ String),
    InvalidValue(/*key:*/ String, /*value :*/ String, /*expected_value :*/ String),
    InvalidValueType(/*key:*/ String, /*value :*/ String, /*expected_type :*/ String),
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

pub struct LineRanges<'a> {
    ranges: &'a [Range<u32>],
}

impl<'a> LineRanges<'a> {
    pub fn new(ranges: &[Range<u32>]) -> LineRanges {
        LineRanges { ranges: ranges }
    }

    pub fn in_ranges(&self, line: u32) -> bool {
        // convert line to from zero based to one based
        let line = line + 1;
        self.ranges.iter().any(|r| line >= r.start && line < r.end)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_style_empty_string() {
        assert_eq!(FormatStyle::default(), FormatStyle::from_toml_str("").unwrap());
    }

    #[test]
    fn test_parse_style_semi_default() {
        let expected = FormatStyle {
            use_tabs: UseTabs::Always,
            tab_width: 64,
            indent_width: 16,
            bin_pack_parameters: false,
            bin_pack_arguments: true,
            ..FormatStyle::default()
        };

        let result = FormatStyle::from_toml_str("\
indent_width = 16
tab_width = 64
use_tabs = \"Always\"
bin_pack_parameters = false
bin_pack_arguments = true
");

        assert_eq!(expected, result.unwrap());
    }

    #[test]
    fn test_toml_parse_error() {
        let toml = "indent_width: 16";
        match FormatStyle::from_toml_str(toml) {
            Err(StyleParseError::ParseError(..)) => {},
            val => panic!(format!("unexpected value: {:?}", val)),
        }
    }

    #[test]
    fn test_toml_invalid_key_error() {
        let toml = "tab_widthh = 64";
        match FormatStyle::from_toml_str(toml) {
            Err(StyleParseError::InvalidKey(..)) => {},
            val => panic!(format!("unexpected value: {:?}", val)),
        }
    }

    #[test]
    fn test_toml_invalid_value() {
        let toml = "use_tabs = \"blah\"";
        match FormatStyle::from_toml_str(toml) {
            Err(StyleParseError::InvalidValue(..)) => {},
            val => panic!(format!("unexpected value: {:?}", val)),
        }
    }

    #[test]
    fn test_toml_use_tab_case_sensitive() {
        let toml = "use_tabs = \"always\"";
        match FormatStyle::from_toml_str(toml) {
            Err(StyleParseError::InvalidValue(..)) => {},
            val => panic!(format!("unexpected value: {:?}", val)),
        }
    }

    #[test]
    fn test_toml_invalid_value_type_error() {
        let toml = "tab_width = \"64\"";
        match FormatStyle::from_toml_str(toml) {
            Err(StyleParseError::InvalidValueType(..)) => {},
            val => panic!(format!("unexpected value: {:?}", val)),
        }
    }

    #[test]
    fn test_toml_serialise_round_trip() {
        let style = FormatStyle { column_limit: 555, ..FormatStyle::default() };
        let style_toml_str = style.to_toml_str();
        let style_again = FormatStyle::from_toml_str(&style_toml_str).unwrap();

        assert_eq!(style, style_again);
    }
}
