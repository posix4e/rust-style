use std::cmp;
use std::default::Default;
use toml::{self, Value, Table};

pub type Penalty = u64;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum UseTabs {
    Never,
    Always,
    ForIndentation,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct FormatStyle {
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

impl FromValue for UseTabs {
    fn from_value(key: &str, value: &Value) -> Result<UseTabs, StyleParseError> {
        match value.as_str() {
            None => Err(StyleParseError::InvalidValueType(key.to_string(), value.to_string(),
                                                          "u64".to_string())),
            Some(value) => {
                // TODO: remove unnecessary allocation. Use case insensitive string comparison?
                let mut lower = String::with_capacity(value.len());
                lower.extend(value[..].chars().flat_map(|c| c.to_lowercase()));
                match &lower[..] {
                    "never" => Ok(UseTabs::Never),
                    "always" => Ok(UseTabs::Always),
                    "forindentation" => Ok(UseTabs::ForIndentation),
                    _ => Err(StyleParseError::InvalidValue(key.to_string(), value.to_string(),
                                                           "Never|Always|ForIndentation"
                                                               .to_string()))
                }
            }
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
            ..FormatStyle::default()
        };

        let result = FormatStyle::from_toml_str("\
indent_width = 16
tab_width = 64
use_tabs = \"Always\"
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
    fn test_toml_invalid_value_type_error() {
        let toml = "tab_width = \"64\"";
        match FormatStyle::from_toml_str(toml) {
            Err(StyleParseError::InvalidValueType(..)) => {},
            val => panic!(format!("unexpected value: {:?}", val)),
        }
    }
}
