use std::default::Default;
use std::cmp;

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