use std::default::Default;

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
            continuation_indent_width: 8,
            use_tabs: UseTabs::Never,
            max_empty_lines_to_keep: 1,
            penalty_excess_character: 1000000,
        }
    }
}

pub struct LineRanges {
    ranges: Vec<(u32, u32)>,
}

impl LineRanges {
    pub fn new() -> LineRanges {
        LineRanges {
            ranges: Vec::new(),
        }
    }

    pub fn new_from_tuples(lines: &Vec<(u32, u32)>) -> LineRanges {
        let mut line_ranges = LineRanges::new();

        for &(line_start, line_end) in lines {
            line_ranges.add_range(line_start, line_end);
        }

        line_ranges
    }

    pub fn add_range(&mut self, line_1: u32, line_2: u32) {
        let range = if line_1 > line_2 {
            (line_2, line_1)
        } else {
            (line_1, line_2)
        };

        self.ranges.push(range);
    }

    pub fn in_ranges(&self, line: u32) -> bool {
        for &(low, high) in &self.ranges {
            if line >= low && line <= high {
                return true;
            }
        }
        return false;
    }
}
