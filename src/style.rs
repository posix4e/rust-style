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
