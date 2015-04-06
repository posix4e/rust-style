use std::default::Default;

#[derive(Copy, Clone)]
pub enum UseTabs {
    Never,
    Always,
    ForIndentation,
}

#[derive(Copy, Clone)]
pub struct FormatStyle {
    pub column_limit: u32,
    pub indent_width: u32,
    pub tab_width: u32,
    pub use_tabs: UseTabs,
    pub max_empty_lines_to_keep: u32,
    pub keep_empty_lines_at_the_start_of_blocks: bool,
}

impl Default for FormatStyle {
    fn default() -> FormatStyle {
        FormatStyle {
            column_limit: 80,
            indent_width: 4,
            tab_width: 4,
            use_tabs: UseTabs::Never,
            max_empty_lines_to_keep: 1,
            keep_empty_lines_at_the_start_of_blocks: false,
        }
    }
}
