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
}
