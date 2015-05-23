extern crate typed_arena;
extern crate rustc_serialize;
extern crate syntex_syntax as syntax;
extern crate toml;

mod affected_lines;
mod annotate;
#[cfg(test)]
mod annotate_test;
mod continuation_indenter;
mod format;
mod format_options;
mod replacement;
mod source;
mod token;
mod unwrapped_line;
mod whitespace_manager;
pub mod unstable;


pub use format_options::{FormatStyle, StyleParseError, Penalty, UseTabs};
pub use internal::reformat;
pub use replacement::Replacement;

mod internal {
    use format_options::{FormatStyle, LineRanges, LineEnding};
    use replacement::Replacement;
    use source::Source;
    use std::ops::Range;
    use unwrapped_line::UnwrappedLine;
    use {affected_lines, annotate, format};

    pub fn annotated_lines(source: &Source, style: &FormatStyle) -> Vec<UnwrappedLine> {
        let tokens = source.format_token_lexer(style).collect();
        let mut lines = UnwrappedLine::parse_lines(source, tokens);
        annotate::annotate_lines(&mut lines[..], style);
        lines
    }

    /// Format a string, specifying style and the line ranges to be formatted.
    /// Ranges are one based. None means the whole string should be formatted.
    pub fn reformat(source: &str, style: &FormatStyle, line_ranges: Option<&[Range<u32>]>) -> Vec<Replacement> {
        if source.chars().all(char::is_whitespace) {
            return vec![];
        }

        let line_ending = LineEnding::derive_from_source(source);
        let source = &Source::new(source);
        let lines = &mut annotated_lines(source, &style);

        if line_ranges.is_some() {
            let line_ranges = LineRanges::new(line_ranges.unwrap());
            affected_lines::compute_affected_lines(lines, &line_ranges);
        } else {
            affected_lines::mark_all_affected(lines);
        }

        let mut replacements = format::format(source, style, line_ending, lines);
        // TODO: assert for replacement duplicates somewhere
        // Remove replacements that do not change anything
        replacements.retain(|r| r.text != source.src_str(r.start_byte, r.end_byte));
        replacements
    }
}
