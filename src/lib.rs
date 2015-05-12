extern crate typed_arena;
extern crate rustc_serialize;
extern crate syntex_syntax as syntax;
extern crate toml;

mod affected_lines;
mod annotate;
mod continuation_indenter;
mod format;
mod format_options;
mod replacement;
mod token;
mod unwrapped_line;
mod whitespace_manager;
pub mod unstable;

#[cfg(test)]
mod test;

pub use format_options::{FormatStyle, StyleParseError, Penalty, UseTabs};
pub use internal::reformat;
pub use replacement::Replacement;

mod internal {
    use format_options::{FormatStyle, LineRanges, LineEnding};
    use replacement::Replacement;
    use token::FormatTokenLexer;
    use unwrapped_line::UnwrappedLine;
    use {affected_lines, annotate, format, syntax};

    pub fn annotated_lines(lexer: &mut FormatTokenLexer, style: &FormatStyle) -> Vec<UnwrappedLine> {
        let mut lines = UnwrappedLine::parse_lines(lexer);
        annotate::annotate_lines(&mut lines[..], style);
        lines
    }

    /// Format a string, specifying style and the line ranges to be formatted.
    /// Ranges are zero based, and inclusive. None means the whole string should be formatted.
    pub fn reformat(source: &str, style: &FormatStyle, line_ranges: Option<&[(u32, u32)]>) -> Vec<Replacement> {
        if source.chars().all(char::is_whitespace) {
            return vec![];
        }

        let line_ending = LineEnding::derive_from_source(source);
        let session = syntax::parse::new_parse_sess();
        let lexer = &mut FormatTokenLexer::new(source, &session, &style);
        let lines = &mut annotated_lines(lexer, &style);

        if line_ranges.is_some() {
            let line_ranges = LineRanges::new_from_tuples(line_ranges.unwrap());
            affected_lines::compute_affected_lines(lines, &line_ranges);
        } else {
            affected_lines::mark_all_affected(lines);
        }

        let mut replacements = format::format(lexer, style, line_ending, lines);
        // TODO: assert for replacement duplicates somewhere
        // Remove replacements that do not change anything
        replacements.retain(|r| r.text != lexer.src_str(r.start_byte, r.end_byte));
        replacements
    }
}
