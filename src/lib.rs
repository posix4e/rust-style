#![feature(rustc_private)]
#![feature(collections)]
#![feature(core)]

extern crate arena;
extern crate syntax;

mod annotate;
mod continuation_indenter;
mod format;
mod join;
mod replacement;
mod style;
mod token;
mod unwrapped_line;
mod whitespace_manager;

#[cfg(test)]
mod test;

pub use replacement::Replacement;
pub use style::{FormatStyle, Penalty, UseTabs};
pub use internal::reformat;

mod internal {
    use replacement::Replacement;
    use style::FormatStyle;
    use token::FormatTokenLexer;
    use unwrapped_line::UnwrappedLine;
    use {syntax, annotate, join, format};

    pub fn annotated_lines(lexer: &mut FormatTokenLexer, style: &FormatStyle) -> Vec<UnwrappedLine> {
        let mut lines = UnwrappedLine::parse_lines(lexer);
        annotate::annotate_lines(&mut lines[..], style);
        join::join_lines(&style, lines)
    }

    pub fn reformat(source: &str, style: &FormatStyle) -> Vec<Replacement> {
        if source.chars().all(char::is_whitespace) {
            return vec![];
        }
        let session = syntax::parse::new_parse_sess();
        let lexer = &mut FormatTokenLexer::new(source, &session, &style);
        let lines = &mut annotated_lines(lexer, &style);
        let mut replacements = format::format(style.clone(), lines);
        // TODO: assert for replacement duplicates somewhere
        // Remove replacements that do not change anything
        replacements.retain(|r| r.text != lexer.src_str(r.start_byte, r.end_byte));
        replacements
    }
}
