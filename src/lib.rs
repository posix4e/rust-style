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

pub fn reformat(source: String, style: FormatStyle) -> Vec<Replacement> {
    use token::FormatTokenLexer;
    use unwrapped_line::UnwrappedLine;
    use {syntax, format, join, annotate};

    if source.chars().all(char::is_whitespace) {
        return vec![];
    }

    // FIXME: Determine if a real name is actually needed
    let name = "".to_string();
    let session = syntax::parse::new_parse_sess();
    let filemap = syntax::parse::string_to_filemap(&session, source, name);
    let lexer = syntax::parse::lexer::StringReader::new(&session.span_diagnostic, filemap);

    let lexer = &mut FormatTokenLexer::new(lexer, style.clone());
    let mut lines = UnwrappedLine::parse_lines(lexer);
    annotate::annotate_lines(&mut lines[..], &style);
    let mut lines = join::join_lines(&style, lines);

    let mut replacements = format::format(style, &mut lines);
    // TODO: check for replacement duplicatess somewhere
    // Remove replacements that do not change anything
    replacements.retain(|r| r.text != lexer.src_str(r.start_byte, r.end_byte));
    replacements
}
