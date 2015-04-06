#![feature(rustc_private)]
#![feature(collections)]

extern crate syntax;

mod annotated_line;
mod format;
mod style;
mod replacement;
mod token;
mod unwrapped_line;
mod whitespace;

#[cfg(test)]
mod test;

use annotated_line::AnnotatedLine;
use style::FormatStyle;
use std::io::prelude::*;
use std::io::{stdin, stdout};
use token::FormatTokenLexer;
use unwrapped_line::UnwrappedLine;
use replacement::Replacement;
use std::default::Default;

fn main() {
    let style = Default::default();
    let name = "<stdin>".to_string();
    let mut source = vec![];
    stdin().read_to_end(&mut source).unwrap();
    let source = String::from_utf8(source).unwrap();

    // FIXME: remove clone?
    let replacements = reformat(source.clone(), name, style);
    // for r in &replacements { println!("{:?}", r); }
    let result = replacement::apply(&source, &replacements);

    write!(&mut stdout(), "{}", result).unwrap();
}

fn reformat(source: String, name: String, style: FormatStyle) -> Vec<Replacement> {
    let session = syntax::parse::new_parse_sess();
    let filemap = syntax::parse::string_to_filemap(&session, source, name);
    let lexer = syntax::parse::lexer::StringReader::new(&session.span_diagnostic, filemap);

    let mut format_lexer = FormatTokenLexer::new(lexer, style);
    let unwrapped_lines = UnwrappedLine::parse_lines(&mut format_lexer);
    let annotated_lines = &mut AnnotatedLine::from_unwrapped_lines(unwrapped_lines);
    // TODO: merge lines here (LineJoiner)
    format::format(style, annotated_lines)
}

