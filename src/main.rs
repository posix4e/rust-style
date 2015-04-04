#![feature(rustc_private)]

extern crate syntax;

mod annotated_line;
mod format_style;
mod line_formatter;
mod replacement;
mod token;
mod unwrapped_line;

use annotated_line::AnnotatedLine;
use format_style::FormatStyle;
use line_formatter::LineFormatter;
use std::io::prelude::*;
use std::io::{stdin, stdout};
use token::FormatTokenLexer;
use unwrapped_line::UnwrappedLine;

fn main() {
    let style = format_style::FormatStyle {
        column_limit: 100,
        indent_width: 4,
        tab_width: 4,
    };

    let name = "<stdin>".to_string();
    let mut source = vec![];
    stdin().read_to_end(&mut source).unwrap();
    let source = String::from_utf8(source).unwrap();

    let replacements = reformat(source, name, style);
}

fn reformat(source: String, name: String, style: FormatStyle) -> Vec<replacement::Replacement> {
    let session = syntax::parse::new_parse_sess();
    let filemap = syntax::parse::string_to_filemap(&session, source, name);
    let lexer = syntax::parse::lexer::StringReader::new(&session.span_diagnostic, filemap);

    let mut format_lexer = FormatTokenLexer::new(lexer, style);
    let lines = UnwrappedLine::parse_lines(&mut format_lexer);
    let annotated_lines = AnnotatedLine::from_unwrapped_lines(lines);
    let mut formatter = LineFormatter::new(style, &format_lexer);

    let replacements = formatter.format(&annotated_lines[..]);
    // TODO: remove
    print_lines(&format_lexer, annotated_lines, 0);
    replacements
}

// TODO: remove
fn print_lines(lexer: &FormatTokenLexer, lines: Vec<AnnotatedLine>, level: u32) {
    for line in lines {
        assert!(line.level == level);
        for _ in 0..level {
            print!("    ");
        }
        for token in line.tokens {
            print!("{} ", lexer.span_str(token.sp));
        }
        println!("");
        print_lines(lexer, line.children, level + 1);
    }
}
