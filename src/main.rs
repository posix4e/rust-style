#![feature(rustc_private)]
#![feature(collections)]

extern crate arena;
extern crate syntax;
extern crate rustc_serialize;
extern crate docopt;

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

use docopt::Docopt;
use replacement::Replacement;
use std::default::Default;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::io::{stdin, stdout};
use std::path::Path;
use style::FormatStyle;
use token::FormatTokenLexer;
use unwrapped_line::UnwrappedLine;

static USAGE: &'static str = "
Overview: A tool to format rust code.

If no arguments are specified, it formats the code from standard input
and writes the result to the standard output.
If <file>s are given, it reformats the files. If -i is specified
together with <file>s, the files are edited in-place. Otherwise, the
result is written to the standard output.

Usage: rustfmt [-i] [<file>]...
       rustfmt (--help | --version)

Options:
    -h, --help     Show this message
    -i, --inplace  Inplace edit <file>s, if specified
    -V, --version  Print version info and exit
";

#[derive(RustcDecodable, Debug)]
struct Args {
    arg_file: Vec<String>,
    flag_inplace: bool,
}

fn main() {
    // FIXME: version should include debug/release
    let version = format!("rustfmt version {}", env!("CARGO_PKG_VERSION"));
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.version(Some(version)).help(true).decode())
        .unwrap_or_else(|e| e.exit());

    let style = FormatStyle::default();

    if args.arg_file.is_empty() {
        match format_stdin(style) {
            Ok(s) | Err(s) => {
                write!(&mut stdout(), "{}", s).unwrap();
            }
        }
    } else {
        for name in args.arg_file {
            let path = Path::new(&name);
            match format_source_file(style.clone(), path) {
                Ok(ref s) if args.flag_inplace => {
                    let mut file = File::create(path).unwrap();
                    write!(file, "{}", s).unwrap();
                },
                Ok(s) | Err(s) => {
                    write!(&mut stdout(), "{}", s).unwrap();
                }
            }
        }
    }
}

fn format_source_file(style: FormatStyle, path: &Path) -> Result<String, String> {
    let source = {
        let mut file = match File::open(path) {
            Err(_) => return Err(format!("Couldn't open file: {}", path.display())),
            Ok(file) => file,
        };

        let mut source = String::new();
        match file.read_to_string(&mut source) {
            Err(_) => return Err(format!("Couldn't read file: {}", path.display())),
            Ok(_) => source,
        }
    };

    let replacements = reformat(source.clone(), style);
    Ok(replacement::apply(&source, &replacements))
}

fn format_stdin(style: FormatStyle) -> Result<String, String> {
    let mut source = String::new();
    match stdin().read_to_string(&mut source) {
        Err(ref e) => return Err(format!("Failed to read stdin: {}", Error::description(e))),
        Ok(_) => {},
    }

    let replacements = reformat(source.clone(), style);
    Ok(replacement::apply(&source, &replacements))
}

fn reformat(source: String, style: FormatStyle) -> Vec<Replacement> {
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

