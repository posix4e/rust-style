#![feature(exit_status)]

extern crate docopt;
extern crate rustc_serialize;
extern crate rustfmt;

use docopt::Docopt;
use rustfmt::{reformat, Replacement, FormatStyle};
use std::default::Default;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::io::{stdin, stdout, stderr};
use std::path::Path;

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
    let debug = if cfg!(debug_assertions) { " debug" } else { "" };
    let version = format!("rustfmt version {}{}", env!("CARGO_PKG_VERSION"), debug);
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.version(Some(version)).help(true).decode())
        .unwrap_or_else(|e| e.exit());

    let style = FormatStyle::default();

    if args.arg_file.is_empty() {
        match format_stdin(&style) {
            Ok(ref s)  => write!(&mut stdout(), "{}", s).unwrap(),
            Err(ref s) => {
                write!(&mut stderr(), "{}", s).unwrap();
                std::env::set_exit_status(1);
            },
        }
    } else {
        for name in args.arg_file {
            let path = Path::new(&name);
            match format_source_file(&style, path) {
                Ok(ref s) if args.flag_inplace => {
                    let mut file = File::create(path).unwrap();
                    write!(file, "{}", s).unwrap();
                },
                Ok(ref s)  => write!(&mut stdout(), "{}", s).unwrap(),
                Err(ref s) => {
                    write!(&mut stderr(), "{}", s).unwrap();
                    std::env::set_exit_status(1);
                },
            }
        }
    }
}

fn format_source_file(style: &FormatStyle, path: &Path) -> Result<String, String> {
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

    let replacements = rustfmt::reformat(&source, style);
    Ok(Replacement::apply_all(&replacements, &source))
}

fn format_stdin(style: &FormatStyle) -> Result<String, String> {
    let mut source = String::new();
    match stdin().read_to_string(&mut source) {
        Err(ref e) => return Err(format!("Failed to read stdin: {}", Error::description(e))),
        Ok(_) => {},
    }

    let replacements = rustfmt::reformat(&source, style);
    Ok(Replacement::apply_all(&replacements, &source))
}
