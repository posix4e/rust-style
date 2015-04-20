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

#[allow(dead_code)]
fn main() {
    let debug = if cfg!(debug_assertions) { " debug" } else { "" };
    let version = format!("rustfmt version {}{}", env!("CARGO_PKG_VERSION"), debug);
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.version(Some(version)).help(true).decode())
        .unwrap_or_else(|e| e.exit());

    let style = FormatStyle::default();

    let actions = get_actions(&args);

    for action in &actions {
        match perform_input(action) {
            Ok(ref source) => {
                let replacements = reformat(source, &style);
                perform_output(action, source, &replacements);
            },
            Err(ref msg) => {
                write!(&mut stderr(), "{}", msg).unwrap();
                std::env::set_exit_status(1);
            },
        }
    }
}

fn get_actions(args: &Args) -> Vec<Action> {
    let mut actions = Vec::new();

    if args.arg_file.is_empty() {
        actions.push(Action { input: Input::StdIn, output: Output::StdOut });
    } else {
        for file in &args.arg_file {
            let output = if args.flag_inplace {
                Output::File(file)
            } else {
                Output::StdOut
            };
            actions.push(Action { input: Input::File(file), output: output });
        }
    }

    actions
}

struct Action<'a> {
    input: Input<'a>,
    output: Output<'a>,
}

enum Input<'a> {
    StdIn,
    File(&'a str),
}

enum Output<'a> {
    StdOut,
    File(&'a str),
}

fn perform_input(action: &Action) -> Result<String, String> {
    let mut source_input = String::new();

    match action.input {
        Input::StdIn => {
            match stdin().read_to_string(&mut source_input) {
                Err(ref e) => Err(format!("Failed to read stdin: {}", Error::description(e))),
                Ok(_) => Ok(source_input),
            }
        },
        Input::File(ref path_string) => {
            let path = Path::new(path_string);

            let mut file = match File::open(path) {
                Err(_) => return Err(format!("Couldn't open file: {}", path.display())),
                Ok(file) => file,
            };
            
            match file.read_to_string(&mut source_input) {
                Err(_) => Err(format!("Couldn't read file: {}", path.display())),
                Ok(_) => Ok(source_input),
            }
        }
    }
}

// TODO: handle output errors encountered?
fn perform_output(action: &Action, source: &String, replacements: &Vec<Replacement>) {
    let output = Replacement::apply_all(replacements, source);

    match action.output {
        Output::StdOut => {
            write!(&mut stdout(), "{}", output).unwrap();
        },
        Output::File(ref path_string) => {
            let mut file = File::create(path_string).unwrap();
            write!(file, "{}", output).unwrap();
        },
    }
}
