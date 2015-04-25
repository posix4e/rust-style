#![feature(exit_status)]
#![cfg(not(test))]
extern crate readline;
extern crate docopt;
extern crate rustc_serialize;
extern crate rustfmt;

use readline::readline;

use std::ffi::CString;

use docopt::Docopt;
use rustc_serialize::json;
use rustfmt::{reformat, Replacement, FormatStyle};
use std::default::Default;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::io::{stdin, stdout, stderr};
use std::path::Path;
use std::str;

static USAGE: &'static str = "
Overview: A tool to format rust code.

Reads the given files, and prints the result to standard output.
If the -w option is specified, the input files are overwritten.
If no file arguments are specified, input is read from standard input.

Usage: rustfmt [-w] [<file>]...
       rustfmt [--output-replacements-json] [<file>]...
       rustfmt (--help | --version | --interactive)

Options:
    -h, --help                      Show this message
    -w, --write                     Overwrite the input files
    -V, --version                   Print version info and exit
    -j, --output-replacements-json  Outputs replacements as JSON
    -i, --interactive               Interactive Mode
";

#[derive(RustcDecodable, Debug)]
struct Args {
    arg_file: Vec<String>,
    flag_write: bool,
    flag_output_replacements_json: bool,
    flag_interactive: bool,
}

#[allow(dead_code)]
fn main() {
    let debug = if cfg!(debug_assertions) { " debug" } else { "" };
    let version = format!("rustfmt version {}{}", env!("CARGO_PKG_VERSION"), debug);

    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.version(Some(version)).help(true).decode())
        .unwrap_or_else(|e| e.exit());

    let style = FormatStyle::default();

    match get_actions(&args) {
        Mode::FileBased(actions) => {
            for action in &actions {
                match perform_input(action) {
                    Ok(ref source) => {
                        let replacements = reformat(source, &style);
                        perform_output(action, &args, source, &replacements);
                    },
                    Err(ref msg) => {
                        write!(&mut stderr(), "{}", msg).unwrap();
                        std::env::set_exit_status(1);
                    },
                }
            }
        }
        Mode::Interactive(ref action) => {

            let prompt_first_line = CString::new(">>> ").unwrap();
            let prompt_additional_lines = CString::new("    ").unwrap();

            while let Ok(source) = readline(&prompt_first_line) {
                let mut source = str::from_utf8(source.to_bytes()).unwrap().to_string();
                while let Ok(additional_line) = readline(&prompt_additional_lines) {
                    let additional_line = str::from_utf8(additional_line.to_bytes()).unwrap();
                    if additional_line.is_empty() {
                        break;
                    }
                    source.push_str(additional_line);
                }

                if !source.is_empty() {
                    let replacements = reformat(&source, &style);
                    perform_output(action, &args, &source, &replacements);
                    print!("\n");
                }
            }
        }
    }
}

fn get_actions(args: &Args) -> Mode {
    if args.flag_interactive {
        return Mode::Interactive(Action {input: Input::StdIn, output: Output::StdOut });
    }

    let mut actions = Vec::new();

    if args.arg_file.is_empty() {
        actions.push(Action { input: Input::StdIn, output: Output::StdOut });
    } else {
        for file in &args.arg_file {
            let output = if args.flag_write {
                Output::File(file)
            } else {
                Output::StdOut
            };
            actions.push(Action { input: Input::File(file), output: output });
        }
    }
    Mode::FileBased(actions)
}

enum Mode <'a>{
    FileBased(Vec<Action<'a>>),
    Interactive(Action<'a>),
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
fn perform_output(action: &Action, args: &Args, source: &String, replacements: &Vec<Replacement>) {
    let output = if args.flag_output_replacements_json {
        json::encode(replacements).unwrap()
    } else {
        Replacement::apply_all(replacements, source)
    };

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
