extern crate docopt;
extern crate glob;
extern crate rustc_serialize;
extern crate rust_style;

use docopt::Docopt;
use rustc_serialize::json;
use rust_style::{reformat, Replacement, FormatStyle};
use std::default::Default;
use std::error::Error;
use std::fs::{self, File};
use std::io::prelude::*;
use std::io::{stdin, stdout, stderr, self};
use std::iter::FromIterator;
use std::path::{Path, PathBuf};

static USAGE: &'static str = "
Overview: A tool to format rust code.

Formats the given files, and prints the result to standard output.
If a direcory is given, all recursively contained .rs files are formatted.
If the -w option is specified, the input files are overwritten.
If no file arguments are specified, input is read from standard input.

Usage: rust-style [-w] [<file>]...
       rust-style [--lines=<string>]... [--output-replacements-json] [<file>]
       rust-style [--output-replacements-json] [<file>]...
       rust-style (--help | --version)

Options:
    -h, --help                     Show this message
    -w, --write                    Overwrite the input files
    -V, --version                  Print version info and exit
    -j --output-replacements-json  Outputs replacements as JSON
    --lines=<string>               Formats lines specified, where
                                   <string> is <uint>:<uint> are
                                   the line number ranges 1-based.
";

#[derive(RustcDecodable, Debug)]
struct Args {
    arg_file: Vec<String>,
    flag_lines: Vec<String>,
    flag_write: bool,
    flag_output_replacements_json: bool,
}

#[allow(dead_code)]
fn main() {
    let mut exit_code = 0;

    // Block to allow destructors to run before exit is called
    {
        let debug = if cfg!(debug_assertions) { " debug" } else { "" };
        let version = format!("rust-style version {}{}", env!("CARGO_PKG_VERSION"), debug);
        let args: Args = Docopt::new(USAGE)
            .and_then(|d| d.version(Some(version)).help(true).decode())
            .unwrap_or_else(|e| e.exit());

        let style = FormatStyle::default();
        let (actions, action_args) = get_actions(&args).unwrap();

        for action in &actions {
            match perform_input(action) {
                Ok(ref source) => {
                    let ranges = action_args.ranges.as_ref().map(|r| &r[..]);
                    let replacements = reformat(source, &style, ranges);
                    perform_output(action, &action_args, source, &replacements).unwrap();
                },
                Err(ref msg) => {
                    writeln!(&mut stderr(), "{}", msg).unwrap();
                    exit_code = 1;
                },
            }
        }
    }

    if exit_code != 0 {
        std::process::exit(exit_code);
    }
}

fn get_actions(args: &Args) -> FormatResult<(Vec<Action>, ActionArgs)> {
    let ranges = if args.flag_lines.is_empty() {
        None
    } else {
        let mut ranges = Vec::new();
        for range_string in &args.flag_lines {
            let range_values = Vec::<&str>::from_iter(range_string.split(':'));
            if range_values.len() != 2 {
                return Err(FormatError::CustomError("incorrect format for line ranges"));
            }
            let a = try!(range_values[0].parse::<u32>());
            let b = try!(range_values[1].parse::<u32>());
            if a == 0 || b == 0 {
                return Err(FormatError::CustomError("cannot specify line 0"));
            }
            // converted from 1-based to 0-based
            ranges.push((a - 1, b - 1));
        }
        Some(ranges)
    };
    let action_args = ActionArgs {
        ranges: ranges,
        output_json: args.flag_output_replacements_json,
    };

    let actions = if args.arg_file.is_empty() {
        vec![Action {
            input: Input::StdIn,
            output: Output::StdOut,
        }]
    } else {
        let new_action = |input: PathBuf| Action {
            output: if args.flag_write { Output::File(input.clone()) }
                    else { Output::StdOut },
            input: Input::File(input),
        };

        let mut actions = vec![];

        for file in &args.arg_file {
            let path = Path::new(file).to_path_buf();
            let metadata = try!(fs::metadata(file));
            if metadata.is_dir() {
                // recursively find all *.rs files
                let pattern = path.join("**/*.rs");
                let pattern = pattern.to_str().unwrap();

                // ignore files that start with '.'
                let match_options = glob::MatchOptions {
                    require_literal_leading_dot: true,
                    ..glob::MatchOptions::default()
                };

                let paths = try!(glob::glob_with(pattern, &match_options));
                for result in paths {
                    actions.push(new_action(try!(result)));
                }
            } else {
                actions.push(new_action(path));
            }
        }

        actions
    };

    Ok((actions, action_args))
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

fn perform_output(action: &Action, args: &ActionArgs, source: &String, replacements: &Vec<Replacement>)
                  -> FormatResult<()> {
    let output = if args.output_json {
        try!(json::encode(replacements))
    } else {
        Replacement::apply_all(replacements, source)
    };

    match action.output {
        Output::StdOut => {
            try!(write!(&mut stdout(), "{}", output));
        },
        Output::File(ref path) => {
            if !replacements.is_empty() {
                let mut file = try!(File::create(path));
                try!(write!(file, "{}", output));
            }
        },
    }

    Ok(())
}

struct ActionArgs {
    ranges: Option<Vec<(u32, u32)>>,
    output_json: bool,
}

struct Action {
    input: Input,
    output: Output,
}

enum Input {
    StdIn,
    File(PathBuf),
}

enum Output {
    StdOut,
    File(PathBuf),
}

type FormatResult<T> = Result<T, FormatError>;

#[derive(Debug)]
enum FormatError {
    IoError(io::Error),
    GlobError(glob::GlobError),
    PatternError(glob::PatternError),
    JsonError(rustc_serialize::json::EncoderError),
    ParseError(std::num::ParseIntError),
    CustomError(&'static str),
}

impl From<io::Error> for FormatError {
    fn from(error: io::Error) -> Self {
        FormatError::IoError(error)
    }
}

impl From<glob::GlobError> for FormatError {
    fn from(error: glob::GlobError) -> Self {
        FormatError::GlobError(error)
    }
}

impl From<glob::PatternError> for FormatError {
    fn from(error: glob::PatternError) -> Self {
        FormatError::PatternError(error)
    }
}

impl From<rustc_serialize::json::EncoderError> for FormatError {
    fn from(error: rustc_serialize::json::EncoderError) -> Self {
        FormatError::JsonError(error)
    }
}

impl From<std::num::ParseIntError> for FormatError {
    fn from(error: std::num::ParseIntError) -> Self {
        FormatError::ParseError(error)
    }
}
