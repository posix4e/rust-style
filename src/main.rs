#![feature(fs_canonicalize)]
#![cfg(not(test))]

extern crate docopt;
extern crate glob;
extern crate rustc_serialize;
extern crate rust_style;

use docopt::Docopt;
use rust_style::{reformat, Replacement, FormatStyle, StyleParseError};
use rustc_serialize::json;
use std::default::Default;
use std::error::Error;
use std::fs::{self, File};
use std::io::prelude::*;
use std::io::{stdin, stdout, stderr, self};
use std::iter::FromIterator;
use std::path::{Path, PathBuf};
use rust_style::unstable::PathExts;

static USAGE: &'static str = "
Overview: A tool to format rust code.

Formats the given files, and prints the result to standard output.
If a directory is given, all recursively contained .rs files are formatted.
If the -w option is specified, the input files are overwritten.
If no file arguments are specified, input is read from standard input.

Usage: rust-style [-w] [<file>]...
       rust-style [--lines=<string>]... [--output-replacements-json] [<file>]
       rust-style [--output-replacements-json] [<file>]...
       rust-style (--help | --version)

Options:
    -h, --help                      - Show this message
    -w, --write                     - Overwrite the input files
    -V, --version                   - Print version info and exit
    -j, --output-replacements-json  - Outputs replacements as JSON
    --lines=<string>                - Formats lines specified, where
                                      <string> is <uint>:<uint> -
                                      the line number ranges. The values
                                      are 1-based and inclusive.
";

#[derive(RustcDecodable, Debug)]
struct Args {
    arg_file: Vec<String>,
    flag_lines: Vec<String>,
    flag_write: bool,
    flag_output_replacements_json: bool,
}

fn main() {
    let exit_code = run();

    if exit_code != 0 {
        std::process::exit(exit_code);
    }
}

fn run() -> i32 {
    let debug = if cfg!(debug_assertions) { " debug" } else { "" };
    let version = format!("rust-style version {}{}", env!("CARGO_PKG_VERSION"), debug);
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.version(Some(version)).help(true).decode())
        .unwrap_or_else(|e| e.exit());

    let args_result = get_actions(&args);

    if args_result.is_err() {
        write_argument_error(&args_result.err().unwrap());
        return 1;
    }

    let (actions, action_args) = args_result.unwrap();

    let mut exit_code = 0;
    for action in &actions {
        match perform_input(action) {
            Err(ref error) => {
                write_input_error(&action.input, error);
                exit_code = 1;
            },
            Ok(ref source) => {
                let ranges = action_args.ranges.as_ref().map(|r| &r[..]);
                let replacements = reformat(source, &action.style, ranges);
                let output_result = perform_output(action, &action_args, source, &replacements);

                if output_result.is_err() {
                    write_output_error(&action.output, &output_result.unwrap_err());
                    exit_code = 1;
                }
            },
        }
    }

    exit_code
}

fn get_actions(args: &Args) -> ArgumentResult<(Vec<Action>, ActionArgs)> {
    let ranges = if args.flag_lines.is_empty() {
        None
    } else {
        let mut ranges = Vec::new();
        for range_string in &args.flag_lines {
            let range_values = Vec::<&str>::from_iter(range_string.split(':'));
            if range_values.len() != 2 {
                return Err(ArgsError::ParseLinesError(
                    "Incorrect format for line ranges, expecting --lines=<uint>:<uint>."));
            }
            let a = try!(range_values[0].parse::<u32>());
            let b = try!(range_values[1].parse::<u32>());
            if a == 0 || b == 0 {
                return Err(ArgsError::ParseLinesError(
                    "Cannot specify line 0, expecting 1-based line numbers."));
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
            style: FormatStyle::default(),
            input: Input::StdIn,
            output: Output::StdOut,
        }]
    } else {
        let mut actions = vec![];

        for file in &args.arg_file {
            let path = Path::new(file).to_path_buf();
            if !path._exists() {
                return Err(ArgsError::DoesNotExist(file.clone()));
            }
            let metadata = fs::metadata(file).unwrap();
            if metadata.is_dir() {
                // recursively find all *.rs files
                let pattern = path.join("**/*.rs");
                let pattern = pattern.to_str().unwrap();
                let pattern = pattern.replace("\\", "/");

                // ignore files that start with '.'
                let match_options = glob::MatchOptions {
                    require_literal_leading_dot: true,
                    ..glob::MatchOptions::default()
                };

                let paths = try!(glob::glob_with(&pattern, &match_options));
                for result in paths {
                    let path = try!(result);
                    actions.push(try!(create_file_action(path, &args)));
                }
            } else {
                actions.push(try!(create_file_action(path, &args)));
            }
        }

        actions
    };

    Ok((actions, action_args))
}

fn create_file_action(input: PathBuf, args: &Args) -> ArgumentResult<Action> {
    let style = try!(search_style_format(&input));
    let output = if args.flag_write {
        Output::File(input.clone())
    } else {
        Output::StdOut
    };

    Ok(Action {
        style: style,
        output: output,
        input: Input::File(input),
    })
}

fn search_style_format(start: &Path) -> ArgumentResult<FormatStyle> {
    let target = ".rust-style.toml";
    let mut node = std::fs::canonicalize(start).unwrap();

    // removes existing file component from the path
    if node.as_path()._is_file() {
        node.pop();
    }

    let mut root_reached = false;
    while !root_reached {
        node.push(target);

        let target_found = {
            let path = node.as_path();
            path._exists() && path._is_file() &&
                path.file_name().unwrap() == target
        };

        if target_found {
            return load_style_format(node.to_str().unwrap());
        }

        root_reached = !node.pop() || !node.pop();
    }

    Ok(FormatStyle::default())
}

fn load_style_format(path_str: &str) -> ArgumentResult<FormatStyle> {
    let path = Path::new(path_str);
    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(err) => return Err(ArgsError::StyleLoadError(path_str.to_string(), err)),
    };

    let mut toml_text = String::new();
    match file.read_to_string(&mut toml_text) {
        Ok(_) => (),
        Err(err) => return Err(ArgsError::StyleLoadError(path_str.to_string(), err)),
    }

    let arg_result = match FormatStyle::from_toml_str(toml_text.as_ref()) {
        Ok(style) => Ok(style),
        Err(err) => Err(ArgsError::StyleParseError(path_str.to_string(), err)),
    };

    return arg_result;
}

fn perform_input(action: &Action) -> IoResult<String> {
    let mut source_input = String::new();

    match action.input {
        Input::StdIn => {
            try!(stdin().read_to_string(&mut source_input));
        },
        Input::File(ref path_string) => {
            let path = Path::new(path_string);
            let mut file = try!(File::open(path));

            try!(file.read_to_string(&mut source_input));
        }
    }

    Ok(source_input)
}

fn perform_output(action: &Action, args: &ActionArgs, source: &String,
                  replacements: &Vec<Replacement>) -> IoResult<()> {
    let output = if args.output_json {
        let result = json::encode(replacements);
        if result.is_err() {
            // should not ever occur
            panic!("Unexpected error: {}", result.unwrap_err());
        }
        result.unwrap()
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

fn write_argument_error(error: &ArgsError) {
    let details = match *error {
        ArgsError::DoesNotExist(ref path) =>
            format!("File '{}' does not exist.", path),
        ArgsError::GlobError(ref err) =>
            format!("Glob error accessing path '{}'. {}", err.path().display(), err.error()),
        ArgsError::PatternError(ref err) => format!("Invalid file pattern used. {}", err),
        ArgsError::ParseIntError(ref err) =>
            format!("Error occurred when parsing lines. {}.", err),
        ArgsError::ParseLinesError(ref err) =>
            format!("Error occurred when parsing lines. {}", err),
        ArgsError::StyleLoadError(ref path, ref err) =>
            format!("Cannot load style file '{}'. {}", path, err),
        ArgsError::StyleParseError(ref path, ref err) => {
            let msg = match *err {
                StyleParseError::ParseError(ref errs) => {
                    let mut errors_string = String::new();
                    for error in errs {
                        errors_string.push_str(error.desc.as_ref());
                        errors_string.push_str(", ");
                    }
                    format!("Failed to parse style toml: {}", errors_string)
                },
                StyleParseError::InvalidKey(ref key, ref value) =>
                    format!("Invalid key for '{} = {}'", key, value),
                StyleParseError::InvalidValue(ref key, ref value, ref expect) =>
                    format!("Invalid value for '{} = {}'. Expecting value: {}.", value, key,
                            expect),
                StyleParseError::InvalidValueType(ref key, ref value, ref expect) =>
                    format!("Invalid value type for '{} = {}'. Expecting type: {}.", value, key,
                            expect),
            };
            format!("Parsing error when loading style file '{}'. {}", path, msg)
        }
    };

    writeln!(&mut stderr(), "Failed to process arguments. {}", details).unwrap();
}

fn write_input_error(input_type: &Input, error: &io::Error) {
    match *input_type {
        Input::StdIn => {
            writeln!(&mut stderr(), "Failed to read from stdin. {}", error).unwrap();
        },
        Input::File(ref path) => {
            let path = path.to_str().unwrap();
            writeln!(&mut stderr(), "Couldn't read from file: {}. {}", path,  error).unwrap();
        },
    }
}

fn write_output_error(output_type: &Output, error: &io::Error) {
    match *output_type {
        Output::StdOut => {
            writeln!(&mut stderr(), "Failed to write to stdout. {}", error).unwrap();
        },
        Output::File(ref path) => {
            let path = path.to_str().unwrap();
            writeln!(&mut stderr(), "Couldn't write to file: {}. {}", path, error).unwrap();
        },
    }
}

struct ActionArgs {
    ranges: Option<Vec<(u32, u32)>>,
    output_json: bool,
}

struct Action {
    style: FormatStyle,
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

type ArgumentResult<T> = Result<T, ArgsError>;
type IoResult<T> = Result<T, io::Error>;

#[derive(Debug)]
enum ArgsError {
    DoesNotExist(/*path:*/ String),
    GlobError(glob::GlobError),
    PatternError(glob::PatternError),
    ParseIntError(std::num::ParseIntError),
    ParseLinesError(&'static str),
    StyleLoadError(/*path:*/ String, io::Error),
    StyleParseError(/*path:*/ String, StyleParseError),
}

impl From<glob::GlobError> for ArgsError {
    fn from(error: glob::GlobError) -> Self {
        ArgsError::GlobError(error)
    }
}

impl From<glob::PatternError> for ArgsError {
    fn from(error: glob::PatternError) -> Self {
        ArgsError::PatternError(error)
    }
}

impl From<std::num::ParseIntError> for ArgsError {
    fn from(error: std::num::ParseIntError) -> Self {
        ArgsError::ParseIntError(error)
    }
}
