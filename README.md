##rust-style [![Build Status](https://travis-ci.org/sp0/rust-style.svg)](https://travis-ci.org/sp0/rust-style)

A tool to format rust code.

This is a work in progress. It is not ready for use yet.

This project was previously called rustfmt. Not to be confused with the similarly named project: https://github.com/nrc/rustfmt/

###Building rust-style

A recent nightly version of Rust is required.

~~~
cargo build --release
~~~

This will result in a rust-style executable.

###Usage

~~~
rust-style [-w] [<file>]...
rust-style [--lines=<string>]... [--output-replacements-json] [<file>]
rust-style [--output-replacements-json] [<file>]...
rust-style (--help | --version)
~~~

Formats the given files, and prints the result to standard output.
If a directory is given, all recursively contained .rs files are formatted.
If the -w option is specified, the input files are overwritten.
If no file arguments are specified, input is read from standard input.

~~~
-h, --help                      - Show this message
-w, --write                     - Overwrite the input files
-V, --version                   - Print version info and exit
-j, --output-replacements-json  - Outputs replacements as JSON
--lines=<string>                - Formats lines specified, where
                                  <string> is <uint>:<uint> -
                                  the line number ranges. The values
                                  are 1-based and inclusive.
~~~
