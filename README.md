##rustfmt

A tool to format rust code.

This is a work in progress. It is not ready for use yet.

###Building rustfmt

~~~
cargo build --release
~~~

This will result in a rustfmt executable.

###Usage

~~~
rustfmt [-w] [<file>]...
rustfmt [--output-replacements-json] [<file>]...
rustfmt (--help | --version)
~~~

Formats the given files, and prints the result to standard output.
If a direcory is given, all recursively contained .rs files are formatted.
If the -w option is specified, the input files are overwritten.
If no file arguments are specified, input is read from standard input.

~~~
-h, --help                      Show this message
-w, --write                     Overwrite the input files
-V, --version                   Print version info and exit
-j, --output-replacements-json  Outputs replacements as JSON
~~~
