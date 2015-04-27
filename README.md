##rustfmt

A tool to format rust code.

###Building rustfmt

~~~
cargo build
~~~

Note: Since rustfmt relies on compiler internals (that are marked as unstable) you will need the nightly distribution of rust.

This will result in a rustfmt executable.

###Usage

~~~
rustfmt [-w] [<file>]...
rustfmt [--output-replacements-json] [<file>]...
rustfmt (--help | --version)
~~~

Reads the given files, and prints the result to standard output.
If the -w option is specified, the input files are overwritten.
If no file arguments are specified, input is read from standard input.

~~~
-h, --help                      Show this message
-w, --write                     Overwrite the input files
-V, --version                   Print version info and exit
-j, --output-replacements-json  Outputs replacements as JSON
~~~

Please be aware of the fact that this is not meant for production use yet! We plan to have this usable by the rust 1.0 release.
