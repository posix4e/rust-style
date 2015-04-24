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
rustfmt [-i] [<file>]...
rustfmt [--output-replacements-json] [<file>]...
rustfmt (--help | --version)
~~~

If no arguments are specified, it formats the code from standard input
and writes the result to the standard output.
If `<file>`s are given, it reformats the files. If `-i` is specified
together with `<file>`s, the files are edited in-place. Otherwise, the
result is written to the standard output.

~~~
-h, --help                  Show this message
-i, --inplace               Inplace edit <file>s, if specified
-V, --version               Print version info and exit
--output-replacements-json  Outputs replacements as JSON
~~~

Please be aware of the fact that this is not meant for production use yet! We plan to have this usable by the rust 1.0 release.