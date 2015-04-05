use replacement;
use style::{UseTabs, FormatStyle};

fn fmt(source: &str) -> String {
    let style = FormatStyle {
        column_limit: 100,
        indent_width: 4,
        tab_width: 4,
        use_tabs: UseTabs::Never,
    };

    let replacements = super::reformat(source.to_string(), "<stdin>".to_string(), style);
    replacement::apply(source, &replacements)
}

#[test]
fn test_whitespace_only() {
    // FIXME: should these return "\n" or ""? This is a bit inconsistent.
    assert_eq!(fmt(""), "");
    assert_eq!(fmt(" "), "\n");
    assert_eq!(fmt("\t"), "\n");
    assert_eq!(fmt("\n"), "\n");
    assert_eq!(fmt("\n\n"), "\n");
    assert_eq!(fmt(" \n \n "), "\n");
    assert_eq!(fmt(" \n \n \n\n\t        \t"), "\n");
}

#[test]
fn test_single_newline_eof() {
    assert_eq!(fmt("fn main() {}"), "fn main() {}\n");
    assert_eq!(fmt("fn main() {}\n"), "fn main() {}\n");
    assert_eq!(fmt("fn main() {}\n\n \n\t\t\n"), "fn main() {}\n");
}
