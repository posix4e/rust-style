use replacement::{self, Replacement};
use std::default::Default;

// TODO: tests using different style options

fn fmt(source: &str) -> String {
    replacement::apply(source, &replacements(source))
}

fn replacements(source: &str) -> Vec<Replacement> {
    super::reformat(source.to_string(), "test".to_string(), Default::default())
}

macro_rules! assert_eq_fmt {
    ($s:expr) => (
        assert_eq!(fmt($s), $s)
    )
}

#[test]
fn test_whitespace_only_pass_through() {
    assert_eq!(fmt(""), "");
    assert_eq!(fmt(" "), " ");
    assert_eq!(fmt("\t"), "\t");
    assert_eq!(fmt("\n"), "\n");
    assert_eq!(fmt("\n\n"), "\n\n");
    assert_eq!(fmt(" \n \n "), " \n \n ");
    assert_eq!(fmt(" \n \n \n\n\t        \t"), " \n \n \n\n\t        \t");
}

#[test]
fn test_struct_declaration_trailing_comma() {
    let input = "struct Foo { a: u32, b: u32, c: bool, }";
    let expected = "struct Foo {
    a: u32,
    b: u32,
    c: bool,
}";
    assert_eq!(fmt(input), expected);
}

#[test]
fn test_struct_declaration() {
    let input = "struct Foo { a: u32, b: u32, c: bool }";
    let expected = "struct Foo {
    a: u32,
    b: u32,
    c: bool
}";
    assert_eq!(fmt(input), expected);
}

#[test]
fn test_single_newline_eof() {
    assert_eq!(fmt("fn main() {}"), "fn main() {}");
    assert_eq!(fmt("fn main() {}"), "fn main() {}");
    assert_eq!(fmt("fn main() {}\n"), "fn main() {}\n");
    assert_eq!(fmt("fn main() {}\n\n \n\t\t\n "), "fn main() {}\n");
}

#[test]
fn test_fix_indentation_hello_world() {
    let input = "fn main() { println!(\"Hi!\"); }";
    let expected = "fn main() {
    println!(\"Hi!\");
}";
    assert_eq!(fmt(input), expected);
}

#[test]
fn test_remove_blank_lines_at_start_of_file() {
    let input = "\n\n\nfn main() {}\n";
    let expected = "fn main() {}\n";
    assert_eq!(fmt(input), expected);
}

#[test]
fn test_remove_blank_lines_at_start_of_block() {
    let input = "fn main() {\n\n\nlet a = 5;}";
    let expected = "fn main() {\n\n    let a = 5;\n}";
    assert_eq!(fmt(input), expected);
}

#[test]
fn test_remove_blank_lines_at_end_of_block() {
    let input = "fn main() { let a = 5;\n\n}\n";
    let expected = "fn main() {\n    let a = 5;\n}\n";
    assert_eq!(fmt(input), expected);
}

#[test]
fn test_empty_match_block() {
    let input = "match foo {Some(a) => (), Some(b) => {}, None => {}}";
    let expected = "match foo {
    Some(a) => (),
    Some(b) => {},
    None => {}
}";
    assert_eq!(fmt(input), expected);
}

#[test]
fn test_paren_after_struct_init() {
    // FIXME: this could change if the struct is put on a single line in future
    assert_eq!(fmt("let a = hello(Foo { a: 32, b: 50 });"),
"let a = hello(Foo {
    a: 32,
    b: 50
});");
}

#[test]
fn test_no_unnecessary_replacements_eof() {
    assert_eq!(0, replacements("fn main() {}").len());
    assert_eq!(0, replacements("fn main() {}\n").len());
    assert_eq!(1, replacements("fn main() {}\n ").len());
    assert_eq!(1, replacements("fn main() {} \n").len());
    assert_eq!(1, replacements("fn main() {}\n\n").len());
    assert_eq!(1, replacements("fn main() {}\n\n").len());
    assert_eq!(1, replacements("fn main() {}\n   \t \t   \n\n").len());
    assert_eq!(1, replacements("fn main() {}\n   \t \t   \n\n  ").len());
    assert_eq!(1, replacements("fn main() {}\n   \t \t   \n\n  \t").len());
}

#[test]
fn test_format_array() {
    assert_eq_fmt!("let mut a = [0; 4];");
    assert_eq_fmt!("let mut a = [0; 4];\nlet mut a = [0; 4];");
}

#[test]
fn test_comma_in_match_expr_arm() {
    assert_eq_fmt!("match (a, b, c) {
    (a, b, _) => (a, b, b),
    (a, b, c) => (a, b, c),
}");
}

#[test]
fn test_semi_after_if_expr() {
    assert_eq_fmt!("let c = if b.is_some() {
    Some(55) // Comment so it doesn't collapse
} else {
    None // Comment so it doesn't collapse
};");
}

#[test]
fn test_if_expr_as_arg() {
    assert_eq_fmt!("let a = function_call(if something {
    Some(55) // Comment so it doesn't collapse
} else {
    None // Comment so it doesn't collapse
});");
}

#[test]
fn test_if_statement() {
    assert_eq_fmt!("if something {
    let a = 52;
    let c = 72;
}\n")
}

#[test]
fn test_statement_trailing_semi() {
    assert_eq_fmt!("if something {
    let a = 52;
    let c = 72;
};\n")
}

#[test]
fn test_if_else_statement() {
    assert_eq_fmt!("if something {
    let a = 52;
    let c = 72;
} else {
    let b = 12;
    let j = 5122;
}\n")
}

#[test]
fn test_if_else_if_statement() {
    assert_eq_fmt!("if something {
    let a = 52;
    let c = 72;
} else if something_else {
    let b = 12;
    let j = 5122;
} else {
    let x = 12;
}\n");
}


#[test]
fn test_match_statement() {
    assert_eq_fmt!("match Some(thing) {
    Some(thing) => 5,
    None => 6,
}\n");
}

#[test]
fn test_match_expr() {
    assert_eq_fmt!("function_call(match Some(thing) {
    Some(thing) => 5,
    None => 6,
});\n");

    assert_eq_fmt!("let a = match Some(thing) {
    Some(thing) => 5,
    None => 6,
};\n");

    assert_eq_fmt!("match Some(thing) {
    Some(thing) => 5,
    None => 6,
};\n");
}
