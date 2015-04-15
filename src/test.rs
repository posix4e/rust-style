use replacement::{self, Replacement};
use std::default::Default;

// TODO: tests using different style options

fn fmt(source: &str) -> String {
    replacement::apply(source, &replacements(source))
}

fn replacements(source: &str) -> Vec<Replacement> {
    super::reformat(source.to_string(), Default::default())
}

macro_rules! assert_fmt_eq (
    ($s:expr) => (
        assert_eq!(fmt($s), $s)
    )
);

#[test]
fn test_whitespace_only_pass_through() {
    assert_fmt_eq!("");
    assert_fmt_eq!(" ");
    assert_fmt_eq!("\t");
    assert_fmt_eq!("\n");
    assert_fmt_eq!("\n\n");
    assert_fmt_eq!(" \n \n ");
    assert_fmt_eq!(" \n \n \n\n\t        \t");
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
    assert_fmt_eq!("let mut a = [0; 4];");
    assert_fmt_eq!("let mut a = [0; 4];\nlet mut a = [0; 4];");
}

#[test]
fn test_comma_in_match_expr_arm() {
    assert_fmt_eq!("match (a, b, c) {
    (a, b, _) => (a, b, b),
    (a, b, c) => (a, b, c),
}");
}

#[test]
fn test_semi_after_if_expr() {
    assert_fmt_eq!("let c = if b.is_some() {
    Some(55) // Comment so it doesn't collapse
} else {
    None // Comment so it doesn't collapse
};");
}

#[test]
fn test_if_expr_as_arg() {
    assert_fmt_eq!("let a = function_call(if something {
    Some(55) // Comment so it doesn't collapse
} else {
    None // Comment so it doesn't collapse
});");
}

#[test]
fn test_if_statement() {
    assert_fmt_eq!("if something {
    let a = 52;
    let c = 72;
}\n")
}

#[test]
fn test_statement_trailing_semi() {
    assert_fmt_eq!("if something {
    let a = 52;
    let c = 72;
};\n")
}

#[test]
fn test_if_else_statement() {
    assert_fmt_eq!("if something {
    let a = 52;
    let c = 72;
} else {
    let b = 12;
    let j = 5122;
}\n")
}

#[test]
fn test_if_else_if_statement() {
    assert_fmt_eq!("if something {
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
    assert_fmt_eq!("match Some(thing) {
    Some(thing) => 5,
    None => 6,
}\n");
}

#[test]
fn test_match_expr() {
    assert_fmt_eq!("function_call(match Some(thing) {
    Some(thing) => 5,
    None => 6,
});\n");

    assert_fmt_eq!("let a = match Some(thing) {
    Some(thing) => 5,
    None => 6,
};\n");

    assert_fmt_eq!("match Some(thing) {
    Some(thing) => 5,
    None => 6,
};\n");
}

#[test]
fn test_code_block() {
    assert_fmt_eq!("let a = 2;
{
    let b = 5;
    let c = 6;
    let d = 5;
}
let e = 5;");
}

#[test]
fn test_token_spacing() {
    assert_fmt_eq!("for i in 5..10 {\n    let b = i;\n}");
    assert_fmt_eq!("let foo = bar.something;");
    assert_fmt_eq!("let foo = bar.something();");
    assert_fmt_eq!("let foo = bar.some.thing();");
    assert_fmt_eq!("let foo = self.something();");
    assert_fmt_eq!("self.ftok.tok == Token::Eof");
    assert_fmt_eq!("use syntax::parse::token;");
    assert_fmt_eq!("use syntax::parse::token::{Token, DelimToken, BinOpToken};");
    assert_fmt_eq!("use syntax::parse::token::{self, DelimToken, BinOpToken};");
    assert_fmt_eq!("pub use syntax::parse::token::{self, DelimToken, BinOpToken};");
    assert_fmt_eq!("let foo: &mut u32 = &mut 5");
    assert_fmt_eq!("let foo = &mut 5");
    assert_fmt_eq!("let a: &u32 = &5;");
    assert_fmt_eq!("let a = &5;");
    assert_fmt_eq!("self.level += 1;");
    assert_fmt_eq!("&63");
    assert_fmt_eq!("let b = func(&3, &2)");
    assert_fmt_eq!("Some(32).map(|a| a * 2)");
    assert_fmt_eq!("fn foo() -> u32 {}");
    assert_fmt_eq!("let a = 5 + 2 * 3 + 312. (5 * 2) / 1.5 / (123 + 62.21)");
    assert_fmt_eq!("let a = *fred;");
    assert_fmt_eq!("AnnotatingParser {
    line: line,
    context: vec![],
}.parse_line();");
    assert_fmt_eq!("token.is_any_keyword() && !token.is_keyword(Keyword::SelfType)");
    assert_fmt_eq!("replacements.retain(|r| r.text != lexer.src_str(r.start_byte, r.end_byte));");
    assert_fmt_eq!("a < b && c <= d && e > f && g >= h && i == j && k != l");
    assert_fmt_eq!("fn hi<T>() {}");
    assert_fmt_eq!("fn hi<T, Y>() {}");
    assert_fmt_eq!("fn hi<T, Y: T>() {}");
    assert_fmt_eq!("fn hi<'a>() {}");
    assert_fmt_eq!("fn hi(something: Here<T>) {}");
    assert_fmt_eq!("fn hi<'a>(something: Here<'a>) {}");
    assert_fmt_eq!("struct Foo<T> {\n    something: Bar<T>\n}");
    assert_fmt_eq!("struct Foo<T, Y> {\n    something: Bar<T, Y>\n}");
    assert_fmt_eq!("struct Foo<T, Y> {\n    something: Bar<T<Y>>\n}");
    assert_fmt_eq!("let a = someting::<T>();");
    assert_fmt_eq!("fn foot<'a>(t: &'a Bar) {}");
    assert_fmt_eq!("impl<T> TypedArenaChunk<T> {}");
    assert_fmt_eq!("fn foo(b: &Bar) -> &Baz {}");
    assert_fmt_eq!("let a = 5 & 6;");
    assert_fmt_eq!("pub use self::Diagnostic::*;");
}

#[test]
fn test_for_loop_indentation() {
    assert_fmt_eq!("for a in b {
    let cccc = dd;
    let ee = fffff;
}
let j = k;");
}

#[test]
fn test_attribute_on_function() {
    assert_fmt_eq!("#[test]\nfn test_attribute_on_function() {}");
}

#[test]
fn test_crate_attributes() {
    assert_fmt_eq!("#![feature(rustc_private)]\n#![feature(collections)]");
    assert_fmt_eq!("#![feature(rustc_private)]\n#![feature(collections)]\n\nfn hi() {}");
}

#[test]
fn test_extern_crate() {
    assert_fmt_eq!("extern crate std;\nfn hello() {}");
    assert_fmt_eq!("extern crate abc;\nextern crate def;\nfn hello() {}");
}

#[test]
fn test_mod() {
    assert_fmt_eq!("mod abc;");
    assert_fmt_eq!("mod abc;\nmod def;");
    assert_fmt_eq!("mod aaa;\nmod bbb;\nmod ccc as ddd;");
    assert_fmt_eq!("#[cfg(test)]
mod tests {
    extern crate test;
    use self::test::Bencher;
    use super::{Arena, TypedArena};

    #[allow(dead_code)]
    struct Point {
        x: i32,
        y: i32,
        z: i32,
    }

}");
}

#[test]
fn test_unsafe_fn() {
    assert_fmt_eq!("unsafe fn destroy_chunk(chunk: &Chunk) {
    let mut idx = 0;
    let buf = chunk.as_ptr();
    let fill = chunk.fill.get();
}

fn another_function() {
    let a = 2;
    let b = 5;
}");
}

#[test]
fn test_static_value() {
    assert_fmt_eq!("\
extern {
    pub static LLVMRustDebugMetadataVersion: u32;

    pub fn LLVMRustAddModuleFlag(M: ModuleRef);
}
");
}

#[test]
fn test_const_value() {
    assert_fmt_eq!("\
pub const INFINITY: f32 = 1;

pub const NEG_INFINITY: f32 = 2;
");
}
