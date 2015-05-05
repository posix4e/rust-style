use reformat;
use replacement::Replacement;
use std::default::Default;
use syntax;
use syntax::parse::token::Token;
use token::{FormatTokenLexer, Precedence, TokenType};
use unwrapped_line::UnwrappedLine;

// TODO: tests using different style options

fn fmt(source: &str) -> String {
    Replacement::apply_all(&replacements(source), source)
}

fn replacements(source: &str) -> Vec<Replacement> {
    let style = Default::default();
    super::reformat(source, &style, None)
}

fn annotated_lines(source: &str) -> Vec<UnwrappedLine> {
    let session = syntax::parse::new_parse_sess();
    let style = Default::default();
    let lexer = &mut FormatTokenLexer::new(source, &session, &style);
    super::internal::annotated_lines(lexer, &style)
}

macro_rules! assert_fmt_eq(
    ($s:expr) => (
        assert_eq!(fmt($s), $s)
    )
);

#[test]
#[should_panic]
fn test_assert_fmt_eq_should_panic() {
    assert_fmt_eq!("let a =5;");
}

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
    assert_fmt_eq!("let a = hello(Foo { a: 32, b: 50 });");
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
fn test_replacement_bytes() {
    let r = replacements("let a  =\n3;let a = 2;");
    assert_eq!(r[0].start_byte, 5);
    assert_eq!(r[0].end_byte, 7);
    assert_eq!(r[0].text, " ");
    assert_eq!(r[1].start_byte, 8);
    assert_eq!(r[1].end_byte, 9);
    assert_eq!(r[1].text, " ");
    assert_eq!(r[2].start_byte, 11);
    assert_eq!(r[2].end_byte, 11);
    assert_eq!(r[2].text, "\n");
    // 䶵 = 3 bytes
    let r = replacements("let 䶵  =\n3;let 䶵 = 2;");
    assert_eq!(r[0].start_byte, 7);
    assert_eq!(r[0].end_byte, 9);
    assert_eq!(r[0].text, " ");
    assert_eq!(r[1].start_byte, 10);
    assert_eq!(r[1].end_byte, 11);
    assert_eq!(r[1].text, " ");
    assert_eq!(r[2].start_byte, 13);
    assert_eq!(r[2].end_byte, 13);
    assert_eq!(r[2].text, "\n");
}

#[test]
fn test_replacement_characters() {
    // same as bytes
    let r = replacements("let a  =\n3;let a = 2;");
    assert_eq!(r[0].start_character, 5);
    assert_eq!(r[0].end_character, 7);
    assert_eq!(r[0].text, " ");
    assert_eq!(r[1].start_character, 8);
    assert_eq!(r[1].end_character, 9);
    assert_eq!(r[1].text, " ");
    assert_eq!(r[2].start_character, 11);
    assert_eq!(r[2].end_character, 11);
    assert_eq!(r[2].text, "\n");
    // 䶵 = 3 bytes
    let r = replacements("let 䶵  =\n3;let 䶵 = 2;");
    assert_eq!(r[0].start_character, 5);
    assert_eq!(r[0].end_character, 7);
    assert_eq!(r[0].text, " ");
    assert_eq!(r[1].start_character, 8);
    assert_eq!(r[1].end_character, 9);
    assert_eq!(r[1].text, " ");
    assert_eq!(r[2].start_character, 11);
    assert_eq!(r[2].end_character, 11);
    assert_eq!(r[2].text, "\n");
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
    assert_fmt_eq!("use {syntax, annotate, join, format};");
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
    assert_fmt_eq!("AnnotatingParser { line: line, context: vec![], }.parse_line();");
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
    assert_fmt_eq!("let a: Vec<u32> = vec![];");
    assert_fmt_eq!("let a: Vec<Vec<u32>> = vec![];");
    assert_fmt_eq!("pub type NodeSet = FnvHashSet<ast::NodeId>;");
    assert_fmt_eq!("type Foo<T> = Bar<T>;");
    assert_fmt_eq!("type Foo<Bar<T, Y>> = Baz<T>;");
    assert_fmt_eq!("unsafe fn as_ptr(&self) -> *const u8 {}");
    assert_fmt_eq!("head.fill.set(round_up(end, mem::align_of::<*const TyDesc>()));");
    assert_fmt_eq!("unsafe fn new(next: *mut A<T>, f: usize) -> *mut B<T>");
    assert_fmt_eq!("for ftok in &self.line.tokens {");
    assert_fmt_eq!("let &Tup(ref val) = self;");
    assert_fmt_eq!("n @ 1...12 => Foo()");
    assert_fmt_eq!("let a = &mut **self._ptr");
    assert_fmt_eq!("((p & !1) as *const TyDesc, p & 1 == 1)");
    assert_fmt_eq!("impl<'a, K, V> IntoIterator for &'a mut BTreeMap<K, V> {}");
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
    assert_fmt_eq!("\
#[cfg(test)]
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

#[test]
fn test_break_after_comment_trailing_comment() {
    assert_fmt_eq!("\
let a = 3 + // hello
        5;");

assert_fmt_eq!("\
let a = 3 // hello
        + 5;");

assert_fmt_eq!("\
let a = 3 /* hello */
        + 5;");
}

#[test]
fn test_consecutive_comments() {
    assert_fmt_eq!("// Comment 1\n// Comment 2");
}

#[test]
fn test_precedence_token() {
    let tok = &annotated_lines("53 + 21")[0].tokens;
    assert_eq!(tok[0].starts_binary_expression, true);
    assert_eq!(tok[1].starts_binary_expression, false);
    assert_eq!(tok[2].starts_binary_expression, false);

    assert_eq!(tok[0].ends_binary_expression, false);
    assert_eq!(tok[1].ends_binary_expression, false);
    assert_eq!(tok[2].ends_binary_expression, true);

    assert_eq!(tok[0].fake_lparens, &[Precedence::Additive]);
    assert_eq!(tok[1].fake_lparens, &[]);
    assert_eq!(tok[2].fake_lparens, &[]);

    assert_eq!(tok[0].fake_rparens, 0);
    assert_eq!(tok[1].fake_rparens, 0);
    assert_eq!(tok[2].fake_rparens, 1);


    let tok = &annotated_lines("1 + 3 * 6")[0].tokens;
    assert_eq!(tok[0].starts_binary_expression, true);
    assert_eq!(tok[1].starts_binary_expression, false);
    assert_eq!(tok[2].starts_binary_expression, true);
    assert_eq!(tok[3].starts_binary_expression, false);
    assert_eq!(tok[4].starts_binary_expression, false);

    assert_eq!(tok[0].ends_binary_expression, false);
    assert_eq!(tok[1].ends_binary_expression, false);
    assert_eq!(tok[2].ends_binary_expression, false);
    assert_eq!(tok[3].ends_binary_expression, false);
    assert_eq!(tok[4].ends_binary_expression, true);

    assert_eq!(tok[0].fake_lparens, &[Precedence::Additive]);
    assert_eq!(tok[1].fake_lparens, &[]);
    assert_eq!(tok[2].fake_lparens, &[Precedence::Multiplictive]);
    assert_eq!(tok[3].fake_lparens, &[]);
    assert_eq!(tok[4].fake_lparens, &[]);

    assert_eq!(tok[0].fake_rparens, 0);
    assert_eq!(tok[1].fake_rparens, 0);
    assert_eq!(tok[2].fake_rparens, 0);
    assert_eq!(tok[3].fake_rparens, 0);
    assert_eq!(tok[4].fake_rparens, 2);

    let tok = &annotated_lines("1 * 3 + 6")[0].tokens;
    assert_eq!(tok[0].starts_binary_expression, true);
    assert_eq!(tok[1].starts_binary_expression, false);
    assert_eq!(tok[2].starts_binary_expression, false);
    assert_eq!(tok[3].starts_binary_expression, false);
    assert_eq!(tok[4].starts_binary_expression, false);

    assert_eq!(tok[0].ends_binary_expression, false);
    assert_eq!(tok[1].ends_binary_expression, false);
    assert_eq!(tok[2].ends_binary_expression, true);
    assert_eq!(tok[3].ends_binary_expression, false);
    assert_eq!(tok[4].ends_binary_expression, true);

    assert_eq!(tok[0].fake_lparens, &[Precedence::Multiplictive, Precedence::Additive]);
    assert_eq!(tok[1].fake_lparens, &[]);
    assert_eq!(tok[2].fake_lparens, &[]);
    assert_eq!(tok[3].fake_lparens, &[]);
    assert_eq!(tok[4].fake_lparens, &[]);

    assert_eq!(tok[0].fake_rparens, 0);
    assert_eq!(tok[1].fake_rparens, 0);
    assert_eq!(tok[2].fake_rparens, 1);
    assert_eq!(tok[3].fake_rparens, 0);
    assert_eq!(tok[4].fake_rparens, 1);
}

#[test]
fn test_precedence_format() {
    assert_fmt_eq!("\
let a = 111 * 222 * 333 * 444 * 555 * 111 * 222 * 333 * 444 +
        111 * 222 * 333 * 444 * 555 * 111 * 222 * 333 * 444 +
        111 * 222 * 333 * 444 * 555 * 111 * 222 * 333 * 444 +
        111 * 222 * 333 * 444 * 555 * 111 * 222 * 333 * 444 +
        111 * 222 * 333 * 444 * 555 * 111 * 222 * 333 * 444 +
        111 * 222 * 333 * 444 * 555 * 111 * 222 * 333 * 444 +
        111 * 222 * 333 * 444 * 555 * 111 * 222 * 333 * 444;");

    assert_fmt_eq!("\
let a = (111 + 222 + 333 + 444 + 111 + 222 + 333 + 444 + 555) *
        (111 + 222 + 333 + 444 + 111 + 222 + 333 + 444 + 555) *
        (111 + 222 + 333 + 444 + 111 + 222 + 333 + 444 + 555) *
        (111 + 222 + 333 + 444 + 111 + 222 + 333 + 444 + 555) *
        (111 + 222 + 333 + 444 + 111 + 222 + 333 + 444 + 555) *
        (111 + 222 + 333 + 444 + 111 + 222 + 333 + 444 + 555) *
        (111 + 222 + 333 + 444 + 111 + 222 + 333 + 444 + 555);");
}

#[test]
fn test_borrow_in_arguments() {
    assert_fmt_eq!("\
self.append_indent_text(text, indent_level, cmp::max(0, c.spaces),
                        c.start_of_token_column - cmp::max(0, c.spaces));");
    assert_fmt_eq!("\
self.append_indent_text(text, indent_level, !cmp::max(0, c.spaces),
                        c.start_of_token_column - cmp::max(0, c.spaces));");
    assert_fmt_eq!("\
self.append_indent_text(text, indent_level, &cmp::max(0, c.spaces),
                        c.start_of_token_column - cmp::max(0, c.spaces));");
    assert_fmt_eq!("\
self.append_indent_text(text, &indent_level, cmp::max(0, c.spaces),
                        c.start_of_token_column - cmp::max(0, c.spaces));");
    assert_fmt_eq!("\
self.append_indent_text(&text, indent_level, cmp::max(0, c.spaces),
                        c.start_of_token_column - cmp::max(0, c.spaces));");
    assert_fmt_eq!("\
self.append_indent_text(&text, !!indent_level, &cmp::max(0, c.spaces),
                        c.start_of_token_column - cmp::max(0, c.spaces));");
}

#[test]
fn test_argument_alignment_shr_generics() {
    assert_fmt_eq!("\
deeeeeeeeeeeeeeeeeeeeeeeeeeallocate(ptr as *mut u8, size_of::<RcBox<T>>(),
                                    min_align_of::<RcBox<T>>())");
}

#[test]
fn test_continuation_indent() {
    assert_fmt_eq!("\
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa +
        bbbbbbbbbbbbbbbbbbbbbbbb");
    assert_fmt_eq!("\
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa(bbbbbbbbbbbbbbbbbbbbbb,
                                                                cccccccccccccc)");
    assert_fmt_eq!("\
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa(bbbbbbbbbbbbbbbbbbbbbb, cccccccccccccc,
                                                        ddddddddddddd)");
    assert_fmt_eq!("\
let join = lines.len() >= 2 && lines[0].children.is_empty() &&
           lines[0].tokens.last().unwrap().tok.very_looooooooooooong_dummy_name ==
                   Token::OpenDelim(DelimToken::Brace) &&
           lines[1].tokens.first().unwrap().tok.very_looooooooooooong_dummy_name ==
                   Token::CloseDelim(DelimToken::Brace);");
}

#[test]
fn test_exern_block() {
    assert_fmt_eq!("\
extern {
    fn fopen() -> FILE;
}

fn foo() {}");

    assert_fmt_eq!("\
extern \"stdcall\" {
    fn fopen() -> FILE;
}

fn foo() {}");
}

#[test]
fn test_dereference_spacing_in_match() {
    assert_fmt_eq!("\
match *self {
    Context::LambdaArgs => 10,
    Context::LambdaArgs => 10,
}");
    assert_fmt_eq!("\
match &self {
    Context::LambdaArgs => 10,
    Context::LambdaArgs => 10,
}");
}

#[test]
fn test_unary_negation() {
    assert_fmt_eq!("let a = -3;");
    assert_fmt_eq!("x + -3;");
    assert_fmt_eq!("if x == -3;");
}

#[test]
fn test_use_brace_break_indentation() {
    assert_fmt_eq!("use token::{FormatToken, TokenType, Precedence, PRECEDENCE_UNARY}");
    assert_fmt_eq!("\
use token::very_looooooooooong_dummy_name::{FormatToken, TokenType, Precedence, PRECEDENCE_UNARY,
                                            PRECEDENCE_DOT}");
    assert_fmt_eq!("\
use super::token::very_long_dummy_name::{FormatToken, TokenType, Precedence, PRECEDENCE_UNARY,
                                         PRECEDENCE_DOT}");
    assert_fmt_eq!("\
use {Something, Very_long_dummy_name, More, FormatToken, TokenType, Precedence, PRECEDENCE_UNARY,
     PRECEDENCE_DOT, Foo, Bar, Baz}");
    assert_fmt_eq!("\
use {Something, Very_long_dummy_name, More, FormatToken, TokenType, Precedence, PRECEDENCE_UNARY,
     PRECEDENCE_DOT, Foo, Bar, Baz, Box, Biz, Ben, What, Else, Can, I, Write, Just, A, Few, More,
     Words}");
}

#[test]
fn test_spacing_after_as() {
    assert_fmt_eq!("foo as &mut Bar<T>");
}

#[test]
fn test_operator_spacing_generics() {
    assert_fmt_eq!("impl<Unit, T: Add<T, Output = T> + Clone + Copy> Add<Length<Unit, T>>");
}


#[test]
fn test_binding_strength_cumulative_effect() {
    let tok = &annotated_lines("53 + 21")[0].tokens;
    assert_eq!(0, tok[0].binding_strength);
    assert_eq!(0, tok[1].binding_strength);
    assert_eq!(0, tok[2].binding_strength);

    let tok = &annotated_lines("(53 + 21) + 0")[0].tokens;
    let paren_strength = tok[2].binding_strength;
    assert!(paren_strength > 0);
    assert_eq!(tok[0].binding_strength, 0);
    assert_eq!(tok[1].binding_strength, paren_strength);
    assert_eq!(tok[2].binding_strength, paren_strength);
    assert_eq!(tok[3].binding_strength, paren_strength);
    assert_eq!(tok[4].binding_strength, paren_strength);
    assert_eq!(tok[5].binding_strength, 0);
    assert_eq!(tok[6].binding_strength, 0);

    let tok = &annotated_lines("(53 + (5 + 2)) + 2")[0].tokens;
    assert_eq!(tok[0].binding_strength, 0);
    assert_eq!(tok[1].binding_strength, paren_strength);
    assert_eq!(tok[2].binding_strength, paren_strength);
    assert_eq!(tok[3].binding_strength, paren_strength);
    assert_eq!(tok[4].binding_strength, paren_strength * 2);
    assert_eq!(tok[5].binding_strength, paren_strength * 2);
    assert_eq!(tok[6].binding_strength, paren_strength * 2);
    assert_eq!(tok[7].binding_strength, paren_strength * 2);
    assert_eq!(tok[8].binding_strength, paren_strength);
    assert_eq!(tok[9].binding_strength, 0);
    assert_eq!(tok[10].binding_strength, 0);
}

#[test]
fn test_binding_strength_affects_split_penalty() {
    // The split penalty between dddd and eeee should be higher, because it has
    // a higher level of nesting. It should break between bbbb and cccc instead.
    assert_fmt_eq!("\
aaaaaaaaaaaaaaaaaaaaaaaaaaaaa(bbbbbbbbbbbbbbbbbbbbbbbbbbbbb,
                              cccccccccccccccccc(ddddddddddddddd, eeeeeeeeeee));
");
}

#[test]
fn test_deeply_nested_generics() {
    assert_fmt_eq!("type Something = Foo<T<U<V<W>>>, X>;");
}

#[test]
fn test_spacing_single_bigger_than() {
    assert_fmt_eq!("self.current_index < self.line.tokens.len()");
}

#[test]
fn test_spacing_in_macro_rules() {
    assert_fmt_eq!("\
macro_rules! assert_fmt_eq {
    ($s: expr) => (
        assert_eq!(fmt($s), $s)
    )
}

fn foo() {
    let a = 12;
}");
}

#[test]
fn test_spacing_macro_corner_cases() {
    assert_fmt_eq!("\
fn foo() -> bool {
    let bar = true;
    return !bar;
}");
}

#[test]
fn test_spacing_dot_dot_struct_pattern() {
    assert_fmt_eq!("\
match something {
    FormatToken { ref tok, ..} => true,
    _ => false,
}");
}

#[test]
fn test_let_struct_pattern() {
    assert_fmt_eq!("let IsBeforeInFile(Span { lo: BytePos(lo_a), ..}) = self;");
    assert_fmt_eq!("\
let IsBeforeInFile(Span { lo: BytePos(lo_a), ..}) = self;
let IsBeforeInFile(Span { lo: BytePos(lo_a), ..}) = self;");
}

#[test]
fn test_let_declaration_only() {
    assert_fmt_eq!("\
let a = 2;
let b;
let c;
let b = 3;");
}

#[test]
fn test_enum_with_struct_variants() {
    assert_fmt_eq!("\
enum Something {
    StructVariant1 { a: u32, b: u32 },
    StructVariant2 { a: u32, b: u32 },
    TupleVariant(u32, u32),
}");
}

#[test]
fn test_fn_declaration_with_struct_pattern() {
    assert_fmt_eq!("fn foo(Bar { baz: baz }: Bar) {}");
    assert_fmt_eq!("fn foo(Bar { baz: baz }: Bar, Bar { baz: baz }: Bar) {}");
}

#[test]
fn test_macro_rule_trailing_semicolon() {
    assert_fmt_eq!("\
macro_rules! min {
    ($x: expr) => (
        $x
    );
}");
}

#[test]
fn test_fixed_size_array() {
    assert_fmt_eq!("let bytes: [u8; 8] = blah;");
    assert_fmt_eq!("fn hello(a: [u8; 8]) {}");
    assert_fmt_eq!("fn hello() -> [u8; 8] {}");
    assert_fmt_eq!("struct Hello {
    something: [u8; 8],
    something: [u8; 8],
}");
}

#[test]
fn test_higher_precedence_has_higher_penalty() {
    // EqEq should have lower a penalty, so it breaks after == instead of +
    assert_fmt_eq!("\
ccccccccccccccccc + cccccccccccccccccccccccccccccccc ==
        dddddddddddddddddddd + ddddddddddddddddddddddd");
}

#[test]
fn test_line_break_between_patterns() {
assert_fmt_eq!("\
match self.current().tok {
    Token::Eq | Token::Le | Token::EqEq | Token::Ne | Token::Ge | Token::Gt | Token::AndAnd
    | Token::OrOr | Token::BinOp(..) | Token::BinOpEq(..) => { TokenType::BinaryOperator }
}");
}

#[test]
fn test_binary_or_in_pattern_guard() {
    let lines = annotated_lines("match foo { A | B if c | d == 2 => () }");
    let toks = &lines[0].tokens[2].children[0].tokens;
    assert_eq!(toks[1].typ, TokenType::PatternOr);
    assert_eq!(toks[5].typ, TokenType::BinaryOperator);
    assert_eq!(toks[7].typ, TokenType::BinaryOperator);
}

#[test]
fn test_empty_match_block_has_no_children() {
    let lines = annotated_lines("match foo { bar => {} }");
    let arm = &lines[0].tokens[2].children[0];
    assert_eq!(arm.tokens.len(), 4);
    assert_eq!(arm.tokens[0].children.len(), 0);
    assert_eq!(arm.tokens[1].children.len(), 0);
    assert_eq!(arm.tokens[2].children.len(), 0);
    assert_eq!(arm.tokens[3].children.len(), 0);
}

#[test]
fn test_fn_decl_arrow_annotated() {
    let lines = annotated_lines("fn aaaa() -> bbb {}");
    let toks = &lines[0].tokens;
    assert_eq!(toks[4].typ, TokenType::FnDeclArrow);
    assert_eq!(toks[4].tok, Token::RArrow);

    let lines = annotated_lines("fn aaaa<P>() -> bbb where P: Fn() -> ccc {}");
    let toks = &lines[0].tokens;
    assert_eq!(toks[7].typ, TokenType::FnDeclArrow);
    assert_eq!(toks[7].tok, Token::RArrow);
    assert_eq!(toks[15].typ, TokenType::Unknown);
    assert_eq!(toks[15].tok, Token::RArrow);

    let lines = annotated_lines("fn aaaa<P: Fn() -> ccc>() -> bbb {}");
    let toks = &lines[0].tokens;
    assert_eq!(toks[8].typ, TokenType::Unknown);
    assert_eq!(toks[8].tok, Token::RArrow);
    assert_eq!(toks[13].typ, TokenType::FnDeclArrow);
    assert_eq!(toks[13].tok, Token::RArrow);

    let lines = annotated_lines("fn aaaa() -> fn() -> u32 {}");
    let toks = &lines[0].tokens;
    assert_eq!(toks[4].typ, TokenType::FnDeclArrow);
    assert_eq!(toks[4].tok, Token::RArrow);
    assert_eq!(toks[8].typ, TokenType::Unknown);
    assert_eq!(toks[8].tok, Token::RArrow);
}

#[test]
fn test_struct_init_all_or_nothing() {
    assert_fmt_eq!("let a = Foo { one: 111111 };");
    assert_fmt_eq!("let a = Foo { one: 111111, two: 22222222 };");
    assert_fmt_eq!("let a = Foo { one: 111111, two: 22222222, three: 333333333333333 };");
    assert_fmt_eq!("let a = Foo { one: 111111, two: 22222222, three: 333333333333333 };");

    let input1 =
        "let a = Foo { one: 1111111111111, two: 22222222222, three: 333333333333333, four: 444444444444444444444 };";
    let input2 =
        "let a = Foo { one: 1111111111111, two: 22222222222,
            three: 333333333333333, four: 444444444444444444444 };";
    let expected = "let a = Foo {
    one: 1111111111111,
    two: 22222222222,
    three: 333333333333333,
    four: 444444444444444444444
};";

    assert_eq!(fmt(input1), expected);
    assert_eq!(fmt(input2), expected);
    assert_fmt_eq!(expected);

    assert_fmt_eq!("let a = call(Foo { one: 111111, two: 22222222, three: 333333333333333 });");
    assert_fmt_eq!("\
let a = call(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,
             Foo { one: 111111, two: 22222222, three: 333333333333333 });");
    assert_fmt_eq!("\
let a = call(Foo { one: 111111, two: 22222222, three: 333333333333333 },
             aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa);");
    assert_fmt_eq!("\
let a = call(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, Foo {
    one: 111111,
    two: 22222222,
    three: 333333333333333,
    four: 444444444444444444444
});");

    assert_fmt_eq!("\
let a = call(Foo {
    one: 1111111111111,
    two: 22222222222,
    three: 333333333333333,
    four: call(Foo {
        one: 1111111111111,
        two: 22222222222,
        three: 333333333333333,
        four: 444444444444444444444
    }),
});");
}

#[test]
fn test_fn_type() {
    assert_fmt_eq!("\
struct TyDesc {
    drop_glue: fn(*const i8),
    size: usize,
    align: usize
}");
    assert_fmt_eq!("let a: fn(*const i8) = b;");
    assert_fmt_eq!("fn a(b: fn(*const i8)) {}");
    assert_fmt_eq!("fn a() -> fn(*const i8) {}");
}

#[test]
fn test_macro_invokation_brace() {
    assert_fmt_eq!("\
from_str_radix_float_impl! {
    f32
}

fn hello() {}");

    assert_fmt_eq!("\
impl_eq! {
    Cow<'a, str>, String
}

#[stable(feature = \"rust1\", since = \"1.0.0\")]
impl fmt::Display for String {}");
}

#[test]
fn test_generics_in_as() {
    assert_fmt_eq!("\
let data = match self.edges {
    None => heap::EMPTY as *const Node<K, V>,
    Some(ref p) => **p as *const Node<K, V>,
};");
}

#[test]
fn test_complex_generics() {
    assert_fmt_eq!("impl<T: Clone, V: AsRef<[T]>> SliceConcatExt<T, Vec<T>> for [V] {}");
    assert_fmt_eq!("struct InvariantLifetime<'id>(marker::PhantomData<::core::cell::Cell<&'id ()>>);");
}

#[test]
fn test_method_chaining() {
    assert_fmt_eq!("\
let a = iter::repeat(1.0f32)
            .take(BENCH_SIZE)
            .something()
            .something()
            .something()
            .something()
            .collect();");

    assert_fmt_eq!("let a = iter::repeat(1.0f32).take(BENCH_SIZE).collect();");
    assert_fmt_eq!("\
let args: Args = Docopt::new(USAGE)
                     .and_then(|d| d.version(Some(version)).help(true).decode())
                     .unwrap_or_else(|e| e.exit());");
}

#[test]
fn test_continuation_indent_after_non_value_keyword() {
    assert_fmt_eq!("\
if aaaaaaaaaaaaaaa && bbbbbbbbbb && ccccccccccccccccccccccccccccccccccccccccccccc &&
           dddddddddddddddddd {
    let a = 1;
}");
    assert_fmt_eq!("\
while aaaaaaaaaaaaaaa && bbbbbbbbbb && ccccccccccccccccccccccccccccccccccccccccccccc &&
              dddddddddddddddddd {
    let a = 1;
}");
}

#[test]
fn test_match_inside_struct_init() {
    assert_fmt_eq!("\
let new_context = Context {
    binding_strength: self.context_binding_strength() + match typ {
        ContextType::Parens => 1,
        ContextType::Generics => 10,
        ContextType::LambdaParams => 10,
        ContextType::WhereClause => 10,
    },
    typ: typ,
};");
}

#[test]
fn test_unsafe_block_in_match() {
    assert_fmt_eq!("\
match foo {
    bar => unsafe {
        let a = 2;
        let b = 3;
    }
}");
    assert_fmt_eq!("\
match foo {
    bar => unsafe {
        let a = 2;
        let b = 3;
    },
    baz => {
        let a = 3;
        let b = 2;
    }
}");
}

#[test]
fn test_fn_return_arrow_position() {
    assert_fmt_eq!("\
pub fn add_token_to_state(&self, line: &mut UnwrappedLine, state: &mut LineState, newline: bool,
                          dry_run: bool, whitespace: &mut WhitespaceManager, something: bool)
                          -> Penalty {
    let a = 2;
}");

    assert_fmt_eq!("\
pub fn add_token_to_state(&self, line: &mut UnwrappedLine, state: &mut LineState, newline: bool,
                          dry_run: bool, whitespace: &mut WhitespaceManager) -> Penalty {
    let a = 2;
}");

    // This may or may not be the desired formatting.
    assert_fmt_eq!("\
pub fn add_token_to_state(&self, line: &mut UnwrappedLine, state: &mut LineState, newline: bool)
                          -> Penalty {
    let a = 2;
}");
}
