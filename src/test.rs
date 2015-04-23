use reformat;
use replacement::Replacement;
use std::default::Default;
use syntax;
use token::{FormatTokenLexer, Precedence};
use unwrapped_line::UnwrappedLine;

// TODO: tests using different style options

fn fmt(source: &str) -> String {
    Replacement::apply_all(&replacements(source), source)
}

fn replacements(source: &str) -> Vec<Replacement> {
    let style = Default::default();
    super::reformat(source, &style)
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
    assert_fmt_eq!("let a: Vec<u32> = vec![];");
    assert_fmt_eq!("let a: Vec<Vec<u32>> = vec![];");
    assert_fmt_eq!("pub type NodeSet = FnvHashSet<ast::NodeId>;");
    assert_fmt_eq!("type Foo<T> = Bar<T>;");
    assert_fmt_eq!("type Foo<Bar<T, Y>> = Baz<T>;");
    assert_fmt_eq!("unsafe fn as_ptr(&self) -> *const u8 {}");
    assert_fmt_eq!("head.fill.set(round_up(end, mem::align_of::<*const TyDesc>()));");
    assert_fmt_eq!("unsafe fn new(next: *mut A<T>, f: usize) -> *mut B<T>");
    assert_fmt_eq!("for ftok in &self.line.tokens {");
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

// // FIXME: these cases produce very bad input currently
// #[test]
// fn test_continuation_indent_control_structures() {
//     assert_fmt_eq!("\
// match something {
//     &Token::Ident(..)) | (&Token::Gt, &Token::Ident(..))
//         if prev.typ == TokenType::GenericBracket => true,
// }
// ");
//     assert_fmt_eq!("\
// if aaaaaaaaaaaaaaaaa == bbbbbbbbbbbbbbbbbbbbbbbbbbbb ||
//    cccccccccccccccccc || ddddddddddddddddddddddddddddd {
//     indent = state.column;
// }");
// }

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
}");
    assert_fmt_eq!("\
match &self {
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