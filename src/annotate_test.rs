use source::Source;
use syntax::parse::token::{Token, DelimToken};
use token::{Precedence, TokenType};
use unwrapped_line::UnwrappedLine;

fn annotated_lines(source: &str) -> Vec<UnwrappedLine> {
    let style = super::FormatStyle::default();
    let source = Source::new(source);
    super::internal::annotated_lines(&source, &style)
}

#[test]
fn test_fake_parens_around_if_condition() {
    for kw in ["if", "match", "while"].iter() {
        let string = format!("{} a && b {{ }}", kw);
        let tok = &annotated_lines(&string)[0].tokens;
        assert_eq!(tok[0].starts_binary_expression, false);
        assert_eq!(tok[1].starts_binary_expression, true);
        assert_eq!(tok[2].starts_binary_expression, false);
        assert_eq!(tok[3].starts_binary_expression, false);
        assert_eq!(tok[4].starts_binary_expression, false);
        assert_eq!(tok[5].starts_binary_expression, false);

        assert_eq!(tok[0].ends_binary_expression, false);
        assert_eq!(tok[1].ends_binary_expression, false);
        assert_eq!(tok[2].ends_binary_expression, false);
        assert_eq!(tok[3].ends_binary_expression, true);
        assert_eq!(tok[4].ends_binary_expression, false);
        assert_eq!(tok[5].ends_binary_expression, false);

        assert_eq!(tok[0].fake_lparens, &[]);
        assert_eq!(tok[1].fake_lparens, &[Precedence::LogicalAnd]);
        assert_eq!(tok[2].fake_lparens, &[]);
        assert_eq!(tok[3].fake_lparens, &[]);
        assert_eq!(tok[4].fake_lparens, &[]);
        assert_eq!(tok[5].fake_lparens, &[]);

        assert_eq!(tok[0].fake_rparens, 0);
        assert_eq!(tok[1].fake_rparens, 0);
        assert_eq!(tok[2].fake_rparens, 0);
        assert_eq!(tok[3].fake_rparens, 1);
        assert_eq!(tok[4].fake_rparens, 0);
        assert_eq!(tok[5].fake_rparens, 0);
    }
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
fn test_binary_expr_in_match_guard() {
    let lines = annotated_lines("match whatever { A | B if b == c && d && e => {} }");
    let tok = &lines[0].tokens[2].children[0].tokens;

    assert_eq!(tok[0].fake_lparens, &[Precedence::PatternOr]);
    assert_eq!(tok[1].fake_lparens, &[]);
    assert_eq!(tok[2].fake_lparens, &[]);
    assert_eq!(tok[3].fake_lparens, &[]);
    assert_eq!(tok[4].fake_lparens, &[Precedence::Relational, Precedence::LogicalAnd]);
    assert_eq!(tok[5].fake_lparens, &[]);
    assert_eq!(tok[6].fake_lparens, &[]);
    assert_eq!(tok[7].fake_lparens, &[]);
    assert_eq!(tok[8].fake_lparens, &[]);
    assert_eq!(tok[9].fake_lparens, &[]);
    assert_eq!(tok[10].fake_lparens, &[]);
    assert_eq!(tok[11].fake_lparens, &[]);
    assert_eq!(tok[12].fake_lparens, &[]);
    assert_eq!(tok[13].fake_lparens, &[]);

    assert_eq!(tok[0].fake_rparens, 0);
    assert_eq!(tok[1].fake_rparens, 0);
    assert_eq!(tok[2].fake_rparens, 1);
    assert_eq!(tok[3].fake_rparens, 0);
    assert_eq!(tok[4].fake_rparens, 0);
    assert_eq!(tok[5].fake_rparens, 0);
    assert_eq!(tok[6].fake_rparens, 1);
    assert_eq!(tok[7].fake_rparens, 0);
    assert_eq!(tok[8].fake_rparens, 0);
    assert_eq!(tok[9].fake_rparens, 0);
    assert_eq!(tok[10].fake_rparens, 1);
    assert_eq!(tok[11].fake_rparens, 0);
    assert_eq!(tok[12].fake_rparens, 0);
    assert_eq!(tok[13].fake_rparens, 0);


    let lines = annotated_lines("match whatever { A if b == c && !d.e => {} }");
    let tok = &lines[0].tokens[2].children[0].tokens;

    assert_eq!(tok[0].fake_lparens, &[]);
    assert_eq!(tok[1].fake_lparens, &[]);
    assert_eq!(tok[2].fake_lparens, &[Precedence::Relational, Precedence::LogicalAnd]);
    assert_eq!(tok[3].fake_lparens, &[]);
    assert_eq!(tok[4].fake_lparens, &[]);
    assert_eq!(tok[5].fake_lparens, &[]);
    assert_eq!(tok[6].fake_lparens, &[Precedence::Unknown]);
    assert_eq!(tok[7].fake_lparens, &[Precedence::Unknown]);
    assert_eq!(tok[8].fake_lparens, &[]);
    assert_eq!(tok[9].fake_lparens, &[]);
    assert_eq!(tok[10].fake_lparens, &[]);
    assert_eq!(tok[11].fake_lparens, &[]);
    assert_eq!(tok[12].fake_lparens, &[]);

    assert_eq!(tok[0].fake_rparens, 0);
    assert_eq!(tok[1].fake_rparens, 0);
    assert_eq!(tok[2].fake_rparens, 0);
    assert_eq!(tok[3].fake_rparens, 0);
    assert_eq!(tok[4].fake_rparens, 1);
    assert_eq!(tok[5].fake_rparens, 0);
    assert_eq!(tok[6].fake_rparens, 0);
    assert_eq!(tok[7].fake_rparens, 0);
    assert_eq!(tok[8].fake_rparens, 0);
    assert_eq!(tok[9].fake_rparens, 3);
    assert_eq!(tok[10].fake_rparens, 0);
    assert_eq!(tok[11].fake_rparens, 0);
    assert_eq!(tok[12].fake_rparens, 0);
}



#[test]
fn test_binding_strength_cumulative_effect() {
    let tok = &annotated_lines("53 + 21")[0].tokens;
    assert_eq!(1, tok[0].binding_strength);
    assert_eq!(1, tok[1].binding_strength);
    assert_eq!(1, tok[2].binding_strength);

    let tok = &annotated_lines("(53 + 21) + 0")[0].tokens;
    let paren_strength = tok[2].binding_strength - 1;
    assert!(paren_strength > 0);
    assert_eq!(tok[0].binding_strength, 1);
    assert_eq!(tok[1].binding_strength, 1 + paren_strength);
    assert_eq!(tok[2].binding_strength, 1 + paren_strength);
    assert_eq!(tok[3].binding_strength, 1 + paren_strength);
    assert_eq!(tok[4].binding_strength, 1 + paren_strength);
    assert_eq!(tok[5].binding_strength, 1);
    assert_eq!(tok[6].binding_strength, 1);

    let tok = &annotated_lines("(53 + (5 + 2)) + 2")[0].tokens;
    assert_eq!(tok[0].binding_strength, 1);
    assert_eq!(tok[1].binding_strength, 1 + paren_strength);
    assert_eq!(tok[2].binding_strength, 1 + paren_strength);
    assert_eq!(tok[3].binding_strength, 1 + paren_strength);
    assert_eq!(tok[4].binding_strength, 1 + paren_strength * 2);
    assert_eq!(tok[5].binding_strength, 1 + paren_strength * 2);
    assert_eq!(tok[6].binding_strength, 1 + paren_strength * 2);
    assert_eq!(tok[7].binding_strength, 1 + paren_strength * 2);
    assert_eq!(tok[8].binding_strength, 1 + paren_strength);
    assert_eq!(tok[9].binding_strength, 1);
    assert_eq!(tok[10].binding_strength, 1);
}



#[test]
fn test_binary_or_in_pattern_guard() {
    let lines = annotated_lines("match foo { A | B if c | d == 2 => () }");
    let toks = &lines[0].tokens[2].children[0].tokens;
    assert_eq!(toks[1].typ, TokenType::PatternOr);
    assert_eq!(toks[3].typ, TokenType::PatternGuardIf);
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
fn test_fn_decl_annotated() {
    let lines = annotated_lines("fn aaaa() -> bbb {}");
    let toks = &lines[0].tokens;
    assert_eq!(toks[2].typ, TokenType::FnDeclParamsStart);
    assert_eq!(toks[2].tok, Token::OpenDelim(DelimToken::Paren));
    assert_eq!(toks[3].typ, TokenType::FnDeclParamsEnd);
    assert_eq!(toks[3].tok, Token::CloseDelim(DelimToken::Paren));
    assert_eq!(toks[4].typ, TokenType::FnDeclArrow);
    assert_eq!(toks[4].tok, Token::RArrow);

    let lines = annotated_lines("fn aaaa<P>() -> bbb where P: Fn() -> ccc {}");
    let toks = &lines[0].tokens;
    assert_eq!(toks[5].typ, TokenType::FnDeclParamsStart);
    assert_eq!(toks[5].tok, Token::OpenDelim(DelimToken::Paren));
    assert_eq!(toks[6].typ, TokenType::FnDeclParamsEnd);
    assert_eq!(toks[6].tok, Token::CloseDelim(DelimToken::Paren));
    assert_eq!(toks[7].typ, TokenType::FnDeclArrow);
    assert_eq!(toks[7].tok, Token::RArrow);
    assert_eq!(toks[13].typ, TokenType::Unknown);
    assert_eq!(toks[13].tok, Token::OpenDelim(DelimToken::Paren));
    assert_eq!(toks[14].typ, TokenType::Unknown);
    assert_eq!(toks[14].tok, Token::CloseDelim(DelimToken::Paren));
    assert_eq!(toks[15].typ, TokenType::Unknown);
    assert_eq!(toks[15].tok, Token::RArrow);

    let lines = annotated_lines("fn aaaa<P: Fn() -> ccc>() -> bbb {}");
    let toks = &lines[0].tokens;
    assert_eq!(toks[6].typ, TokenType::Unknown);
    assert_eq!(toks[6].tok, Token::OpenDelim(DelimToken::Paren));
    assert_eq!(toks[7].typ, TokenType::Unknown);
    assert_eq!(toks[7].tok, Token::CloseDelim(DelimToken::Paren));
    assert_eq!(toks[8].typ, TokenType::Unknown);
    assert_eq!(toks[8].tok, Token::RArrow);
    assert_eq!(toks[11].typ, TokenType::FnDeclParamsStart);
    assert_eq!(toks[11].tok, Token::OpenDelim(DelimToken::Paren));
    assert_eq!(toks[12].typ, TokenType::FnDeclParamsEnd);
    assert_eq!(toks[12].tok, Token::CloseDelim(DelimToken::Paren));
    assert_eq!(toks[13].typ, TokenType::FnDeclArrow);
    assert_eq!(toks[13].tok, Token::RArrow);

    let lines = annotated_lines("fn aaaa() -> fn() -> u32 {}");
    let toks = &lines[0].tokens;
    assert_eq!(toks[2].typ, TokenType::FnDeclParamsStart);
    assert_eq!(toks[2].tok, Token::OpenDelim(DelimToken::Paren));
    assert_eq!(toks[3].typ, TokenType::FnDeclParamsEnd);
    assert_eq!(toks[3].tok, Token::CloseDelim(DelimToken::Paren));
    assert_eq!(toks[4].typ, TokenType::FnDeclArrow);
    assert_eq!(toks[4].tok, Token::RArrow);
    assert_eq!(toks[6].typ, TokenType::Unknown);
    assert_eq!(toks[6].tok, Token::OpenDelim(DelimToken::Paren));
    assert_eq!(toks[7].typ, TokenType::Unknown);
    assert_eq!(toks[7].tok, Token::CloseDelim(DelimToken::Paren));
    assert_eq!(toks[8].typ, TokenType::Unknown);
    assert_eq!(toks[8].tok, Token::RArrow);
}

