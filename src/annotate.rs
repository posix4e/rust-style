use style::{FormatStyle, Penalty};
use syntax::parse::token::keywords::Keyword;
use syntax::parse::token::{Token, DelimToken, BinOpToken};
use token::{FormatToken, TokenType};
use unwrapped_line::{UnwrappedLine, LineType};

pub fn annotate_lines(lines: &mut [UnwrappedLine], style: &FormatStyle) {
    for line in lines {
        annotate_lines(&mut line.children, style);
        AnnotatingParser {
            line: line,
            style: style,
            current: 0,
            context: vec![],
        }.parse_line();
        calculate_formatting_information(line);
    }
}

struct AnnotatingParser<'a> {
    line: &'a mut UnwrappedLine,
    style: &'a FormatStyle,
    current: usize,
    context: Vec<Context>,
}

#[derive(Eq, PartialEq)]
enum Context {
    LambdaArgs,
    Generics,
    Type,
}

impl<'a> AnnotatingParser<'a> {
    fn parse_line(&mut self) {
        self.line.typ = self.determine_line_type();
        while self.current < self.line.tokens.len() {
            self.line.tokens[self.current].typ = self.determine_token_type();
            self.current += 1;
        }
    }

    fn determine_line_type(&self) -> LineType {
        for ftok in &self.line.tokens {
            let tok = &ftok.tok;
            match tok {
                &Token::Ident(..) if tok.is_keyword(Keyword::Use) => return LineType::Use,
                &Token::Ident(..) if tok.is_keyword(Keyword::Struct) => return LineType::StructDecl,
                &Token::Ident(..) if tok.is_keyword(Keyword::Enum) => return LineType::EnumDecl,
                &Token::Ident(..) if tok.is_keyword(Keyword::Impl) => return LineType::ImplDecl,
                &Token::Ident(..) if tok.is_keyword(Keyword::Trait) => return LineType::TraitDecl,
                &Token::Ident(..) if tok.is_keyword(Keyword::Fn) => return LineType::FnDecl,
                _ => {},
            }
        }
        LineType::Unknown
    }

    fn determine_token_type(&mut self) -> TokenType {
        let curr = &self.line.tokens[self.current];
        let prev = self.line.prev_non_comment_token(self.current);
        let next = self.line.next_non_comment_token(self.current);
        let prev_tok = prev.map(|t| &t.tok);
        let next_tok = next.map(|t| &t.tok);

        match (prev_tok, &curr.tok, next_tok) {
            (Some(&Token::Colon), &Token::BinOp(BinOpToken::And), _) => TokenType::Pointer,
            (_, &Token::BinOp(BinOpToken::Star), _) if unary_follows(prev) => TokenType::UnaryOperator,
            (_, &Token::BinOp(BinOpToken::And), _) if unary_follows(prev) => TokenType::UnaryOperator,
            (_, &Token::BinOp(BinOpToken::Or), _) if unary_follows(prev) => {
                self.context.push(Context::LambdaArgs);
                TokenType::LambdaArgsStart
            },
            (_, &Token::BinOp(BinOpToken::Or), _) if self.context.last() == Some(&Context::LambdaArgs) => {
                self.context.pop();
                TokenType::LambdaArgsEnd
            }


            (_, &Token::Lt, _) if self.line.typ == LineType::StructDecl ||
                                  self.line.typ == LineType::EnumDecl  ||
                                  self.line.typ == LineType::ImplDecl ||
                                  self.line.typ == LineType::TraitDecl ||
                                  self.line.typ == LineType::FnDecl ||
                                  match prev_tok { Some(&Token::ModSep) => true, _ => false } ||
                                  self.context.last() == Some(&Context::Type) ||
                                  self.context.last() == Some(&Context::Generics) => {
                self.context.push(Context::Generics);
                TokenType::GenericBracket
            }

            (_, &Token::Gt, _) |
            (_, &Token::BinOp(BinOpToken::Shr), _) if self.context.last() == Some(&Context::Generics) => {
                self.context.pop();
                TokenType::GenericBracket
            }

            (_, &Token::Colon, _) if self.context.last() != Some(&Context::Generics) => {
                self.context.push(Context::Type);
                TokenType::Unknown
            }

            (_, &Token::Semi, _) |
            (_, &Token::Comma, _) |
            (_, &Token::CloseDelim(..), _) if self.context.last() == Some(&Context::Type) => {
                self.context.pop();
                TokenType::Unknown
            }

            _ => TokenType::Unknown,
        }
    }
}

fn calculate_formatting_information(line: &mut UnwrappedLine) {
    for i in 1..line.tokens.len() {
        let mut spaces_required_before = 0;
        let can_break_before_;
        let must_break_before_;
        let split_penalty_;
        {
            let curr = &line.tokens[i];
            let prev = &line.tokens[i - 1];

            if space_required_before(line, prev, curr) {
                spaces_required_before = 1;
            }
            must_break_before_ = must_break_before(line, prev, curr);
            can_break_before_ = must_break_before_ || can_break_before(line, prev, curr);
            split_penalty_ = split_penalty(line, prev, curr);
        }

        let curr = &mut line.tokens[i];
        curr.split_penalty = split_penalty_;
        curr.spaces_required_before = spaces_required_before;
        curr.must_break_before = must_break_before_;
        curr.can_break_before = can_break_before_;
    }
}

fn unary_follows(prev: Option<&FormatToken>) -> bool {
    let prev = match prev {
        None => return true,
        Some(prev) => prev,
    };

    match prev.tok {
        Token::Eq |
        Token::OpenDelim(..) |
        Token::Comma |
        Token::Semi => return true,
        Token::Ident(..) if prev.tok.is_keyword(Keyword::Return) => return true,
        _ => {},
    }

    match prev.typ {
        TokenType::BinaryOperator |
        TokenType::UnaryOperator => true,
        _ => false
    }
}


fn space_required_before(line: &UnwrappedLine,
                         prev: &FormatToken,
                         curr: &FormatToken) -> bool {
    match (&prev.tok, &curr.tok) {
        (_, &Token::Comma) |
        (_, &Token::Semi) |
        (_, &Token::CloseDelim(DelimToken::Paren)) |
        (&Token::OpenDelim(DelimToken::Paren), _) |
        (_, &Token::CloseDelim(DelimToken::Bracket)) |
        (&Token::OpenDelim(DelimToken::Bracket), _) |
        (_, &Token::Dot) |
        (&Token::Dot, _) |
        (_, &Token::DotDot) |
        (&Token::DotDot, _) |
        (_, &Token::DotDotDot) |
        (&Token::DotDotDot, _) => false,

        (&Token::Lt, _) if prev.typ == TokenType::GenericBracket => false,
        (_, &Token::Lt) if curr.typ == TokenType::GenericBracket => false,
        (&Token::Gt, _) |
        (&Token::BinOp(BinOpToken::Shr), _)
            if prev.typ == TokenType::GenericBracket &&
               curr.tok != Token::OpenDelim(DelimToken::Brace) => false,
        (_, &Token::Gt) |
        (_, &Token::BinOp(BinOpToken::Shr)) if curr.typ == TokenType::GenericBracket => false,

        (_, &Token::OpenDelim(DelimToken::Brace)) |
        (&Token::OpenDelim(DelimToken::Brace), _) |
        (_, &Token::CloseDelim(DelimToken::Brace)) |
        (&Token::CloseDelim(DelimToken::Brace), _) if line.typ == LineType::Use => false,

        (&Token::BinOp(BinOpToken::And), _) if prev.typ == TokenType::Pointer => false,
        (&Token::BinOp(BinOpToken::And), _) if prev.typ == TokenType::UnaryOperator => false,
        (&Token::BinOp(BinOpToken::Star), _) if prev.typ == TokenType::UnaryOperator => false,
        (&Token::BinOp(BinOpToken::Or), _) if prev.typ == TokenType::LambdaArgsStart => false,
        (_, &Token::BinOp(BinOpToken::Or)) if curr.typ == TokenType::LambdaArgsEnd => false,

        (&Token::OpenDelim(DelimToken::Brace), _) |
        (_, &Token::OpenDelim(DelimToken::Brace)) |
        (&Token::CloseDelim(DelimToken::Brace), _) |
        (_, &Token::CloseDelim(DelimToken::Brace)) |
        (&Token::FatArrow, _) |
        (_, &Token::FatArrow) |
        (&Token::RArrow, _) |
        (_, &Token::RArrow) |
        (&Token::Comma, _) |
        (&Token::Semi, _) |
        (_, &Token::Comment) |
        (&Token::Colon, _) |
        (&Token::BinOp(..), _) |
        (_, &Token::BinOp(..)) |
        (&Token::BinOpEq(..), _) |
        (_, &Token::BinOpEq(..)) |
        (&Token::Le, _) |
        (_, &Token::Le) |
        (&Token::Lt, _) |
        (_, &Token::Lt) |
        (&Token::Ge, _) |
        (_, &Token::Ge) |
        (&Token::Gt, _) |
        (_, &Token::Gt) |
        (&Token::Ne, _) |
        (_, &Token::Ne) |
        (&Token::Eq, _) |
        (_, &Token::Eq) |
        (&Token::EqEq, _) |
        (_, &Token::EqEq) |
        (&Token::AndAnd, _) |
        (_, &Token::AndAnd) |
        (&Token::OrOr, _) |
        (_, &Token::OrOr) |
        (&Token::Literal(..), _) |
        (_, &Token::Literal(..)) |
        (&Token::Lifetime(..), &Token::Ident(..)) => true,

        (&Token::Ident(..), _) if is_spaced_keyword(&prev.tok) => true,
        (_, &Token::Ident(..)) if is_spaced_keyword(&curr.tok) => true,

        _ => false,
    }
}

fn is_spaced_keyword(token: &Token) -> bool {
    token.is_any_keyword() &&
        !token.is_keyword(Keyword::SelfType) &&
        !token.is_keyword(Keyword::SelfValue) &&
        !token.is_keyword(Keyword::True) &&
        !token.is_keyword(Keyword::False)
}

fn must_break_before(line: &UnwrappedLine, prev: &FormatToken, curr: &FormatToken) -> bool {
    curr.newlines_before > 1
}

fn can_break_before(line: &UnwrappedLine, prev: &FormatToken, curr: &FormatToken) -> bool {
    match (&prev.tok, &curr.tok) {
        _ if prev.typ == TokenType::UnaryOperator => false,
        (&Token::OpenDelim(..), &Token::CloseDelim(..)) => false,

        (&Token::Eq, _) |
        (&Token::EqEq, _) |
        (&Token::AndAnd, _) |
        (&Token::OrOr, _) |
        (&Token::BinOp(..), _) |
        (&Token::Comma, _) |
        (&Token::FatArrow, _) |
        (&Token::CloseDelim(..), &Token::Dot) => true,
        _ => false,
    }
}

fn split_penalty(line: &UnwrappedLine, prev: &FormatToken, curr: &FormatToken) -> Penalty {
    match (&prev.tok, &curr.tok) {
        (&Token::Comma, _) => 1,
        (&Token::Eq, &Token::OpenDelim(DelimToken::Brace)) => 100,
        (&Token::ModSep, _) => 500,
        (_, &Token::OpenDelim(DelimToken::Brace)) => 1,
        _ => 3,
    }
}
