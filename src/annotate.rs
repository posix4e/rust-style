use style::{FormatStyle, Penalty};
use syntax::parse::token::keywords::Keyword;
use syntax::parse::token::{Token, DelimToken, BinOpToken};
use token::{FormatToken, TokenType, Precedence, CommentType};
use unwrapped_line::{UnwrappedLine, LineType};

pub fn annotate_lines(lines: &mut [UnwrappedLine], style: &FormatStyle) {
    for line in lines {
        annotate_lines(&mut line.children, style);
        AnnotatingParser {
            style: style,
            current: 0,
            context: vec![],
            context_parens_index: vec![],
            binding_strength: 0,
            matched_parens: None,
        }.parse_line(line);
        // ExpressionParser {
        //     style: style,
        //     current_index: 0,
        //     line: line,
        // }.parse(Precedence::Unknown);
        calculate_formatting_information(line);
    }
}

struct AnnotatingParser<'a> {
    style: &'a FormatStyle,
    current: usize,
    context: Vec<Context>,
    context_parens_index: Vec<usize>,
    binding_strength: Penalty,
    matched_parens: Option<(usize, usize)>,
}

#[derive(Eq, PartialEq)]
enum Context {
    LambdaArgs,
    Generics,
    Type,
    Parens,
}

impl Context {
    fn binding_strength(&self) -> Penalty {
        match *self {
            Context::LambdaArgs => 10,
            Context::Generics => 10,
            Context::Type => 10,
            Context::Parens => 1,
        }
    }
}

impl<'a> AnnotatingParser<'a> {
    fn parse_line(&mut self, line: &mut UnwrappedLine) {
        line.typ = self.determine_line_type(line);
        while self.current < line.tokens.len() {
            line.tokens[self.current].typ = self.determine_token_type(line);
            line.tokens[self.current].binding_strength = self.binding_strength;
            if let Some((l, r)) = self.matched_parens.take() {
                line.tokens[l].matching_paren_index = Some(r);
                line.tokens[r].matching_paren_index = Some(l);
            }
            self.current += 1;
        }
    }

    fn push_context(&mut self, context: Context) {
        self.binding_strength += context.binding_strength();

        if let Context::Parens = context {
            self.context_parens_index.push(self.current);
        }

        self.context.push(context);
    }

    fn pop_context(&mut self) {
        assert!(self.context.len() > 0);

        let context = self.context.pop().unwrap();
        self.binding_strength -= context.binding_strength();

        if let Context::Parens = context {
            let start = self.context_parens_index.pop().unwrap();
            self.matched_parens = Some((start, self.current));
        }

        if self.context.is_empty() {
            assert!(self.binding_strength == 0);
            assert!(self.context_parens_index.is_empty());
        }
    }

    fn context_is(&self, context: &Context) -> bool {
        self.context.last() == Some(context)
    }

    fn determine_line_type(&self, line: &mut UnwrappedLine) -> LineType {
        for ftok in &line.tokens {
            let tok = &ftok.tok;
            match tok {
                &Token::Ident(..) if tok.is_keyword(Keyword::Use) => return LineType::Use,
                &Token::Ident(..) if tok.is_keyword(Keyword::Struct) => return LineType::StructDecl,
                &Token::Ident(..) if tok.is_keyword(Keyword::Enum) => return LineType::EnumDecl,
                &Token::Ident(..) if tok.is_keyword(Keyword::Impl) => return LineType::ImplDecl,
                &Token::Ident(..) if tok.is_keyword(Keyword::Trait) => return LineType::TraitDecl,
                &Token::Ident(..) if tok.is_keyword(Keyword::Fn) => return LineType::FnDecl,
                &Token::Ident(..) if tok.is_keyword(Keyword::Type) => return LineType::TypeDecl,
                _ => {},
            }
        }
        LineType::Unknown
    }

    fn determine_token_type(&mut self, line: &mut UnwrappedLine) -> TokenType {
        let curr = &line.tokens[self.current];
        let prev = line.prev_non_comment_token(self.current);
        let next = line.next_non_comment_token(self.current);
        let prev_tok = prev.map(|t| &t.tok);
        let next_tok = next.map(|t| &t.tok);

        // FIXME: match shouldnt be required, I think the compiler was bugging out
        //        when I tried to use the == operator? Maybe not.
        let prev_is_mod_sep = match prev_tok { Some(&Token::ModSep) => true, _ => false };

        match &curr.tok {
            &Token::BinOp(BinOpToken::And) if self.context_is(&Context::Type) => TokenType::Pointer,
            &Token::BinOp(BinOpToken::Star) if unary_follows(prev) => TokenType::UnaryOperator,
            &Token::BinOp(BinOpToken::And) if unary_follows(prev) => TokenType::UnaryOperator,
            &Token::BinOp(BinOpToken::Or) if unary_follows(prev) => {
                self.push_context(Context::LambdaArgs);
                TokenType::LambdaArgsStart
            },
            &Token::BinOp(BinOpToken::Or) if self.context.last() == Some(&Context::LambdaArgs) => {
                self.pop_context();
                TokenType::LambdaArgsEnd
            }

            &Token::Lt if line.typ == LineType::StructDecl ||
                          line.typ == LineType::EnumDecl  ||
                          line.typ == LineType::ImplDecl ||
                          line.typ == LineType::TraitDecl ||
                          line.typ == LineType::FnDecl ||
                          line.typ == LineType::TypeDecl ||
                          prev_is_mod_sep ||
                          self.context_is(&Context::Type) ||
                          self.context_is(&Context::Generics) => {
                self.push_context(Context::Generics);
                TokenType::GenericBracket
            }

            &Token::Gt if self.context_is(&Context::Generics) => {
                self.pop_context();
                TokenType::GenericBracket
            }

            &Token::BinOp(BinOpToken::Shr) if self.context_is(&Context::Generics) => {
                self.pop_context();
                if self.context_is(&Context::Generics) {
                    self.pop_context();
                }
                TokenType::GenericBracket
            }

            &Token::RArrow => {
                self.push_context(Context::Type);
                TokenType::Unknown
            }
            &Token::Colon if !self.context_is(&Context::Generics) => {
                self.push_context(Context::Type);
                TokenType::Unknown
            }

            &Token::Semi |
            &Token::Comma |
            &Token::OpenDelim(DelimToken::Brace) |
            &Token::CloseDelim(..) if self.context_is(&Context::Type) => {
                self.pop_context();
                TokenType::Unknown
            }

            &Token::OpenDelim(DelimToken::Paren) => {
                self.push_context(Context::Parens);
                TokenType::Unknown
            }

            &Token::CloseDelim(DelimToken::Paren) if self.context_is(&Context::Parens) => {
                self.pop_context();
                TokenType::Unknown
            }

            &Token::Eq |
            &Token::Lt |
            &Token::Le |
            &Token::EqEq |
            &Token::Ne |
            &Token::Ge |
            &Token::Gt |
            &Token::AndAnd |
            &Token::OrOr |
            &Token::BinOp(..) |
            &Token::BinOpEq(..) => TokenType::BinaryOperator,

            _ => TokenType::Unknown,
        }
    }
}

struct ExpressionParser<'a> {
    style: &'a FormatStyle,
    line: &'a mut UnwrappedLine,
    current_index: usize,
}

impl<'a> ExpressionParser<'a> {
    fn parse(&mut self, precedence: Precedence) {
        unimplemented!();
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
            split_penalty_ = 20 * curr.binding_strength + split_penalty(line, prev, curr);
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
        (_, &Token::Comma) => false,
        (_, &Token::Semi) => false,

        (_, &Token::CloseDelim(DelimToken::Paren)) => false,
        (&Token::OpenDelim(DelimToken::Paren), _) => false,

        (_, &Token::CloseDelim(DelimToken::Bracket)) => false,
        (&Token::OpenDelim(DelimToken::Bracket), _) => false,

        (_, &Token::OpenDelim(DelimToken::Brace)) if line.typ == LineType::Use => false,
        (&Token::OpenDelim(DelimToken::Brace), _) if line.typ == LineType::Use => false,
        (_, &Token::CloseDelim(DelimToken::Brace)) if line.typ == LineType::Use => false,
        (&Token::CloseDelim(DelimToken::Brace), _) if line.typ == LineType::Use => false,
        (_, &Token::BinOp(BinOpToken::Star)) if line.typ == LineType::Use => false,
        (&Token::BinOp(BinOpToken::Star), _) if line.typ == LineType::Use => false,

        (_, &Token::Dot) => false,
        (&Token::Dot, _) => false,
        (_, &Token::DotDot) => false,
        (&Token::DotDot, _) => false,
        (_, &Token::DotDotDot) => false,
        (&Token::DotDotDot, _) => false,

        (&Token::BinOp(BinOpToken::Shr), &Token::Eq) |
        (&Token::Gt, &Token::Eq) if prev.typ == TokenType::GenericBracket => true,
        (&Token::BinOp(BinOpToken::Shr), &Token::Ident(..)) |
        (&Token::Gt, &Token::Ident(..)) if prev.typ == TokenType::GenericBracket => true,
        (_, &Token::OpenDelim(DelimToken::Brace)) if prev.typ == TokenType::GenericBracket => true,
        _ if prev.typ == TokenType::GenericBracket => false,
        _ if curr.typ == TokenType::GenericBracket => false,

        _ if prev.typ == TokenType::Pointer => false,
        _ if prev.typ == TokenType::UnaryOperator => false,

        _ if prev.typ == TokenType::LambdaArgsStart => false,
        _ if prev.typ == TokenType::LambdaArgsEnd => true,
        _ if curr.typ == TokenType::LambdaArgsEnd => false,

        _ if prev.typ == TokenType::BinaryOperator => true,
        _ if curr.typ == TokenType::BinaryOperator => true,

        (&Token::OpenDelim(DelimToken::Brace), _) => true,
        (_, &Token::OpenDelim(DelimToken::Brace)) => true,

        (&Token::CloseDelim(DelimToken::Brace), _) => true,
        (_, &Token::CloseDelim(DelimToken::Brace)) => true,

        (&Token::FatArrow, _) => true,
        (_, &Token::FatArrow) => true,

        (&Token::RArrow, _) => true,
        (_, &Token::RArrow) => true,

        (&Token::Comma, _) => true,
        (&Token::Semi, _) => true,
        (&Token::Colon, _) => true,

        (_, &Token::Comment) => true,
        (&Token::Comment, _) => true,

        (&Token::Literal(..), _) => true,
        (_, &Token::Literal(..)) => true,

        (&Token::Lifetime(..), &Token::Ident(..)) => true,
        (&Token::Ident(..), &Token::Ident(..)) => true,

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
    if curr.newlines_before > 1 {
        return true;
    }
    if let Some(CommentType::SlashSlash) = prev.comment_type {
        return true;
    }
    false
}

fn can_break_before(line: &UnwrappedLine, prev: &FormatToken, curr: &FormatToken) -> bool {
    match (&prev.tok, &curr.tok) {
        _ if prev.typ == TokenType::UnaryOperator => false,
        (&Token::OpenDelim(..), &Token::CloseDelim(..)) => false,

        (_, &Token::Ident(..)) if curr.tok.is_keyword(Keyword::If) => true,

        (&Token::Eq, _) |
        (&Token::EqEq, _) |
        (&Token::AndAnd, _) |
        (&Token::OrOr, _) |
        (&Token::BinOp(..), _) |
        (&Token::Comma, _) |
        (_, &Token::RArrow) |
        (&Token::FatArrow, _) |
        // FIXME: dots should only break on method calls
        (_, &Token::Dot) => true,
        _ => false,
    }
}

fn split_penalty(line: &UnwrappedLine, prev: &FormatToken, curr: &FormatToken) -> Penalty {
    match (&prev.tok, &curr.tok) {
        (&Token::Comma, _) => 1,
        (&Token::Eq, _) => 100,
        (_, &Token::Dot) => 10,
        (_, &Token::OpenDelim(DelimToken::Brace)) => 1,
        _ => 3,
    }
}
