use style::{FormatStyle, Penalty};
use syntax::parse::token::keywords::Keyword;
use syntax::parse::token::{Token, DelimToken, BinOpToken};
use token::{FormatToken, TokenType, Precedence, PRECEDENCE_UNARY, PRECEDENCE_DOT};
use unwrapped_line::{UnwrappedLine, LineType};

pub fn annotate_lines(lines: &mut [UnwrappedLine], style: &FormatStyle) {
    for line in lines {
        annotate_lines(&mut line.children, style);
        AnnotatingParser {
            style: style,
            current: 0,
            binding_strength: 0,
            matched_parens: None,
        }.parse_line(line);

        // debug
        /*
        for i in 0..line.tokens.len() {
            print!("{:?}", line.tokens[i].tok);
            println!("    {:?}", line.tokens[i].typ);
        }
        */

        ExpressionParser {
            style: style,
            current_index: 0,
            line: line,
        }.parse(0);
        calculate_formatting_information(line);
    }
}

struct AnnotatingParser<'a> {
    #[allow(dead_code)]
    style: &'a FormatStyle,
    current: usize,
    binding_strength: Penalty,
    matched_parens: Option<(usize, usize)>,
}


impl<'a> AnnotatingParser<'a> {

    // The base parse function
    fn parse_line(&mut self, line: &mut UnwrappedLine) {
        let local_line = line.clone();

        for i in 0..line.tokens.len() {
            line.tokens[i].index = i;
        }
        line.typ = self.determine_line_type(&local_line);

        loop {
            if self.current >= line.tokens.len() { break; }

            let ref curr = local_line.tokens[self.current];
            let ref prev = local_line.prev_non_comment_token(self.current);

            match &curr.tok {
                &Token::BinOp(BinOpToken::Star) |
                &Token::BinOp(BinOpToken::And) |
                &Token::BinOp(BinOpToken::Minus) if unary_follows(*prev) => {
                    line.tokens[self.current].typ = TokenType::UnaryOperator;
                }
                &Token::BinOp(BinOpToken::Or) if unary_follows(*prev) => {
                    line.tokens[self.current].typ = TokenType::LambdaArgsStart;
                    self.parse_lambda(line);
                },
                &Token::OpenDelim(DelimToken::Paren) => {
                    self.parse_paren(line);
                },
                &Token::Lt => {
                    let safepoint = self.current;
                    if !self.parse_angle_bracket(line) {
                        self.current = safepoint;
                        line.tokens[self.current].typ = TokenType::BinaryOperator;
                    }
                }
                &Token::Eq |
                &Token::Le |
                &Token::EqEq |
                &Token::Ne |
                &Token::Ge |
                &Token::Gt |
                &Token::AndAnd |
                &Token::OrOr |
                &Token::BinOp(..) |
                &Token::BinOpEq(..) => {
                    line.tokens[self.current].typ = TokenType::BinaryOperator;
                }
                _ => {
                    line.tokens[self.current].typ = TokenType::Unknown;
                }
            }

            if self.current >= line.tokens.len() { break; }

            line.tokens[self.current].binding_strength = 1;
            if let Some((l, r)) = self.matched_parens.take() {
                line.tokens[l].matching_paren_index = Some(r);
                line.tokens[r].matching_paren_index = Some(l);
            }
            self.current += 1;
        }
    }

    fn parse_angle_bracket(&mut self, line: &mut UnwrappedLine) -> bool {

        let local_line = line.clone();
        let mut is_first_token = true;
        let ref prev = local_line.prev_non_comment_token(self.current);

        loop {
            if self.current >= line.tokens.len() { break; }

            let ref curr = &local_line.tokens[self.current];

            match &curr.tok {
                &Token::BinOp(BinOpToken::Star) |
                &Token::BinOp(BinOpToken::And) |
                &Token::BinOp(BinOpToken::Minus) if unary_follows(*prev) => {
                    line.tokens[self.current].typ = TokenType::UnaryOperator;
                }
                &Token::Lt if is_first_token => {
                    is_first_token = false;
                    line.tokens[self.current].typ = TokenType::GenericBracket;
                }
                &Token::Lt => {
                    // if a nestes angle bracket parse fails, this must fail too
                    if !self.parse_angle_bracket(line) {
                        return false;
                    }

                    // Test if the child parse call ended with a ">>" symbol. In that case, this
                    // parse function must exit too.
                    if line.tokens[self.current].tok == Token::BinOp(BinOpToken::Shr) {
                        return true;
                    }

                }
                &Token::Gt => {
                    line.tokens[self.current].typ = TokenType::GenericBracket;
                    return true;
                }

                // The last token in "Foo<Bar<Baz>>" is ">>". We must terminate the parent
                // parse_angle_bracket too.
                &Token::BinOp(BinOpToken::Shr) => {
                    line.tokens[self.current].typ = TokenType::GenericBracket;
                    return true;
                }
                // this symbols are unliky in generics
                &Token::AndAnd |
                &Token::OrOr |
                &Token::BinOpEq(..) => {
                    return false;
                }
                _ => {
                    line.tokens[self.current].typ = TokenType::Unknown;
                }
            };

            if self.current >= line.tokens.len() { break; }

            line.tokens[self.current].binding_strength = 10;
            if let Some((l, r)) = self.matched_parens.take() {
                line.tokens[l].matching_paren_index = Some(r);
                line.tokens[r].matching_paren_index = Some(l);
            }
            self.current += 1;
        }
        true
    }

    fn parse_paren(&mut self, line: &mut UnwrappedLine) {
        self.current += 1; // advance over the starting paren

        let local_line = line.clone();

        loop {
            if self.current >= line.tokens.len() { return; }

            let ref curr = local_line.tokens[self.current];
            let ref prev = local_line.prev_non_comment_token(self.current);

            match &curr.tok {
                &Token::BinOp(BinOpToken::Star) |
                &Token::BinOp(BinOpToken::And) |
                &Token::BinOp(BinOpToken::Minus) if unary_follows(*prev) => {
                    line.tokens[self.current].typ = TokenType::UnaryOperator;
                }
                &Token::BinOp(BinOpToken::Or) if unary_follows(*prev) => {
                    line.tokens[self.current].typ = TokenType::LambdaArgsStart;
                    self.parse_lambda(line);
                }
                // this is a nested paren block
                &Token::OpenDelim(DelimToken::Paren) => {
                    self.parse_paren(line);
                    line.tokens[self.current].typ = TokenType::Unknown;
                }
                &Token::Lt => {
                    let safepoint = self.current;
                    if !self.parse_angle_bracket(line) {
                        self.current = safepoint;
                        line.tokens[self.current].typ = TokenType::BinaryOperator;
                    }
                }

                // end the paren parsing
                &Token::CloseDelim(DelimToken::Paren) => {
                    line.tokens[self.current].typ = TokenType::Unknown;
                    return;
                }

                &Token::Eq |
                &Token::Le |
                &Token::EqEq |
                &Token::Ne |
                &Token::Ge |
                &Token::Gt |
                &Token::AndAnd |
                &Token::OrOr |
                &Token::BinOp(..) |
                &Token::BinOpEq(..) => {
                    line.tokens[self.current].typ = TokenType::BinaryOperator;
                }
                _ => {
                    line.tokens[self.current].typ = TokenType::Unknown;
                }
            }

            if self.current >= line.tokens.len() { break; }

            line.tokens[self.current].binding_strength = 10;
            if let Some((l, r)) = self.matched_parens.take() {
                line.tokens[l].matching_paren_index = Some(r);
                line.tokens[r].matching_paren_index = Some(l);
            }
            self.current += 1;
        }
    }

    fn parse_lambda(&mut self, line: &mut UnwrappedLine) {
        self.current += 1; // advance over the first pipe
        let local_line = line.clone();

        loop {
            if self.current >= line.tokens.len() { break; }

            let ref curr = local_line.tokens[self.current];
            let ref prev = local_line.prev_non_comment_token(self.current);

            match &curr.tok {
                &Token::BinOp(BinOpToken::Star) |
                &Token::BinOp(BinOpToken::And) |
                &Token::BinOp(BinOpToken::Minus) if unary_follows(*prev) => {
                    line.tokens[self.current].typ = TokenType::UnaryOperator;
                }
                // end lambda parsing
                &Token::BinOp(BinOpToken::Or) => {
                    line.tokens[self.current].typ = TokenType::LambdaArgsEnd;
                    return
                },
                &Token::Lt => {
                    let safepoint = self.current;
                    if !self.parse_angle_bracket(line) {
                        self.current = safepoint;
                        line.tokens[self.current].typ = TokenType::BinaryOperator;
                    }
                }
                &Token::Eq |
                &Token::Le |
                &Token::EqEq |
                &Token::Ne |
                &Token::Ge |
                &Token::Gt |
                &Token::AndAnd |
                &Token::OrOr |
                &Token::BinOp(..) |
                &Token::BinOpEq(..) => {
                    line.tokens[self.current].typ = TokenType::BinaryOperator;
                }
                _ => {
                    line.tokens[self.current].typ = TokenType::Unknown;
                }
            }

            if self.current >= line.tokens.len() { break; }

            line.tokens[self.current].binding_strength = 10;
            if let Some((l, r)) = self.matched_parens.take() {
                line.tokens[l].matching_paren_index = Some(r);
                line.tokens[r].matching_paren_index = Some(l);
            }
            self.current += 1;
        }
    }

    fn determine_line_type(&self, line: &UnwrappedLine) -> LineType {
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
}

struct ExpressionParser<'a> {
    #[allow(dead_code)]
    style: &'a FormatStyle,
    line: &'a mut UnwrappedLine,
    current_index: usize,
}

impl<'a> ExpressionParser<'a> {
    fn parse(&mut self, precedence: i32) {
        while self.has_current() && self.current().tok.is_keyword(Keyword::Return) {
            self.next();
        }

        if !self.has_current() { return; }
        if precedence > PRECEDENCE_DOT { return; }

        if precedence == PRECEDENCE_UNARY {
            return self.parse_unary_operator();
        }

        let start_index = self.current_index;
        let mut latest_operator_index = None;
        let mut operator_index = 0;

        while self.has_current() {
            // Recursive call to consume operators with higher precedence.
            self.parse(precedence + 1);

            if !self.has_current() { break; }
            if self.current().closes_scope() { break; }

            // check if current operator has higher precedence
            let current_precedence = self.current_precedence();
            if current_precedence != -1 && current_precedence < precedence {
                break;
            }

            if self.current().opens_scope() {
                while self.has_current() && !self.current().closes_scope(){
                    self.next();
                    self.parse(0);
                }
                self.next();
            } else {
                if current_precedence == precedence {
                    latest_operator_index = Some(self.current_index);
                    self.current_mut().operator_index += operator_index;
                    operator_index += 1;
                }
                self.next_skip_leading_comments(precedence > 0);
            }
        }

        if let Some(latest_operator_index) = latest_operator_index {
            if self.has_current() || precedence > 0 {
                self.line.tokens[latest_operator_index].last_operator = true;
                if precedence == PRECEDENCE_DOT {
                    self.add_fake_parenthesis(start_index, Precedence::Unknown);
                } else {
                    self.add_fake_parenthesis(start_index, Precedence::from_u32(precedence).unwrap())
                }
            }
        }
    }

    fn parse_unary_operator(&mut self) {
        if !self.has_current() || self.current().typ != TokenType::UnaryOperator {
            return self.parse(PRECEDENCE_DOT);
        }

        let start_index = self.current_index;
        self.next();
        self.parse_unary_operator();
        self.add_fake_parenthesis(start_index, Precedence::Unknown);
    }

    fn add_fake_parenthesis(&mut self, start: usize, prec: Precedence) {
        self.line.tokens[start].fake_lparens.push(prec);
        if prec != Precedence::Unknown {
            self.line.tokens[start].starts_binary_expression = true;
        }

        let mut previous = self.current_index - 1;
        while self.line.tokens[previous].tok == Token::Comment && previous > 0 {
            previous -= 1;
        }
        self.line.tokens[previous].fake_rparens += 1;
        if prec != Precedence::Unknown {
            self.line.tokens[previous].ends_binary_expression = true;
        }
    }

    fn has_current(&self) -> bool {
        self.current_index < self.line.tokens.len()
    }

    fn current(&self) -> &FormatToken {
        &self.line.tokens[self.current_index]
    }

    fn current_mut(&mut self) -> &mut FormatToken {
        &mut self.line.tokens[self.current_index]
    }

    fn current_precedence(&self) -> i32 {
        if self.current().typ == TokenType::UnaryOperator {
            PRECEDENCE_UNARY
        } else if self.current().tok == Token::Dot {
            PRECEDENCE_DOT
        } else {
            self.current()
                .precedence()
                .map(|p| p.to_i32())
                .unwrap_or(-1)
        }
    }

    fn next(&mut self) {
        self.next_skip_leading_comments(true);
    }

    fn next_skip_leading_comments(&mut self, skip_leading_comments: bool) {
        if self.has_current() {
            self.current_index += 1;
        }
        while self.has_current() &&
              (self.current().newlines_before == 0 || skip_leading_comments) &&
              self.current().is_trailing_comment(self.line) {
            self.current_index += 1;
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
            can_break_before_ = must_break_before_ || can_break_before(prev, curr);
            split_penalty_ = 20 * curr.binding_strength + split_penalty(prev, curr);
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
        Token::EqEq |
        Token::Le |
        Token::Ge |
        Token::Lt |
        Token::Gt |
        Token::LArrow |
        Token::RArrow |
        Token::FatArrow |
        Token::AndAnd |
        Token::OrOr |
        Token::BinOp(..) |
        Token::BinOpEq(..) |
        Token::OpenDelim(..) |
        Token::Comma |
        Token::Colon |
        Token::ModSep |
        Token::Semi => return true,
        Token::Ident(..)
            if prev.tok.is_keyword(Keyword::Return) ||
               prev.tok.is_keyword(Keyword::Match) ||
               prev.tok.is_keyword(Keyword::As) ||
               prev.tok.is_keyword(Keyword::If) => return true,
        _ => false,
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

        (&Token::ModSep, &Token::OpenDelim(DelimToken::Brace)) if line.typ == LineType::Use => false,
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
    if prev.is_trailing_comment(line) {
        return true;
    }
    false
}

fn can_break_before(prev: &FormatToken, curr: &FormatToken) -> bool {
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

fn split_penalty(prev: &FormatToken, curr: &FormatToken) -> Penalty {
    match (&prev.tok, &curr.tok) {
        (&Token::Comma, _) => 1,
        (_, &Token::RArrow) => 1,
        (&Token::Eq, _) | (&Token::BinOpEq(..), _) => 100,
        (_, &Token::Dot) => 10,
        (_, &Token::OpenDelim(DelimToken::Brace)) => 1,
        _ => 3,
    }
}
