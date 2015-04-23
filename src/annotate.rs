use style::{FormatStyle, Penalty};
use syntax::parse::token::keywords::Keyword;
use syntax::parse::token::{Token, DelimToken, BinOpToken};
use token::{FormatToken, TokenType, Precedence, PRECEDENCE_UNARY, PRECEDENCE_DOT};
use unwrapped_line::{UnwrappedLine, LineType};

pub fn annotate_lines(lines: &mut [UnwrappedLine], style: &FormatStyle) {
    for line in lines {
        // Recursive call to annotate child lines
        annotate_lines(&mut line.children, style);
        // The annotating parser disambiguate specific tokens.
        // For example derefenciation vs. multiplication, angle brackets in generics vs.
        // greater/smaller signs and so on.
        AnnotatingParser {
            style: style,
            current_index: 0,
            line: line,
            context_stack: vec![],
        }.parse_line();
        // The expression parser adds invisible braces to the tokens in respect to their precedence.
        // This is used later to retain logical units in the line breaking process.
        ExpressionParser {
            style: style,
            current_index: 0,
            line: line,
        }.parse(0);
        // Decide if tokens must be set with whitespace in between them using the information
        // the annotating parser put in.
        calculate_formatting_information(line);
    }
}

// The following definitions are used in the annotation parser.
//
// The contexts the annotating parser currently disambiguates.
enum ContextType {
    // (...)
    Parens,
    // <...>
    Generics,
    // |...|
    LambdaParams,
}

struct Context {
    binding_strength: Penalty,
}

struct AnnotatingParser<'a> {
    #[allow(dead_code)]
    style: &'a FormatStyle,
    current_index: usize,
    line: &'a mut UnwrappedLine,
    context_stack: Vec<Context>,
}

// the return value of the parse_angle_bracket
enum GenericsReturn {
    Fail,
    Success,
    // This is the case when a ">>" token is found that also terminates the parent parse function.
    SuccessDouble,
}

impl<'a> AnnotatingParser<'a> {
    // Helper functions.
    fn current(&self) -> &FormatToken {
        &self.line.tokens[self.current_index]
    }

    fn current_mut(&mut self) -> &mut FormatToken {
        &mut self.line.tokens[self.current_index]
    }

    fn has_current(&self) -> bool {
        self.current_index < self.line.tokens.len()
    }

    fn current_is_unary(&self) -> bool {
        unary_follows(self.line.prev_non_comment_token(self.current_index))
    }

    fn next(&mut self) {
        if self.has_current() {
            self.current_mut().binding_strength = self.context_binding_strength();
            self.current_index += 1;
        }
    }

    fn context_binding_strength(&self) -> Penalty {
        self.context_stack.last().map(|c| c.binding_strength).unwrap_or(0)
    }

    fn using_context<R, F: Fn(&mut Self) -> R>(&mut self, typ: ContextType, f: F) -> R {
        let new_context = Context {
            binding_strength: self.context_binding_strength() + match typ {
                ContextType::Parens => 1,
                ContextType::Generics => 10,
                ContextType::LambdaParams => 10,
            },
        };

        self.context_stack.push(new_context);
        let result = f(self);
        self.context_stack.pop();
        result
    }

    // The base parse function. It's pretty similar to parse_paren, but it doesn't exit on any
    // closing delimiters, so the line is annoted until the end under all circumstances.
    // The parser is implemented as an recursive descent parser. The parse functions are relativly
    // similar and there is some code duplication.
    fn parse_line(&mut self) {
        for i in 0..self.line.tokens.len() {
            self.line.tokens[i].index = i;
        }
        self.line.typ = self.determine_line_type();

        while self.has_current() {
            match &self.current().tok {
                // Unary operators
                &Token::BinOp(BinOpToken::Star) |
                &Token::BinOp(BinOpToken::And) |
                &Token::BinOp(BinOpToken::Minus) if self.current_is_unary() => {
                    self.current_mut().typ = TokenType::UnaryOperator;
                }
                // Lambda parameter "|...|"
                &Token::BinOp(BinOpToken::Or) if self.current_is_unary() => {
                    self.current_mut().typ = TokenType::LambdaParamsStart;
                    self.parse_lambda_params();
                },
                &Token::OpenDelim(DelimToken::Paren) => {
                    self.parse_paren();
                    // parse_paren consumes the closing parens
                    // We don't want to call next again.
                    continue;
                },
                // This is either the binary operator "<" or the start of a generic type.
                // That can't be decided at this place, so we start parsing as if it where a generic
                // type and bracktrack if parse_generics fails to parse correctly.
                &Token::Lt => {
                    let safepoint = self.current_index;
                    match self.parse_generics() {
                        // If a nested angle bracket parse fails, we must backtrack and reparse
                        GenericsReturn::Fail => {
                            self.current_index = safepoint;
                            self.current_mut().typ = TokenType::BinaryOperator;
                        }
                        // SuccessDouble has to be an error in the source, but i think it's the most
                        // robust solution to handle it as if it was valid
                        GenericsReturn::SuccessDouble |
                        GenericsReturn::Success => {}
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
                    self.current_mut().typ = TokenType::BinaryOperator;
                }
                _ => {
                    self.current_mut().typ = TokenType::Unknown;
                }
            }

            self.next()
        }
    }

    // The following parse functions are pretty similar to parse_line.
    fn parse_generics(&mut self) -> GenericsReturn {
        self.using_context(ContextType::Generics, |this| {
            let mut is_first_token = true;

            while this.has_current() {
                match &this.current().tok {
                    &Token::BinOp(BinOpToken::Star) |
                    &Token::BinOp(BinOpToken::And) |
                    &Token::BinOp(BinOpToken::Minus) if this.current_is_unary() => {
                        this.current_mut().typ = TokenType::UnaryOperator;
                    }
                    &Token::Lt if is_first_token => {
                        is_first_token = false;
                        this.current_mut().typ = TokenType::GenericBracket;
                    }
                    &Token::Lt => {
                        match this.parse_generics() {
                            // if a nested angle bracket parse fails, this must fail too
                            GenericsReturn::Fail => {
                                return GenericsReturn::Fail;
                            }
                            // Test if the child parse call ended with a ">>" symbol. In that case
                            // this parse function must exit too.
                            GenericsReturn::SuccessDouble => {
                                return GenericsReturn::Success;
                            }
                            GenericsReturn::Success => {}
                        }
                    }
                    &Token::Gt => {
                        this.current_mut().typ = TokenType::GenericBracket;
                        return GenericsReturn::Success;
                    }
                    // The last token in "Foo<Bar<Baz>>" is ">>". We must terminate the parent
                    // parse_angle_bracket too.
                    &Token::BinOp(BinOpToken::Shr) => {
                        this.current_mut().typ = TokenType::GenericBracket;
                        return GenericsReturn::SuccessDouble;
                    }
                    // These symbols are unliky in generics
                    &Token::AndAnd |
                    &Token::OrOr => {
                        return GenericsReturn::Fail;
                    }

                    &Token::Eq |
                    &Token::EqEq |
                    &Token::Ne |
                    &Token::BinOp(..) |
                    &Token::BinOpEq(..) => {
                        this.current_mut().typ = TokenType::BinaryOperator;
                    }
                    _ => {
                        this.current_mut().typ = TokenType::Unknown;
                    }
                };
                this.next()
            }
            GenericsReturn::Fail
        })
    }

    fn parse_paren(&mut self) {
        assert_eq!(self.current().tok, Token::OpenDelim(DelimToken::Paren));
        self.next(); // advance over the starting paren

        self.using_context(ContextType::Parens, |this| {
            while this.has_current() {
                match &this.current().tok {
                    &Token::BinOp(BinOpToken::Star) |
                    &Token::BinOp(BinOpToken::And) |
                    &Token::BinOp(BinOpToken::Minus) if this.current_is_unary() => {
                        this.current_mut().typ = TokenType::UnaryOperator;
                    }
                    &Token::BinOp(BinOpToken::Or) if this.current_is_unary() => {
                        this.current_mut().typ = TokenType::LambdaParamsStart;
                        this.parse_lambda_params();
                    }
                    // A nested paren block
                    &Token::OpenDelim(DelimToken::Paren) => {
                        this.parse_paren();
                        this.current_mut().typ = TokenType::Unknown;
                        // parse_paren consumes the closing parens
                        // We don't want to call next again.
                        continue;
                    }
                    &Token::Lt => {
                        let safepoint = this.current_index;
                        match this.parse_generics() {
                            // If a nested angle bracket parse fails, we must backtrack and reparse
                            GenericsReturn::Fail => {
                                this.current_index = safepoint;
                                this.current_mut().typ = TokenType::BinaryOperator;
                            }
                            // SuccessDouble has to be an error in the source, but i think it's the most
                            // robust solution to handle this parse as if it was valid
                            GenericsReturn::SuccessDouble |
                            GenericsReturn::Success => {}
                        }
                    }
                    // end the paren parsing
                    &Token::CloseDelim(DelimToken::Paren) => {
                        this.current_mut().typ = TokenType::Unknown;
                        // consume the closing parens in this context
                        this.next();
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
                        this.current_mut().typ = TokenType::BinaryOperator;
                    }
                    _ => {
                        this.current_mut().typ = TokenType::Unknown;
                    }
                }

                this.next();
            }
        })
    }

    fn parse_lambda_params(&mut self) {
        assert_eq!(self.current().tok, Token::BinOp(BinOpToken::Or));
        self.next(); // advance over the first pipe

        self.using_context(ContextType::LambdaParams, |this| {
            while this.has_current() {
                match &this.current().tok {
                    &Token::BinOp(BinOpToken::Star) |
                    &Token::BinOp(BinOpToken::And) |
                    &Token::BinOp(BinOpToken::Minus) if this.current_is_unary() => {
                        this.current_mut().typ = TokenType::UnaryOperator;
                    }
                    // end lambda parsing
                    &Token::BinOp(BinOpToken::Or) => {
                        this.current_mut().typ = TokenType::LambdaParamsEnd;
                        return
                    },
                    &Token::Lt => {
                        let safepoint = this.current_index;
                        match this.parse_generics() {
                            // If a nested angle bracket parse fails, we must backtrack and reparse
                            GenericsReturn::Fail => {
                                this.current_index = safepoint;
                                this.current_mut().typ = TokenType::BinaryOperator;
                            }
                            // SuccessDouble has to be an error in the source, but i think it's the most
                            // robust solution to handle this parse as if it was valid
                            GenericsReturn::SuccessDouble |
                            GenericsReturn::Success => {}
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
                        this.current_mut().typ = TokenType::BinaryOperator;
                    }
                    _ => {
                        this.current_mut().typ = TokenType::Unknown;
                    }
                }

                this.next()
            }
        })
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

fn space_required_before(line: &UnwrappedLine, prev: &FormatToken, curr: &FormatToken) -> bool {
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
        (&Token::Gt, &Token::BinOp(..)) |
        (&Token::BinOp(BinOpToken::Shr), &Token::BinOp(..)) |
        (&Token::Gt, &Token::Ident(..)) if prev.typ == TokenType::GenericBracket => true,
        (_, &Token::OpenDelim(DelimToken::Brace)) if prev.typ == TokenType::GenericBracket => true,
        _ if prev.typ == TokenType::GenericBracket => false,
        _ if curr.typ == TokenType::GenericBracket => false,

        _ if prev.typ == TokenType::Pointer => false,
        _ if prev.typ == TokenType::UnaryOperator => false,

        _ if prev.typ == TokenType::LambdaParamsStart => false,
        _ if prev.typ == TokenType::LambdaParamsEnd => true,
        _ if curr.typ == TokenType::LambdaParamsEnd => false,

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
