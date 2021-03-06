use format_options::{FormatStyle, Penalty};
use syntax::parse::token::keywords::Keyword;
use syntax::parse::token::{Token, DelimToken, BinOpToken};
use token::{FormatToken, TokenType, Precedence, PRECEDENCE_UNARY, PRECEDENCE_DOT};
use unwrapped_line::{UnwrappedLine, LineType, Block};

pub fn annotate_lines(lines: &mut [UnwrappedLine], style: &FormatStyle) {
    for line in lines {
        // Recursive call to annotate child lines
        for token in &mut line.tokens {
            annotate_lines(&mut token.children, style);
        }

        // The annotating parser disambiguate specific tokens.
        // For example derefenciation vs. multiplication, angle brackets in generics vs.
        // greater/smaller signs and so on.
        AnnotatingParser {
            style: style,
            current_index: 0,
            line: line,
            context_stack: vec![],
            in_pattern_guard: false,
            seen_fn_decl_params: false,
        }.parse_line();

        // The expression parser adds invisible braces to the tokens in respect to their precedence.
        // This is used later to retain logical units in the line breaking process.
        ExpressionParser {
            style: style,
            current_index: 0,
            line: line,
        }.parse(0, false);

        // Decide if tokens must be set with whitespace in between them using the information
        // the annotating parser put in.
        calculate_formatting_information(line, style);
    }
}

// The following definitions are used in the annotation parser.
//
// The contexts the annotating parser disambiguates.
#[derive(Eq, PartialEq)]
enum ContextType {
    // (...)
    Parens,
    // <...>
    Generics,
    // |...|
    LambdaParams,
    // where ... {
    WhereClause,
}

struct Context {
    typ: ContextType,
    binding_strength: Penalty,
}

struct AnnotatingParser<'a> {
    #[allow(dead_code)]
    style: &'a FormatStyle,
    current_index: usize,
    line: &'a mut UnwrappedLine,
    context_stack: Vec<Context>,
    in_pattern_guard: bool,
    seen_fn_decl_params: bool,
}

impl<'a> AnnotatingParser<'a> {
    // The main interface of this struct.
    // Parses over the line and annotates the line and each token.
    fn parse_line(&mut self) {
        self.line.reset_token_indices();
        self.line.typ = self.determine_line_type();

        while self.has_current() {
            match self.consume_token() {
                Ok(()) => (),
                Err(()) => break,
            }
        }
    }

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
        let prev = self.line.prev_non_comment_token(self.current_index);
        let curr = self.current();
        is_unary(prev, curr)
    }

    fn is_after_non_keyword_ident(&self) -> bool {
        match self.line.prev_non_comment_token(self.current_index) {
            Some(&FormatToken{ref tok, ..}) => tok.is_ident() && !tok.is_any_keyword(),
            None => false,
        }
    }

    fn context_is(&self, typ: ContextType) -> bool {
        match self.context_stack.last() {
            Some(c) => c.typ == typ,
            None => false,
        }
    }

    fn context_binding_strength(&self) -> Penalty {
        self.context_stack.last().map(|c| c.binding_strength).unwrap_or(1)
    }

    fn using_context<R, F: Fn(&mut Self) -> R>(&mut self, typ: ContextType, f: F) -> R {
        let new_context = Context {
            binding_strength: self.context_binding_strength() + match typ {
                ContextType::Parens => 2,
                ContextType::Generics => 10,
                ContextType::LambdaParams => 10,
                ContextType::WhereClause => 10,
            },
            typ: typ,
        };

        self.context_stack.push(new_context);
        let result = f(self);
        self.context_stack.pop();
        result
    }

    // Advance to the next token, TokenType will be automatically deduced
    fn next(&mut self) {
        self.next_with_token_type(None)
    }

    // Advance to the next token, and specify the TokenType of the current token
    fn next_with_token_type(&mut self, typ: Option<TokenType>) {
        if self.has_current() {
            self.current_mut().binding_strength = self.context_binding_strength();
            self.current_mut().typ = typ.unwrap_or_else(|| self.determine_token_type());
            self.in_pattern_guard = self.in_pattern_guard || self.current().typ == TokenType::PatternGuardIf;

            if !self.seen_fn_decl_params && self.current().typ == TokenType::FnDeclParamsEnd &&
                   self.current().matching_paren.is_some() {
                let start_index = self.current().matching_paren.unwrap();
                self.line.tokens[start_index].typ = TokenType::FnDeclParamsStart;
                self.seen_fn_decl_params = true;
            }

            self.current_index += 1;
        }
    }

    // Advance to the next token or parse any potential productions (generics, lambda, parens etc.)
    fn consume_token(&mut self) -> Result<(), ()> {
        match &self.current().tok {
            &Token::BinOp(BinOpToken::Or) if self.current_is_unary() => {
                self.parse_lambda_params()
            },
            &Token::OpenDelim(DelimToken::Paren) => {
                self.parse_paren()
            },
            &Token::Lt => {
                let safepoint = self.current_index;
                if let Err(()) = self.parse_generics() {
                    // If a nested angle bracket parse fails, we must backtrack and reparse
                    self.current_index = safepoint;
                    self.next_with_token_type(Some(TokenType::BinaryOperator));
                }
                Ok(())
            }
            &Token::Ident(..) if self.current().tok.is_keyword(Keyword::Where) => {
                self.parse_where_clause()
            }
            _ => {
                self.next();
                Ok(())
            },
        }
    }

    fn parse_generics(&mut self) -> Result<(), ()> {
        assert_eq!(self.current().tok, Token::Lt);
        self.next_with_token_type(Some(TokenType::GenericBracket));

        self.using_context(ContextType::Generics, |this| {
            while this.has_current() {
                match &this.current().tok {
                    &Token::Gt => {
                        // consume the closing angle bracket in this context
                        this.next();
                        return Ok(());
                    }
                    // These symbols are unliky in generics
                    &Token::AndAnd |
                    &Token::OrOr => {
                        return Err(());
                    }
                    _ => {
                        try!(this.consume_token());
                    }
                };
            }

            // Failed find a matching closing angle bracket
            Err(())
        })
    }

    fn parse_paren(&mut self) -> Result<(), ()> {
        assert_eq!(self.current().tok, Token::OpenDelim(DelimToken::Paren));
        let start_index = self.current_index;
        self.next(); // advance over the opening paren

        self.using_context(ContextType::Parens, |this| {
            while this.has_current() {
                match &this.current().tok {
                    // end the paren parsing
                    &Token::CloseDelim(DelimToken::Paren) => {
                        let end_index = this.current_index;
                        this.line.tokens[start_index].matching_paren = Some(end_index);
                        this.line.tokens[end_index].matching_paren = Some(start_index);

                        this.next();
                        return Ok(());
                    }
                    _ => {
                        try!(this.consume_token());
                    }
                }
            }

            // Failed find a matching closing paren
            Err(())
        })
    }

    fn parse_lambda_params(&mut self) -> Result<(), ()> {
        assert_eq!(self.current().tok, Token::BinOp(BinOpToken::Or));
        self.next_with_token_type(Some(TokenType::LambdaParamsStart));

        self.using_context(ContextType::LambdaParams, |this| {
            while this.has_current() {
                match &this.current().tok {
                    &Token::BinOp(BinOpToken::Or) => {
                        // consume ending pipe in this context
                        this.next_with_token_type(Some(TokenType::LambdaParamsEnd));
                        return Ok(());
                    },
                    _ => {
                        try!(this.consume_token());
                    }
                }
            }

            // Failed find a matching closing lambda pipe
            Err(())
        })
    }

    fn parse_where_clause(&mut self) -> Result<(), ()> {
        assert!(self.current().tok.is_keyword(Keyword::Where));
        self.next();

        self.using_context(ContextType::WhereClause, |this| {
            while this.has_current() {
                match &this.current().tok {
                    &Token::OpenDelim(DelimToken::Brace) => {
                        // consume brace in this context
                        this.next();
                        return Ok(());
                    },
                    _ => {
                        try!(this.consume_token());
                    }
                }
            }

            // Failed find a matching closing lambda pipe
            Err(())
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
                _ if !tok.is_any_keyword() => return LineType::Unknown,
                _ => {},
            }
        }
        LineType::Unknown
    }

    fn determine_token_type(&self) -> TokenType {
        let prev = self.line.prev_non_comment_token(self.current_index);

        match self.current().tok {
            Token::AndAnd |
            Token::Not |
            Token::BinOp(BinOpToken::Star) |
            Token::BinOp(BinOpToken::And) |
            Token::BinOp(BinOpToken::Minus) if self.current_is_unary() => {
                TokenType::UnaryOperator
            }

            Token::OrOr if self.current_is_unary() => {
                TokenType::LambdaParamsEmpty
            }

            Token::BinOp(BinOpToken::Or)
                    if self.line.block == Block::Match && !self.in_pattern_guard => {
                TokenType::PatternOr
            }

            Token::Not if self.is_after_non_keyword_ident() => {
                TokenType::Postfix
            }

            Token::Lt | Token::Gt if self.context_is(ContextType::Generics) => {
                TokenType::GenericBracket
            }

            Token::Eq |
            Token::At |
            Token::Le |
            Token::EqEq |
            Token::Ne |
            Token::Ge |
            Token::Gt |
            Token::AndAnd |
            Token::OrOr |
            Token::BinOp(..) |
            Token::BinOpEq(..) => {
                TokenType::BinaryOperator
            }

            Token::CloseDelim(DelimToken::Paren) if self.line.typ == LineType::FnDecl &&
                                                        self.context_stack.len() == 1 &&
                                                        !self.seen_fn_decl_params => {
                TokenType::FnDeclParamsEnd
            }

            Token::RArrow
                if prev.map(|t| t.typ == TokenType::FnDeclParamsEnd).unwrap_or(false) => {
                TokenType::FnDeclArrow
            }

            Token::Ident(..) if self.current().tok.is_keyword(Keyword::If) &&
                                    self.line.block == Block::Match => {
                TokenType::PatternGuardIf
            }

            _ => TokenType::Unknown,
        }
    }
}

struct ExpressionParser<'a> {
    #[allow(dead_code)]
    style: &'a FormatStyle,
    line: &'a mut UnwrappedLine,
    current_index: usize,
}

impl<'a> ExpressionParser<'a> {
    fn parse(&mut self, precedence: i32, mut in_control_structure: bool) {
        // These keywords should not be considered part of the start of a binary expression
        while self.has_current() && (self.current().tok.is_keyword(Keyword::Return) ||
                                     self.current().tok.is_keyword(Keyword::While) ||
                                     self.current().tok.is_keyword(Keyword::If) &&
                                         self.current().typ != TokenType::PatternGuardIf ||
                                     self.current().tok.is_keyword(Keyword::Match) ||
                                     self.current().tok.is_keyword(Keyword::While)) {

            in_control_structure = in_control_structure ||
                                   self.current().tok.is_keyword(Keyword::While) ||
                                   self.current().tok.is_keyword(Keyword::If) ||
                                   self.current().tok.is_keyword(Keyword::Match);
            self.next();
        }

        if !self.has_current() { return; }
        if precedence > PRECEDENCE_DOT { return; }

        if precedence == PRECEDENCE_UNARY {
            return self.parse_unary_operator(in_control_structure);
        }

        let start_index = self.current_index;
        let mut latest_operator_index = None;
        let mut operator_index = 0;

        while self.has_current() {
            // Recursive call to consume operators with higher precedence.
            self.parse(precedence + 1, in_control_structure);

            if !self.has_current() { break; }
            if self.current().closes_scope() { break; }

            // Check if current operator has higher precedence.
            let current_precedence = self.current_precedence();
            if current_precedence != -1 && current_precedence < precedence {
                break;
            }

            if precedence > 0 {
                // Check if we found a statement brace.
                if in_control_structure && self.current().tok == Token::OpenDelim(DelimToken::Brace) {
                    break;
                }

                // Check for pattern guards
                if self.current().typ == TokenType::PatternGuardIf {
                    break;
                }

                // In match patterns, => token ends boolean expression.
                if self.current().tok == Token::FatArrow {
                    break;
                }
            }

            if self.current().opens_scope() {
                while self.has_current() && !self.current().closes_scope() {
                    // Inside a new delim scope
                    self.next();
                    self.parse(0, false);
                }

                if self.has_current() && (self.current().typ == TokenType::LambdaParamsEnd) {
                    self.next();
                    self.parse(0, false);
                } else {
                    self.next();
                }
            } else if self.current().typ == TokenType::LambdaParamsEmpty {
                self.next();
                self.parse(0, false);
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
                    self.add_fake_parenthesis(start_index, Precedence::from_i32(precedence).unwrap())
                }
            }
        }
    }

    fn parse_unary_operator(&mut self, in_control_structure: bool) {
        if !self.has_current() || self.current().typ != TokenType::UnaryOperator {
            return self.parse(PRECEDENCE_DOT, in_control_structure);
        }

        let start_index = self.current_index;
        self.next();
        self.parse_unary_operator(in_control_structure);
        self.add_fake_parenthesis(start_index, Precedence::Unknown);
    }

    fn add_fake_parenthesis(&mut self, start: usize, prec: Precedence) {
        self.line.tokens[start].fake_lparens.push(prec);
        if prec != Precedence::Unknown {
            self.line.tokens[start].starts_binary_expression = true;
        }

        let mut previous = self.current_index - 1;
        while self.line.tokens[previous].is_comment() && previous > 0 {
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
        let prev1 = self.line.prev_non_comment_token(self.current_index);
        let prev2 = prev1.and_then(|t| self.line.prev_non_comment_token(t.index));
        let is_field_init = self.current().tok == Token::Colon &&
                            prev1.map(|t| t.tok.is_ident()).unwrap_or(false) &&
                            prev2.map(|t| t.tok == Token::OpenDelim(DelimToken::Brace) ||
                                              t.tok == Token::Comma).unwrap_or(false);

        if self.current().typ == TokenType::UnaryOperator {
            PRECEDENCE_UNARY
        } else if self.current().tok == Token::Dot {
            PRECEDENCE_DOT
        } else if is_field_init {
            Precedence::Comma.to_i32()
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

fn calculate_formatting_information(line: &mut UnwrappedLine, style: &FormatStyle) {
    line.tokens[0].total_length = line.tokens[0].column_width;

    for i in 1..line.tokens.len() {
        let spaces_required_before;
        let can_break_before_;
        let must_break_before_;
        let split_penalty_;
        let total_length;

        {
            let curr = &line.tokens[i];
            let prev = &line.tokens[i - 1];

            spaces_required_before = if curr.in_non_whitelisted_macro { curr.spaces_before }
                                     else if space_required_before(line, prev, curr) { 1 }
                                     else { 0 };
            must_break_before_ = must_break_before(line, prev, curr);
            can_break_before_ = must_break_before_ || can_break_before(prev, curr);
            split_penalty_ = 20 * curr.binding_strength + split_penalty(prev, curr);

            let mut child_length = 0;
            if prev.children.len() == 1 {
                child_length = prev.children[0].tokens.last().unwrap().total_length + style.column_limit;
            }

            if must_break_before_ || prev.children.len() > 1 || curr.last_line_column_width.is_some() {
                total_length = prev.total_length + style.column_limit;
            } else {
                total_length = prev.total_length + curr.column_width + child_length + spaces_required_before;
            }
        }

        let curr = &mut line.tokens[i];
        curr.split_penalty = split_penalty_;
        curr.spaces_required_before = spaces_required_before;
        curr.must_break_before = must_break_before_;
        curr.can_break_before = can_break_before_;
        curr.total_length = total_length;
    }
}

fn is_unary(prev: Option<&FormatToken>, curr: &FormatToken) -> bool {
    let prev = match prev {
        None => return true,
        Some(prev) => prev,
    };
    match prev.tok {
        Token::RArrow if curr.tok == Token::Not => false,

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
        Token::Semi => true,

        Token::Ident(..)
            if prev.tok.is_keyword(Keyword::Return) ||
               prev.tok.is_keyword(Keyword::Match) ||
               prev.tok.is_keyword(Keyword::As) ||
               prev.tok.is_keyword(Keyword::In) ||
               prev.tok.is_keyword(Keyword::If) ||
               prev.tok.is_keyword(Keyword::Let) ||
               prev.tok.is_keyword(Keyword::Mut) ||
               prev.tok.is_keyword(Keyword::Const) ||
               prev.tok.is_keyword(Keyword::For) => true,

        _ if prev.typ == TokenType::BinaryOperator ||
             prev.typ == TokenType::UnaryOperator ||
             prev.typ == TokenType::LambdaParamsStart => true,

        _ => false,
    }
}

fn space_required_before(line: &UnwrappedLine, prev: &FormatToken, curr: &FormatToken) -> bool {
    match (&prev.tok, &curr.tok) {
        _ if prev.is_comment() => true,
        _ if curr.is_comment() => true,

        (&Token::Ident(..), _) if prev.tok.is_keyword(Keyword::Mut) => true,
        (&Token::Ident(..), _) if prev.tok.is_keyword(Keyword::Const) => true,

        (_, &Token::Comma) => false,
        (_, &Token::Semi) => false,

        // spacing after macro invocation
        (&Token::Not, &Token::OpenDelim(DelimToken::Paren)) |
        (&Token::Not, &Token::OpenDelim(DelimToken::Bracket))
            if prev.typ == TokenType::Postfix => false,
        (&Token::Not, _) if prev.typ == TokenType::Postfix => true,

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

        (&Token::Gt, _) if prev.typ == TokenType::GenericBracket &&
                           curr.typ == TokenType::UnaryOperator => true,
        (&Token::Gt, &Token::Eq) if prev.typ == TokenType::GenericBracket => true,
        (&Token::Gt, &Token::BinOp(..)) |
        (&Token::Gt, &Token::Ident(..)) if prev.typ == TokenType::GenericBracket => true,
        (_, &Token::OpenDelim(DelimToken::Brace)) if prev.typ == TokenType::GenericBracket => true,
        _ if prev.typ == TokenType::GenericBracket => false,
        (&Token::Ident(..), _) if curr.typ == TokenType::GenericBracket => false,

        _ if prev.typ == TokenType::UnaryOperator => false,

        _ if prev.typ == TokenType::LambdaParamsStart => false,
        _ if prev.typ == TokenType::LambdaParamsEnd => true,
        _ if curr.typ == TokenType::LambdaParamsEnd => false,
        _ if prev.typ == TokenType::LambdaParamsEmpty => true,

        _ if prev.typ == TokenType::PatternOr => true,
        _ if curr.typ == TokenType::PatternOr => true,

        _ if prev.typ == TokenType::BinaryOperator => true,
        _ if curr.typ == TokenType::BinaryOperator => true,

        (&Token::FatArrow, _) => true,
        (_, &Token::FatArrow) => true,

        (&Token::RArrow, _) => true,
        (_, &Token::RArrow) => true,

        (&Token::Comma, _) => true,
        (&Token::Semi, _) => true,
        (_, &Token::Colon) => false,
        (&Token::Colon, _) => true,

        (_, &Token::DotDot) => false,
        (&Token::DotDot, _) => false,
        (_, &Token::DotDotDot) => false,
        (&Token::DotDotDot, _) => false,

        (&Token::OpenDelim(..), &Token::CloseDelim(..))
            if prev.children.is_empty() => false,
        (&Token::OpenDelim(DelimToken::Brace), _) => true,
        (_, &Token::OpenDelim(DelimToken::Brace)) => true,
        (&Token::CloseDelim(DelimToken::Brace), _) => true,
        (_, &Token::CloseDelim(DelimToken::Brace)) => true,

        (&Token::Literal(..), _) => true,
        (_, &Token::Literal(..)) => true,

        (&Token::Lifetime(..), &Token::OpenDelim(DelimToken::Bracket)) => true,
        (&Token::Lifetime(..), &Token::OpenDelim(DelimToken::Paren)) => true,
        (&Token::Lifetime(..), &Token::Ident(..)) => true,
        (&Token::Ident(..), &Token::Ident(..)) => true,

        // fn keyword as a type
        (&Token::Ident(..), &Token::OpenDelim(DelimToken::Paren))
            if prev.tok.is_keyword(Keyword::Fn) => false,
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
    if curr.newlines_before > 0 && curr.is_comment() {
        return true;
    }
    if prev.is_trailing_comment(line) {
        return true;
    }
    if curr.newlines_before > 0 && prev.tok == Token::OpenDelim(DelimToken::Brace) {
        return true;
    }
    if curr.newlines_before > 0 && curr.in_non_whitelisted_macro {
        return true;
    }

    false
}

fn can_break_before(prev: &FormatToken, curr: &FormatToken) -> bool {
    if curr.newlines_before == 0 && curr.in_non_whitelisted_macro {
        return false;
    }


    match (&prev.tok, &curr.tok) {
        _ if prev.typ == TokenType::UnaryOperator => false,
        (&Token::OpenDelim(..), &Token::CloseDelim(..)) if prev.children.is_empty() => false,

        (&Token::OpenDelim(DelimToken::Brace), _) => true,
        (_, &Token::CloseDelim(DelimToken::Brace)) => true,

        (&Token::OpenDelim(DelimToken::Paren), _) => true,

        (_, &Token::Ident(..)) if curr.typ == TokenType::PatternGuardIf => true,
        (_, &Token::Ident(..)) if curr.tok.is_keyword(Keyword::Where) => true,

        _ if prev.typ == TokenType::BinaryOperator => true,
        _ if curr.typ == TokenType::PatternOr => true,

        (&Token::Comma, _) |
        (_, &Token::RArrow) |
        (&Token::FatArrow, _) |
        (_, &Token::Dot) => true,

        _ => false,
    }
}

fn split_penalty(prev: &FormatToken, curr: &FormatToken) -> Penalty {
    match (&prev.tok, &curr.tok) {
        (&Token::Comma, _) => 1,
        (_, &Token::RArrow) => 1,
        (&Token::FatArrow, _) => 10,
        (_, &Token::Ident(..)) if curr.tok.is_keyword(Keyword::Where) => 1,
        (&Token::CloseDelim(DelimToken::Paren), &Token::Dot) => 20,
        (_, &Token::Dot) => 100,
        (&Token::OpenDelim(DelimToken::Paren), _) => 200,
        (_, &Token::OpenDelim(DelimToken::Brace)) => 1,
        _ => match prev.precedence() {
            None | Some(Precedence::Unknown) => 3,
            Some(precedence) => precedence as Penalty,
        },
    }
}
