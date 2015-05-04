use std::mem;
use syntax::codemap::{mk_sp, BytePos};
use syntax::parse::token::keywords::Keyword;
use syntax::parse::token::{Token, DelimToken, BinOpToken, Lit};
use token::{FormatToken, FormatTokenLexer};

// An unwrapped line is a sequence of FormatTokens, that we would like to
// put on a single line if there was no column limit. Changing the formatting
// within an unwrapped line does not affect any other unwrapped lines.
#[derive(Debug, Clone)]
pub struct UnwrappedLine {
    // The tokens of the line
    pub tokens: Vec<FormatToken>,
    // The indentation level. File level starts at 0.
    pub level: u32,
    // The type of line
    pub typ: LineType,
    // The block type this line is contained in
    pub block: Block,
    // If the line intersects with the input line ranges.
    pub affected: bool,
    // If the leading empty lines above intersects with one of the input
    // line ranges.
    pub leading_empty_lines_affected: bool,
    // If any of the children of this lines intersects with the input ranges.
    pub children_affected: bool,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum LineType {
    Use,
    StructDecl,
    EnumDecl,
    ImplDecl,
    TraitDecl,
    FnDecl,
    TypeDecl,
    Unknown,
}

impl UnwrappedLine {
    // Takes a stream of tokens (from the lexer), and divides them into logical lines.
    pub fn parse_lines(lexer: &mut FormatTokenLexer) -> Vec<UnwrappedLine> {
        let mut parser = UnwrappedLineParser {
            lexer: lexer,
            output: vec![],
            // FIXME: Placeholder token. Change to an Option?
            ftok: FormatToken::default(),
            line_stack: vec![vec![]],
            block_stack: vec![],
            level: 0,
            comments_before_next_token: vec![],
        };

        parser.parse();
        assert!(parser.level == 0);
        assert!(parser.comments_before_next_token.is_empty());
        assert!(parser.line_stack.len() == 1);
        assert!(parser.line_stack.last().unwrap().is_empty());
        assert!(parser.block_stack.is_empty());
        parser.output
    }

    pub fn prev_non_comment_token(&self, index: usize) -> Option<&FormatToken> {
        self.tokens[..index].iter().rev()
            .filter(|t| !t.is_comment())
            .next()
    }

    pub fn reset_token_indices(&mut self) {
        for i in 0..self.tokens.len() {
            self.tokens[i].index = i;
        }
    }
}

struct UnwrappedLineParser<'a, 'b: 'a> {
    // Completed lines
    output: Vec<UnwrappedLine>,
    // Input token source
    lexer: &'a mut FormatTokenLexer<'b>,
    // The current token being processed
    ftok: FormatToken,
    // A stack of lines that is currently being processed.
    // A new level is pushed when a child block is started.
    // Processed tokens are added to the end of the top line.
    line_stack: Vec<Vec<FormatToken>>,
    // The block context of the current line being processed
    block_stack: Vec<Block>,
    // The indentation level of the current line being processed
    level: u32,
    // Comment tokens that appear before the next token
    comments_before_next_token: Vec<FormatToken>,
}

#[derive(Copy,Clone,Eq,PartialEq,Debug)]
pub enum Block {
    Match,
    Statements,
    StructOrEnum,
    TopLevel,
    StructInit,
    MacroRules,
}

enum Context {
    Declaration,
    Statements,
}

impl<'a, 'b> UnwrappedLineParser<'a, 'b> {
    fn parse(&mut self) {
        self.read_token();
        self.parse_level(Block::TopLevel);

        // add any remaining tokens
        self.flush_comments(true);
        self.add_line();

        assert_eq!(self.ftok.tok, Token::Eof);
        assert_eq!(self.ftok.children.len(), 0);

        // push through eof
        let eof = self.ftok.clone();
        self.push_token(eof);
        self.add_line();
    }

    // Parse the indentation level of a block. The level will end when any
    // unmatched delimiter is found.
    fn parse_level(&mut self, block: Block) {
        loop {
            // Match the first token of the line to control structures,
            // attributes, declarations, macro_rules and so on.
            match self.ftok.tok {
                Token::Eof => {
                    break;
                }
                Token::CloseDelim(..) if block == Block::TopLevel => {
                    // error
                    self.next_token();
                    self.add_line();
                }
                Token::CloseDelim(..) => {
                    break;
                }
                Token::OpenDelim(DelimToken::Brace) => {
                    self.parse_block(Block::Statements);
                    self.add_line();
                }
                Token::Pound => {
                    self.parse_attribute();
                }
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Mod) => {
                    self.parse_mod();
                }
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Use) => {
                    self.parse_use();
                }
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Loop) ||
                                    self.ftok.tok.is_keyword(Keyword::For) ||
                                    self.ftok.tok.is_keyword(Keyword::While) => {
                    self.parse_loop();
                }
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Struct)  ||
                                    self.ftok.tok.is_keyword(Keyword::Enum) => {
                    self.parse_decl(Block::StructOrEnum);
                }
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Fn) ||
                                    self.ftok.tok.is_keyword(Keyword::Impl) ||
                                    self.ftok.tok.is_keyword(Keyword::Trait) ||
                                    self.ftok.tok.is_keyword(Keyword::Impl) ||
                                    self.ftok.tok.is_keyword(Keyword::Type) => {
                    self.parse_decl(Block::Statements);
                }
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Pub) ||
                                    self.ftok.tok.is_keyword(Keyword::Unsafe) => {
                    self.next_token();
                }
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Extern) => {
                    self.next_token();
                    if let Token::Literal(Lit::Str_(..), _) = self.ftok.tok {
                        self.next_token();
                        if self.try_parse_brace_block(Block::Statements) {
                            self.add_line();
                        }
                    }
                }
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::If) => {
                    self.parse_if_then_else();
                    self.next_token_if(&Token::Semi);
                    self.add_line();
                }
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Match) => {
                    self.parse_match();
                    self.next_token_if(&Token::Semi);
                    self.add_line();
                }
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Let) => {
                    self.parse_let();
                }
                Token::Ident(..) if self.is_macro_rules() => {
                    self.parse_macro_rules();
                }
                _ => {
                    match block {
                        Block::StructOrEnum => self.parse_enum_variant_or_struct_field(),
                        Block::Match => self.parse_match_arm(),
                        Block::MacroRules => self.parse_macro_rule(),
                        Block::StructInit => self.parse_field_init(),
                        Block::Statements |
                        Block::TopLevel => self.parse_stmt(),
                    }
                }
            }
        }
    }

    fn try_parse_brace_block(&mut self, block: Block) -> bool {
        if self.ftok.tok == Token::OpenDelim(DelimToken::Brace) {
            self.parse_block(block);
            true
        } else {
            false
        }
    }

    // Ends the current line, and parses the following block in a nested level.
    // Blocks can start/end with any delimeter ({}, (), []), but they are normally
    // braces. The other delimeters are used for macro_rules.
    fn parse_block(&mut self, block: Block) {
        let delim = match self.ftok.tok {
            Token::OpenDelim(delim) => delim,
            _ => panic!("expected a delimeter"),
        };
        self.next_token();
        self.add_line();

        let intial_level = self.level;
        self.level += 1;
        self.block_stack.push(block);
        self.parse_level(block);
        self.block_stack.pop().unwrap();

        if self.ftok.tok != Token::CloseDelim(delim) {
            self.level = intial_level;
            return;
        }

        // Eat closing delim
        self.next_token();
        self.level = intial_level;
    }

    // Parses a single field of a struct initializer
    fn parse_field_init(&mut self) {
        if self.parse_stmt_up_to(|t| *t == Token::Comma) {
            self.next_token();
            self.add_line();
        }
    }

    fn parse_child_block(&mut self, block: Block) {
        // Consume opening brace
        assert_eq!(self.ftok.tok, Token::OpenDelim(DelimToken::Brace));
        self.next_token();

        // Add new level to stack, so children lines are added to above brace
        self.line_stack.push(vec![]);
        self.block_stack.push(block);
        self.level += 1;

        // Process children
        self.parse_level(block);

        // Flush out the last child line
        self.add_line();
        assert!(self.line_stack.last().unwrap().is_empty());

        // Remove a level of the stack, and resume processing of the parent line
        self.level -= 1;
        self.block_stack.pop().unwrap();
        self.line_stack.pop().unwrap();

        // Consume closing brace
        if self.ftok.tok == Token::CloseDelim(DelimToken::Brace) {
            self.next_token();
        }
    }

    fn parse_if_then_else(&mut self) {
        assert!(self.ftok.tok.is_keyword(Keyword::If), "expected 'if'");
        self.next_token();

        if !self.parse_stmt_up_to(|t| *t == Token::OpenDelim(DelimToken::Brace)) {
            return;
        }

        self.parse_block(Block::Statements);
        if self.ftok.tok.is_keyword(Keyword::Else) {
            self.next_token();
            if self.ftok.tok.is_keyword(Keyword::If) {
                self.parse_if_then_else();
                return;
            }
            self.try_parse_brace_block(Block::Statements);
        }
    }

    fn parse_match(&mut self) {
        assert!(self.ftok.tok.is_keyword(Keyword::Match), "expected 'match'");
        self.next_token();

        if self.parse_stmt_up_to(|t| *t == Token::OpenDelim(DelimToken::Brace)) {
            self.parse_child_block(Block::Match);
        }
    }

    // Parses a single arm of a match statement
    fn parse_match_arm(&mut self) {
        if self.parse_decl_up_to(|t| *t == Token::FatArrow) {
            self.next_token();
            if self.try_parse_brace_block(Block::Statements) {
                if self.ftok.tok == Token::Comma {
                    self.next_token();
                }
            } else {
                if self.parse_stmt_up_to(|t| *t == Token::Comma) {
                    self.next_token();
                }
            }
            self.add_line();
        }
    }

    fn parse_let(&mut self) {
        assert!(self.ftok.tok.is_keyword(Keyword::Let), "expected 'let'");
        self.next_token();

        self.parse_decl_up_to(|t| {
            *t == Token::Semi || *t == Token::Eq
        });

        match self.ftok.tok {
            Token::Semi => {
                // declaration without initialization
                self.next_token();
                self.add_line();
            }
            Token::Eq => {
                self.parse_stmt();
                self.add_line();
            },
            _ => (),
        }
    }

    // Parses a single field or enum variant of a struct or enum declaration
    fn parse_enum_variant_or_struct_field(&mut self) {
        loop {
            match self.ftok.tok {
                Token::Eof => {
                    break;
                },
                Token::Comma => {
                    self.next_token();
                    self.add_line();
                    break;
                },
                Token::CloseDelim(DelimToken::Brace) => {
                    self.add_line();
                    break;
                },
                Token::Lt => {
                    self.parse_generics();
                },
                Token::OpenDelim(d) => {
                    self.parse_delim_pair(Context::Declaration, d);
                }
                _ => {
                    self.next_token();
                },
            }
        }
    }

    fn parse_attribute(&mut self) {
        assert!(self.ftok.tok == Token::Pound, "expected '#'");
        self.next_token();

        if self.ftok.tok == Token::Not {
            self.next_token();
        }

        if self.ftok.tok == Token::OpenDelim(DelimToken::Bracket) {
            if self.parse_delim_pair(Context::Declaration, DelimToken::Bracket) {
                self.add_line();
            }
        }
    }

    // Parses any type of loop (for, while, loop)
    fn parse_loop(&mut self) {
        let is_for = self.ftok.tok.is_keyword(Keyword::For);
        let is_while = self.ftok.tok.is_keyword(Keyword::While);
        let is_loop = self.ftok.tok.is_keyword(Keyword::Loop);
        assert!(is_for || is_while || is_loop, "expected 'loop' or 'for' or 'while'");

        self.next_token();
        if self.parse_stmt_up_to(|t| *t == Token::OpenDelim(DelimToken::Brace)) {
            if self.try_parse_brace_block(Block::Statements) {
                self.add_line();
            }
        }
    }

    fn parse_use(&mut self) {
        assert!(self.ftok.tok.is_keyword(Keyword::Use), "expected 'use'");
        self.next_token();
        if self.parse_decl_up_to(|t| *t == Token::Semi) {
            self.next_token();
            self.add_line();
        }
    }

    fn parse_mod(&mut self) {
        assert!(self.ftok.tok.is_keyword(Keyword::Mod), "expected 'mod'");
        self.next_token();
        self.parse_decl_up_to(|t| {
            *t == Token::OpenDelim(DelimToken::Brace) || *t == Token::Semi
        });
        match self.ftok.tok {
            Token::Semi => {
                self.next_token();
                self.add_line();
            },
            Token::OpenDelim(DelimToken::Brace) => {
                self.parse_block(Block::Statements);
                self.add_line();
            }
            _ => {},
        }
    }

    fn parse_delim_pair(&mut self, context: Context, delim: DelimToken) -> bool {
        let open = Token::OpenDelim(delim);
        let close = Token::CloseDelim(delim);

        assert!(self.ftok.tok == open);
        self.next_token();

        match context {
            Context::Declaration => {
                self.parse_decl_up_to(|t| match t {&Token::CloseDelim(_) => true, _ => false, });
            }
            Context::Statements => {
                self.parse_stmt_up_to(|t| match t {&Token::CloseDelim(_) => true, _ => false, });
            }
        }

        if self.ftok.tok == close {
            self.next_token();
            return true;
        }
        false
    }

    // Parses a declaration (enum, struct, eimpl, trait, fn, type)
    fn parse_decl(&mut self, block: Block) {
        assert!(self.ftok.tok.is_keyword(Keyword::Enum) ||
                self.ftok.tok.is_keyword(Keyword::Struct) ||
                self.ftok.tok.is_keyword(Keyword::Impl) ||
                self.ftok.tok.is_keyword(Keyword::Trait) ||
                self.ftok.tok.is_keyword(Keyword::Fn) ||
                self.ftok.tok.is_keyword(Keyword::Type));
        self.next_token();

        loop {
            match self.ftok.tok {
                Token::Eof => {
                    break;
                },
                Token::CloseDelim(..) => {
                    break;
                },
                Token::Semi => {
                    self.next_token();
                    self.add_line();
                    break;
                },
                Token::OpenDelim(DelimToken::Brace) => {
                    self.parse_block(block);
                    self.add_line();
                    break;
                },
                Token::OpenDelim(delim) => {
                    self.parse_delim_pair(Context::Declaration, delim);
                },
                Token::Lt => {
                    self.parse_generics();
                },
                _ => {
                    self.next_token();
                },
            }
        }
    }

    fn parse_stmt(&mut self) {
        if self.parse_stmt_up_to(|t| *t == Token::Semi) {
            self.next_token();
            self.add_line();
        }
    }

    fn parse_stmt_up_to<P>(&mut self, pred: P) -> bool where P: Fn(&Token) -> bool {
        let mut may_be_struct_init = false;

        loop {
            if pred(&self.ftok.tok) {
                return true;
            }
            match self.ftok.tok {
                Token::Eof => {
                    return false;
                },
                Token::CloseDelim(..) => {
                    self.add_line();
                    return false;
                },
                Token::OpenDelim(DelimToken::Brace) if may_be_struct_init => {
                    // struct inits on same line
                    self.parse_delim_pair(Context::Statements, DelimToken::Brace);
                },
                Token::OpenDelim(DelimToken::Brace) => {
                    self.parse_block(Block::Statements);
                },
                Token::OpenDelim(delim) => {
                    self.parse_delim_pair(Context::Statements, delim);
                },
                Token::Ident(..) if self.is_macro_invocation() => {
                    self.next_token(); // macro name
                    self.next_token(); // macro exclamation mark
                    if self.try_parse_brace_block(Block::Statements) {
                        self.add_line();
                        return false;
                    }
                },
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::If) => {
                    self.parse_if_then_else();
                },
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Match) => {
                    self.parse_match();
                },
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::As) => {
                    self.next_token();
                    self.parse_type();
                },
                Token::Ident(..) => {
                    self.next_token();
                    may_be_struct_init = true;
                    continue;
                },
                Token::ModSep => {
                    self.next_token();
                    if self.ftok.tok == Token::Lt {
                        self.parse_generics();
                        may_be_struct_init = true;
                        continue;
                    }
                },
                _ => {
                    self.next_token();
                }
            }
            may_be_struct_init = false;
        }
    }

    fn parse_decl_up_to<P>(&mut self, pred: P) -> bool where P: Fn(&Token) -> bool {
        loop {
            if pred(&self.ftok.tok) {
                return true;
            }
            match self.ftok.tok {
                Token::Eof => {
                    return false;
                },
                Token::CloseDelim(..) => {
                    self.add_line();
                    return false;
                },
                Token::OpenDelim(delim) => {
                    self.parse_delim_pair(Context::Declaration, delim);
                },
                Token::Lt => {
                    self.parse_generics();
                }
                _ => {
                    self.next_token();
                }
            }
        }
    }

    fn parse_type(&mut self) {
        // consume pointers etc
        loop {
            match self.ftok.tok {
                Token::Eof => return,
                Token::BinOp(BinOpToken::And) |
                Token::BinOp(BinOpToken::Star) |
                Token::Lifetime(..) => {
                    self.next_token();
                }
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Mut) ||
                                    self.ftok.tok.is_keyword(Keyword::Const) => {
                    self.next_token();
                }
                _ => break,
            }
        }

        // consume path
        loop {
            match self.ftok.tok {
                Token::ModSep |
                Token::Ident(..) => {
                    self.next_token();
                }
                _ => break,
            }
        }

        // consume generics
        if self.ftok.tok == Token::Lt  {
            self.parse_generics();
        }
    }

    fn parse_generics(&mut self) {
        assert!(self.ftok.tok == Token::Lt, "expected '<'");
        self.next_token();

        let mut nest_count = 0u32;
        loop {
            match self.ftok.tok {
                Token::Eof => {
                    break;
                },
                Token::CloseDelim(DelimToken::Brace) => {
                    break;
                },
                Token::Lt => {
                    self.next_token();
                    nest_count += 1;
                },
                Token::Gt if nest_count > 0 => {
                    self.next_token();
                    nest_count -= 1;
                },
                Token::BinOp(BinOpToken::Shr) if nest_count > 1 => {
                    self.split_generic_shr_into_two_gt();
                    nest_count -= 2;
                },
                Token::BinOp(BinOpToken::Shr) => {
                    self.split_generic_shr_into_two_gt();
                    break;
                },
                Token::Gt => {
                    self.next_token();
                    break;
                },
                _ => {
                    self.next_token();
                },
            }
        }
    }

    fn parse_macro_rules(&mut self) {
        assert!(self.is_macro_rules());
        self.next_token(); // skip "macro_rules" identifiers
        self.next_token(); // skip ! token

        if let Token::Ident(..) = self.ftok.tok {
            self.next_token(); // macro name
            if let Token::OpenDelim(..) = self.ftok.tok {
                self.parse_block(Block::MacroRules);
                self.next_token_if(&Token::Semi);
                self.add_line();
            }
        }
    }

    fn parse_macro_rule(&mut self) {
        if self.parse_decl_up_to(|t| *t == Token::FatArrow) {
            self.next_token();
            if let Token::OpenDelim(..) = self.ftok.tok {
                self.parse_block(Block::Statements);
                self.next_token_if(&Token::Semi);
                self.add_line();
            }
        }
    }

    fn is_macro_rules(&self) -> bool {
        self.is_macro_invocation() && self.lexer.span_str(self.ftok.span) == "macro_rules"
    }


    // Checks for an identifier, then a !
    fn is_macro_invocation(&self) -> bool {
        match self.ftok.tok {
            Token::Ident(..) => self.lexer.peek() == &Token::Not,
            _ => false,
        }
    }

    fn next_token_if(&mut self, token: &Token) {
        if self.ftok.tok == *token {
            self.next_token();
        }
    }

    // Ends the current line, and pushes it into the output
    fn add_line(&mut self) {
        if self.line_stack.last().unwrap().is_empty() {
            return;
        }

        let finished_line = UnwrappedLine {
            tokens: self.line_stack.pop().unwrap(),
            level: self.level,
            typ: LineType::Unknown,
            block: *self.block_stack.last().unwrap_or(&Block::TopLevel),
            affected : false,
            leading_empty_lines_affected : false,
            children_affected : false,
        };

        match self.line_stack.last_mut() {
            None => {
                // insert into the regular output vector
                self.output.push(finished_line);
            },
            Some(top) => {
                // insert into the last token on the top of the stack
                assert!(!top.is_empty());
                let last_tok: &mut FormatToken = top.last_mut().unwrap();
                last_tok.children.push(finished_line);

            }
        }

        // replace popped vector ready for the next line
        self.line_stack.push(vec![]);
    }

    // Gets the next token in the lexer, and places it in self.ftok
    // Any comments with be handled, so other functions do not have to handle any comments.
    fn read_token(&mut self) {
        let mut comments_in_line = true;
        loop {
            let ftok = self.lexer.next().unwrap();
            if ftok.is_comment() {
                if ftok.is_on_newline() || ftok.is_first_token {
                    comments_in_line = false;
                }
                if comments_in_line {
                    self.push_token(ftok);
                } else {
                    self.comments_before_next_token.push(ftok)
                }
            } else {
                self.ftok = ftok;
                break;
            }
        }
    }

    fn push_token(&mut self, token: FormatToken) {
        self.line_stack.last_mut().unwrap().push(token);
    }

    // Pushes any stored comments into the output
    fn flush_comments(&mut self, new_line_before_next: bool) {
        // FIXME: is it really necessary to create a new vector?
        let comments = mem::replace(&mut self.comments_before_next_token, vec![]);
        let just_comments = self.line_stack.last().unwrap().is_empty();

        for comment in comments {
            if comment.is_on_newline() && just_comments {
                self.add_line();
            }
            self.push_token(comment);
        }

        if new_line_before_next && just_comments {
            self.add_line();
        }
    }

    // Proceed to the next token. Any comments will be skipped.
    fn next_token(&mut self) {
        if self.ftok.tok == Token::Eof {
            return;
        }

        let is_on_newline = self.ftok.is_on_newline();
        self.flush_comments(is_on_newline);

        let ftok = mem::replace(&mut self.ftok, FormatToken::default());
        self.push_token(ftok);
        self.read_token();
    }

    // Shr tokens (>>) can also represent closing two levels of generic brackets.
    // Go ahead and split them into two separate Gt tokens (>), to minimise any
    // further pain and suffering. It will reduce bugs as well.
    fn split_generic_shr_into_two_gt(&mut self) {
        assert_eq!(self.ftok.tok, Token::BinOp(BinOpToken::Shr));
        assert_eq!(self.ftok.span.lo + BytePos(2), self.ftok.span.hi);
        assert_eq!(self.ftok.column_width, 2);
        assert_eq!(self.ftok.children.len(), 0);

        let mut ftok = self.ftok.clone();
        let span_mid = ftok.span.lo + BytePos(1);

        self.ftok.tok = Token::Gt;
        self.ftok.span = mk_sp(ftok.span.lo, span_mid);
        self.ftok.column_width = 1;

        ftok.tok = Token::Gt;
        ftok.span = mk_sp(span_mid, ftok.span.hi);
        ftok.preceding_whitespace_span = mk_sp(span_mid, span_mid);
        ftok.original_column = ftok.original_column + 1;
        ftok.is_first_token = false;
        ftok.newlines_before = 0;
        ftok.column_width = 1;

        // push both tokens through
        self.next_token();
        self.push_token(ftok);
    }
}
