use std::mem;
use syntax::codemap::{mk_sp, BytePos};
use syntax::parse::token::keywords::Keyword;
use syntax::parse::token::{Token, DelimToken, BinOpToken};
use token::{FormatToken, FormatTokenLexer, FormatDecision};

// An unwrapped line is a sequence of FormatTokens, that we would like to
// put on a single line if there was no column limit. Changing the formatting
// within an unwrapped line does not affect any other unwrapped lines.
pub struct UnwrappedLine {
    pub tokens: Vec<FormatToken>,
    pub children: Vec<UnwrappedLine>,
    pub level: u32,
}

impl UnwrappedLine {
    pub fn parse_lines(lexer: &mut FormatTokenLexer) -> Vec<UnwrappedLine> {
        let mut parser = UnwrappedLineParser {
            lexer: lexer,
            output: vec![],
            // FIXME: Placeholder token. Change to an Option?
            ftok: FormatToken {
                tok: Token::Eof,
                span: mk_sp(BytePos(0), BytePos(0)),
                preceding_whitespace_span: mk_sp(BytePos(0), BytePos(0)),
                is_first_token: false,
                newlines_before: 0,
                original_column: 0,
                decision: FormatDecision::Unformatted,
            },
            line: vec![],
            level_stack: vec![],
            level: 0,
            comments_before_next_token: vec![],
        };

        parser.parse();
        assert!(parser.line.is_empty());
        assert!(parser.level == 0);
        assert!(parser.comments_before_next_token.is_empty());
        assert!(parser.level_stack.is_empty());
        parser.output
    }
}

struct UnwrappedLineParser<'a, 'b: 'a> {
    output: Vec<UnwrappedLine>,
    lexer: &'a mut FormatTokenLexer<'b>,
    ftok: FormatToken,
    line: Vec<FormatToken>,
    level_stack: Vec<UnwrappedLine>,
    level: u32,
    comments_before_next_token: Vec<FormatToken>,
}

#[derive(Copy,Clone,Eq,PartialEq,Debug)]
enum Block {
    Impl,
    Match,
    Statements,
    StructOrEnum,
    TopLevel,
    Trait,
    StructInit,
}

impl<'a, 'b> UnwrappedLineParser<'a, 'b> {
    fn parse(&mut self) {
        self.read_token();
        self.parse_level(Block::TopLevel);

        // add any remaining tokens
        self.flush_comments(true);
        self.add_line();

        // push through eof
        let eof = self.ftok.clone();
        self.push_token(eof);
        self.add_line();
    }

    fn parse_level(&mut self, block: Block) {
        loop {
            match self.ftok.tok {
                Token::Eof => {
                    break;
                },
                Token::CloseDelim(DelimToken::Brace) if block == Block::TopLevel => {
                    // error
                    self.next_token();
                    self.add_line();
                },
                Token::CloseDelim(DelimToken::Brace) => {
                    break;
                },
                Token::Pound => {
                    self.parse_attribute();
                },
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Loop) => {
                    self.parse_loop();
                },
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Use) => {
                    self.parse_use();
                },
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Fn) => {
                    self.parse_decl(Block::Statements);
                },
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Struct) => {
                    self.parse_decl(Block::StructOrEnum);
                },
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Enum) => {
                    self.parse_decl(Block::StructOrEnum);
                },
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Impl) => {
                    self.parse_decl(Block::Impl);
                },
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Trait) => {
                    self.parse_decl(Block::Trait);
                },
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Pub) => {
                    self.next_token();
                },
                _ => {
                    match block {
                        Block::StructOrEnum => self.parse_enum_variant_or_struct_field(),
                        Block::Match => self.parse_match_item(),
                        Block::StructInit => self.parse_field_init(),
                        Block::Statements |
                        Block::TopLevel |
                        Block::Trait |
                        Block::Impl => self.parse_stmt(),
                    }
                }
            }
        }
    }

    fn try_parse_block(&mut self, block: Block) -> bool {
        if self.ftok.tok == Token::OpenDelim(DelimToken::Brace) {
            self.parse_block(block);
            true
        } else {
            false
        }
    }

    fn parse_block(&mut self, block: Block) {
        assert!(self.ftok.tok == Token::OpenDelim(DelimToken::Brace), "expected '{'");
        self.next_token();

        // Push current line onto level stack,
        // to prepare to collect the children
        let intial_level = self.level;
        let level = self.end_line();
        self.level += 1;
        self.level_stack.push(level);

        // Collect children into the line
        self.parse_level(block);

        // Pop the line off the level stack
        // and place into its parent
        let level = self.level_stack.pop().unwrap();
        self.push_line(level);

        if self.ftok.tok != Token::CloseDelim(DelimToken::Brace) {
            self.level = intial_level;
            return;
        }

        // Eat closing brace
        self.next_token();

        // Eat trailing semicolons/commas
        if self.ftok.tok == Token::Semi || self.ftok.tok == Token::Comma {
            self.next_token();
        }
        self.level = intial_level;
    }

    fn parse_field_init(&mut self) {
        if self.parse_stmt_up_to(|t| *t == Token::Comma) {
            self.next_token();
            self.add_line();
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
            self.try_parse_block(Block::Statements);
        }
        self.add_line();
    }

    fn parse_match(&mut self) {
        assert!(self.ftok.tok.is_keyword(Keyword::Match), "expected 'match'");
        self.next_token();

        if self.parse_stmt_up_to(|t| *t == Token::OpenDelim(DelimToken::Brace)) {
            self.parse_block(Block::Match);
        }
    }

    fn parse_match_item(&mut self) {
        loop {
            match self.ftok.tok {
                Token::Eof => {
                    break;
                },
                Token::FatArrow => {
                    self.next_token();
                    if !self.try_parse_block(Block::Statements) {
                        if self.parse_stmt_up_to(|t| *t == Token::Comma) {
                            self.next_token();
                        }
                    }
                    self.add_line();
                    break;
                },
                _ => {
                    self.next_token();
                },
            }
        }
    }

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
                    self.parse_delim_pair(d);
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
        if self.ftok.tok == Token::OpenDelim(DelimToken::Bracket) {
            if self.parse_delim_pair(DelimToken::Bracket) {
                self.add_line();
            }
        }
    }

    fn parse_loop(&mut self) {
        assert!(self.ftok.tok.is_keyword(Keyword::Loop), "expected 'loop'");
        self.next_token();
        if self.try_parse_block(Block::Statements) {
            self.add_line();
        }
    }

    fn parse_use(&mut self) {
        assert!(self.ftok.tok.is_keyword(Keyword::Use), "expected 'use'");
        self.next_token();
        loop {
            match self.ftok.tok {
                Token::Semi => {
                    self.next_token();
                    self.add_line();
                    break;
                }
                _ => {
                    self.next_token();
                }
            }
        }
    }


    fn parse_delim_pair(&mut self, delim: DelimToken) -> bool {
        let open = Token::OpenDelim(delim);
        let close = Token::CloseDelim(delim);

        assert!(self.ftok.tok == open);
        self.next_token();

        loop {
            if self.ftok.tok == Token::Eof {
                return false;
            } else if self.ftok.tok == close {
                self.next_token();
                return true;
            } else if let Token::OpenDelim(d) = self.ftok.tok {
                if !self.parse_delim_pair(d) {
                    return false;
                }
            } else if let Token::CloseDelim(..) = self.ftok.tok {
                // unmatched delimeter error
                return false;
            } else {
                self.next_token();
            }
        }
    }

    fn parse_decl(&mut self, block: Block) {
        assert!(self.ftok.tok.is_keyword(Keyword::Enum) ||
                self.ftok.tok.is_keyword(Keyword::Struct) ||
                self.ftok.tok.is_keyword(Keyword::Impl) ||
                self.ftok.tok.is_keyword(Keyword::Trait) ||
                self.ftok.tok.is_keyword(Keyword::Fn));
        self.next_token();

        loop {
            match self.ftok.tok {
                Token::Eof => {
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
                }
                Token::Lt => {
                    self.parse_generics();
                }
                _ => {
                    self.next_token();
                }
            }
        }
    }

    fn parse_stmt(&mut self) {
        self.parse_stmt_up_to(|_| false);
    }

    fn parse_stmt_up_to<P>(&mut self, pred: P) -> bool where P: Fn(&Token) -> bool {
        // FIXME: This is totally broken. Lots of blocks are considered to be
        //        a struct init when they are not. For example: for a in b { }
        let mut may_be_struct_init = false;

        loop {
            if pred(&self.ftok.tok) {
                return true;
            }
            match self.ftok.tok {
                Token::Eof => {
                    return false;
                },
                Token::Semi => {
                    self.next_token();
                    self.add_line();
                    return false;
                }
                Token::CloseDelim(DelimToken::Brace) => {
                    self.add_line();
                    return false;
                },
                Token::OpenDelim(DelimToken::Brace) if may_be_struct_init => {
                    self.parse_block(Block::StructInit);
                    // Don't end line, allow more tokens to follow
                }
                Token::OpenDelim(DelimToken::Brace) => {
                    self.parse_block(Block::Statements);
                    self.add_line();
                }
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::If) => {
                    self.parse_if_then_else();
                },
                Token::Ident(..) if self.ftok.tok.is_keyword(Keyword::Match) => {
                    self.parse_match();
                },
                Token::Ident(..) => {
                    self.next_token();
                    may_be_struct_init = true;
                    continue;
                }
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

    fn parse_generics(&mut self) {
        assert!(self.ftok.tok == Token::Lt, "expected '<'");
        self.next_token();

        let mut nest_count = 0u32;
        loop {
            match self.ftok.tok {
                Token::Eof => {
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
                    self.next_token();
                    nest_count -= 2;
                },
                Token::Gt | Token::BinOp(BinOpToken::Shr) => {
                    self.next_token();
                    break;
                },
                _ => {
                    self.next_token();
                },
            }
        }
    }

    fn add_line(&mut self) {
        if self.line.is_empty() {
            return;
        }

        let line = self.end_line();
        self.push_line(line);
    }

    fn push_line(&mut self, line: UnwrappedLine) {
        match self.level_stack.last_mut() {
            None => self.output.push(line),
            Some(last) => last.children.push(line),
        };
    }

    fn end_line(&mut self) -> UnwrappedLine {
        assert!(!self.line.is_empty());
        UnwrappedLine {
            tokens: mem::replace(&mut self.line, vec![]),
            level: self.level,
            children: vec![],
        }
    }

    fn read_token(&mut self) {
        let mut comments_in_line = true;
        loop {
            let ftok = self.lexer.next().unwrap();
            match ftok.tok {
                Token::DocComment(..) | Token::Comment => {
                    if ftok.is_on_newline() || ftok.is_first_token {
                        comments_in_line = false;
                    }
                    if comments_in_line {
                        self.push_token(ftok);
                    } else {
                        self.comments_before_next_token.push(ftok)
                    }
                },
                _ => {
                    self.ftok = ftok;
                    break;
                }
            }
        }
    }

    fn push_token(&mut self, token: FormatToken) {
        self.line.push(token);
    }

    fn flush_comments(&mut self, new_line_before_next: bool) {
        // FIXME: is it really necessary to create a new vector?
        let comments = mem::replace(&mut self.comments_before_next_token, vec![]);
        let just_comments = self.line.is_empty();

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

    fn next_token(&mut self) {
        if self.ftok.tok == Token::Eof {
            return;
        }

        let is_on_newline = self.ftok.is_on_newline();
        self.flush_comments(is_on_newline);

        let ftok = self.ftok.clone();
        self.push_token(ftok);
        self.read_token();
    }
}
