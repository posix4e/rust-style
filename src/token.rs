use style::{FormatStyle, Penalty};
use syntax::codemap::{mk_sp, Span, Pos, BytePos};
use syntax::parse::lexer::{StringReader, Reader};
use syntax::parse::token::{self, Token};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FormatDecision {
  Unformatted,
  Continue,
  Break,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenType {
    Pointer,
    BinaryOperator,
    UnaryOperator,
    LambdaArgsStart,
    LambdaArgsEnd,
    GenericBracket,
    Unknown
}


#[derive(Clone, Debug)]
pub struct FormatToken {
    pub typ: TokenType,
    pub tok: Token,
    pub span: Span,
    pub preceding_whitespace_span: Span,
    pub is_first_token: bool,
    pub newlines_before: u32,
    pub original_column: u32,
    pub decision: FormatDecision,
    pub split_penalty: Penalty,
    pub column_width: u32,
    pub spaces_required_before: u32,
    pub can_break_before: bool,
    pub must_break_before: bool,
    pub binding_strength: Penalty,
    pub matching_paren_index: Option<usize>,
}

impl FormatToken {
    pub fn is_on_newline(&self) -> bool {
        self.newlines_before > 0
    }
}

pub struct FormatTokenLexer<'s> {
    lexer: StringReader<'s>,
    eof: bool,
    is_first_token: bool,
    column: u32,
    style: FormatStyle,
    previous_token_span: Span,
}

impl<'s> FormatTokenLexer<'s> {
    pub fn new(lexer: StringReader, style: FormatStyle) -> FormatTokenLexer {
        FormatTokenLexer {
            lexer: lexer,
            eof: false,
            is_first_token: true,
            column: 0,
            style: style,
            previous_token_span: mk_sp(BytePos(0), BytePos(0)),
        }
    }

    pub fn span_str(&self, sp: Span) -> &str {
        if sp.lo.0 == sp.hi.0 {
            return "";
        }

        let local_begin = self.lexer.span_diagnostic.cm.lookup_byte_offset(sp.lo);
        let local_end = self.lexer.span_diagnostic.cm.lookup_byte_offset(sp.hi);
        let start_index = local_begin.pos.to_usize();
        let end_index = local_end.pos.to_usize();
        self.src_str(start_index, end_index)
    }

    pub fn src_str(&self, start_index: usize, end_index: usize) -> &str {
        &self.lexer.filemap.src.as_ref().unwrap()[start_index..end_index]
    }
}

impl<'s> Iterator for FormatTokenLexer<'s> {
    type Item = FormatToken;

    fn next(&mut self) -> Option<FormatToken> {
        if self.eof {
            return None;
        }

        let mut column = self.column;
        let mut tok_sp = self.lexer.next_token();
        let mut newlines_before = 0;
        while let Token::Whitespace = tok_sp.tok {
            let whitespace = tok_sp;
            tok_sp = self.lexer.next_token();
            for c in self.span_str(whitespace.sp).chars() {
                match c {
                    '\n' => {
                        newlines_before += 1;
                        column = 0;
                    },
                    '\r' => {
                        column = 0;
                    },
                    '\t' => {
                        column += self.style.tab_width - column % self.style.tab_width;
                    },
                    _ => {
                        column += 1;
                    },
                }
            }
        }

        let mut column_width = 0;
        for _ in self.span_str(tok_sp.sp).chars() {
            column_width += 1;
        }

        let token = FormatToken {
            typ: TokenType::Unknown,
            tok: tok_sp.tok,
            span: tok_sp.sp,
            preceding_whitespace_span: mk_sp(self.previous_token_span.hi, tok_sp.sp.lo),
            is_first_token: self.is_first_token,
            newlines_before: newlines_before,
            original_column: column,
            decision: FormatDecision::Unformatted,
            split_penalty: 0,
            column_width: column_width,
            spaces_required_before: 0,
            can_break_before: false,
            must_break_before: false,
            binding_strength: 0,
            matching_paren_index: None,
        };

        column += column_width;

        self.column = column;
        self.eof = token.tok == token::Eof;
        self.is_first_token = false;
        self.previous_token_span = tok_sp.sp;

        Some(token)
    }
}
