use syntax::codemap::{mk_sp, Span, Pos, BytePos};
use syntax::parse::lexer::{StringReader, Reader};
use syntax::parse::token::{self, Token};
use style::FormatStyle;

#[derive(Copy, Clone, Debug)]
pub enum FormatDecision {
  Unformatted,
  Continue,
  Break,
}

#[derive(Clone, Debug)]
pub struct FormatToken {
    pub tok: Token,
    pub span: Span,
    pub preceding_whitespace_span: Span,
    pub is_first_token: bool,
    pub newlines_before: u32,
    pub original_column: u32,
    pub decision: FormatDecision,
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

        let token = FormatToken {
            tok: tok_sp.tok,
            span: tok_sp.sp,
            preceding_whitespace_span: mk_sp(self.previous_token_span.hi, tok_sp.sp.lo),
            is_first_token: self.is_first_token,
            newlines_before: newlines_before,
            original_column: column,
            decision: FormatDecision::Unformatted,
        };

        for _ in self.span_str(tok_sp.sp).chars() {
            column += 1;
        }

        self.column = column;
        self.eof = token.tok == token::Eof;
        self.is_first_token = false;
        self.previous_token_span = tok_sp.sp;

        Some(token)
    }
}