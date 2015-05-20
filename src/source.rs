use format_options::FormatStyle;
use std::cmp;
use syntax::codemap::{mk_sp, Span, Pos, BytePos};
use syntax::parse::lexer::{StringReader, Reader};
use syntax::parse::ParseSess;
use syntax::parse::token::{self, Token};
use syntax;
use token::{FormatToken, TokenType, CommentType};

pub struct Source<'a> {
    src: &'a str,
    session: ParseSess,

}

impl<'a> Source<'a> {
    pub fn new<'s>(src: &'s str) -> Source<'s> {
        Source {
            src: src,
            session: syntax::parse::new_parse_sess(),
        }
    }

    pub fn span_str(&self, sp: Span) -> &str {
        if sp.lo.0 == sp.hi.0 {
            return "";
        }

        let local_begin = self.session.span_diagnostic.cm.lookup_byte_offset(sp.lo);
        let local_end = self.session.span_diagnostic.cm.lookup_byte_offset(sp.hi);
        let start_index = local_begin.pos.to_usize();
        let end_index = local_end.pos.to_usize();
        self.src_str(start_index, end_index)
    }

    pub fn src_str(&self, start_index: usize, end_index: usize) -> &str {
        // Eof can span outside the string.
        // Clamp the end to work around this.
        let end_index = cmp::min(end_index, self.src.as_bytes().len());
        &self.src[start_index..end_index]
    }

    pub fn format_token_lexer<'s>(&'s self, style: &'s FormatStyle) -> FormatTokenLexer<'s> {
        // TODO: Determine if a real name is actually needed
        let name = "".to_string();
        let filemap = syntax::parse::string_to_filemap(&self.session, self.src.to_string(), name);
        let lexer = syntax::parse::lexer::StringReader::new(&self.session.span_diagnostic, filemap);

        FormatTokenLexer {
            lexer: lexer,
            src: self,
            eof: false,
            is_first_token: true,
            column: 0,
            row: 0,
            style: style,
            previous_token_span: mk_sp(BytePos(0), BytePos(0)),
        }
    }
}

pub struct FormatTokenLexer<'s> {
    src: &'s Source<'s>,
    lexer: StringReader<'s>,
    eof: bool,
    is_first_token: bool,
    column: u32,
    row: u32,
    style: &'s FormatStyle,
    previous_token_span: Span,
}

impl<'s> Iterator for FormatTokenLexer<'s> {
    type Item = FormatToken;

    fn next(&mut self) -> Option<FormatToken> {
        if self.eof {
            return None;
        }

        let prev_column = self.column;
        let mut column = self.column;
        let mut row = self.row;
        let mut tok_sp = self.lexer.next_token();
        let mut newlines_before = 0;
        while let Token::Whitespace = tok_sp.tok {
            let whitespace = tok_sp;
            tok_sp = self.lexer.next_token();
            for c in self.src.span_str(whitespace.sp).chars() {
                match c {
                    '\n' => {
                        newlines_before += 1;
                        column = 0;
                        row += 1;
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
        let mut row_height = 0;
        let mut last_line_column_width = None;
        for character in self.src.span_str(tok_sp.sp).chars() {
            if character == '\n' {
                last_line_column_width = Some(0);
                row_height += 1;
            }
            match last_line_column_width {
                Some(ref mut last) => *last += 1,
                None => column_width += 1,
            };
        }

        let comment_type = match tok_sp.tok {
            Token::Comment | Token::DocComment(..) => {
                let text = self.src.span_str(tok_sp.sp);
                if text.starts_with("//") {
                    Some(CommentType::Line)
                } else if text.starts_with("/*") {
                    Some(CommentType::Block)
                } else {
                    panic!(format!("Unknown comment style: {}", text))
                }
            }
            _ => None,
        };

        let token = FormatToken {
            typ: TokenType::Unknown,
            tok: tok_sp.tok,
            span: tok_sp.sp,
            preceding_whitespace_span: mk_sp(self.previous_token_span.hi, tok_sp.sp.lo),
            is_first_token: self.is_first_token,
            newlines_before: newlines_before,
            spaces_before: if newlines_before > 0 { column }
                           else { column - prev_column },
            original_column: column,
            original_row: row,
            column_width: column_width,
            last_line_column_width: last_line_column_width,
            comment_type: comment_type,
            ..FormatToken::default()
        };


        column += column_width;
        row += row_height;

        self.column = column;
        self.row = row;
        self.eof = token.tok == token::Eof;
        self.is_first_token = false;
        self.previous_token_span = tok_sp.sp;

        Some(token)
    }
}
