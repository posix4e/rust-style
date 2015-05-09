use format_options::{FormatStyle, Penalty};
use syntax::codemap::{mk_sp, Span, Pos, BytePos};
use syntax::parse::lexer::{StringReader, Reader};
use syntax::parse::ParseSess;
use syntax::parse::token::keywords::Keyword;
use syntax::parse::token::{self, Token, BinOp, BinOpToken};
use syntax;
use unwrapped_line::UnwrappedLine;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FormatDecision {
  Unformatted,
  Continue,
  Break,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenType {
    // only used for macro the exlam in invocations (for example "println!(...)")
    Postfix,
    BinaryOperator,
    UnaryOperator,
    LambdaParamsStart,
    LambdaParamsEnd,
    GenericBracket,
    PatternOr,
    PatternGuardIf,
    FnDeclArrow,
    Unknown
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Precedence {
    Unknown        = 0,  // Not a binary operator
    PatternOr      = 1,  // | (in a pattern context)
    Comma          = 2,  // ,
    Assignment     = 3,  // = += -= *= /= <<= >>= &= ^= |=
    LogicalOr      = 4,  // ||
    LogicalAnd     = 5,  // &&
    Relational     = 6,  // == != < > <= >=
    BitInclusiveOr = 7,  // |
    BitExclusiveOr = 8,  // ^
    BitAnd         = 9,  // &
    Shift          = 10, // << >>
    Additive       = 11, // + -
    Multiplictive  = 12, // * / %
    As             = 13, // as
}

// These arn't binary operators, but are used when parsing expressions
pub const PRECEDENCE_UNARY: i32 = Precedence::As as i32 + 1;
pub const PRECEDENCE_DOT: i32 = Precedence::As as i32 + 2;

impl Precedence {
    pub fn from_i32(val: i32) -> Option<Precedence> {
        // FIXME: This is error prone, and really shouldn't be necessary.
        //        But FromPrimitive was removed. Fix this with its safe replacement.
        Some(match val {
            0  => Precedence::Unknown,
            1  => Precedence::PatternOr,
            2  => Precedence::Comma,
            3  => Precedence::Assignment,
            4  => Precedence::LogicalOr,
            5  => Precedence::LogicalAnd,
            6  => Precedence::Relational,
            7  => Precedence::BitInclusiveOr,
            8  => Precedence::BitExclusiveOr,
            9  => Precedence::BitAnd,
            10 => Precedence::Shift,
            11 => Precedence::Additive,
            12 => Precedence::Multiplictive,
            13 => Precedence::As,
            _  => return None,
        })
    }

    pub fn to_i32(&self) -> i32 {
        *self as i32
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum CommentType {
    Line,
    Block,
}

#[derive(Debug, Clone)]
pub struct FormatToken {
    // The index of the token in the unwrapped line
    pub index: usize,
    // The type of the token
    pub typ: TokenType,
    // The token
    pub tok: Token,
    // The byte span of the token itself in the input
    pub span: Span,
    // The byte span of the whitespace before this token in the original input
    pub preceding_whitespace_span: Span,
    // Whether the token is the first token in the input
    pub is_first_token: bool,
    // The number of new lines preceding this token in the original input
    pub newlines_before: u32,
    // The character position (not bytes) of the start of the token
    // in the original input
    pub original_column: u32,
    // The row position in the original input, 0-based.
    pub original_row: u32,
    // The character width (not bytes) of this token.
    pub column_width: u32,
    // The total length of the line up to and including this token.
    pub total_length: u32,
    // The column width of the final line of this token, if it is multiline.
    pub last_line_column_width: Option<u32>,

    // Children lines that come after this token
    pub children: Vec<UnwrappedLine>,
    // Whether a break will appear before this token in the output.
    pub decision: FormatDecision,
    // The penalty value for inserting a line break before this token.
    pub split_penalty: Penalty,
    // Whether a space should be placed before this token.
    pub spaces_required_before: u32,
    // Whether a line break is allowed before this token.
    pub can_break_before: bool,
    // Whether a line break is required before this token.
    pub must_break_before: bool,
    // The binding strength of a token. This is a combined value of
    // operator precedence, parenthesis nesting, etc.
    // Used to calculate split_penalty.
    pub binding_strength: Penalty,
    // The type of comment, if this is a comment token.
    pub comment_type: Option<CommentType>,

    // Whether this token starts a binary expression.
    pub starts_binary_expression: bool,
    // Whether this token ends a binary expression.
    pub ends_binary_expression: bool,
    // The 0-based index of the binary operators of the same precedence.
    pub operator_index: u32,
    // Whether this is the last operator in a sequence of operators in the
    // same precedence.
    pub last_operator: bool,
    // The precedence fake parentheses starting at this token.
    // They are stored in reverse order.
    pub fake_lparens: Vec<Precedence>,
    // The number of fake parenthesis which end on this token
    pub fake_rparens: u32,
}

impl Default for FormatToken {
    fn default() -> FormatToken {
        FormatToken {
            index: 0,
            typ: TokenType::Unknown,
            tok: Token::Eof,
            span: mk_sp(BytePos(0), BytePos(0)),
            preceding_whitespace_span: mk_sp(BytePos(0), BytePos(0)),
            is_first_token: false,
            newlines_before: 0,
            original_column: 0,
            original_row: 0,
            total_length: 0,
            column_width: 0,
            last_line_column_width: None,
            children: vec![],
            decision: FormatDecision::Unformatted,
            split_penalty: 0,
            spaces_required_before: 0,
            can_break_before: false,
            must_break_before: false,
            binding_strength: 0,
            comment_type: None,
            starts_binary_expression: false,
            ends_binary_expression: false,
            operator_index: 0,
            last_operator: false,
            fake_lparens: vec![],
            fake_rparens: 0,
        }
    }
}

impl FormatToken {
    pub fn is_on_newline(&self) -> bool {
        self.newlines_before > 0
    }

    pub fn precedence(&self) -> Option<Precedence> {
        if self.tok.is_keyword(Keyword::As) {
            return Some(Precedence::As);
        }
        if self.typ == TokenType::PatternOr {
            return Some(Precedence::PatternOr);
        }
        if self.typ != TokenType::BinaryOperator && self.tok != Token::Comma {
            return None;
        }
        match self.tok {
            Token::Comma                      => Some(Precedence::Comma),
            Token::Eq                         => Some(Precedence::Assignment),
            Token::BinOpEq(..)                => Some(Precedence::Assignment),
            Token::OrOr                       => Some(Precedence::LogicalOr),
            Token::AndAnd                     => Some(Precedence::LogicalAnd),
            Token::Lt                         => Some(Precedence::Relational),
            Token::Le                         => Some(Precedence::Relational),
            Token::EqEq                       => Some(Precedence::Relational),
            Token::Ne                         => Some(Precedence::Relational),
            Token::Ge                         => Some(Precedence::Relational),
            Token::Gt                         => Some(Precedence::Relational),
            Token::BinOp(BinOpToken::Or)      => Some(Precedence::BitInclusiveOr),
            Token::BinOp(BinOpToken::Caret)   => Some(Precedence::BitExclusiveOr),
            Token::BinOp(BinOpToken::And)     => Some(Precedence::BitAnd),
            Token::BinOp(BinOpToken::Shl)     => Some(Precedence::Shift),
            Token::BinOp(BinOpToken::Shr)     => Some(Precedence::Shift),
            Token::BinOp(BinOpToken::Plus)    => Some(Precedence::Additive),
            Token::BinOp(BinOpToken::Minus)   => Some(Precedence::Additive),
            Token::BinOp(BinOpToken::Star)    => Some(Precedence::Multiplictive),
            Token::BinOp(BinOpToken::Slash)   => Some(Precedence::Multiplictive),
            Token::BinOp(BinOpToken::Percent) => Some(Precedence::Multiplictive),
            _ => None,
        }
    }

    pub fn opens_scope(&self) -> bool {
        match self.tok {
            Token::OpenDelim(..) => true,
            Token::Lt => self.typ == TokenType::GenericBracket,
             _ => false,
        }
    }

    pub fn closes_scope(&self) -> bool {
        match self.tok {
            Token::CloseDelim(..) => true,
            Token::Gt => self.typ == TokenType::GenericBracket,
             _ => false,
        }
    }

    pub fn next<'a>(&self, line: &'a UnwrappedLine) -> Option<&'a FormatToken> {
        line.tokens.get(self.index + 1)
    }

    pub fn is_comment(&self) -> bool {
        match self.tok {
            Token::Comment => true,
            Token::DocComment(..) => true,
            _ => false,
        }
    }

    pub fn is_trailing_comment(&self, line: &UnwrappedLine) -> bool {
        match self.comment_type {
            None => false,
            Some(CommentType::Line) => true,
            Some(CommentType::Block) => match self.next(line) {
                None => true,
                Some(next) => next.newlines_before > 0
            }
        }
    }
}

pub struct FormatTokenLexer<'s> {
    #[allow(dead_code)]
    session: &'s ParseSess,
    lexer: StringReader<'s>,
    eof: bool,
    is_first_token: bool,
    column: u32,
    row: u32,
    style: &'s FormatStyle,
    previous_token_span: Span,
}

impl<'s> FormatTokenLexer<'s> {
    pub fn new<'a>(source: &'a str, session: &'a ParseSess, style: &'a FormatStyle) -> FormatTokenLexer<'a> {
        // TODO: Determine if a real name is actually needed
        let name = "".to_string();
        let filemap = syntax::parse::string_to_filemap(&session, source.to_string(), name);
        let lexer = syntax::parse::lexer::StringReader::new(&session.span_diagnostic, filemap);

        FormatTokenLexer {
            lexer: lexer,
            session: session,
            eof: false,
            is_first_token: true,
            column: 0,
            row: 0,
            style: style,
            previous_token_span: mk_sp(BytePos(0), BytePos(0)),
        }
    }

    pub fn peek(&self) -> &Token {
        &self.lexer.peek_tok
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
        let mut row = self.row;
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
        for character in self.span_str(tok_sp.sp).chars() {
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
                let text = self.span_str(tok_sp.sp);
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
            original_column: column,
            original_row: row,
            decision: FormatDecision::Unformatted,
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
