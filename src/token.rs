use format_options::Penalty;
use syntax::codemap::{mk_sp, Span, BytePos};
use syntax::parse::token::keywords::Keyword;
use syntax::parse::token::{Token, BinOp, BinOpToken};
use unwrapped_line::UnwrappedLine;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenType {
    // only used for macro the exlam in invocations (for example "println!(...)")
    Postfix,
    BinaryOperator,
    UnaryOperator,
    LambdaParamsStart,
    LambdaParamsEnd,
    LambdaParamsEmpty,
    FnDeclParamsStart,
    FnDeclParamsEnd,
    FnDeclArrow,
    GenericBracket,
    PatternOr,
    PatternGuardIf,
    Unknown
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Precedence {
    Unknown        = 0,  // Not a binary operator
    Comma          = 1,  // ,
    PatternOr      = 2,  // | (in a pattern context)
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
        let prec = match val {
            0  => Precedence::Unknown,
            1  => Precedence::Comma,
            2  => Precedence::PatternOr,
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
        };

        assert_eq!(prec.to_i32(), val);
        Some(prec)
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
    // The number of spaces preceding this token (after the newlines) in the original input
    pub spaces_before: u32,
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

    // The index of the matching real paren, if this is a real paren
    pub matching_paren: Option<usize>,
    // Whether this token is inside a macro definition, or an invocation of a macro
    // which doesn't appear on the whitelist.
    pub in_non_whitelisted_macro: bool,
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
            spaces_before: 0,
            last_line_column_width: None,
            children: vec![],
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
            matching_paren: None,
            in_non_whitelisted_macro: false,
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
            Token::BinOp(BinOpToken::Or) => self.typ == TokenType::LambdaParamsStart,
             _ => false,
        }
    }

    pub fn closes_scope(&self) -> bool {
        match self.tok {
            Token::CloseDelim(..) => true,
            Token::Gt => self.typ == TokenType::GenericBracket,
            Token::BinOp(BinOpToken::Or) => self.typ == TokenType::LambdaParamsEnd,
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

