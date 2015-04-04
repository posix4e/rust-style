use annotated_line::AnnotatedLine;
use format_style::FormatStyle;
use token::FormatTokenLexer;
use replacement::Replacement;

pub struct LineFormatter<'a, 'b: 'a> {
    style: FormatStyle,
    lexer: &'a FormatTokenLexer<'b>,
}

impl<'a, 'b> LineFormatter<'a, 'b> {
    pub fn new(style: FormatStyle, lexer: &'a FormatTokenLexer<'b>) -> LineFormatter<'a, 'b> {
        LineFormatter {
            style: style,
            lexer: lexer,
        }
    }

    pub fn format(&mut self, lines: &[AnnotatedLine]) -> Vec<Replacement> {
        for line in lines {
            let first = &line.tokens[0];
            let indent = line.level * self.style.indent_width;

            // println!("indent({}), original_column({}), {}", indent, first.original_column, self.lexer.span_str(first.sp));
            // TODO
        }

        vec![]
    }
}
