use annotated_line::AnnotatedLine;
use replacement::Replacement;
use std::cmp;
use style::FormatStyle;
use syntax::parse::token::Token;
use whitespace::WhitespaceManager;

pub fn format(style: FormatStyle, mut lines: Vec<AnnotatedLine>) -> Vec<Replacement> {
    let mut formatter = LineFormatter {
        style: style,
        whitespace: WhitespaceManager::new(style),
    };

    formatter.format(&mut lines[..]);
    formatter.whitespace.generate_replacements()
}

type Penalty = u32;

struct LineFormatter {
    style: FormatStyle,
    whitespace: WhitespaceManager,
}

impl LineFormatter {
    fn format(&mut self, lines: &mut [AnnotatedLine]) -> Penalty {
        let mut penalty = 0;

        for i in 0..lines.len() {
            let line = &mut lines[i];
            let indent = line.level * self.style.indent_width;
            let fix_indentation = indent != line.tokens[0].original_column;

            if line.tokens[0].tok == Token::Eof {
                assert!(line.tokens.len() == 1);
                let newlines = cmp::min(1, line.tokens[0].newlines_before);
                self.whitespace.replace_whitespace(&mut line.tokens[0], newlines, 0, 0, 0);
            }
        }

        penalty
    }
}

