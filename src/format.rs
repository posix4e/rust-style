use annotated_line::AnnotatedLine;
use replacement::Replacement;
use std::cmp;
use style::FormatStyle;
use syntax::parse::token::{Token, DelimToken};
use whitespace::WhitespaceManager;

pub fn format(style: FormatStyle, lines: &mut [AnnotatedLine]) -> Vec<Replacement> {
    let mut formatter = LineFormatter {
        style: style.clone(),
        whitespace: WhitespaceManager::new(style),
    };

    formatter.format(lines);
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
            let (left, right) = lines.split_at_mut(i);
            let curr_line = right.first_mut().unwrap();
            let prev_line = left.last();
            assert!(curr_line.tokens.len() > 0);
            let indent = curr_line.level * self.style.indent_width;

            if curr_line.tokens[0].tok == Token::Eof {
                assert!(curr_line.tokens.len() == 1);
                let newlines = cmp::min(1, curr_line.tokens[0].newlines_before);
                self.whitespace.replace_whitespace(&mut curr_line.tokens[0], newlines, 0, 0, 0);
            } else {
                self.format_first_token(curr_line, prev_line, indent);
            }

            penalty += self.format(&mut curr_line.children);
        }

        penalty
    }

    fn format_first_token(&mut self, curr_line: &mut AnnotatedLine,
                          prev_line: Option<&AnnotatedLine>, indent: u32) {
        let token = &mut curr_line.tokens[0];
        let prev_last_token = prev_line
            .and_then(|l| l.tokens.last())
            .map(|t| &t.tok);

        let mut newlines = cmp::min(token.newlines_before,
                                    self.style.max_empty_lines_to_keep + 1);

        // Remove empty lines before "}" where applicable.
        // if token.tok == Token::CloseDelim(DelimToken::Brace) {
        //     newlines = cmp::min(newlines, 1);
        // }

        if newlines == 0 {
            newlines = 1;
        }
        if token.is_first_token {
            newlines = 0;
        }

        // FIXME: uncomment and test its working
        // Remove empty lines after "{"
        // if !self.style.keep_empty_lines_at_the_start_of_blocks {
        //     if let Some(&Token::OpenDelim(DelimToken::Brace)) = prev_last_token {
        //         newlines = 1;
        //     }
        // }

        self.whitespace.replace_whitespace(token, newlines, curr_line.level, indent, indent);
    }
}

