use style::{UseTabs, FormatStyle};
use replacement::Replacement;
use std::cmp::{self, PartialOrd, Ordering};
use std::mem;
use syntax::codemap::{Span, BytePos};
use token::{FormatToken, FormatDecision};

struct Change {
    create_replacement: bool,
    token_span: Span,
    preceding_whitespace_span: Span,
    indent_level: u32,
    spaces: u32,
    start_of_token_column: u32,
    newlines_before: u32,

    // FIXME: These are often empty strings.
    //        Allocations could easily be avoided.
    previous_line_postfix: String,
    current_line_prefix: String,
}

struct IsBeforeInFile(Span);

impl PartialEq for IsBeforeInFile {
    fn eq(&self, other: &IsBeforeInFile) -> bool {
        let &IsBeforeInFile(Span { lo: BytePos(lo_a), .. }) = self;
        let &IsBeforeInFile(Span { lo: BytePos(lo_b), .. }) = other;
        lo_a == lo_b
    }
}

impl Eq for IsBeforeInFile {}

impl PartialOrd for IsBeforeInFile {
    fn partial_cmp(&self, other: &IsBeforeInFile) -> Option<Ordering> {
        let &IsBeforeInFile(Span { lo: BytePos(lo_a), .. }) = self;
        let &IsBeforeInFile(Span { lo: BytePos(lo_b), .. }) = other;
        Some(lo_a.cmp(&lo_b))
    }
}

impl Ord for IsBeforeInFile {
    fn cmp(&self, other: &IsBeforeInFile) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

pub struct WhitespaceManager {
    use_crlf: bool,
    style: FormatStyle,
    changes: Vec<Change>,
}

impl WhitespaceManager {
    pub fn new(style: FormatStyle) -> WhitespaceManager {
        WhitespaceManager {
            // FIXME: determine wheter to use CRLF or not
            use_crlf: false,
            changes: vec![],
            style: style,
        }
    }

    pub fn replace_whitespace(&mut self, tok: &mut FormatToken, newlines_before: u32,
                              indent_level: u32, spaces: u32, start_of_token_column: u32) {
        tok.decision = if newlines_before > 0 {
            FormatDecision::Break
        } else {
            FormatDecision::Continue
        };

        self.changes.push(Change {
            create_replacement: true,
            token_span: tok.span,
            preceding_whitespace_span: tok.preceding_whitespace_span,
            indent_level: indent_level,
            spaces: spaces,
            newlines_before: newlines_before,
            start_of_token_column: start_of_token_column,
            previous_line_postfix: "".to_string(),
            current_line_prefix: "".to_string(),
        });
    }


    pub fn generate_replacements(mut self) -> Vec<Replacement> {
        let mut replacements = vec![];
        let mut changes = mem::replace(&mut self.changes, vec![]);

        if changes.is_empty() {
            return replacements
        }

        changes.sort_by(|a, b| {
            IsBeforeInFile(a.token_span).cmp(&IsBeforeInFile(b.token_span))
        });

        // TODO:
        // self.calculate_line_break_information(&mut changes);
        // self.align_trailing_comments();

        for c in changes {
            if c.create_replacement {
                let mut text = c.previous_line_postfix;
                self.append_newline_text(&mut text, c.newlines_before);
                self.append_indent_text(&mut text, c.indent_level, cmp::max(0, c.spaces),
                                        c.start_of_token_column - cmp::max(0, c.spaces));
                text.push_str(&c.current_line_prefix);
                replacements.push(Replacement {
                    start_byte: c.preceding_whitespace_span.lo.0 as usize,
                    end_byte: c.preceding_whitespace_span.hi.0 as usize,
                    text: text,
                })
            }
        }

        replacements
    }

    fn append_newline_text(&self, text: &mut String, newlines: u32) {
        for _ in 0..newlines {
            text.push_str(if self.use_crlf { "\r\n" } else { "\n" });
        }
    }

    fn append_indent_text(&self, text: &mut String, indent_level: u32,
                          spaces: u32, whitespace_start_column: u32) {
        match self.style.use_tabs {
            UseTabs::Never => {
                for _ in 0..spaces {
                    text.push_str(" ");
                }
            },
            _ => {
                unimplemented!();
            }
        }
    }
}
