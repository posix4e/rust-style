use format_options::{UseTabs, FormatStyle, LineEnding};
use replacement::Replacement;
use std::cmp::{self, PartialOrd, Ordering};
use std::mem;
use syntax::codemap::{mk_sp, Span, BytePos};
use syntax::parse::token::Token;
use token::FormatToken;
use source::Source;

#[derive(Debug)]
struct Change {
    create_replacement: bool,
    token: Token,
    token_span: Span,
    preceding_whitespace_span: Span,
    indent_level: u32,
    spaces: u32,
    start_of_token_column: u32,
    newlines_before: u32,
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

pub struct WhitespaceManager<'a> {
    changes: Vec<Change>,
    line_ending: LineEnding,
    style: &'a FormatStyle,
}

impl<'a> WhitespaceManager<'a> {
    pub fn new(style: &FormatStyle, line_ending: LineEnding) -> WhitespaceManager {
        WhitespaceManager {
            line_ending: line_ending,
            changes: vec![],
            style: style,
        }
    }

    pub fn replace_whitespace(&mut self, tok: &FormatToken, newlines_before: u32,
                              indent_level: u32, spaces: u32, start_of_token_column: u32) {
        self.changes.push(Change {
            create_replacement: true,
            token: tok.tok.clone(),
            token_span: tok.span,
            preceding_whitespace_span: tok.preceding_whitespace_span,
            indent_level: indent_level,
            spaces: spaces,
            newlines_before: newlines_before,
            start_of_token_column: start_of_token_column,
        });
    }


    pub fn generate_replacements(mut self, source: &Source) -> Vec<Replacement> {
        let mut replacements = vec![];
        let mut changes = mem::replace(&mut self.changes, vec![]);

        if changes.is_empty() {
            return replacements
        }

        changes.sort_by(|a, b| {
            IsBeforeInFile(a.token_span).cmp(&IsBeforeInFile(b.token_span))
        });

        // TODO:
        // self.align_trailing_comments();

        // for counting character indices
        let mut character_index = 0;
        let mut character_span = mk_sp(BytePos(0), BytePos(0));

        for c in changes {
            if c.create_replacement {
                let mut text = String::new();
                self.append_newline_text(&mut text, c.newlines_before);
                self.append_indent_text(&mut text, c.indent_level, cmp::max(0, c.spaces),
                                        c.start_of_token_column - cmp::max(0, c.spaces));

                let start_byte = c.preceding_whitespace_span.lo.0;
                let end_byte = if c.token == Token::Eof {
                    c.token_span.hi.0
                } else {
                    c.preceding_whitespace_span.hi.0
                };

                // measures chars from previous measure to start_byte
                character_span.lo = character_span.hi;
                character_span.hi.0 = start_byte;
                character_index += source.span_str(character_span).chars().count();
                let start_character = character_index;

                // measures chars from start_byte to end_byte
                character_span.lo = character_span.hi;
                character_span.hi.0 = end_byte;
                character_index += source.span_str(character_span).chars().count();
                let end_character = character_index;

                replacements.push(Replacement {
                    start_byte:  start_byte as usize,
                    end_byte: end_byte as usize,
                    start_character: start_character,
                    end_character: end_character,
                    text: text,
                });
            }
        }

        replacements
    }

    fn append_newline_text(&self, text: &mut String, newlines: u32) {
        for _ in 0..newlines {
            text.push_str(match self.line_ending {
                LineEnding::CRLF => "\r\n",
                LineEnding::LF => "\n",
            })
        }
    }

    #[allow(unused_variables)]
    fn append_indent_text(&self, text: &mut String, indent_level: u32,
                          spaces: u32, whitespace_start_column: u32) {
        match self.style.use_tabs {
            UseTabs::Never => {
                for _ in 0..spaces {
                    text.push_str(" ");
                }
            },
            UseTabs::Always => {
                unimplemented!();
            },
            UseTabs::ForIndentation => {
                unimplemented!();
            },
        }
    }
}
