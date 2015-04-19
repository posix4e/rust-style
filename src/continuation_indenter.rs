use format::{LineState, ParenState};
use std::cmp;
use style::{FormatStyle, Penalty};
use syntax::parse::token::{Token, DelimToken};
use token::{FormatToken, Precedence};
use unwrapped_line::UnwrappedLine;
use whitespace_manager::WhitespaceManager;

pub struct ContinuationIndenter {
    style: FormatStyle,
}

impl ContinuationIndenter {
    pub fn new(style: FormatStyle) -> ContinuationIndenter {
        ContinuationIndenter {
            style: style,
        }
    }

    pub fn get_initial_state(&self, line: &UnwrappedLine, first_indent: u32) -> LineState {
        let mut state = LineState {
            column: first_indent,
            first_indent: first_indent,
            next_token_index: 0,
            stack: vec![ParenState {
                indent: first_indent + self.style.continuation_indent_width,
                indent_level: line.level,
                contains_line_break: false,
            }],
        };
        // First token is already consumed
        self.move_state_to_next_token(line, &mut state);
        state
    }

    pub fn add_token_to_state(&self, line: &mut UnwrappedLine, state: &mut LineState,
                              newline: bool, dry_run: bool, whitespace: &mut WhitespaceManager) -> Penalty {
        let mut penalty = 0;
        if newline {
            penalty = self.add_token_on_new_line(line, state, dry_run, whitespace)
        } else {
            self.add_token_on_current_line(line, state, dry_run, whitespace);
        };

        self.move_state_to_next_token(line, state) + penalty
    }

    fn add_token_on_new_line(&self, line: &mut UnwrappedLine, state: &mut LineState,
                                 dry_run: bool, whitespace: &mut WhitespaceManager) -> Penalty {
        let current = &mut line.tokens[state.next_token_index];
        let mut penalty = 0;

        // The first line break on any NestingLevel causes an extra
        // penalty in order prefer similar line breaks.
        if !state.stack_top().contains_line_break {
            penalty += 15;
            state.stack_top_mut().contains_line_break = true;
        }

        penalty += current.split_penalty;
        state.column = self.get_newline_column(state);

        if !dry_run {
            let newlines = cmp::max(1, cmp::min(current.newlines_before, self.style.max_empty_lines_to_keep + 1));
            whitespace.replace_whitespace(current, newlines, state.stack_top().indent_level, state.column, state.column);
        }

        penalty
    }

    fn add_token_on_current_line(&self,
                                 line: &mut UnwrappedLine,
                                 state: &mut LineState,
                                 dry_run: bool,
                                 whitespace: &mut WhitespaceManager) {
        let newlines = 0;
        let indent = 0;
        let spaces = state.current(line).spaces_required_before;
        if !dry_run {
            whitespace.replace_whitespace(state.current_mut(line), newlines, indent, spaces, state.column + spaces);

        }

        state.column += spaces;
    }

    fn move_state_to_next_token(&self, line: &UnwrappedLine, state: &mut LineState) -> Penalty {
        self.move_state_past_fake_lparens(line, state);
        self.move_state_past_delim_open(line, state);
        self.move_state_past_delim_close(line, state);
        self.move_state_past_fake_rparens(line, state);

        let mut penalty = 0;
        let current = &line.tokens[state.next_token_index];
        state.column += current.column_width;
        state.next_token_index += 1;

        if state.column > self.style.column_limit {
            let excess_characters = (state.column - self.style.column_limit) as Penalty;
            penalty += self.style.penalty_excess_character * excess_characters;
        }

        penalty
    }

    fn move_state_past_fake_lparens(&self, line: &UnwrappedLine, state: &mut LineState) {
        let current = &line.tokens[state.next_token_index];
        let prev: Option<&FormatToken> = line.prev_non_comment_token(state.next_token_index);

        let mut indent = state.column + self.style.continuation_indent_width;
        if let Some(prev) = prev {
            if prev.precedence() == Some(Precedence::Assignment) ||
               prev.opens_scope() {
                indent = state.column;
            }
        }

        for _ in &current.fake_lparens {
            let new_state = ParenState {
                indent: indent,
                indent_level: state.stack_top().indent_level,
                contains_line_break: false,
            };
            state.stack.push(new_state);
        }
    }

    fn move_state_past_fake_rparens(&self, line: &UnwrappedLine, state: &mut LineState) {
        let current = &line.tokens[state.next_token_index];
        for _ in 0..current.fake_rparens {
            if state.stack.len() == 1 {
                break;
            }
            state.stack.pop().unwrap();
        }
    }

    fn move_state_past_delim_open(&self, line: &UnwrappedLine, state: &mut LineState) {
        let current = &line.tokens[state.next_token_index];
        let new_paren_state = match current.tok {
            Token::OpenDelim(DelimToken::Brace) => {
                let top = state.stack_top();
                ParenState {
                    indent: top.indent + self.style.continuation_indent_width,
                    indent_level: top.indent_level + 1,
                    contains_line_break: false,
                }
            },
            Token::OpenDelim(DelimToken::Paren) => {
                let top = state.stack_top();
                ParenState {
                    indent: state.column + 1,
                    indent_level: top.indent_level,
                    contains_line_break: false,
                }
            },
            _ => return,
        };

        state.stack.push(new_paren_state);
    }

    fn move_state_past_delim_close(&self, line: &UnwrappedLine, state: &mut LineState) {
        let current = &line.tokens[state.next_token_index];
        match current.tok {
            Token::CloseDelim(DelimToken::Brace) |
            Token::CloseDelim(DelimToken::Paren) => {
                if state.stack.len() > 1 {
                    state.stack.pop();
                }
            }
            _ => {},
        }
    }

    fn get_newline_column(&self, state: &LineState) -> u32 {
        state.stack_top().indent
    }
}
