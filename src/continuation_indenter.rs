use format::LineState;
use std::cmp;
use style::{FormatStyle, Penalty};
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

    pub fn get_initial_state(&self, line: &UnwrappedLine, first_indent: u32,
                             dry_run: bool, whitespace: &mut WhitespaceManager) -> LineState {
        let mut state = LineState {
            column: first_indent,
            first_indent: first_indent,
            next_token_index: 0,

        };
        // First token is already consumed
        self.move_state_to_next_token(line, &mut state, dry_run, false, whitespace);
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

        self.move_state_to_next_token(line, state, dry_run, newline, whitespace) + penalty
    }

    pub fn add_token_on_new_line(&self, line: &mut UnwrappedLine, state: &mut LineState,
                                 dry_run: bool, whitespace: &mut WhitespaceManager) -> Penalty {
        let current = &mut line.tokens[state.next_token_index];
        let penalty = current.split_penalty;
        let indent = 0;

        state.column = state.first_indent + self.style.continuation_indent_width;

        if !dry_run {
            let newlines = cmp::max(1, cmp::min(current.newlines_before, self.style.max_empty_lines_to_keep + 1));
            whitespace.replace_whitespace(current, newlines, indent, state.column, state.column);
        }

        penalty
    }

    pub fn add_token_on_current_line(&self, line: &mut UnwrappedLine,
                                     state: &mut LineState,
                                     dry_run: bool,
                                     whitespace: &mut WhitespaceManager) {
        let current = &mut line.tokens[state.next_token_index];

        let newlines = 0;
        let indent = 0;
        let spaces = current.spaces_required_before;
        if !dry_run {
            whitespace.replace_whitespace(current, newlines, indent, spaces, state.column + spaces);
        }
        state.column += spaces;
    }

    pub fn move_state_to_next_token(&self, line: &UnwrappedLine, state: &mut LineState,
                                    dry_run: bool, newline: bool,
                                    whitespace: &mut WhitespaceManager) -> Penalty {
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
}
