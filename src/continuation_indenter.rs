use format::{LineState, ParenState};
use format_options::{FormatStyle, Penalty};
use std::cmp;
use syntax::parse::token::{Token, DelimToken};
use token::{FormatToken, Precedence, TokenType};
use unwrapped_line::{UnwrappedLine, LineType};
use whitespace_manager::WhitespaceManager;

pub struct ContinuationIndenter<'a> {
    pub style: &'a FormatStyle,
}

impl<'a> ContinuationIndenter<'a> {
    pub fn get_initial_state(&self, line: &UnwrappedLine, first_indent: u32) -> LineState {
        let mut state = LineState {
            column: first_indent,
            first_indent: first_indent,
            next_token_index: 0,
            fn_decl_arrow_indent: None,
            stack: vec![ParenState {
                indent: first_indent + self.style.continuation_indent_width,
                nested_block_indent: first_indent,
                indent_level: line.level,
                ..ParenState::default()
            }],
        };
        // First token is already consumed
        self.move_state_to_next_token(line, &mut state);
        state
    }

    pub fn can_break(&self, line: &UnwrappedLine, state: &LineState) -> bool {
        let current = state.current(line);

        if !current.can_break_before {
            return false;
        }
        if is_chained_method_call(line, state) && state.stack_top().unwrapped_method_chain {
            return false;
        }

        !state.stack_top().no_line_break
    }

    pub fn must_break(&self, line: &UnwrappedLine, state: &LineState) -> bool {
        if state.current(line).must_break_before {
            return true;
        }
        if state.stack_top().break_between_paramters && is_between_struct_parameter(line, state) {
            return true;
        }
        if state.stack_top().method_chain_indent.is_some() && is_chained_method_call(line, state) {
            return true;
        }

        false
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
        let mut penalty = 0;

        // The first line break on any NestingLevel causes an extra
        // penalty in order prefer similar line breaks.
        if !state.stack_top().contains_line_break {
            penalty += 15;
            state.stack_top_mut().contains_line_break = true;
        }

        penalty += state.current(line).split_penalty;
        let newline_column = self.get_newline_column(line, state);
        state.column = newline_column;
        state.stack_top_mut().nested_block_indent = newline_column;

        if !dry_run {
            let current = state.current_mut(line);
            let newlines = cmp::max(1, cmp::min(current.newlines_before, self.style.max_empty_lines_to_keep + 1));
            whitespace.replace_whitespace(current, newlines, state.stack_top().indent_level, state.column, state.column);
        }

        if state.stack_top().avoid_bin_packing {
            // Bin packing is being avoided, and this token was added to a new line.
            // Ensure breaking occurs between every parameter for the rest of this scope.
            state.stack_top_mut().break_between_paramters = true;
        }

        if is_chained_method_call(line, state) && state.stack_top().method_chain_indent.is_none() {
            // A break occured before method call.
            // Calculate the indentation for following chained calls.
            // Following chained calls will always break.s
            state.stack_top_mut().method_chain_indent = Some(newline_column);
        }

        penalty
    }

    fn add_token_on_current_line(&self, line: &mut UnwrappedLine, state: &mut LineState,
                                 dry_run: bool, whitespace: &mut WhitespaceManager) {
        let newlines = 0;
        let indent = 0;
        let spaces = state.current(line).spaces_required_before;
        if !dry_run {
            whitespace.replace_whitespace(state.current_mut(line), newlines, indent, spaces, state.column + spaces);
        }

        if state.stack_top().avoid_bin_packing && is_between_struct_parameter(line, state) {
            // Bin packing is being avoided, and this token was added to the current line.
            // Avoid breaking for the rest of this scope.
            state.stack_top_mut().no_line_break = true;
        }

        if is_chained_method_call(line, state) {
            // A break did not occur before the method call.
            // Never break on method calls in future.
            state.stack_top_mut().unwrapped_method_chain = true;
        }

        state.column += spaces;
    }

    fn move_state_to_next_token(&self, line: &UnwrappedLine, state: &mut LineState) -> Penalty {
        // remember the position of the fn opening brace parameters
        if line.typ == LineType::FnDecl &&
               state.current(line).tok == Token::OpenDelim(DelimToken::Paren) &&
               state.fn_decl_arrow_indent.is_none() && state.stack.len() == 1 {
            state.fn_decl_arrow_indent = Some(state.column + 1);
        }

        self.move_state_past_fake_lparens(line, state);
        self.move_state_past_scope_opener(line, state);
        self.move_state_past_scope_closer(line, state);
        self.move_state_past_fake_rparens(line, state);

        let mut penalty = 0;
        let current = state.current(line);
        state.column += current.column_width;
        penalty += self.excess_character_penalty(state);

        // Handle multiline tokens
        if let Some(last_line_column_width) = state.current(line).last_line_column_width {
            state.column = last_line_column_width;
            penalty += self.excess_character_penalty(state);
        }

        state.next_token_index += 1;
        penalty
    }

    fn excess_character_penalty(&self, state: &LineState) -> Penalty {
        if state.column <= self.style.column_limit {
            return 0;
        }
        let excess_characters = (state.column - self.style.column_limit) as Penalty;
        self.style.penalty_excess_character * excess_characters
    }

    fn move_state_past_fake_lparens(&self, line: &UnwrappedLine, state: &mut LineState) {
        let current = state.current(line);
        let prev: Option<&FormatToken> = line.prev_non_comment_token(state.next_token_index);

        let mut indent = state.column + self.style.continuation_indent_width;

        if let Some(prev) = prev {
            // Special case indentation when fake parens coincides with real scope open,
            // Or it is an assignment.
            if prev.precedence() == Some(Precedence::Assignment) ||
                prev.tok == Token::OpenDelim(DelimToken::Bracket) ||
                prev.tok == Token::OpenDelim(DelimToken::Paren) ||
                prev.tok == Token::Lt && prev.typ == TokenType::GenericBracket {
                // Align token with opening brace.
                indent = state.column;
            } else if prev.tok == Token::OpenDelim(DelimToken::Brace) {
                // Regular indent for braces.
                // It has already been applied in move_state_past_delim_open.
                indent = state.stack_top().indent;
            }
        }

        for p in &current.fake_lparens {
            let mut new_state = ParenState {
                indent: indent,
                nested_block_indent: state.stack_top().nested_block_indent,
                indent_level: state.stack_top().indent_level,
                avoid_bin_packing: state.stack_top().avoid_bin_packing,
                break_between_paramters: state.stack_top().break_between_paramters &&
                                            p.to_i32() <= Precedence::Comma.to_i32(),
                no_line_break: state.stack_top().no_line_break,
                ..ParenState::default()
            };

            // No indentation for breaking on PatternOr
            if *p == Precedence::PatternOr {
                new_state.indent = state.column;
            }

            state.stack.push(new_state);
        }
    }

    fn move_state_past_fake_rparens(&self, line: &UnwrappedLine, state: &mut LineState) {
        let current = state.current(line);
        for _ in 0..current.fake_rparens {
            if state.stack.len() == 1 {
                break;
            }
            state.stack.pop().unwrap();
        }
    }

    fn move_state_past_scope_opener(&self, line: &UnwrappedLine, state: &mut LineState) {
        let current = state.current(line);
        if !current.opens_scope() {
            return;
        }

        let new_paren_state = {
            let top = state.stack_top();
            if current.tok == Token::OpenDelim(DelimToken::Paren) ||
                   current.tok == Token::OpenDelim(DelimToken::Brace) &&
                       line.typ == LineType::Use ||
                   current.tok == Token::OpenDelim(DelimToken::Bracket) ||
                   current.typ == TokenType::GenericBracket {
                ParenState {
                    indent: state.column + 1,
                    nested_block_indent: top.nested_block_indent,
                    indent_level: top.indent_level,
                    no_line_break: top.no_line_break,
                    ..ParenState::default()
                }
            } else if current.tok == Token::OpenDelim(DelimToken::Brace) {
                ParenState {
                    indent: top.nested_block_indent + self.style.indent_width,
                    nested_block_indent: top.nested_block_indent,
                    indent_level: top.indent_level + 1,
                    avoid_bin_packing: true,
                    no_line_break: top.no_line_break,
                    ..ParenState::default()
                }
            } else {
                unreachable!("scope opener not handled");
            }
        };

        state.stack.push(new_paren_state);
    }

    fn move_state_past_scope_closer(&self, line: &UnwrappedLine, state: &mut LineState) {
        let current = state.current(line);
        if current.closes_scope() {
            // Don't removing the first level
            if state.stack.len() > 1 {
                state.stack.pop();
            }
        }
    }

    fn get_newline_column(&self, line: &UnwrappedLine, state: &LineState) -> u32 {
        let stack_top = state.stack_top();
        let current = &line.tokens[state.next_token_index];

        if current.tok == Token::CloseDelim(DelimToken::Brace) && state.stack.len() > 1 {
            return state.stack[state.stack.len() - 2].nested_block_indent;
        }
        if is_chained_method_call(line, state) {
            return match stack_top.method_chain_indent {
                Some(indent) => indent,
                None => stack_top.indent + self.style.method_chain_indent_width,
            }
        }
        if current.typ == TokenType::FnDeclArrow && state.fn_decl_arrow_indent.is_some() {
            return state.fn_decl_arrow_indent.unwrap();
        }

        stack_top.indent
    }
}

fn is_between_struct_parameter(line: &UnwrappedLine, state: &LineState) -> bool {
    let current = &line.tokens[state.next_token_index];
    let previous = &line.tokens[state.next_token_index - 1];

    if previous.tok == Token::Comma && !current.is_trailing_comment(line) {
        return true;
    }
    if let Token::OpenDelim(DelimToken::Brace) = previous.tok {
        return true;
    }
    if let Token::CloseDelim(DelimToken::Brace) = current.tok {
        return true;
    }

    false
}

fn is_chained_method_call(line: &UnwrappedLine, state: &LineState) -> bool {
    let current = &line.tokens[state.next_token_index];
    let previous = line.prev_non_comment_token(state.next_token_index);
    match previous {
        None => false,
        Some(previous) => current.tok == Token::Dot && previous.closes_scope()
    }
}
