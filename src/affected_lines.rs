use style::LineRanges;
use token::FormatToken;
use unwrapped_line::UnwrappedLine;

pub fn mark_all_affected(lines: &mut Vec<UnwrappedLine>) {
    for line in lines {
        line.affected = true;
        if !line.children.is_empty() {
            mark_all_affected(&mut line.children);
        }
    }
}

pub fn compute_affected_lines(lines: &mut Vec<UnwrappedLine>, ranges: &LineRanges) -> bool {
    let mut some_line_affected = false;
    let mut previous_line: Option<&UnwrappedLine> = None;

    for line in lines {
        line.leading_empty_lines_affected = affects_leading_empty_lines(&line.tokens[0], ranges);

        line.children_affected = compute_affected_lines(&mut line.children, ranges);
        if line.children_affected {
            some_line_affected = true;
        }

        // stores whether one of the line's tokens is directly affected
        let some_token_affected = affects_tokens(&line.tokens, ranges);

        // may need to do:
        // let mut include_leading_new_lines = false;
        // let some_first_child_affected = false;

        // was this line moved, and has it previously been on the same
        // line as an affected line
        let line_moved =
            previous_line.is_some() &&
            previous_line.unwrap().affected &&
            line.tokens[0].newlines_before == 0;

        let is_continued_affected_comment = {

            let is_comment_without_gap = 
                line.tokens[0].comment_type.is_some() &&
                line.tokens.len() == 1 &&
                line.tokens[0].newlines_before < 2;

            let is_prev_line_token_a_comment = if previous_line.is_some() {
                let previous_line = previous_line.unwrap();
                let token_qty = previous_line.tokens.len();
                
                previous_line.affected &&
                previous_line.tokens[token_qty - 1].comment_type.is_some()
            } else {
                false
            };

            is_comment_without_gap &&
            is_prev_line_token_a_comment
        };

        if some_token_affected || // some_first_child_affected ||
                line_moved || is_continued_affected_comment {
            line.affected = true;
            some_line_affected = true;
        }

        previous_line = Some(line);
    }

    some_line_affected
}

fn affects_leading_empty_lines(token: &FormatToken, ranges: &LineRanges) -> bool {
    // check if empty lines exist
    if token.newlines_before < 2 {
        return false;
    }

    let newlines_before_start = token.original_row - token.newlines_before + 1;
    let newlines_before_end = token.original_row;

    for line in newlines_before_start..newlines_before_end {
        if ranges.in_ranges(line) {
            return true;
        }
    }
    false
}

fn affects_tokens(tokens: &Vec<FormatToken>, ranges: &LineRanges) -> bool {
    for token in tokens {
        if ranges.in_ranges(token.original_row) {
            return true;
        }
    }
    false
}
