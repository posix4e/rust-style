use arena::TypedArena;
use continuation_indenter::ContinuationIndenter;
use replacement::Replacement;
use std::cmp::{self, Ordering};
use std::collections::{BinaryHeap, HashSet};
use style::{FormatStyle, Penalty};
use syntax::parse::token::{Token, DelimToken};
use token::{FormatToken, FormatDecision};
use unwrapped_line::UnwrappedLine;
use whitespace_manager::WhitespaceManager;

pub fn format(style: FormatStyle, lines: &mut [UnwrappedLine]) -> Vec<Replacement> {
    let mut formatter = LineFormatter {
        style: style.clone(),
        whitespace: WhitespaceManager::new(style.clone()),
        indenter: ContinuationIndenter::new(style),
        arena: &TypedArena::new(),
    };

    formatter.format(lines);
    formatter.whitespace.generate_replacements()
}

struct LineFormatter<'a> {
    style: FormatStyle,
    whitespace: WhitespaceManager,
    indenter: ContinuationIndenter,
    arena: &'a TypedArena<StateNode<'a>>,
}

impl<'a> LineFormatter<'a> {
    fn format(&mut self, lines: &mut [UnwrappedLine]) -> Penalty {
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
                continue;
            }

            // TODO:
            // If everything fits on a single line, just put it there,
            // instead of going through the line breaking algorithm.

            self.format_first_token(curr_line, prev_line, indent);
            penalty += self.format_line(curr_line, indent);

            // TODO: remove direct recursive call, and implement format_children
            penalty += self.format(&mut curr_line.children);
        }

        penalty
    }

    fn format_first_token(&mut self, curr_line: &mut UnwrappedLine,
                          prev_line: Option<&UnwrappedLine>, indent: u32) {
        let token = &mut curr_line.tokens[0];
        let mut newlines = cmp::min(token.newlines_before,
            self.style.max_empty_lines_to_keep + 1);

        // Remove empty lines before "}" where applicable.
        if token.tok == Token::CloseDelim(DelimToken::Brace) {
            newlines = cmp::min(newlines, 1);
        }

        if newlines == 0 {
            newlines = 1;
        }
        if token.is_first_token {
            newlines = 0;
        }

        self.whitespace.replace_whitespace(token, newlines, curr_line.level, indent, indent);
    }

    fn format_line(&mut self, line: &mut UnwrappedLine, indent: u32) -> Penalty {
        let mut seen = HashSet::<&LineState>::new();
        let mut queue = BinaryHeap::<QueueItem>::new();
        let mut penalty = 0;
        let mut count = 0;
        let dry_run = true;

        let initial_state = self.indenter.get_initial_state(line, indent, dry_run, &mut self.whitespace);
        let mut node = &*self.arena.alloc(StateNode {
            newline: false,
            previous: None,
            state: initial_state.clone(),
        });

        queue.push(QueueItem { penalty: penalty, id: count, node: node });
        count += 1;

        loop {
            let item = match queue.pop() {
                // We were unable to find a solution
                None => return 0,
                Some(item) => item,
            };
            penalty = item.penalty;
            node = item.node;

            if node.state.path_complete(line) {
                break;
            }

            if !seen.insert(&node.state) {
                // State already examined with lower penalty.
                continue;
            }

            match line.tokens[node.state.next_token_index].decision {
                FormatDecision::Unformatted => {
                    self.add_next_state_to_queue(penalty, node, false, &mut count, &mut queue, line);
                    self.add_next_state_to_queue(penalty, node, true, &mut count, &mut queue, line);
                },
                FormatDecision::Continue => {
                    self.add_next_state_to_queue(penalty, node, false, &mut count, &mut queue, line);
                },
                FormatDecision::Break => {
                    self.add_next_state_to_queue(penalty, node, true, &mut count, &mut queue, line);
                },
            }
        }

        let reconstructed_penalty = self.reconstruct_path(line, initial_state, node);
        assert_eq!(reconstructed_penalty, penalty);

        penalty
    }

    fn reconstruct_path(&mut self, line: &mut UnwrappedLine, mut state: LineState, mut current: &StateNode) -> Penalty {
        let mut penalty = 0;
        let mut path = vec![];

        // We do not need a break before the initial token.
        while let Some(previous) = current.previous {
            path.push(current);
            current = previous;
        }

        path.reverse();

        for node in path {
            let dry_run = false;
            self.format_children(node.newline, dry_run, &mut penalty, &mut state);
            penalty += self.indenter.add_token_to_state(line, &mut state, node.newline, dry_run, &mut self.whitespace);
        }

        penalty
    }

    fn add_next_state_to_queue(&mut self, mut penalty: Penalty, prev_node: &'a StateNode<'a>,
                               newline: bool, count: &mut u32, queue: &mut BinaryHeap<QueueItem<'a>>,
                               line: &mut UnwrappedLine) {
        {
            let token = &line.tokens[prev_node.state.next_token_index];
            if newline && !token.can_break_before {
                return;
            }
            if !newline && token.must_break_before {
                return;
            }
        }

        let dry_run = true;
        let node = self.arena.alloc(StateNode {
            newline: newline,
            previous: Some(prev_node),
            state: prev_node.state.clone(),
        });

        if !self.format_children(newline, dry_run, &mut penalty, &mut node.state) {
            return;
        }

        penalty += self.indenter.add_token_to_state(line, &mut node.state, newline, dry_run, &mut self.whitespace);
        queue.push(QueueItem { penalty: penalty, id: *count, node: node });
        *count += 1;
    }

    // TODO
    fn format_children(&mut self, newline: bool, dry_run: bool, penalty: &mut Penalty,
                       state: &mut LineState) -> bool {
        true
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct ParenState {
    pub indent: u32,
    pub indent_level: u32,
    pub contains_line_break: bool,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct LineState {
    // The index of the token that needs to be next formatted.
    pub next_token_index: usize,
    // The number of used columns in the current line.
    pub column: u32,
    // The indent of the first token
    pub first_indent: u32,
    // A stack keeping track of properties applying to parenthesis.
    pub stack: Vec<ParenState>,
}

impl LineState {
    fn path_complete(&self, line: &UnwrappedLine) -> bool {
        self.next_token_index >= line.tokens.len()
    }

    pub fn stack_top(&self) -> &ParenState {
        self.stack.last().unwrap()
    }

    pub fn stack_top_mut(&mut self) -> &mut ParenState {
        self.stack.last_mut().unwrap()
    }

    pub fn current_mut<'a>(&self, line: &'a mut UnwrappedLine) -> &'a mut FormatToken {
        &mut line.tokens[self.next_token_index]
    }

    pub fn current<'a>(&'a self, line: &'a UnwrappedLine) -> &'a FormatToken {
        &line.tokens[self.next_token_index]
    }

    pub fn previous<'a>(&'a self, line: &'a UnwrappedLine) -> &'a FormatToken {
        &line.tokens[self.next_token_index - 1]
    }
}

#[derive(Debug)]
struct StateNode<'a> {
    state:  LineState,
    newline: bool,
    previous: Option<&'a StateNode<'a>>
}

#[derive(Debug)]
struct QueueItem<'a> {
    penalty: Penalty,
    id: u32,
    node: &'a StateNode<'a>
}

impl<'a> PartialEq for QueueItem<'a> {
    fn eq(&self, other: &QueueItem) -> bool {
        self.penalty == other.penalty &&
            self.id == other.id
    }
}

impl<'a> Eq for QueueItem<'a> {}

// implemented backwards to get min priority queue
impl<'a> PartialOrd for QueueItem<'a> {
    fn partial_cmp(&self, other: &QueueItem) -> Option<Ordering> {
        match other.penalty.cmp(&self.penalty) {
            Ordering::Equal => Some(other.id.cmp(&self.id)),
            ord => Some(ord),
        }
    }
}

impl<'a> Ord for QueueItem<'a> {
    fn cmp(&self, other: &QueueItem) -> Ordering {
        self.partial_cmp(&other).unwrap()
    }
}
