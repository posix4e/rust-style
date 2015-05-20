use continuation_indenter::ContinuationIndenter;
use format_options::{FormatStyle, Penalty, LineEnding};
use replacement::Replacement;
use source::Source;
use std::cmp::{self, Ordering};
use std::collections::{BinaryHeap, HashSet, HashMap};
use syntax::parse::token::{Token, DelimToken};
use token::FormatToken;
use typed_arena::Arena;
use unwrapped_line::UnwrappedLine;
use whitespace_manager::WhitespaceManager;

pub fn format(source: &Source, style: &FormatStyle, line_ending: LineEnding,
              lines: &mut [UnwrappedLine]) -> Vec<Replacement> {
    let mut formatter = LineFormatter {
        style: style,
        whitespace: WhitespaceManager::new(style, line_ending),
        indenter: ContinuationIndenter { style: style },
        arena: &Arena::new(),
        penalty_cache: HashMap::new(),
    };

    formatter.format(lines, false, 0, false);
    formatter.whitespace.generate_replacements(source)
}

struct LineFormatter<'a> {
    style: &'a FormatStyle,
    whitespace: WhitespaceManager<'a>,
    indenter: ContinuationIndenter<'a>,
    arena: &'a Arena<StateNode<'a>>,
    penalty_cache: HashMap<CacheKey, Penalty>,
}

impl<'a> LineFormatter<'a> {
    fn format(&mut self, lines: &mut [UnwrappedLine], dry_run: bool, additional_indent: u32,
              fix_bad_indentation: bool) -> Penalty {
        let cache_key = CacheKey::new(lines, additional_indent);

        // If these lines have already been calculated in a previous dry run,
        // just return the previously calculated value, instead of recalculating it.
        if dry_run {
            match self.penalty_cache.get(&cache_key) {
                Some(penalty) => return *penalty,
                None => (),
            }
        }

        let mut penalty = 0;
        for i in 0..lines.len() {
            assert!(lines[i].tokens.len() > 0);
            let indent = additional_indent + lines[i].level * self.style.indent_width;
            let fix_indentation = fix_bad_indentation &&
                                  (indent != lines[i].tokens[0].original_column);

            if lines[i].tokens[0].tok == Token::Eof {
                self.format_eof(lines, i, dry_run);
                continue;
            }

            if lines[i].affected || fix_indentation {
                self.format_first_token(&mut lines[i], indent, dry_run);

                // If everything fits on a single line, just put it there.
                if lines[i].tokens.last().unwrap().total_length + indent < self.style.column_limit {
                    self.format_on_single_line(&mut lines[i], indent, dry_run);
                } else {
                    penalty += self.analyze_solution_space(&mut lines[i], indent, dry_run);
                }
            } else if lines[i].children_affected {
                for token in &mut lines[i].tokens {
                    penalty += self.format(&mut token.children, dry_run, 0, false);
                }
            } else {
                // format the first token if necessary
                if (i > 0 && lines[i - 1].affected) ||
                   lines[i].leading_empty_lines_affected {
                    self.format_first_token(&mut lines[i], indent, dry_run);
                }
            }
        }

        let previous_penalty = self.penalty_cache.insert(cache_key, penalty);
        assert!(previous_penalty.is_none() || previous_penalty.unwrap() == penalty);

        penalty
    }

    fn format_eof(&mut self, lines: &mut [UnwrappedLine], index: usize, dry_run: bool) {
        assert!(lines[index].tokens.len() == 1);
        assert!(lines[index].tokens[0].tok == Token::Eof);

        let prev_line_affected = if index > 0 {
            lines[index - 1].affected
        } else {
            false
        };

        if prev_line_affected && !dry_run {
            let newlines = cmp::min(1, lines[index].tokens[0].newlines_before);
            self.whitespace.replace_whitespace(&mut lines[index].tokens[0], newlines, 0, 0, 0);
        }
    }

    fn format_first_token(&mut self, line: &mut UnwrappedLine, indent: u32, dry_run: bool) {
        let token = &mut line.tokens[0];
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

        if !dry_run {
            self.whitespace.replace_whitespace(token, newlines, line.level, indent, indent);
        }
    }

    fn format_on_single_line(&mut self, line: &mut UnwrappedLine, indent: u32, dry_run: bool) -> Penalty {
        let newline = false;
        let mut penalty = 0;
        let mut state = self.indenter.get_initial_state(line, indent);

        // first token already formatted
        while !state.path_complete(line) {
            self.format_children(newline, dry_run, &mut penalty, &mut state, line);
            self.indenter.add_token_to_state(line, &mut state, newline, dry_run, &mut self.whitespace);
        }

        penalty
    }

    // Use a variant of Dijkstra's algorithm to find the line with the lowest penalty,
    // By breaking and not breaking at every possible line breaking point.
    fn analyze_solution_space(&mut self, line: &mut UnwrappedLine, indent: u32, dry_run: bool) -> Penalty {
        let mut seen = HashSet::<&LineState>::new();
        let mut queue = BinaryHeap::<QueueItem>::new();
        let mut penalty = 0;
        let mut count = 0;

        let initial_state = self.indenter.get_initial_state(line, indent);
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

            self.add_next_state_to_queue(penalty, node, false, &mut count, &mut queue, line);
            self.add_next_state_to_queue(penalty, node, true, &mut count, &mut queue, line);
        }

        if !dry_run {
            let reconstructed_penalty = self.reconstruct_path(line, initial_state, node);
            assert_eq!(reconstructed_penalty, penalty);
        }

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
            self.format_children(node.newline, dry_run, &mut penalty, &mut state, line);
            penalty += self.indenter.add_token_to_state(line, &mut state, node.newline, dry_run, &mut self.whitespace);
        }

        penalty
    }

    fn add_next_state_to_queue(&mut self, mut penalty: Penalty, prev_node: &'a StateNode<'a>,
                               newline: bool, count: &mut u32, queue: &mut BinaryHeap<QueueItem<'a>>,
                               line: &mut UnwrappedLine) {
        if newline && !self.indenter.can_break(line, &prev_node.state) {
            return;
        }
        if !newline && self.indenter.must_break(line, &prev_node.state) {
            return;
        }

        let dry_run = true;
        let node = self.arena.alloc(StateNode {
            newline: newline,
            previous: Some(prev_node),
            state: prev_node.state.clone(),
        });

        if !self.format_children(newline, dry_run, &mut penalty, &mut node.state, line) {
            return;
        }

        penalty += self.indenter.add_token_to_state(line, &mut node.state, newline, dry_run, &mut self.whitespace);
        queue.push(QueueItem { penalty: penalty, id: *count, node: node });
        *count += 1;
    }

    fn format_children(&mut self, newline: bool, dry_run: bool, penalty: &mut Penalty,
                       state: &mut LineState, line: &mut UnwrappedLine) -> bool {
        let previous = &mut line.tokens[state.next_token_index - 1];

        if previous.children.is_empty() {
            return true;
        }

        if newline {
            let additional_indent = state.stack_top().indent - previous.children[0].level * self.style.indent_width;
            let fix_bad_indentation = true;
            *penalty += self.format(&mut previous.children, dry_run, additional_indent, fix_bad_indentation);
            return true;
        }

        if previous.children[0].tokens[0].must_break_before {
            return false;
        }

        // Can only merge the lines if there is a single child line
        if previous.children.len() > 1 {
            return false;
        }

        // Cannot merge when child has a trailing comment
        if previous.children[0].tokens.last().unwrap().is_trailing_comment(&previous.children[0]) {
            return false;
        }

        // Don't merge when child has semicolon
        // This may actually need to be allowed as a style option to allow things like: if foo { return; }
        if previous.children[0].tokens.last().unwrap().tok == Token::Semi {
            return false;
        }

        // Check the line fits. The is + 2 for the trailing " }"
        if previous.children[0].tokens.last().unwrap().total_length + state.column + 2 > self.style.column_limit {
            return false;
        }

        // If the first token of the child had a newline, keep it
        if previous.children[0].tokens[0].newlines_before > 0 {
            return false;
        }

        let spaces = match previous.tok {
            Token::OpenDelim(DelimToken::Brace) => 1,
            Token::OpenDelim(DelimToken::Bracket) => 0,
            _ => panic!(format!("Unexpected parent token: {:?}", previous.tok)),
        };

        // Place the child on the same line
        if !dry_run {
            self.whitespace.replace_whitespace(&mut previous.children[0].tokens[0], 0, 0, spaces, state.column);
        }

        state.column += spaces;
        *penalty += self.analyze_solution_space(&mut previous.children[0], state.column, dry_run);
        state.column += previous.children[0].tokens.last().unwrap().total_length;

        true
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Default)]
pub struct ParenState {
    // The column to start at if a line break occurs.
    pub indent: u32,
    // The number of indents to appear at the start of the line if a line break occurs
    pub indent_level: u32,
    // The column to start at if a line break occurs in a nested block.
    pub nested_block_indent: u32,
    // Whether this state contains a line break yet
    pub contains_line_break: bool,
    // Whether we should bin packing in this state.
    // If this is true, all or none behavior will be used.
    pub avoid_bin_packing: bool,
    // If true, line breaks will never on this state.
    pub no_line_break: bool,
    // If true, line breaks will always occur between parameters.
    pub break_between_paramters: bool,
    // If a method call expression was broken, the column
    // the start column of the second line.
    pub method_chain_indent: Option<u32>,
    // If true, line breaks will never occur before method calls.
    pub unwrapped_method_chain: bool,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct LineState {
    // The index of the token that needs to be next formatted.
    pub next_token_index: usize,
    // The number of used columns in the current line.
    pub column: u32,
    // The indent of the first token
    pub first_indent: u32,
    // A stack keeping track of properties applying to indentation scope.
    pub stack: Vec<ParenState>,
    // If some, the column to indent when breaking on function return type arrow
    pub fn_decl_arrow_indent: Option<u32>,
    // If true, the return type arrow must indent
    pub fn_decl_arrow_must_break: bool,
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

    pub fn current<'a>(&self, line: &'a UnwrappedLine) -> &'a FormatToken {
        &line.tokens[self.next_token_index]
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

#[derive(Hash, Eq, PartialEq)]
struct CacheKey {
    lines_address: usize,
    indent: u32,
}

impl CacheKey {
    fn new(lines: &[UnwrappedLine], indent: u32) -> CacheKey {
        CacheKey {
            lines_address: lines.as_ptr() as usize,
            indent: indent,
        }
    }
}
