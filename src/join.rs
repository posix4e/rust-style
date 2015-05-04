use style::FormatStyle;
use std::mem;
use syntax::parse::token::{Token, DelimToken};
use unwrapped_line::UnwrappedLine;

pub fn join_lines(style: &FormatStyle, lines: Vec<UnwrappedLine>) -> Vec<UnwrappedLine> {
    let mut joins = vec![];
    let mut i = 0;
    while i < lines.len() {
        let lines = &lines[i..];
        let joined_lines = try_join_empty_block(lines);
        assert!(joined_lines != Some(0));
        let joined_lines = joined_lines.unwrap_or(0);

        joins.push(joined_lines);
        // Skip joined lines
        i += joined_lines;
        // Next line
        i += 1;
    }

    let mut temp = vec![];
    let mut out = vec![];
    let mut lines = lines.into_iter();
    for j in joins {
        let mut line = lines.next().unwrap();
        for _ in 0..j {
            line = join(line, lines.next().unwrap());
        }
        for token in &mut line.tokens {
            // FIXME: there has to be a better way
            mem::swap(&mut token.children, &mut temp);
            temp = join_lines(style, temp);
            mem::swap(&mut token.children, &mut temp);
        }
        out.push(line);
    }

    assert!(lines.next().is_none());
    out
}


fn try_join_empty_block(lines: &[UnwrappedLine]) -> Option<usize> {
    let join = lines.len() >= 2 &&
        lines[0].tokens.last().unwrap().children.is_empty() &&
        lines[0].tokens.last().unwrap().tok == Token::OpenDelim(DelimToken::Brace) &&
        lines[1].tokens.first().unwrap().tok == Token::CloseDelim(DelimToken::Brace);
    if join { Some(1) } else { None }
}

// TODO: more joins

fn join(mut a: UnwrappedLine, b: UnwrappedLine) -> UnwrappedLine {
    assert!(a.block == b.block);

    // FIXME: replace loop with a single function call when a stable function exists in std
    for token in b.tokens {
        a.tokens.push(token);
    }

    let mut line = UnwrappedLine {
        tokens: a.tokens,
        level: a.level,
        typ: a.typ,
        block: a.block,
        affected : a.affected || b.affected,
        leading_empty_lines_affected : a.leading_empty_lines_affected ||
                                       b.leading_empty_lines_affected,
        children_affected : a.children_affected || b.children_affected,
    };


    line.reset_token_indices();
    line
}
