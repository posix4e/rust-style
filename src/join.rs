use annotated_line::AnnotatedLine;
use style::FormatStyle;
use syntax::parse::token::{Token, DelimToken};

pub fn join_lines(style: &FormatStyle, lines: Vec<AnnotatedLine>) -> Vec<AnnotatedLine> {
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

    let mut out = vec![];
    let mut lines = lines.into_iter();
    for j in joins {
        let mut line = lines.next().unwrap();
        for _ in 0..j {
            line = join(style, line, lines.next().unwrap());
        }
        line.children = join_lines(style, line.children);
        out.push(line);
    }

    assert!(lines.next().is_none());
    out
}


fn try_join_empty_block(lines: &[AnnotatedLine]) -> Option<usize> {
    let join = lines.len() >= 2 &&
        lines[0].children.is_empty() &&
        lines[0].tokens.last().unwrap().tok == Token::OpenDelim(DelimToken::Brace) &&
        lines[1].tokens.first().unwrap().tok == Token::CloseDelim(DelimToken::Brace);
    if join { Some(1) } else { None }
}

fn join(style: &FormatStyle, mut a: AnnotatedLine, mut b: AnnotatedLine) -> AnnotatedLine {
    assert!(a.children.len() == 0);
    a.tokens.append(&mut b.tokens);

    AnnotatedLine {
        tokens: a.tokens,
        level: a.level,
        children: b.children,
    }
}
