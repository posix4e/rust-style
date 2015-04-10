use token::FormatToken;
use unwrapped_line::UnwrappedLine;

// TODO: actually annotate the lines. These fields are currently the
//       same as UnwrappedLine.
#[derive(Debug)]
pub struct AnnotatedLine {
    pub tokens: Vec<FormatToken>,
    pub children: Vec<AnnotatedLine>,
    pub level: u32,
}


impl AnnotatedLine {
    pub fn from_unwrapped_lines(lines: Vec<UnwrappedLine>) -> Vec<AnnotatedLine> {
        lines.into_iter().map(|l| {
            AnnotatedLine {
                tokens: l.tokens,
                level: l.level,
                children: AnnotatedLine::from_unwrapped_lines(l.children),
            }
        }).collect()
    }
}
