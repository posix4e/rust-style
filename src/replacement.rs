#[derive(Clone, Debug, RustcEncodable)]
pub struct Replacement {
    pub start_byte: usize,
    pub end_byte: usize,
    pub text: String,
}

impl Replacement {
    pub fn apply_all(replacements: &[Replacement], source: &str) -> String {
        let source = source.as_bytes();
        let mut output = vec![];
        let mut i = 0;
        for replacement in replacements {
            output.push_all(&source[i..replacement.start_byte]);
            output.push_all(&replacement.text.as_bytes());
            i = replacement.end_byte;
        }

        if i < source.len() {
            output.push_all(&source[i..]);
        }
        String::from_utf8(output).unwrap()
    }
}
