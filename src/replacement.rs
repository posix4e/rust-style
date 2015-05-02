#[derive(Clone, Debug, RustcEncodable)]
pub struct Replacement {
    pub start_byte: usize,
    pub end_byte: usize,
    pub start_character: usize,
    pub end_character: usize,
    pub text: String,
}

impl Replacement {
    pub fn apply_all(replacements: &[Replacement], source: &str) -> String {
        let source = source.as_bytes();
        let mut output = vec![];
        let mut i = 0;
        for replacement in replacements {
            push_all(&mut output, &source[i..replacement.start_byte]);
            push_all(&mut output, &replacement.text.as_bytes());
            i = replacement.end_byte;
        }

        if i < source.len() {
            push_all(&mut output, &source[i..]);
        }
        String::from_utf8(output).unwrap()
    }
}

// FIXME: replace when std::vec::Vec has a stable replacement
fn push_all(dest: &mut Vec<u8>, src: &[u8]) {
    for b in src {
        dest.push(*b);
    }
}
