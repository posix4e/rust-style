extern crate rust_style;

use rust_style::{FormatStyle, Replacement};

fn replacements(source: &str) -> Vec<Replacement> {
    let style = FormatStyle::default();
    rust_style::reformat(source, &style, None)
}

#[test]
fn test_no_unnecessary_replacements_eof() {
    assert_eq!(0, replacements("fn main() {}").len());
    assert_eq!(0, replacements("fn main() {}\n").len());
    assert_eq!(1, replacements("fn main() {}\n ").len());
    assert_eq!(1, replacements("fn main() {} \n").len());
    assert_eq!(1, replacements("fn main() {}\n\n").len());
    assert_eq!(1, replacements("fn main() {}\n\n").len());
    assert_eq!(1, replacements("fn main() {}\n   \t \t   \n\n").len());
    assert_eq!(1, replacements("fn main() {}\n   \t \t   \n\n  ").len());
    assert_eq!(1, replacements("fn main() {}\n   \t \t   \n\n  \t").len());
}

#[test]
fn test_replacement_bytes() {
    let r = replacements("let a  =\n3;let a = 2;");
    assert_eq!(r[0].start_byte, 5);
    assert_eq!(r[0].end_byte, 7);
    assert_eq!(r[0].text, " ");
    assert_eq!(r[1].start_byte, 8);
    assert_eq!(r[1].end_byte, 9);
    assert_eq!(r[1].text, " ");
    assert_eq!(r[2].start_byte, 11);
    assert_eq!(r[2].end_byte, 11);
    assert_eq!(r[2].text, "\n");
    // 䶵 = 3 bytes
    let r = replacements("let 䶵  =\n3;let 䶵 = 2;");
    assert_eq!(r[0].start_byte, 7);
    assert_eq!(r[0].end_byte, 9);
    assert_eq!(r[0].text, " ");
    assert_eq!(r[1].start_byte, 10);
    assert_eq!(r[1].end_byte, 11);
    assert_eq!(r[1].text, " ");
    assert_eq!(r[2].start_byte, 13);
    assert_eq!(r[2].end_byte, 13);
    assert_eq!(r[2].text, "\n");
}

#[test]
fn test_replacement_characters() {
    // same as bytes
    let r = replacements("let a  =\n3;let a = 2;");
    assert_eq!(r[0].start_character, 5);
    assert_eq!(r[0].end_character, 7);
    assert_eq!(r[0].text, " ");
    assert_eq!(r[1].start_character, 8);
    assert_eq!(r[1].end_character, 9);
    assert_eq!(r[1].text, " ");
    assert_eq!(r[2].start_character, 11);
    assert_eq!(r[2].end_character, 11);
    assert_eq!(r[2].text, "\n");
    // 䶵 = 3 bytes
    let r = replacements("let 䶵  =\n3;let 䶵 = 2;");
    assert_eq!(r[0].start_character, 5);
    assert_eq!(r[0].end_character, 7);
    assert_eq!(r[0].text, " ");
    assert_eq!(r[1].start_character, 8);
    assert_eq!(r[1].end_character, 9);
    assert_eq!(r[1].text, " ");
    assert_eq!(r[2].start_character, 11);
    assert_eq!(r[2].end_character, 11);
    assert_eq!(r[2].text, "\n");
}
