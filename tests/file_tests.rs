extern crate rust_style;
extern crate diff;

use std::fs;
use std::io::Read;
use rust_style::{Replacement, FormatStyle};

// idempotent tests - the input and output must match exactly
#[test]
fn test_idem() {
    // Get all files in the tests/idem directory
    let files = fs::read_dir("tests/idem").unwrap();

    // For each file, run rustfmt and collect the output
    let mut count = 0;
    let mut failures = 0;
    for filename in files {
        let filename = filename.unwrap().path().to_str().unwrap().to_owned();
        println!("Testing '{}'...", filename);

        let mut source = String::new();
        let mut file = fs::File::open(&filename).unwrap();
        file.read_to_string(&mut source).unwrap();

        let style = FormatStyle::default();
        let replacements = rust_style::reformat(&source, &style, None);
        let formatted = Replacement::apply_all(&replacements, &source);

        if source != formatted {
            println!("{}", make_diff(&filename, &source, &formatted));
            failures += 1;
        }
        count += 1;

    }

    println!("Ran {} idem tests; {} failures.", count, failures);
    assert!(failures == 0, "{} idem tests failed", failures);
}

// Copied from rustfmt
fn make_diff(file_name: &str, expected: &str, actual: &str) -> String {
    let mut line_number = 1;
    let mut prev_both = true;
    let mut text = String::new();

    for result in diff::lines(expected, actual) {
        match result {
            diff::Result::Left(str) => {
                if prev_both {
                    text.push_str(&format!("Mismatch @ {}:{}\n", file_name, line_number));
                }
                text.push_str(&format!("-{}⏎\n", str));
                prev_both = false;
            }
            diff::Result::Right(str) => {
                if prev_both {
                    text.push_str(&format!("Mismatch @ {}:{}\n", file_name, line_number));
                }
                text.push_str(&format!("+{}⏎\n", str));
                prev_both = false;
                line_number += 1;
            }
            diff::Result::Both(..) => {
                line_number += 1;
                prev_both = true;
            }
        }
    }

    text
}
