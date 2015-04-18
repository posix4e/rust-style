#![feature(rustc_private)]
#![feature(collections)]
#![feature(core)]

extern crate arena;
extern crate syntax;

pub mod annotate;
pub mod continuation_indenter;
pub mod format;
pub mod join;
pub mod reformat;
pub mod replacement;
pub mod style;
pub mod token;
pub mod unwrapped_line;
pub mod whitespace_manager;

#[cfg(test)]
mod test;
