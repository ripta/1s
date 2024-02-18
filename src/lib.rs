pub mod lexer;
pub mod parser;
pub mod state;
pub mod sym;

pub const PKG_NAME: &str = env!("CARGO_PKG_NAME");
pub const PKG_VERSION: &str = env!("CARGO_PKG_VERSION");
