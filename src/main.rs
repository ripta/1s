mod lexer;
mod parser;
mod state;
mod sym;

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use snafu::prelude::*;
use state::{EvaluationError, State};
use std::iter::Iterator;
use std::result;
use std::string::ToString;
use std::time::Instant;

const PKG_NAME: &str = env!("CARGO_PKG_NAME");
const PKG_VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    let flags = parse_args();
    if let Err(e) = flags {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }

    if let Err(e) = run(flags.unwrap()) {
        println!("Error: {}", e);
        std::process::exit(1);
    }
}

#[derive(Debug)]
struct Flags {
    interactive: bool,
    no_banner: bool,
    trace_exec: bool,
    trace_load: bool,
    files: Vec<String>,
}

fn parse_args() -> state::Result<Flags> {
    let mut args = pico_args::Arguments::from_env();
    let mut flags = Flags {
        interactive: args.contains("-i"),
        no_banner: args.contains("-q"),
        trace_exec: args.contains("-t"),
        trace_load: args.contains("-l"),
        files: Vec::new(),
    };

    let rem = args.finish();
    if !rem.is_empty() {
        flags.files = rem.iter().map(|s| s.to_string_lossy().to_string()).collect();
    }

    return Ok(flags);
}

#[derive(Debug, Snafu)]
enum Error {
    #[snafu(display("evaluation error: {source}"))]
    Evaluation { source: EvaluationError },
    #[snafu(display("editor error: {source}"))]
    Editor { source: ReadlineError },
}

impl From<EvaluationError> for Error {
    fn from(value: EvaluationError) -> Self {
        return Error::Evaluation { source: value };
    }
}

impl From<ReadlineError> for Error {
    fn from(value: ReadlineError) -> Self {
        return Error::Editor { source: value };
    }
}

type Result<T> = result::Result<T, Error>;

fn run(flags: Flags) -> Result<u8> {
    let mut state = State::new();
    if !flags.no_banner {
        println!("-- 1s :: {} v{}", PKG_NAME, PKG_VERSION);
    }

    for filename in flags.files {
        let content = state::read_file(filename.clone())?;

        let size = content.len();
        let t0 = Instant::now();
        state = state::run_string(state, content, flags.trace_exec)?;

        let dur = t0.elapsed().as_micros();
        if flags.trace_load {
            println!("{filename:?} {{LOAD}} [ #size {size} #runtime_µs {dur} ]");
        }
    }

    if flags.interactive {
        let mut reader = DefaultEditor::new()?;
        println!("Entering interactive session; ^D to exit");
        loop {
            match reader.readline("1s> ") {
                Ok(line) => {
                    if line.is_empty() {
                        continue;
                    }

                    // println!("{}", line);
                    match state::run_string(state.clone(), line, flags.trace_exec) {
                        Ok(ns) => {
                            if ns.stack.is_empty() {
                                println!("Empty Stack");
                            } else {
                                println!("Stack:");
                                for st in &ns.stack {
                                    println!("  {}", st);
                                }
                            }
                            state = ns;
                        }
                        Err(EvaluationError::UndefinedWord { word, location: _ }) => {
                            println!("Error: word '{word}' is undefined");
                            println!(
                                "Defined words: {:?}",
                                state::dump_definitions(state.clone().definitions)
                            );
                        }
                        Err(e) => {
                            println!("Error: {}", e);
                            if state.stack.is_empty() {
                                println!("Empty Stack");
                            } else {
                                println!("Stack:");
                                for st in &state.stack {
                                    println!("  {}", st);
                                }
                            }
                        }
                    }
                }
                Err(rustyline::error::ReadlineError::Interrupted) => {
                    println!("^C detected");
                    break;
                }
                Err(rustyline::error::ReadlineError::Eof) => {
                    println!("^D detected");
                    break;
                }
                Err(e) => {
                    println!("Unhandled error: {:?}", e);
                    break;
                }
            }

            print!("\n");
        }
    } else {
        // println!("RESULT {:?}", state.stack);
        println!("Result:");
        for item in state.stack {
            println!("  {}", item);
        }
    }

    return Ok(0);
}
