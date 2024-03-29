use one_stack;
use one_stack::lexer::Location;
use one_stack::parser::{ParseKind, ParseNode};
use one_stack::state;
use one_stack::state::{EvaluationError, State};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use snafu::prelude::*;
use std::iter::Iterator;
use std::result;
use std::string::ToString;

const PKG_NAME: &str = env!("CARGO_PKG_NAME");
const PKG_VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    let flags = parse_args();
    if let Err(e) = flags {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }

    if let Err(e) = run(flags.unwrap()) {
        eprintln!("Error: {}", e);
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
    let mut sm = one_stack::sym::SymbolManager::new();
    sm.set_trace(flags.trace_load);

    let mut state = State::new_with_symbol_manager(sm);
    if !flags.no_banner {
        println!("-- 1s :: {} v{}", PKG_NAME, PKG_VERSION);
    }

    for filename in flags.files {
        let loader = vec![
            ParseNode {
                kind: ParseKind::StringValue(filename),
                location: Location::Static("1s::run"),
            },
            ParseNode {
                kind: ParseKind::WordRef("{LOAD}".to_string()),
                location: Location::Static("1s::run"),
            },
        ];
        state = state::run_program(state, loader, false)?;
    }

    if flags.interactive {
        let mut reader = DefaultEditor::new()?;

        let history_path = "history.txt";
        if reader.load_history(history_path).is_err() {
            println!("No previous interactive session history.");
        }

        let mut line_num = 0;

        println!("Entering interactive session; ^D to exit");
        loop {
            line_num += 1;
            match reader.readline(&format!("1s:{}> ", line_num)) {
                Ok(line) => {
                    if line.is_empty() {
                        continue;
                    }

                    reader.add_history_entry(line.as_str())?;

                    match state::run_string(state.clone(), line, flags.trace_exec) {
                        Ok(ns) => {
                            if ns.is_stack_empty() {
                                println!("Empty Stack");
                            } else {
                                println!("Stack:");
                                ns.show_stack()?;
                            }
                            state = ns;
                        }
                        Err(EvaluationError::UndefinedWord { word, location: _ }) => {
                            println!("Error: word '{word}' is undefined");
                        }
                        Err(e) => {
                            println!("Error: {}", e);
                            if state.is_stack_empty() {
                                println!("Empty Stack");
                            } else {
                                println!("Stack:");
                                state.show_stack()?;
                            }
                        }
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    println!("^C detected");
                    break;
                }
                Err(ReadlineError::Eof) => {
                    println!("^D detected");
                    break;
                }
                Err(e) => {
                    println!("Unhandled error: {:?}", e);
                    break;
                }
            }

            println!();
        }

        reader.save_history(history_path)?;
    } else {
        println!("Result:");
        state.show_stack()?;
        // println!("Statistics: {:?}", state.counter);
    }

    return Ok(0);
}
