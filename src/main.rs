mod sym;

use rand::Rng;
use rand_chacha::rand_core::SeedableRng;
use rand_chacha::ChaCha20Rng;
use rustyline::DefaultEditor;
use snafu::prelude::*;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::iter::Iterator;
use std::slice::Iter;
use std::string::ToString;
use std::time::Instant;
use std::{fmt, result};
use string_interner::DefaultSymbol;

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

fn parse_args() -> Result<Flags> {
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

fn run(flags: Flags) -> Result<u8> {
    let mut state = State::new();
    if !flags.no_banner {
        println!("-- 1s :: {} v{}", PKG_NAME, PKG_VERSION);
    }

    for filename in flags.files {
        let content = std::fs::read_to_string(filename.clone()).context(FileLoadSnafu {
            filename: filename.clone(),
        })?;

        let size = content.len();
        let t0 = Instant::now();
        state = run_string(state, content, flags.trace_exec)?;

        let dur = t0.elapsed().as_micros();
        if flags.trace_load {
            println!("{filename:?} {{LOAD}} [ #size {size} #runtime_µs {dur} ]");
        }
    }

    if flags.interactive {
        let mut reader = DefaultEditor::new().context(NoInteractiveEditorSnafu)?;
        println!("Entering interactive session; ^D to exit");
        loop {
            match reader.readline("1s> ") {
                Ok(line) => {
                    if line.is_empty() {
                        continue;
                    }

                    // println!("{}", line);
                    match run_string(state.clone(), line, flags.trace_exec) {
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
                            println!("Defined words: {:?}", dump_definitions(state.clone().definitions));
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

fn run_string(mut state: State, content: String, trace_exec: bool) -> Result<State> {
    let tokens = lex(content)?;
    if trace_exec {
        println!("LEXED-TOKENS {}", tokens.len());
    }

    let pt = parse(&mut state.symbols, tokens)?;

    let mut prog = pt.top_level.clone();
    prog.reverse();

    let s = State::with(state, prog);
    return run_state(s, trace_exec);
}

fn run_state(mut s: State, trace_exec: bool) -> Result<State> {
    if trace_exec {
        println!("Pre-eval {:?}", s.counter);
        println!("  Stack: {:?}", s.stack);
        println!("  Program: {:?}", s.program);

        println!("  Definitions: {:?}", dump_definitions(s.clone().definitions));
        print!("\n");
    }

    while !s.program.is_empty() {
        s = eval(s)?;

        if trace_exec {
            println!("Step {:?}", s.counter);
            println!("  Stack: ");
            for st in &s.stack {
                println!("    {}", st);
            }
            println!("  Program: {:?}", s.program);

            println!("  Definitions: {:?}", dump_definitions(s.clone().definitions));
            print!("\n");
        }
    }

    return Ok(s);
}

fn dump_definitions(definitions: HashMap<String, Code>) -> Vec<String> {
    let mut def_names: Vec<String> = Vec::with_capacity(definitions.len());
    for (def_name, _) in &definitions {
        def_names.push(def_name.to_string());
    }
    def_names.sort();
    return def_names;
}

#[derive(Debug, Clone)]
struct Token {
    kind: TokenKind,
    location: Location,
}

#[derive(Debug, Clone)]
enum TokenKind {
    Comment(String),
    LiteralFloat(f64),
    LiteralInteger(i64),
    LiteralString(String),
    Word(String),
}

#[derive(Debug, Clone)]
enum Location {
    Evaluation(usize),
    Source(usize, usize),
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

type Result<T> = result::Result<T, EvaluationError>;

#[derive(Debug, Snafu)]
enum EvaluationError {
    #[snafu(display("guard violation: {reason}"))]
    GuardViolation { reason: String },
    #[snafu(display("invalid command line flag"))]
    InvalidFlag { source: pico_args::Error },
    #[snafu(display("cannot load file '{filename}'"))]
    FileLoad { source: std::io::Error, filename: String },
    #[snafu(display("no interactive editor available"))]
    NoInteractiveEditor { source: rustyline::error::ReadlineError },
    #[snafu(display("invalid float token '{token}'"))]
    InvalidFloatToken {
        source: std::num::ParseFloatError,
        token: String,
    },
    #[snafu(display("invalid integer token '{token}'"))]
    InvalidIntegerToken {
        source: std::num::ParseIntError,
        token: String,
    },
    #[snafu(display("unclosed string"))]
    UnclosedString,
    #[snafu(display("incompatible value on stack: expected '{expected}', but found '{found}'"))]
    IncompatibleValue { expected: String, found: String },
    #[snafu(display("stack underflow: {reason}"))]
    ReasonedStackUnderflow { reason: String },
    #[snafu(display("stack underflow"))]
    StackUnderflow,
    #[snafu(display("{op}: cannot operate on {value}"))]
    CannotOperate { op: String, value: ParseNode },
    #[snafu(display("word '{word}' is undefined"))]
    UndefinedWord { word: String, location: Location },
}

fn get_block(s: ParseNode) -> Result<Vec<ParseNode>> {
    match s.kind {
        ParseKind::Block(b) => Ok(b.to_vec()),
        otherwise => IncompatibleValueSnafu {
            expected: "quoted block",
            found: otherwise.to_string(),
        }
        .fail(),
    }
}

fn get_float(s: &ParseNode) -> Result<f64> {
    match s.clone().kind {
        ParseKind::FloatValue(f) => Ok(f),
        otherwise => IncompatibleValueSnafu {
            expected: "float",
            found: otherwise.to_string(),
        }
        .fail(),
    }
}

fn get_integer(s: &ParseNode) -> Result<i64> {
    match s.clone().kind {
        ParseKind::IntegerValue(i) => Ok(i),
        otherwise => IncompatibleValueSnafu {
            expected: "integer",
            found: otherwise.to_string(),
        }
        .fail(),
    }
}

fn get_string(s: &ParseNode) -> Result<String> {
    match s.clone().kind {
        ParseKind::StringValue(s) => Ok(s),
        otherwise => IncompatibleValueSnafu {
            expected: "string",
            found: otherwise.to_string(),
        }
        .fail(),
    }
}

fn get_sym(s: &ParseNode) -> Result<DefaultSymbol> {
    match s.clone().kind {
        ParseKind::Symbol(s) => Ok(s),
        otherwise => IncompatibleValueSnafu {
            expected: "symbol",
            found: otherwise.to_string(),
        }
        .fail(),
    }
}

fn get_word(s: ParseNode) -> Result<String> {
    match s.kind {
        ParseKind::WordRef(w) => Ok(w.to_string()),
        otherwise => IncompatibleValueSnafu {
            expected: "word",
            found: otherwise.to_string(),
        }
        .fail(),
    }
}

macro_rules! checked_pop {
    ( $e:expr ) => {
        $e.stack.pop().context(ReasonedStackUnderflowSnafu {
            reason: "safe-popping from stack",
        })?
    };
}

fn builtin_add(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    return match node.kind {
        ParseKind::Block(b) => {
            let vals = b.iter().map(get_integer).collect::<Result<Vec<i64>>>()?;
            let sum = vals.iter().fold(0, |acc, v| acc + v);
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(sum),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::FloatValue(a) => {
            let b = get_float(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::FloatValue(a + b),
                location: state.clone().location,
            });
            Ok(state)
        }

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(a + b),
                location: state.clone().location,
            });
            Ok(state)
        }

        ParseKind::StringValue(a) => {
            let b = get_string(&checked_pop!(state))?;

            let mut res = a.to_owned();
            res.push_str(&b);
            state.stack.push(ParseNode {
                kind: ParseKind::StringValue(res),
                location: state.clone().location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{+}".to_string(),
            value: node.clone(),
        }),
    };
}

fn builtin_car(mut state: State) -> Result<State> {
    // `car` of a non-quoted block should error out
    let mut a = get_block(checked_pop!(state))?;

    // `car` of an empty block is itself an empty block, so ignore removal of an already empty block
    if a.len() > 0 {
        a.remove(0);
    }
    state.stack.push(ParseNode {
        kind: ParseKind::Block(a),
        location: state.clone().location,
    });

    return Ok(state);
}

fn builtin_cdr(mut state: State) -> Result<State> {
    // `cdr` of a non-quoted block should error out
    let mut a = get_block(checked_pop!(state))?;

    // `cdr` of an empty block is itself an empty block, so ignore removal of an already empty block
    if a.len() > 1 {
        state.stack.push(a.remove(0));
    } else {
        state.stack.push(ParseNode {
            kind: ParseKind::Block(vec![]),
            location: state.clone().location,
        });
    }

    return Ok(state);
}

fn builtin_ceil(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    return match node.kind {
        ParseKind::FloatValue(a) => {
            state.stack.push(ParseNode {
                kind: ParseKind::FloatValue(a.ceil()),
                location: node.location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{⌈}".to_string(),
            value: node.clone(),
        }),
    };
}

fn builtin_cond(mut state: State) -> Result<State> {
    let mut cond = get_block(checked_pop!(state))?;
    let sym_true = state.symbols.get_true();

    let mut s2 = state.clone();
    loop {
        let check = get_block(cond.pop().context(ReasonedStackUnderflowSnafu {
            reason: "looking for a CHECK block in {COND}",
        })?)?;
        let mut branch = get_block(cond.pop().context(ReasonedStackUnderflowSnafu {
            reason: "looking for a BRANCH block in {COND}",
        })?)?;

        s2 = State::with(s2, check);
        s2 = run_state(s2, false)?;

        if get_sym(&checked_pop!(s2))? == sym_true {
            branch.reverse();
            state.program.append(&mut branch);
            break;
        }

        if cond.is_empty() {
            return Err(EvaluationError::GuardViolation {
                reason: "condition never matches".to_string(),
            });
        }
    }

    return Ok(state);
}

fn builtin_const_pi(mut state: State) -> Result<State> {
    state.stack.push(ParseNode {
        kind: ParseKind::FloatValue(std::f64::consts::PI),
        location: state.clone().location,
    });
    return Ok(state);
}

fn builtin_const_sqrt2(mut state: State) -> Result<State> {
    state.stack.push(ParseNode {
        kind: ParseKind::FloatValue(std::f64::consts::SQRT_2),
        location: state.clone().location,
    });
    return Ok(state);
}

// -- [B] [A] cons == [[B] A]
fn builtin_cons(mut state: State) -> Result<State> {
    let a = get_block(checked_pop!(state))?;
    let b = checked_pop!(state);

    // let n = Vec::from(VecDeque::from(a).push_front(b.clone()));
    let mut n: Vec<ParseNode> = Vec::with_capacity(a.len() + 1);
    n.push(b.clone());
    n.extend(a);

    state.stack.push(ParseNode {
        kind: ParseKind::Block(n),
        location: state.clone().location,
    });

    return Ok(state);
}

fn builtin_define(mut state: State) -> Result<State> {
    let syms = get_block(checked_pop!(state))?;
    let body = get_block(checked_pop!(state))?;

    for sym in syms {
        let word = get_word(sym)?;
        state.definitions.insert(word, Code::Program(body.clone()));
    }
    return Ok(state);
}

fn builtin_div(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    return match node.kind {
        ParseKind::Block(b) => {
            let vals = b.iter().map(get_integer).collect::<Result<Vec<i64>>>()?;
            let sum = vals
                .iter()
                .copied()
                .reduce(|acc, v| acc / v)
                .context(ReasonedStackUnderflowSnafu {
                    reason: "no elements to divide",
                })?;
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(sum),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::FloatValue(a) => {
            let b = get_float(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::FloatValue(b / a),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(b / a),
                location: node.location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{/}".to_string(),
            value: node.clone(),
        }),
    };
}

fn builtin_exactly_equal(mut state: State) -> Result<State> {
    let a = checked_pop!(state);
    let b = checked_pop!(state);

    return match (a.kind, b.kind) {
        (ParseKind::Symbol(s1), ParseKind::Symbol(s2)) => {
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(s1.eq(&s2))),
                location: a.location,
            });
            Ok(state)
        }
        (ka, kb) => {
            // TODO(ripta): when ParseNode locations are fixed, this will break;
            //              properly impelement equality that ignores location
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(ka == kb)),
                location: a.location,
            });
            Ok(state)
        }
    };
}

fn builtin_assert(mut state: State) -> Result<State> {
    let msg = get_string(&checked_pop!(state))?;
    let val = get_sym(&checked_pop!(state))?;

    if val == state.symbols.get_true() {
        return Ok(state);
    }

    return Err(EvaluationError::GuardViolation { reason: msg });
}

fn builtin_floor(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    return match node.kind {
        ParseKind::FloatValue(a) => {
            state.stack.push(ParseNode {
                kind: ParseKind::FloatValue(a.floor()),
                location: node.location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{⌊}".to_string(),
            value: node.clone(),
        }),
    };
}

fn builtin_gt(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    return match node.kind {
        ParseKind::FloatValue(a) => {
            let b = get_float(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(b > a)),
                location: state.clone().location,
            });
            Ok(state)
        }

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(b > a)),
                location: state.clone().location,
            });
            Ok(state)
        }

        ParseKind::StringValue(a) => {
            let b = get_string(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(b > a)),
                location: state.clone().location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{>}".to_string(),
            value: node.clone(),
        }),
    };
}

fn builtin_gte(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    return match node.kind {
        ParseKind::FloatValue(a) => {
            let b = get_float(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(b >= a)),
                location: state.clone().location,
            });
            Ok(state)
        }

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(b >= a)),
                location: state.clone().location,
            });
            Ok(state)
        }

        ParseKind::StringValue(a) => {
            let b = get_string(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(b >= a)),
                location: state.clone().location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{>=}".to_string(),
            value: node.clone(),
        }),
    };
}

fn builtin_if(mut state: State) -> Result<State> {
    let false_sym = state.symbols.get_false();
    let true_sym = state.symbols.get_true();

    let mut false_branch = get_block(checked_pop!(state))?;
    let mut true_branch = get_block(checked_pop!(state))?;

    match get_sym(&checked_pop!(state))? {
        sym if sym == false_sym => {
            false_branch.reverse();
            state.program.extend(false_branch);
            Ok(state)
        }
        sym if sym == true_sym => {
            true_branch.reverse();
            state.program.extend(true_branch);
            Ok(state)
        }
        sym => Err(EvaluationError::GuardViolation {
            reason: format!(
                "condition #{} is neither #true nor #false",
                state.symbols.find(sym).unwrap()
            ),
        }),
    }
}

fn builtin_len(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    return match node.kind {
        ParseKind::Block(v) => {
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(v.len() as i64),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::StringValue(s) => {
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(s.len() as i64),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::Symbol(sym) if sym == state.symbols.get("stack") => {
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(state.stack.len() as i64),
                location: node.location,
            });
            Ok(state)
        }

        t => Err(EvaluationError::IncompatibleValue {
            expected: "string or #stack".to_string(),
            found: t.to_string(),
        }),
    };
}

fn builtin_lt(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    return match node.kind {
        ParseKind::FloatValue(a) => {
            let b = get_float(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(b < a)),
                location: state.clone().location,
            });
            Ok(state)
        }

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(b < a)),
                location: state.clone().location,
            });
            Ok(state)
        }

        ParseKind::StringValue(a) => {
            let b = get_string(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(b < a)),
                location: state.clone().location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{<}".to_string(),
            value: node.clone(),
        }),
    };
}

fn builtin_lte(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    return match node.kind {
        ParseKind::FloatValue(a) => {
            let b = get_float(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(b <= a)),
                location: state.clone().location,
            });
            Ok(state)
        }

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(b <= a)),
                location: state.clone().location,
            });
            Ok(state)
        }

        ParseKind::StringValue(a) => {
            let b = get_string(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::Symbol(state.symbols.get_bool(b <= a)),
                location: state.clone().location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{<=}".to_string(),
            value: node.clone(),
        }),
    };
}

fn builtin_mod(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    return match node.kind {
        ParseKind::Block(b) => {
            let vals = b.iter().rev().map(get_integer).collect::<Result<Vec<i64>>>()?;
            let sum = vals
                .iter()
                .copied()
                .reduce(|acc, v| acc % v)
                .context(ReasonedStackUnderflowSnafu {
                    reason: "no elements to mod",
                })?;
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(sum),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::FloatValue(a) => {
            let b = get_float(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::FloatValue(b % a),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(b % a),
                location: node.location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{%}".to_string(),
            value: node.clone(),
        }),
    };
}

fn builtin_mul(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    return match node.kind {
        ParseKind::Block(b) => {
            let vals = b.iter().map(get_integer).collect::<Result<Vec<i64>>>()?;
            let sum = vals.iter().fold(1, |acc, v| acc * v);
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(sum),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::FloatValue(a) => {
            let b = get_float(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::FloatValue(a * b),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(a * b),
                location: node.location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{*}".to_string(),
            value: node.clone(),
        }),
    };
}

fn builtin_stack_empty(state: crate::State) -> Result<crate::State> {
    if state.stack.is_empty() {
        return Ok(state);
    }
    return Err(EvaluationError::GuardViolation {
        reason: "stack is not empty".to_string(),
    });
}

fn builtin_show(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    builtin_show_node(&state.symbols, node)?;
    eprintln!();
    return Ok(state);
}

fn builtin_show_node(symbols: &sym::SymbolManager, node: ParseNode) -> Result<()> {
    match node.kind {
        ParseKind::FloatValue(f) => {
            eprint!("{} ", f);
            Ok(())
        }

        ParseKind::IntegerValue(i) => {
            eprint!("{} ", i);
            Ok(())
        }

        ParseKind::StringValue(s) => {
            eprint!("{:?} ", s);
            Ok(())
        }

        ParseKind::Symbol(sym) => match symbols.find(sym) {
            None => Err(EvaluationError::GuardViolation {
                reason: "!!BUG!! symbol should have existed, but doesn't".to_string(),
            }),
            Some(s) => {
                eprint!("#{} ", s);
                Ok(())
            }
        },

        ParseKind::Block(nodes) => {
            for node in nodes {
                builtin_show_node(symbols, node)?;
            }
            Ok(())
        }

        ParseKind::WordRef(word) => {
            eprint!("{} ", word);
            Ok(())
        }
    }
}

fn builtin_sub(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    return match node.kind {
        ParseKind::Block(b) => {
            let vals = b.iter().rev().map(get_integer).collect::<Result<Vec<i64>>>()?;
            let sum = vals
                .iter()
                .copied()
                .reduce(|acc, v| acc - v)
                .context(ReasonedStackUnderflowSnafu {
                    reason: "no elements to subtract",
                })?;
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(sum),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::FloatValue(a) => {
            let b = get_float(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::FloatValue(b - a),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(b - a),
                location: node.location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{-}".to_string(),
            value: node.clone(),
        }),
    };
}

fn builtin_sym_attr(mut state: State) -> Result<State> {
    let attr = get_sym(&checked_pop!(state))?;
    let sym = get_sym(&checked_pop!(state))?;

    let val = state.symbols.attribute(&sym, &attr);
    state.stack.push(ParseNode {
        kind: ParseKind::Symbol(state.symbols.get_bool(val)),
        location: state.location.clone(),
    });

    return Ok(state);
}

fn builtin_sym_prop(mut state: State) -> Result<State> {
    let key = get_sym(&checked_pop!(state))?;
    let sym = get_sym(&checked_pop!(state))?;

    let none = state.symbols.get("none");
    match state.symbols.property(&sym, &key) {
        None => state.stack.push(ParseNode {
            kind: ParseKind::Symbol(none),
            location: state.location.clone(),
        }),
        Some(val) => state.stack.push(ParseNode {
            kind: ParseKind::Symbol(*val),
            location: state.location.clone(),
        }),
    }

    return Ok(state);
}

fn builtin_k(mut state: State) -> Result<State> {
    let mut a = get_block(checked_pop!(state))?;
    checked_pop!(state);

    a.reverse();
    state.program.append(&mut a);

    return Ok(state);
}

fn builtin_rand(mut state: State) -> Result<State> {
    let i: i64 = ChaCha20Rng::from_entropy().sample(rand::distributions::Standard);
    state.stack.push(ParseNode {
        kind: ParseKind::IntegerValue(i),
        location: state.location.clone(),
    });
    return Ok(state);
}

fn builtin_sip(mut state: State) -> Result<State> {
    let a = get_block(checked_pop!(state))?;
    let b = checked_pop!(state);

    state.stack.push(b.clone());
    state.program.push(b.clone());
    let mut p = a.clone();
    p.reverse();
    state.program.extend(p);

    return Ok(state);
}

fn builtin_time(mut state: State) -> Result<State> {
    state.stack.push(ParseNode {
        kind: ParseKind::FloatValue(state.t0.elapsed().as_secs_f64()),
        location: state.location.clone(),
    });
    return Ok(state);
}

fn is_float(word: String) -> bool {
    if word.len() < 2 {
        return false;
    }
    // TODO(ripta): refine criterion, e.g., 2.1e5, .1, 1.2.3
    if word.chars().all(|c| matches!(c, '0'..='9' | '.')) {
        return true;
    }
    if word.starts_with('-') && word.len() > 1 {
        return match word.chars().nth(1) {
            Some(c) if c.is_ascii_digit() => true,
            _ => false,
        };
    }
    return false;
}

fn is_integer(word: String) -> bool {
    if word.chars().all(|c| c.is_ascii_digit()) {
        return true;
    }
    if word.contains('.') {
        return false;
    }
    if word.starts_with('-') && word.len() > 1 {
        return match word.chars().nth(1) {
            Some(c) if c.is_ascii_digit() => true,
            _ => false,
        };
    }
    return false;
}

fn lex(content: String) -> Result<Vec<Token>> {
    let lines = content
        .split("\n")
        .map(|line| line.split_whitespace().map(str::to_string));

    let mut buf = String::new();
    let mut in_comment = false;
    let mut in_string = false;
    let mut comment_word_num = 0usize;

    let mut tokens: Vec<Token> = Vec::new();
    for (line_num, words) in lines.enumerate() {
        for (word_num, word) in words.enumerate() {
            let loc = Location::Source(line_num, word_num);

            if in_comment {
                buf.push_str(" ");
                buf.push_str(&word);
                continue;
            }

            if in_string {
                buf.push_str(" ");

                if word.ends_with('"') {
                    in_string = false;
                    buf.push_str(&word[0..word.len() - 1]);
                    tokens.push(Token {
                        kind: TokenKind::LiteralString(buf.to_string()),
                        location: Location::Source(line_num, word_num),
                    });
                    continue;
                }

                buf.push_str(&word);
                continue;
            }

            // TODO(ripta): stop compressing away multiple whitespace characters in comments
            if word.len() >= 2 && word.chars().all(|c| c == '-') {
                in_comment = true;
                comment_word_num = word_num;
                buf.push_str(&word);
                continue;
            }

            if word.len() >= 2 && word.starts_with("\"") && word.ends_with("\"") {
                tokens.push(Token {
                    kind: TokenKind::LiteralString(word[1..word.len() - 1].to_string()),
                    location: loc,
                });
                continue;
            } else if word.len() >= 2 && word.starts_with('"') {
                in_string = true;
                buf.push_str(&word[1..]);
                continue;
            }

            if is_integer(word.clone()) {
                // word.chars().all(|c| matches!(c, '0'..='9' | '-' | '_' | '.'))
                // i64::from_str();
                let v = word.parse().context(InvalidIntegerTokenSnafu { token: word })?;
                tokens.push(Token {
                    kind: TokenKind::LiteralInteger(v),
                    location: loc,
                });
                continue;
            }

            if is_float(word.clone()) {
                let v = word.parse().context(InvalidFloatTokenSnafu { token: word })?;
                tokens.push(Token {
                    kind: TokenKind::LiteralFloat(v),
                    location: loc,
                });
                continue;
            }

            tokens.push(Token {
                kind: TokenKind::Word(word),
                location: loc,
            });
        }

        if in_comment {
            tokens.push(Token {
                kind: TokenKind::Comment(buf.to_string()),
                location: Location::Source(line_num, comment_word_num),
            });

            in_comment = false;
            comment_word_num = 0;
        }
    }

    if in_string {
        return Err(EvaluationError::UnclosedString);
    }

    return Ok(tokens);
}

#[derive(Debug)]
struct ParseTree {
    top_level: Vec<ParseNode>,
}

#[derive(Debug, Clone)]
struct ParseNode {
    kind: ParseKind,
    location: Location,
}

impl Display for ParseNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ParseKind::Block(vs) => {
                write!(f, "[ ")?;
                for v in vs {
                    write!(f, "{} ", v)?;
                }
                write!(f, "]")?;
                Ok(())
            }
            ParseKind::FloatValue(v) => write!(f, "{:?}", v),
            ParseKind::IntegerValue(v) => write!(f, "{:?}", v),
            ParseKind::StringValue(v) => write!(f, "{:?}", v),
            ParseKind::Symbol(s) => write!(f, "{:?}", s),
            ParseKind::WordRef(w) => write!(f, "{}", w),
            // _ => write!(f, "{:?}", self),
        }
    }
}

impl PartialEq for ParseNode {
    fn eq(&self, other: &Self) -> bool {
        return self.kind == other.kind;
    }
}

#[derive(Debug, Clone)]
enum ParseKind {
    // Binding(String, Vec<Semantic>),
    Block(Vec<ParseNode>),
    // Compiled(
    //     String,
    //     fn(Vec<Semantic>) -> Result<Vec<Semantic>>,
    // ),
    FloatValue(f64),
    IntegerValue(i64),
    StringValue(String),
    Symbol(DefaultSymbol),
    WordRef(String),
}

impl Display for ParseKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl PartialEq for ParseKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ParseKind::Block(b1), ParseKind::Block(b2)) => b1 == b2,
            (ParseKind::FloatValue(f1), ParseKind::FloatValue(f2)) => *f1 == *f2,
            (ParseKind::IntegerValue(i1), ParseKind::IntegerValue(i2)) => *i1 == *i2,
            (ParseKind::StringValue(s1), ParseKind::StringValue(s2)) => s1.eq(s2),
            (ParseKind::WordRef(w1), ParseKind::WordRef(w2)) => w1.eq(w2),
            (_, _) => false,
        }
    }
}

fn parse(symbols: &mut sym::SymbolManager, tokens: Vec<Token>) -> Result<ParseTree> {
    let top = parse_(symbols, &mut tokens.iter());
    return Ok(ParseTree { top_level: top });
}

/// parse_ is the recursive version of parse, during which the (linear) stream of tokens is converted into one or more
/// levels of nested blocks.
///
/// TODO(ripta): recursive descent
fn parse_(symbols: &mut sym::SymbolManager, tokens: &mut Iter<Token>) -> Vec<ParseNode> {
    let mut span: Vec<ParseNode> = Vec::with_capacity(8);

    loop {
        let token = tokens.next();
        match token {
            None => break,
            Some(token) => {
                let token = token.clone();
                match token.kind {
                    TokenKind::Comment(_) => {}
                    TokenKind::LiteralFloat(f) => span.push(ParseNode {
                        kind: ParseKind::FloatValue(f),
                        location: token.location,
                    }),
                    TokenKind::LiteralInteger(d) => span.push(ParseNode {
                        kind: ParseKind::IntegerValue(d),
                        location: token.location,
                    }),
                    TokenKind::LiteralString(s) => span.push(ParseNode {
                        kind: ParseKind::StringValue(s),
                        location: token.location,
                    }),
                    TokenKind::Word(w) => match w.as_str() {
                        "[" => {
                            let block = parse_(symbols, tokens);
                            span.push(ParseNode {
                                kind: ParseKind::Block(block),
                                location: token.location,
                            });
                        }
                        "]" => break,
                        s if s.starts_with('#') => {
                            let sym = symbols.get(&s[1..]);
                            span.push(ParseNode {
                                kind: ParseKind::Symbol(sym),
                                location: token.location,
                            });
                        }
                        _ => span.push(ParseNode {
                            kind: ParseKind::WordRef(w.to_string()),
                            location: token.location,
                        }),
                    },
                }
            }
        }
    }

    return span;
}

#[derive(Debug, Clone)]
enum Code {
    Native(String, fn(State) -> Result<State>),
    Program(Vec<ParseNode>),
}

#[derive(Debug, Clone)]
struct State {
    t0: Instant,
    counter: (usize, usize),
    location: Location,

    stack: Vec<ParseNode>,
    program: Vec<ParseNode>,
    symbols: sym::SymbolManager,
    definitions: HashMap<String, Code>,
}

impl State {
    fn new() -> State {
        let mut defs = HashMap::with_capacity(64);

        defs.insert("{:}".to_string(), Code::Native("{:}".to_string(), builtin_define));
        defs.insert("{ø}".to_string(), Code::Native("{ø}".to_string(), builtin_stack_empty));
        defs.insert(
            "{==}".to_string(),
            Code::Native("{==}".to_string(), builtin_exactly_equal),
        );
        defs.insert("{!!}".to_string(), Code::Native("{!!}".to_string(), builtin_assert));

        defs.insert("{CONS}".to_string(), Code::Native("{CONS}".to_string(), builtin_cons));
        defs.insert("{K}".to_string(), Code::Native("{K}".to_string(), builtin_k));
        defs.insert("{SIP}".to_string(), Code::Native("{SIP}".to_string(), builtin_sip));

        defs.insert("{CAR}".to_string(), Code::Native("{CAR}".to_string(), builtin_car));
        defs.insert("{CDR}".to_string(), Code::Native("{CDR}".to_string(), builtin_cdr));

        defs.insert("{+}".to_string(), Code::Native("{+}".to_string(), builtin_add));
        defs.insert("{/}".to_string(), Code::Native("{/}".to_string(), builtin_div));
        defs.insert("{%}".to_string(), Code::Native("{%}".to_string(), builtin_mod));
        defs.insert("{*}".to_string(), Code::Native("{*}".to_string(), builtin_mul));
        defs.insert("{-}".to_string(), Code::Native("{-}".to_string(), builtin_sub));

        defs.insert("{>}".to_string(), Code::Native("{>}".to_string(), builtin_gt));
        defs.insert("{>=}".to_string(), Code::Native("{>=}".to_string(), builtin_gte));
        defs.insert("{<}".to_string(), Code::Native("{<}".to_string(), builtin_lt));
        defs.insert("{<=}".to_string(), Code::Native("{<=}".to_string(), builtin_lte));

        defs.insert("{⌈}".to_string(), Code::Native("{⌈}".to_string(), builtin_ceil));
        defs.insert("{⌊}".to_string(), Code::Native("{⌊}".to_string(), builtin_floor));

        defs.insert("√2".to_string(), Code::Native("√2".to_string(), builtin_const_sqrt2));
        defs.insert("π".to_string(), Code::Native("π".to_string(), builtin_const_pi));

        defs.insert("{LEN}".to_string(), Code::Native("{LEN}".to_string(), builtin_len));
        defs.insert("{SHOW}".to_string(), Code::Native("{SHOW}".to_string(), builtin_show));
        defs.insert(
            "{RELTIME}".to_string(),
            Code::Native("{RELTIME}".to_string(), builtin_time),
        );

        defs.insert(
            "{SYM:attr?}".to_string(),
            Code::Native("{SYM:attr?}".to_string(), builtin_sym_attr),
        );
        defs.insert(
            "{SYM:prop}".to_string(),
            Code::Native("{SYM:prop}".to_string(), builtin_sym_prop),
        );

        defs.insert("{COND}".to_string(), Code::Native("{COND}".to_string(), builtin_cond));
        defs.insert("{IF}".to_string(), Code::Native("{IF}".to_string(), builtin_if));

        defs.insert(
            "{RAND:ChaCha20}".to_string(),
            Code::Native("{RAND:ChaCha20}".to_string(), builtin_rand),
        );

        return State {
            t0: Instant::now(),
            counter: (0usize, 0usize),
            location: Location::Source(0usize, 0usize),
            stack: Vec::with_capacity(64),
            symbols: sym::SymbolManager::new(),
            definitions: defs,
            program: Vec::with_capacity(64),
        };
    }

    fn with(s: State, tl: Vec<ParseNode>) -> State {
        return State {
            t0: s.t0,
            counter: s.counter,
            location: s.location,
            stack: s.stack,
            symbols: s.symbols,
            definitions: s.definitions,
            program: tl,
        };
    }
}

fn eval(mut state: State) -> Result<State> {
    let item = state.program.pop().ok_or(EvaluationError::StackUnderflow)?;

    match item.clone().kind {
        ParseKind::Block(_) => {
            state.location = item.clone().location;
            state.stack.push(item);
        }
        ParseKind::FloatValue(_) => {
            state.location = item.clone().location;
            state.stack.push(item);
        }
        ParseKind::IntegerValue(_) => {
            state.location = item.clone().location;
            state.stack.push(item);
        }
        ParseKind::StringValue(_) => {
            state.location = item.clone().location;
            state.stack.push(item);
        }
        ParseKind::Symbol(_) => {
            state.location = item.clone().location;
            state.stack.push(item);
        }
        ParseKind::WordRef(word) => {
            match state.definitions.get(&word).ok_or(EvaluationError::UndefinedWord {
                word: word,
                location: item.location,
            })? {
                Code::Native(_, f) => {
                    state = f(state)?;
                }
                Code::Program(ps) => {
                    let mut prog = ps.clone();
                    prog.reverse();
                    state.program.append(prog.as_mut());
                }
            }
        }
    }

    state.counter.1 += 1;
    return Ok(state);
}
