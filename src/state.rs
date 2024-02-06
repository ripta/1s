use crate::lexer::Location;
use crate::parser::{ParseKind, ParseNode};
use crate::sym::SymbolManager;
use crate::{lexer, parser, sym};
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha20Rng;
use snafu::{ResultExt, Snafu};
use std::collections::HashMap;
use std::ops::Index;
use std::path::Path;
use std::result;
use std::time::Instant;
use string_interner::DefaultSymbol;

pub fn run_string(mut state: State, content: String, trace_exec: bool) -> Result<State> {
    let tokens = lexer::lex(content)?;
    if trace_exec {
        println!("LEXED-TOKENS {}", tokens.len());
    }

    let pt = parser::parse(&mut state.symbols, tokens);
    return run_program(state, pt.top_level, trace_exec);
}

pub fn run_program(state: State, mut prog: Vec<ParseNode>, trace_exec: bool) -> Result<State> {
    prog.reverse();
    return run_state(State::with(state, prog), trace_exec);
}

pub fn run_state(mut s: State, trace_exec: bool) -> Result<State> {
    if trace_exec {
        println!("Pre-eval {:?}", s.counter);
        println!("  Stack: {:?}", s.stack);
        println!("  Program: {:?}", s.program);

        println!("  Definitions: {:?}", dump_definitions(s.clone().definitions));
        println!();
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
            println!();
        }
    }

    return Ok(s);
}

pub fn dump_definitions(definitions: HashMap<String, Code>) -> Vec<String> {
    let mut def_names: Vec<String> = Vec::with_capacity(definitions.len());
    for def_name in definitions.keys() {
        def_names.push(def_name.to_string());
    }
    def_names.sort();
    return def_names;
}

pub type Result<T> = result::Result<T, EvaluationError>;

#[derive(Debug, Snafu)]
pub enum EvaluationError {
    #[snafu(display("guard violation: {reason}"))]
    GuardViolation { reason: String },
    #[snafu(display("invalid command line flag"))]
    InvalidFlag { source: pico_args::Error },
    #[snafu(display("cannot load file '{filename}'"))]
    FileLoad { source: std::io::Error, filename: String },
    #[snafu(display("lexing error: {source}"))]
    LexingError { source: lexer::LexerError },
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
    UndefinedWord { word: String, location: lexer::Location },
}

impl From<lexer::LexerError> for EvaluationError {
    fn from(value: lexer::LexerError) -> Self {
        return EvaluationError::LexingError { source: value };
    }
}

pub fn read_file<P: AsRef<Path>>(path: P) -> Result<String> {
    return std::fs::read_to_string(path.as_ref()).context(FileLoadSnafu {
        filename: path.as_ref().to_str().unwrap().to_string(),
    });
}

fn get_block(s: ParseNode) -> Result<Vec<ParseNode>> {
    match s.kind {
        ParseKind::Block(b) => Ok(b.to_vec()),
        otherwise => Err(EvaluationError::IncompatibleValue {
            expected: "quoted block".to_string(),
            found: otherwise.to_string(),
        }),
    }
}

fn get_float(s: &ParseNode) -> Result<f64> {
    match s.clone().kind {
        ParseKind::FloatValue(f) => Ok(f),
        otherwise => Err(EvaluationError::IncompatibleValue {
            expected: "float".to_string(),
            found: otherwise.to_string(),
        }),
    }
}

fn get_integer(s: &ParseNode) -> Result<i64> {
    match s.clone().kind {
        ParseKind::IntegerValue(i) => Ok(i),
        otherwise => Err(EvaluationError::IncompatibleValue {
            expected: "integer".to_string(),
            found: otherwise.to_string(),
        }),
    }
}

fn get_string(s: &ParseNode) -> Result<String> {
    match s.clone().kind {
        ParseKind::StringValue(s) => Ok(s),
        otherwise => Err(EvaluationError::IncompatibleValue {
            expected: "string".to_string(),
            found: otherwise.to_string(),
        }),
    }
}

fn get_sym(s: &ParseNode) -> Result<DefaultSymbol> {
    match s.clone().kind {
        ParseKind::Symbol(s) => Ok(s),
        otherwise => Err(EvaluationError::IncompatibleValue {
            expected: "symbol".to_string(),
            found: otherwise.to_string(),
        }),
    }
}

fn get_word(s: ParseNode) -> Result<String> {
    match s.kind {
        ParseKind::WordRef(w) => Ok(w.to_string()),
        otherwise => Err(EvaluationError::IncompatibleValue {
            expected: "word".to_string(),
            found: otherwise.to_string(),
        }),
    }
}

macro_rules! checked_pop {
    ( $e:expr ) => {
        $e.stack.pop().ok_or(EvaluationError::ReasonedStackUnderflow {
            reason: "safe-popping from stack".to_string(),
        })?
    };
}

fn builtin_add(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    return match node.kind {
        ParseKind::Block(b) => {
            let vals = b.iter().map(get_integer).collect::<Result<Vec<i64>>>()?;
            let sum = vals.iter().sum::<i64>();
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
    if !a.is_empty() {
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
        let check = get_block(cond.pop().ok_or(EvaluationError::ReasonedStackUnderflow {
            reason: "looking for a CHECK block in {COND}".to_string(),
        })?)?;
        let mut branch = get_block(cond.pop().ok_or(EvaluationError::ReasonedStackUnderflow {
            reason: "looking for a BRANCH block in {COND}".to_string(),
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
                .ok_or(EvaluationError::ReasonedStackUnderflow {
                    reason: "no elements to divide".to_string(),
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

fn builtin_load(mut state: State) -> Result<State> {
    let filename = get_string(&checked_pop!(state))?;

    let content = read_file(filename.clone())?;

    let size = content.len();
    let t0 = Instant::now();

    let tokens = lexer::lex(content)?;
    let pt = parser::parse(&mut state.symbols, tokens);
    let mut prog = pt.top_level;

    prog.reverse();
    state.program.append(prog.as_mut());

    if state.symbols.has_trace() {
        let dur = t0.elapsed().as_micros();
        println!("{filename:?} {{LOAD}} [ #size {size} #runtime_µs {dur} ]");
    }

    return Ok(state);
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
                .ok_or(EvaluationError::ReasonedStackUnderflow {
                    reason: "no elements to mod".to_string(),
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
            let sum = vals.iter().product::<i64>();
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

fn builtin_nth_get(mut state: State) -> Result<State> {
    let node = get_block(checked_pop!(state))?;
    let index = get_integer(&checked_pop!(state))? as usize;

    if node.is_empty() {
        return Err(EvaluationError::GuardViolation {
            reason: "block is empty".to_string(),
        });
    }

    if index >= node.len() {
        return Err(EvaluationError::GuardViolation {
            reason: "index out of bounds".to_string(),
        });
    }

    state.stack.push(node.index(index).clone());
    return Ok(state);
}

// TODO(ripta): nth= usually leaves nothing on the stack, but modifies the seq
fn builtin_nth_set(mut state: State) -> Result<State> {
    let mut node = get_block(checked_pop!(state))?;
    let index = get_integer(&checked_pop!(state))? as usize;
    let element = checked_pop!(state);

    if node.is_empty() {
        return Err(EvaluationError::GuardViolation {
            reason: "block is empty".to_string(),
        });
    }

    if index >= node.len() {
        return Err(EvaluationError::GuardViolation {
            reason: "index out of bounds".to_string(),
        });
    }

    node.remove(index);
    node.insert(index, element);
    state.stack.push(ParseNode {
        kind: ParseKind::Block(node),
        location: Location::Evaluation(state.counter),
    });

    return Ok(state);
}

fn builtin_stack_empty(state: State) -> Result<State> {
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
                .ok_or(EvaluationError::ReasonedStackUnderflow {
                    reason: "no elements to subtract".to_string(),
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

#[derive(Debug, Clone)]
pub enum Code {
    Native(String, fn(State) -> Result<State>),
    Program(Vec<ParseNode>),
}

#[derive(Debug, Clone)]
pub struct State {
    pub t0: Instant,
    pub counter: (usize, usize),
    pub location: lexer::Location,

    pub stack: Vec<ParseNode>,
    pub program: Vec<ParseNode>,
    pub symbols: sym::SymbolManager,
    pub definitions: HashMap<String, Code>,
}

impl State {
    pub fn new(sm: SymbolManager) -> State {
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
        defs.insert("{LOAD}".to_string(), Code::Native("{LOAD}".to_string(), builtin_load));
        defs.insert("{NTH}".to_string(), Code::Native("{NTH}".to_string(), builtin_nth_get));
        defs.insert(
            "{NTH=}".to_string(),
            Code::Native("{NTH=}".to_string(), builtin_nth_set),
        );
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
            location: lexer::Location::Source(0usize, 0usize),
            stack: Vec::with_capacity(64),
            symbols: sm,
            definitions: defs,
            program: Vec::with_capacity(64),
        };
    }

    pub fn with(s: State, tl: Vec<ParseNode>) -> State {
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

pub fn eval(mut state: State) -> Result<State> {
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
                word,
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
