mod sym;

use rustyline::DefaultEditor;
use snafu::prelude::*;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::iter::Iterator;
use std::slice::Iter;
use std::{fmt, result};
use string_interner::DefaultSymbol;

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
    trace_exec: bool,
    files: Vec<String>,
}

fn parse_args() -> Result<Flags> {
    let mut args = pico_args::Arguments::from_env();
    // let filenames: Vec<String> = env::args().skip(1).collect();
    let mut flags = Flags {
        interactive: args.contains("-i"),
        trace_exec: args.contains("-t"),
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
    for filename in flags.files {
        let content = std::fs::read_to_string(filename.clone()).context(FileLoadSnafu {
            filename: filename.clone(),
        })?;

        let size = content.len();
        println!("READ {filename} SIZE {size}");

        state = run_string(state, content, flags.trace_exec)?;
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
                            println!("Stack: ");
                            for st in &ns.stack {
                                println!("  {}", st);
                            }

                            state = ns;
                        }
                        Err(EvaluationError::UndefinedWord { word, location: _ }) => {
                            println!("Error: word '{word}' is undefined");
                            println!("Defined words: {:?}", dump_definitions(state.clone().definitions));
                        }
                        Err(e) => {
                            println!("Error: {}", e);
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
        $e.stack.pop().ok_or(EvaluationError::StackUnderflow)?
    };
}

fn builtin_add(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    state = match node.kind {
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
    }?;

    return Ok(state);
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
    state = match node.kind {
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
    }?;

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
    state = match node.kind {
        ParseKind::Block(b) => {
            let vals = b.iter().map(get_integer).collect::<Result<Vec<i64>>>()?;
            let sum = vals.iter().fold(0, |acc, v| acc / v);
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(sum),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::FloatValue(a) => {
            let b = get_float(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::FloatValue(a / b),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(a / b),
                location: node.location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{/}".to_string(),
            value: node.clone(),
        }),
    }?;

    return Ok(state);
}

fn builtin_exactly_equal(mut state: crate::State) -> Result<crate::State> {
    let a = checked_pop!(state);
    let b = checked_pop!(state);
    if a == b {
        return Ok(state);
    }
    return Err(EvaluationError::GuardViolation {
        reason: "top two items on stack do not match exactly".to_string(),
    });
}

fn builtin_floor(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    state = match node.kind {
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
    }?;

    return Ok(state);
}

fn builtin_mod(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    state = match node.kind {
        ParseKind::Block(b) => {
            let vals = b.iter().map(get_integer).collect::<Result<Vec<i64>>>()?;
            let sum = vals.iter().fold(0, |acc, v| acc % v);
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(sum),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::FloatValue(a) => {
            let b = get_float(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::FloatValue(a % b),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(a % b),
                location: node.location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{%}".to_string(),
            value: node.clone(),
        }),
    }?;

    return Ok(state);
}

fn builtin_mul(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    state = match node.kind {
        ParseKind::Block(b) => {
            let vals = b.iter().map(get_integer).collect::<Result<Vec<i64>>>()?;
            let sum = vals.iter().fold(0, |acc, v| acc * v);
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
    }?;

    return Ok(state);
}

fn builtin_stack_empty(state: crate::State) -> Result<crate::State> {
    if state.stack.is_empty() {
        return Ok(state);
    }
    return Err(EvaluationError::GuardViolation {
        reason: "stack is not empty".to_string(),
    });
}

fn builtin_sub(mut state: State) -> Result<State> {
    let node = checked_pop!(state);
    state = match node.kind {
        ParseKind::Block(b) => {
            let vals = b.iter().map(get_integer).collect::<Result<Vec<i64>>>()?;
            let sum = vals.iter().fold(0, |acc, v| acc - v);
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(sum),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::FloatValue(a) => {
            let b = get_float(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::FloatValue(a - b),
                location: node.location,
            });
            Ok(state)
        }

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&checked_pop!(state))?;
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(a - b),
                location: node.location,
            });
            Ok(state)
        }

        _ => Err(EvaluationError::CannotOperate {
            op: "{-}".to_string(),
            value: node.clone(),
        }),
    }?;

    return Ok(state);
}

fn builtin_k(mut state: State) -> Result<State> {
    let mut a = get_block(checked_pop!(state))?;
    checked_pop!(state);

    a.reverse();
    state.program.append(&mut a);

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
    counter: (usize, usize),
    location: Location,

    stack: Vec<ParseNode>,
    program: Vec<ParseNode>,
    symbols: sym::SymbolManager,
    definitions: HashMap<String, Code>,
}

impl State {
    fn new() -> State {
        let mut symbols = sym::SymbolManager::new();

        let mut defs = HashMap::with_capacity(64);

        defs.insert("{:}".to_string(), Code::Native("{:}".to_string(), builtin_define));
        defs.insert("{ø}".to_string(), Code::Native("{ø}".to_string(), builtin_stack_empty));
        defs.insert(
            "{=:=}".to_string(),
            Code::Native("{=:=}".to_string(), builtin_exactly_equal),
        );

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

        defs.insert("{⌈}".to_string(), Code::Native("{⌈}".to_string(), builtin_ceil));
        defs.insert("{⌊}".to_string(), Code::Native("{⌊}".to_string(), builtin_floor));

        defs.insert("√2".to_string(), Code::Native("√2".to_string(), builtin_const_sqrt2));
        defs.insert("π".to_string(), Code::Native("π".to_string(), builtin_const_pi));

        return State {
            counter: (0usize, 0usize),
            location: Location::Source(0usize, 0usize),
            stack: Vec::with_capacity(64),
            symbols: symbols,
            definitions: defs,
            program: Vec::with_capacity(64),
        };
    }

    fn with(s: State, tl: Vec<ParseNode>) -> State {
        return State {
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
