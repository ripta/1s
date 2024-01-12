use rustyline::DefaultEditor;
use snafu::prelude::*;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::iter::Iterator;
use std::slice::Iter;
use std::{fmt, result};

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
                            println!("Stack: {:?}", ns.stack);
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

fn run_string(state: State, content: String, trace_exec: bool) -> Result<State> {
    let tokens = lex(content)?;
    if trace_exec {
        println!("LEXED-TOKENS {}", tokens.len());
    }

    let pt = parse(tokens)?;

    let mut prog = pt.top_level.clone();
    prog.reverse();

    let mut s = State::with(state, prog);
    // println!("Pre-eval: {:?}", state);
    while !s.program.is_empty() {
        s = eval(s.clone())?;

        if trace_exec {
            println!("Step {:?}", s.counter);
            println!("  Stack: {:?}", s.stack);
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

fn builtin_add(mut state: State) -> Result<State> {
    let node = state.stack.pop().ok_or(EvaluationError::StackUnderflow)?;
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

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&state.stack.pop().ok_or(EvaluationError::StackUnderflow)?)?;
            state.stack.push(ParseNode {
                kind: ParseKind::IntegerValue(a + b),
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

// -- [B] [A] cons == [[B] A]
fn builtin_cons(mut state: State) -> Result<State> {
    let a = get_block(state.stack.pop().ok_or(EvaluationError::StackUnderflow)?)?;
    let b = state.stack.pop().ok_or(EvaluationError::StackUnderflow)?;

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
    let mut syms = get_block(state.stack.pop().ok_or(EvaluationError::StackUnderflow)?)?;
    let body = get_block(state.stack.pop().ok_or(EvaluationError::StackUnderflow)?)?;

    let sym = get_word(syms.pop().ok_or(EvaluationError::StackUnderflow)?)?;
    state.definitions.insert(sym, Code::Program(body));
    return Ok(state);
}

fn builtin_mul(mut state: State) -> Result<State> {
    let node = state.stack.pop().ok_or(EvaluationError::StackUnderflow)?;
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

        ParseKind::IntegerValue(a) => {
            let b = get_integer(&state.stack.pop().ok_or(EvaluationError::StackUnderflow)?)?;
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

fn builtin_k(mut state: State) -> Result<State> {
    let mut a = get_block(state.stack.pop().ok_or(EvaluationError::StackUnderflow)?)?;
    let _b = get_block(state.stack.pop().ok_or(EvaluationError::StackUnderflow)?)?;

    state.program.append(&mut a);

    return Ok(state);
}

fn builtin_sip(mut state: State) -> Result<State> {
    let a = get_block(state.stack.pop().ok_or(EvaluationError::StackUnderflow)?)?;
    let b = state.stack.pop().ok_or(EvaluationError::StackUnderflow)?;

    // TODO(ripta): execute A
    let mut res = a;

    state.stack.push(b.clone());
    state.stack.append(&mut res);
    state.stack.push(b.clone());

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
    if word.starts_with('-') {
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
    if word.starts_with('-') {
        return match word.chars().nth(1) {
            Some(c) if c.is_ascii_digit() => true,
            _ => false,
        };
    }
    return false;
}

fn lex(content: String) -> Result<Vec<Token>> {
    // let mut builtins: Vec<(String, fn(Vec<Token>) -> Result<()>)> = Vec::new();
    // builtins.push(("1s".to_string(), builtin_1s));
    // builtins.push(("+".to_string(), builtin_add));
    // builtins.push(("[".to_string(), builtin_noop));
    // builtins.push(("]".to_string(), builtin_noop));

    // let mut atoms: Vec<String> = Vec::new();
    // let mut atoms: Vec<String> = ["1s", "[", "]"].iter().map(|a| a.to_string()).collect();

    // let mut atoms: Vec<String> = builtins.iter().map(|(n, _f)| n.to_string()).collect();
    // let mut atoms_map: HashMap<String, usize> = HashMap::new();
    // for (i, atom) in atoms.iter().enumerate() {
    //     atoms_map.insert(atom.to_string(), i);
    // }

    // arena.define("1s", builtin_1s);
    // arena.define("+", builtin_add);
    // arena.define("[", builtin_noop);
    // arena.define("]", builtin_noop);

    let lines = content
        .split("\n")
        .map(|line| line.split_whitespace().map(str::to_string));

    let mut buf = String::new();
    let mut in_comment = false;
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

            // TODO(ripta): stop compressing away multiple whitespace characters in comments
            if word.chars().all(|c| c == '-') {
                in_comment = true;
                comment_word_num = word_num;
                buf.push_str(&word);
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

            // match arena.atoms_map.get(word) {
            //     Some(t) => {
            //         // println!("L{line_num} W{word_num}: {word}");
            //         arena.tokens.push(Token::TWord(*t));
            //     }
            //     None => {
            //         // println!("Hi!");
            //         // arena.atoms.push(word.to_string());
            //         // arena.atoms_map.insert(word.to_string(), arena.atoms.len());
            //         arena.tokens.push(Token::TWord(arena.atoms.len() - 1));
            //     }
            // }
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
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
enum ParseKind {
    // Binding(String, Vec<Semantic>),
    Block(Vec<ParseNode>),
    // BoolValue(bool),
    // Compiled(
    //     String,
    //     fn(Vec<Semantic>) -> Result<Vec<Semantic>>,
    // ),
    FloatValue(f64),
    IntegerValue(i64),
    StringValue(String),
    // WordInternal(String),
    WordRef(String),
}

impl Display for ParseKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn parse(tokens: Vec<Token>) -> Result<ParseTree> {
    let top = parse_(&mut tokens.iter());
    return Ok(ParseTree { top_level: top });
}

/// parse_ is the recursive version of parse, during which the (linear) stream of tokens is converted into one or more
/// levels of nested blocks.
///
/// TODO(ripta): interned strings
/// TODO(ripta): recursive descent
fn parse_(tokens: &mut Iter<Token>) -> Vec<ParseNode> {
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
                        kind: ParseKind::StringValue(s.to_string()),
                        location: token.location,
                    }),
                    TokenKind::Word(w) => match w.as_str() {
                        "[" => {
                            let block = parse_(tokens);
                            span.push(ParseNode {
                                kind: ParseKind::Block(block),
                                location: token.location,
                            });
                        }
                        "]" => break,
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
    definitions: HashMap<String, Code>,
}

impl State {
    fn new() -> State {
        let mut defs = HashMap::with_capacity(64);

        defs.insert("{:}".to_string(), Code::Native("{:}".to_string(), builtin_define));
        defs.insert("{CONS}".to_string(), Code::Native("{CONS}".to_string(), builtin_cons));
        defs.insert("{K}".to_string(), Code::Native("{K}".to_string(), builtin_k));
        defs.insert("{SIP}".to_string(), Code::Native("{SIP}".to_string(), builtin_sip));

        defs.insert("{+}".to_string(), Code::Native("{+}".to_string(), builtin_add));
        defs.insert("{*}".to_string(), Code::Native("{*}".to_string(), builtin_mul));

        return State {
            counter: (0usize, 0usize),
            location: Location::Source(0usize, 0usize),
            stack: Vec::with_capacity(64),
            definitions: defs,
            program: Vec::with_capacity(64),
        };
    }

    fn with(s: State, tl: Vec<ParseNode>) -> State {
        return State {
            counter: s.counter,
            location: s.location,
            stack: s.stack,
            definitions: s.definitions,
            program: tl,
        };
    }
}

// struct Arena {
//     atoms: Vec<String>,
//     atoms_map: HashMap<String, usize>,
// }

// impl Arena {
//     fn new() -> Arena {
//         return Arena {
//             atoms: Vec::with_capacity(64),
//             atoms_map: HashMap::with_capacity(64),
//         };
//     }
//
//     // fn define(&mut self, word: &str, f: fn(Vec<Token>) -> Result<()>) {
//     //     match self.atoms_map.get(word) {
//     //         Some(t) => {
//     //             // println!("L{line_num} W{word_num}: {word}");
//     //             self.tokens.push(Token::Word(*t));
//     //         }
//     //         None => {
//     //             // println!("Hi!");
//     //             self.atoms.push(word.to_string());
//     //             self.atoms_map.insert(word.to_string(), self.atoms.len());
//     //             self.tokens.push(Token::Word(self.atoms.len() - 1));
//     //         }
//     //     }
//     // }
// }

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
        ParseKind::WordRef(word) => {
            match state.definitions.get(&word).ok_or(EvaluationError::UndefinedWord {
                word: word,
                location: item.location,
            })? {
                Code::Program(ps) => {
                    let mut prog = ps.clone();
                    prog.reverse();
                    state.program.append(prog.as_mut());
                }
                Code::Native(_, f) => {
                    state = f(state)?;
                }
            }
        }
    }

    state.counter.1 += 1;
    return Ok(state);
}
