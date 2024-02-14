use crate::{lexer, sym};
use std::fmt;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::slice::Iter;
use string_interner::DefaultSymbol;

#[derive(Debug)]
pub struct ParseTree {
    pub top_level: Vec<ParseNode>,
}

#[derive(Debug, Clone, Hash)]
pub struct ParseNode {
    pub kind: ParseKind,
    pub location: lexer::Location,
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
pub enum ParseKind {
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

impl Hash for ParseKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self {
            ParseKind::Block(ps) => ps.hash(state),
            ParseKind::FloatValue(f) => f.to_bits().hash(state),
            ParseKind::IntegerValue(i) => i.hash(state),
            ParseKind::StringValue(s) => s.hash(state),
            ParseKind::Symbol(s) => s.hash(state),
            ParseKind::WordRef(w) => w.hash(state),
        }
    }
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

pub fn parse(symbols: &mut sym::SymbolManager, tokens: Vec<lexer::Token>) -> ParseTree {
    let top = parse_(symbols, &mut tokens.iter());
    return ParseTree { top_level: top };
}

/// parse_ is the recursive version of parse, during which the (linear) stream of tokens is converted into one or more
/// levels of nested blocks.
///
/// TODO(ripta): recursive descent
fn parse_(symbols: &mut sym::SymbolManager, tokens: &mut Iter<lexer::Token>) -> Vec<ParseNode> {
    let mut span: Vec<ParseNode> = Vec::with_capacity(8);

    loop {
        let token = tokens.next();
        match token {
            None => break,
            Some(token) => {
                let token = token.clone();
                match token.kind {
                    lexer::TokenKind::Comment(_) => {}
                    lexer::TokenKind::LiteralFloat(f) => span.push(ParseNode {
                        kind: ParseKind::FloatValue(f),
                        location: token.location,
                    }),
                    lexer::TokenKind::LiteralInteger(d) => span.push(ParseNode {
                        kind: ParseKind::IntegerValue(d),
                        location: token.location,
                    }),
                    lexer::TokenKind::LiteralString(s) => span.push(ParseNode {
                        kind: ParseKind::StringValue(s),
                        location: token.location,
                    }),
                    lexer::TokenKind::Word(w) => match w.as_str() {
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
