use crate::state;
use crate::state::EvaluationError;
use snafu::{ResultExt, Snafu};
use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug, Snafu)]
pub enum LexerError {
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
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Location,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Comment(String),
    LiteralFloat(f64),
    LiteralInteger(i64),
    LiteralString(String),
    Word(String),
}

#[derive(Debug, Clone)]
pub enum Location {
    Static(&'static str),
    Evaluation(usize),
    Source(usize, usize),
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn is_float(word: &str) -> bool {
    if word.len() < 2 {
        return false;
    }
    // TODO(ripta): refine criterion, e.g., 2.1e5, .1, 1.2.3
    if word.chars().all(|c| matches!(c, '0'..='9' | '.')) {
        return true;
    }
    if word.starts_with('-') && word.len() > 2 {
        return matches!(word.chars().nth(1), Some(c) if c.is_ascii_digit());
    }
    return false;
}

fn is_integer(word: &str) -> bool {
    if word.is_empty() {
        return false;
    }
    if word.chars().all(|c| c.is_ascii_digit()) {
        return true;
    }
    if word.contains('.') {
        return false;
    }
    if word.starts_with('-') && word.len() > 1 {
        return matches!(word.chars().nth(1), Some(c) if c.is_ascii_digit());
    }
    return false;
}

pub fn lex(content: String) -> state::Result<Vec<Token>> {
    let lines = content
        .split('\n')
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
                buf.push(' ');
                buf.push_str(&word);
                continue;
            }

            if in_string {
                buf.push(' ');

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

            if word.len() >= 2 && word.starts_with('\"') && word.ends_with('\"') {
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

            if is_integer(&word) {
                // word.chars().all(|c| matches!(c, '0'..='9' | '-' | '_' | '.'))
                // i64::from_str();
                let v = word.parse().context(InvalidIntegerTokenSnafu { token: word })?;
                tokens.push(Token {
                    kind: TokenKind::LiteralInteger(v),
                    location: loc,
                });
                continue;
            }

            if is_float(&word) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_float() {
        assert_eq!(is_float(""), false);
        assert_eq!(is_float("foo"), false);

        assert_eq!(is_float("1"), false);
        assert_eq!(is_float("-1"), false);

        assert_eq!(is_float("1."), true);
        assert_eq!(is_float("-1."), true);

        assert_eq!(is_float("1.2"), true);
        assert_eq!(is_float("-1.2"), true);
    }

    #[test]
    fn test_is_integer() {
        assert_eq!(is_integer(""), false);
        assert_eq!(is_integer("foo"), false);

        assert_eq!(is_integer("1"), true);
        assert_eq!(is_integer("-1"), true);

        assert_eq!(is_integer("1."), false);
        assert_eq!(is_integer("-1."), false);

        assert_eq!(is_integer("1.2"), false);
        assert_eq!(is_integer("-1.2"), false);
    }
}
