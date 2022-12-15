//! A simple implementation of pratt parser for simple expressions.

use core::fmt;
use std::{io, iter::Peekable, slice::Iter};

enum Token {
    Atomic(String),
    Operator(char),
}

fn tokenize(source: &str) -> Result<Vec<Token>, String> {
    let chars = source.chars().collect::<Vec<char>>();
    let mut i = 0;
    let mut tokens: Vec<Token> = Vec::new();

    while i < chars.len() {
        let c = chars[i];
        match c {
            '0'..='9' | 'a'..='z' | 'A'..='Z' => {
                let mut s = String::new();
                s.push(c);
                i += 1;
                while i < chars.len() {
                    let c = chars[i];
                    if c.is_alphanumeric() {
                        s.push(c);
                        i += 1;
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Atomic(s));
            }
            '+' | '-' | '*' | '/' | '(' | ')' | '^' => {
                tokens.push(Token::Operator(c));
                i += 1;
            }

            ' ' => {
                i += 1;
            }
            '\n' | '\r' => {
                break;
            }
            _ => {
                return Err(format!("Unexpected character: {}", c));
            }
        }
    }

    return Ok(tokens);
}

#[derive(Debug, Clone, PartialEq)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Pow => write!(f, "^"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum UnaryOp {
    /// Unary positive
    Pos,
    /// Unary negation
    Neg,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Pos => write!(f, "+"),
            UnaryOp::Neg => write!(f, "-"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Atomic(String),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Atomic(ident) => write!(f, "{}", ident),
            Expr::Unary(op, rhs) => write!(f, "({}{})", op, rhs),
            Expr::Binary(op, lhs, rhs) => write!(f, "({}{}{})", lhs, op, rhs),
        }
    }
}

impl UnaryOp {
    fn from_char(c: char) -> Option<UnaryOp> {
        match c {
            '+' => Some(UnaryOp::Pos),
            '-' => Some(UnaryOp::Neg),
            _ => None,
        }
    }
    fn precedence(&self) -> u8 {
        match self {
            UnaryOp::Neg | UnaryOp::Pos => 15,
        }
    }
}

impl BinOp {
    fn from_char(c: char) -> Option<BinOp> {
        match c {
            '+' => Some(BinOp::Add),
            '-' => Some(BinOp::Sub),
            '*' => Some(BinOp::Mul),
            '/' => Some(BinOp::Div),
            '^' => Some(BinOp::Pow),
            _ => None,
        }
    }

    fn associativity(&self) -> i8 {
        match self {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => 1,
            BinOp::Pow => -1,
        }
    }

    fn precedence(&self) -> u8 {
        match self {
            BinOp::Add | BinOp::Sub => 5,
            BinOp::Mul | BinOp::Div => 10,
            BinOp::Pow => 15,
        }
    }
}

fn parse_expr(iter: &mut Peekable<Iter<Token>>, ctx_precedence: u8) -> Result<Box<Expr>, String> {
    let prefix = iter
        .next()
        .ok_or("Unexpected end of expression".to_string())?;

    let mut lhs: Box<Expr> = match prefix {
        Token::Atomic(ident) => Box::new(Expr::Atomic(ident.clone())),
        Token::Operator('(') => {
            let expr = parse_expr(iter, 0)?;
            if let Some(Token::Operator(')')) = iter.next() {
                expr
            } else {
                return Err("Expected ')'".to_string());
            }
        }
        Token::Operator(ch) => {
            let unary_op = UnaryOp::from_char(*ch).ok_or(format!("Unexpected operator: {}", ch))?;
            let rhs = parse_expr(iter, unary_op.precedence())?;
            Box::new(Expr::Unary(unary_op, rhs))
        }
    };

    loop {
        let op = match iter.peek() {
            Some(Token::Operator(')')) => break,
            Some(Token::Operator(op)) => {
                BinOp::from_char(*op).ok_or(format!("Unexpected operator in loop: {}", op))?
            }
            _ => break,
        };
        let precedence = op.precedence();

        if precedence < ctx_precedence {
            break;
        }

        iter.next(); // consume operator

        let rhs = parse_expr(iter, (precedence as i8 + op.associativity()) as u8)?;

        lhs = Box::new(Expr::Binary(op, lhs, rhs));
    }

    Ok(lhs)
}

fn main() {
    let mut source = String::new();
    io::stdin()
        .read_line(&mut source)
        .expect("Failed to read line");
    let tokens = tokenize(source.as_str()).unwrap();
    let mut iter = tokens.iter().peekable();
    let expr = parse_expr(&mut iter, 0).expect("Failed to parse expression");
    println!("{:#?}", expr);
}

#[cfg(test)]
mod tests {
    use super::*;
    
    // just compare format string to test

    #[test]
    fn test_parse_expr1() {
        let tokens = tokenize("1 + 2 * 3").unwrap();
        let mut iter = tokens.iter().peekable();
        let expr = parse_expr(&mut iter, 0).unwrap();
        assert_eq!(format!("{}", expr), "(1+(2*3))");
    }

    #[test]
    fn test_parse_expr2() {
        let tokens = tokenize("1 + 2 * 3 ^ 4").unwrap();
        let mut iter = tokens.iter().peekable();
        let expr = parse_expr(&mut iter, 0).unwrap();
        assert_eq!(format!("{}", expr), "(1+(2*(3^4)))");
    }

    #[test]
    fn test_parse_expr3() {
        let tokens = tokenize("+ ++ aaaa ^ 5 ^ (1 + 2 / 3 * b^ a ^ c) * 5 / 3 + c").unwrap();
        let mut iter = tokens.iter().peekable();
        let expr = parse_expr(&mut iter, 0).unwrap();
        assert_eq!(format!("{}", expr), "((((+(+(+(aaaa^(5^(1+((2/3)*(b^(a^c)))))))))*5)/3)+c)");
    }

}
