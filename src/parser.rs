//! Parser for jq expressions

pub use crate::ast::*;

use crate::error::{JqError, Result};
use crate::value::{Number, Value};
use crate::lexer::lex;
use crate::token::Token;

/// Parse a jq program from a string
pub fn parse(input: &str) -> Result<Expr> {
    let tokens = lex(input).map_err(JqError::Parse)?;
    let mut parser = Parser::new(tokens);
    parser.parse_expr()
}

/// Recursive descent parser for jq
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<Token> {
        if self.pos < self.tokens.len() {
            let token = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(token)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: &Token) -> Result<()> {
        match self.peek() {
            Some(t) if t == expected => {
                self.advance();
                Ok(())
            }
            Some(t) => Err(JqError::Parse(format!(
                "Expected {:?}, got {:?}",
                expected, t
            ))),
            None => Err(JqError::Parse(format!(
                "Expected {:?}, got end of input",
                expected
            ))),
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Parse a complete expression
    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_pipe()
    }

    /// Parse pipe expressions: expr | expr
    fn parse_pipe(&mut self) -> Result<Expr> {
        let mut left = self.parse_comma()?;
        while matches!(self.peek(), Some(Token::Pipe)) {
            self.advance();
            let right = self.parse_comma()?;
            left = Expr::Pipe(Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    /// Parse comma expressions: expr, expr
    fn parse_comma(&mut self) -> Result<Expr> {
        let mut left = self.parse_alternative()?;
        while matches!(self.peek(), Some(Token::Comma)) {
            self.advance();
            let right = self.parse_alternative()?;
            left = Expr::Comma(Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    /// Parse alternative: expr // expr
    fn parse_alternative(&mut self) -> Result<Expr> {
        let mut left = self.parse_or()?;
        while matches!(self.peek(), Some(Token::Alt)) {
            self.advance();
            let right = self.parse_or()?;
            left = Expr::BinOp(BinOp::Alt, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    /// Parse logical or: expr or expr
    fn parse_or(&mut self) -> Result<Expr> {
        let mut left = self.parse_and()?;
        while matches!(self.peek(), Some(Token::Or)) {
            self.advance();
            let right = self.parse_and()?;
            left = Expr::BinOp(BinOp::Or, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    /// Parse logical and: expr and expr
    fn parse_and(&mut self) -> Result<Expr> {
        let mut left = self.parse_not()?;
        while matches!(self.peek(), Some(Token::And)) {
            self.advance();
            let right = self.parse_not()?;
            left = Expr::BinOp(BinOp::And, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    /// Parse not: not expr
    fn parse_not(&mut self) -> Result<Expr> {
        if matches!(self.peek(), Some(Token::Not)) {
            self.advance();
            let expr = self.parse_not()?;
            Ok(Expr::UnaryOp(UnaryOp::Not, Box::new(expr)))
        } else {
            self.parse_comparison()
        }
    }

    /// Parse comparison: expr == expr, expr < expr, etc.
    fn parse_comparison(&mut self) -> Result<Expr> {
        let left = self.parse_additive()?;
        let op = match self.peek() {
            Some(Token::Eq) => Some(BinOp::Eq),
            Some(Token::Ne) => Some(BinOp::Ne),
            Some(Token::Lt) => Some(BinOp::Lt),
            Some(Token::Le) => Some(BinOp::Le),
            Some(Token::Gt) => Some(BinOp::Gt),
            Some(Token::Ge) => Some(BinOp::Ge),
            _ => None,
        };
        if let Some(op) = op {
            self.advance();
            let right = self.parse_additive()?;
            Ok(Expr::BinOp(op, Box::new(left), Box::new(right)))
        } else {
            Ok(left)
        }
    }

    /// Parse additive: expr + expr, expr - expr
    fn parse_additive(&mut self) -> Result<Expr> {
        let mut left = self.parse_multiplicative()?;
        loop {
            let op = match self.peek() {
                Some(Token::Plus) => BinOp::Add,
                Some(Token::Minus) => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            left = Expr::BinOp(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    /// Parse multiplicative: expr * expr, expr / expr, expr % expr
    fn parse_multiplicative(&mut self) -> Result<Expr> {
        let mut left = self.parse_unary()?;
        loop {
            let op = match self.peek() {
                Some(Token::Star) => BinOp::Mul,
                Some(Token::Slash) => BinOp::Div,
                Some(Token::Percent) => BinOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary()?;
            left = Expr::BinOp(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    /// Parse unary: -expr
    fn parse_unary(&mut self) -> Result<Expr> {
        if matches!(self.peek(), Some(Token::Minus)) {
            self.advance();
            let expr = self.parse_unary()?;
            Ok(Expr::UnaryOp(UnaryOp::Neg, Box::new(expr)))
        } else {
            self.parse_postfix()
        }
    }

    /// Parse postfix: expr?, expr[], expr.field
    fn parse_postfix(&mut self) -> Result<Expr> {
        let mut expr = self.parse_primary()?;
        loop {
            match self.peek() {
                Some(Token::Question) => {
                    self.advance();
                    expr = Expr::Optional(Box::new(expr));
                }
                Some(Token::LBracket) => {
                    self.advance();
                    if matches!(self.peek(), Some(Token::RBracket)) {
                        // .[]
                        self.advance();
                        expr = Expr::Pipe(Box::new(expr), Box::new(Expr::Iterator));
                    } else {
                        // .[expr] or .[start:end]
                        let index = self.parse_expr()?;
                        if matches!(self.peek(), Some(Token::Colon)) {
                            // Slice
                            self.advance();
                            let end = if matches!(self.peek(), Some(Token::RBracket)) {
                                None
                            } else {
                                Some(Box::new(self.parse_expr()?))
                            };
                            self.expect(&Token::RBracket)?;
                            expr = Expr::Pipe(
                                Box::new(expr),
                                Box::new(Expr::Slice {
                                    start: Some(Box::new(index)),
                                    end,
                                }),
                            );
                        } else {
                            self.expect(&Token::RBracket)?;
                            expr = Expr::Pipe(
                                Box::new(expr),
                                Box::new(Expr::Index(Box::new(index))),
                            );
                        }
                    }
                }
                Some(Token::Field(name)) => {
                    let name = name.clone();
                    self.advance();
                    expr = Expr::Pipe(Box::new(expr), Box::new(Expr::Field(name)));
                }
                Some(Token::Dot) => {
                    self.advance();
                    // Could be .field or just . (identity after pipe)
                    if let Some(Token::Ident(name)) = self.peek().cloned() {
                        self.advance();
                        expr = Expr::Pipe(Box::new(expr), Box::new(Expr::Field(name)));
                    } else {
                        // Just identity
                        expr = Expr::Pipe(Box::new(expr), Box::new(Expr::Identity));
                    }
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// Parse primary expressions
    fn parse_primary(&mut self) -> Result<Expr> {
        match self.peek().cloned() {
            // Recursive descent: ..
            Some(Token::DotDot) => {
                self.advance();
                Ok(Expr::RecursiveDescent)
            }

            // Identity
            Some(Token::Dot) => {
                self.advance();
                // Check for iterator or index
                match self.peek().cloned() {
                    Some(Token::LBracket) => {
                        self.advance();
                        if matches!(self.peek(), Some(Token::RBracket)) {
                            self.advance();
                            Ok(Expr::Iterator)
                        } else {
                            let index = self.parse_expr()?;
                            if matches!(self.peek(), Some(Token::Colon)) {
                                self.advance();
                                let end = if matches!(self.peek(), Some(Token::RBracket)) {
                                    None
                                } else {
                                    Some(Box::new(self.parse_expr()?))
                                };
                                self.expect(&Token::RBracket)?;
                                Ok(Expr::Slice {
                                    start: Some(Box::new(index)),
                                    end,
                                })
                            } else {
                                self.expect(&Token::RBracket)?;
                                Ok(Expr::Index(Box::new(index)))
                            }
                        }
                    }
                    _ => Ok(Expr::Identity),
                }
            }

            // Field access: .foo
            Some(Token::Field(name)) => {
                self.advance();
                Ok(Expr::Field(name))
            }

            // Literals
            Some(Token::Null) => {
                self.advance();
                Ok(Expr::Literal(Value::Null))
            }
            Some(Token::True) => {
                self.advance();
                Ok(Expr::Literal(Value::Bool(true)))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Expr::Literal(Value::Bool(false)))
            }
            Some(Token::Number(s)) => {
                self.advance();
                let num = if s.contains('.') || s.contains('e') || s.contains('E') {
                    s.parse::<f64>()
                        .map(Number::Float)
                        .map_err(|_| JqError::Parse(format!("Invalid number: {}", s)))?
                } else {
                    s.parse::<i64>()
                        .map(Number::Int)
                        .map_err(|_| JqError::Parse(format!("Invalid number: {}", s)))?
                };
                Ok(Expr::Literal(Value::Number(num)))
            }
            Some(Token::String(s)) => {
                self.advance();
                // TODO: Handle string interpolation
                Ok(Expr::Literal(Value::String(s)))
            }

            // Variable
            Some(Token::Var(name)) => {
                self.advance();
                Ok(Expr::Var(name))
            }

            // Parenthesized expression
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
            }

            // Array construction
            Some(Token::LBracket) => {
                self.advance();
                if matches!(self.peek(), Some(Token::RBracket)) {
                    self.advance();
                    Ok(Expr::Array(None))
                } else {
                    let expr = self.parse_expr()?;
                    self.expect(&Token::RBracket)?;
                    Ok(Expr::Array(Some(Box::new(expr))))
                }
            }

            // Object construction
            Some(Token::LBrace) => {
                self.advance();
                let mut entries = Vec::new();
                if !matches!(self.peek(), Some(Token::RBrace)) {
                    loop {
                        entries.push(self.parse_object_entry()?);
                        if matches!(self.peek(), Some(Token::Comma)) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                self.expect(&Token::RBrace)?;
                Ok(Expr::Object(entries))
            }

            // If-then-else
            Some(Token::If) => {
                self.advance();
                let cond = self.parse_expr()?;
                self.expect(&Token::Then)?;
                let then_branch = self.parse_expr()?;
                let else_branch = if matches!(self.peek(), Some(Token::Else)) {
                    self.advance();
                    Some(Box::new(self.parse_expr()?))
                } else {
                    None
                };
                self.expect(&Token::End)?;
                Ok(Expr::If {
                    cond: Box::new(cond),
                    then_branch: Box::new(then_branch),
                    else_branch,
                })
            }

            // Try-catch
            Some(Token::Try) => {
                self.advance();
                let try_expr = self.parse_expr()?;
                let catch_expr = if matches!(self.peek(), Some(Token::Catch)) {
                    self.advance();
                    Some(Box::new(self.parse_expr()?))
                } else {
                    None
                };
                Ok(Expr::TryCatch {
                    try_expr: Box::new(try_expr),
                    catch_expr,
                })
            }

            // Reduce
            Some(Token::Reduce) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(&Token::As)?;
                let var = match self.advance() {
                    Some(Token::Var(name)) => name,
                    _ => return Err(JqError::Parse("Expected variable after 'as'".to_string())),
                };
                self.expect(&Token::LParen)?;
                let init = self.parse_expr()?;
                self.expect(&Token::Semicolon)?;
                let update = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                Ok(Expr::Reduce {
                    expr: Box::new(expr),
                    var,
                    init: Box::new(init),
                    update: Box::new(update),
                })
            }

            // Function definition
            Some(Token::Def) => {
                self.advance();
                let name = match self.advance() {
                    Some(Token::Ident(name)) => name,
                    _ => return Err(JqError::Parse("Expected function name after 'def'".to_string())),
                };
                let params = if matches!(self.peek(), Some(Token::LParen)) {
                    self.advance();
                    let mut params = Vec::new();
                    if !matches!(self.peek(), Some(Token::RParen)) {
                        loop {
                            match self.advance() {
                                Some(Token::Ident(p)) => params.push(p),
                                Some(Token::Var(p)) => params.push(p),
                                _ => return Err(JqError::Parse("Expected parameter name".to_string())),
                            }
                            if matches!(self.peek(), Some(Token::Semicolon)) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(&Token::RParen)?;
                    params
                } else {
                    Vec::new()
                };
                self.expect(&Token::Colon)?;
                let body = self.parse_expr()?;
                self.expect(&Token::Semicolon)?;
                let rest = self.parse_expr()?;
                Ok(Expr::FuncDef {
                    def: FuncDef {
                        name,
                        params,
                        body: Box::new(body),
                    },
                    body: Box::new(rest),
                })
            }

            // Function call or identifier
            Some(Token::Ident(name)) => {
                self.advance();
                if matches!(self.peek(), Some(Token::LParen)) {
                    // Function call with arguments
                    self.advance();
                    let mut args = Vec::new();
                    if !matches!(self.peek(), Some(Token::RParen)) {
                        loop {
                            args.push(self.parse_expr()?);
                            if matches!(self.peek(), Some(Token::Semicolon)) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(&Token::RParen)?;
                    Ok(Expr::FuncCall { name, args })
                } else {
                    // Function call without arguments (like `length`, `keys`, etc.)
                    Ok(Expr::FuncCall { name, args: vec![] })
                }
            }

            Some(token) => Err(JqError::Parse(format!("Unexpected token: {:?}", token))),
            None => Err(JqError::Parse("Unexpected end of input".to_string())),
        }
    }

    /// Parse an object entry
    fn parse_object_entry(&mut self) -> Result<ObjectEntry> {
        match self.peek().cloned() {
            Some(Token::Ident(name)) => {
                self.advance();
                if matches!(self.peek(), Some(Token::Colon)) {
                    self.advance();
                    // Use parse_alternative instead of parse_expr to avoid consuming comma
                    let value = self.parse_alternative()?;
                    Ok(ObjectEntry::KeyValue {
                        key: ObjectKey::Ident(name),
                        value: Box::new(value),
                    })
                } else {
                    // Shorthand: {foo} means {foo: .foo}
                    Ok(ObjectEntry::Ident(name))
                }
            }
            Some(Token::String(s)) => {
                self.advance();
                self.expect(&Token::Colon)?;
                let value = self.parse_alternative()?;
                Ok(ObjectEntry::KeyValue {
                    key: ObjectKey::Literal(s),
                    value: Box::new(value),
                })
            }
            Some(Token::LParen) => {
                self.advance();
                let key_expr = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                self.expect(&Token::Colon)?;
                let value = self.parse_alternative()?;
                Ok(ObjectEntry::KeyValue {
                    key: ObjectKey::Computed(Box::new(key_expr)),
                    value: Box::new(value),
                })
            }
            _ => Err(JqError::Parse("Expected object key".to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_identity() {
        let expr = parse(".").unwrap();
        assert!(matches!(expr, Expr::Identity));
    }

    #[test]
    fn test_parse_field() {
        let expr = parse(".foo").unwrap();
        assert!(matches!(expr, Expr::Field(ref name) if name == "foo"));
    }

    #[test]
    fn test_parse_pipe() {
        let expr = parse(". | .foo").unwrap();
        assert!(matches!(expr, Expr::Pipe(_, _)));
    }

    #[test]
    fn test_parse_array() {
        let expr = parse("[1, 2, 3]").unwrap();
        assert!(matches!(expr, Expr::Array(_)));
    }

    #[test]
    fn test_parse_object() {
        let expr = parse("{foo: 1}").unwrap();
        assert!(matches!(expr, Expr::Object(_)));
    }

    #[test]
    fn test_parse_if() {
        let expr = parse("if . then 1 else 2 end").unwrap();
        assert!(matches!(expr, Expr::If { .. }));
    }
}
