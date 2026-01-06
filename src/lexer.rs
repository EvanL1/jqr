//! Lexer for jq expressions using nom

use crate::token::Token;

use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag, take_while, take_while1},
    character::complete::{char, digit1, multispace0, none_of, one_of},
    combinator::{opt, recognize, value},
    multi::many0,
    sequence::{delimited, pair, preceded},
    IResult, Parser,
};

/// Lex a complete jq program into tokens
#[inline]
pub fn lex(input: &str) -> Result<Vec<Token>, String> {
    let (remaining, tokens) = tokens(input).map_err(|e| format!("Lexer error: {:?}", e))?;
    if !remaining.trim().is_empty() {
        return Err(format!("Unexpected input: {}", remaining));
    }
    Ok(tokens)
}

fn tokens(input: &str) -> IResult<&str, Vec<Token>> {
    let (input, tokens) = many0(preceded(multispace0, token)).parse(input)?;
    let (input, _) = multispace0.parse(input)?;
    Ok((input, tokens))
}

fn token(input: &str) -> IResult<&str, Token> {
    alt((
        // Multi-character operators first (like ..)
        multi_char_ops,
        // Variables and fields (before single char ops to match .foo before .)
        variable,
        field_access,
        // Single character operators
        single_char_ops,
        // Literals
        string_literal,
        number_literal,
        // Identifiers (must be last)
        identifier,
    ))
    .parse(input)
}

fn multi_char_ops(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::DotDot, tag("..")),
        value(Token::AltEq, tag("//=")),
        value(Token::Alt, tag("//")),
        value(Token::Eq, tag("==")),
        value(Token::Ne, tag("!=")),
        value(Token::Le, tag("<=")),
        value(Token::Ge, tag(">=")),
        value(Token::PipeEq, tag("|=")),
        value(Token::PlusEq, tag("+=")),
        value(Token::MinusEq, tag("-=")),
        value(Token::StarEq, tag("*=")),
        value(Token::SlashEq, tag("/=")),
        value(Token::PercentEq, tag("%=")),
    ))
    .parse(input)
}

fn single_char_ops(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::Dot, char('.')),
        value(Token::Pipe, char('|')),
        value(Token::Semicolon, char(';')),
        value(Token::Comma, char(',')),
        value(Token::Colon, char(':')),
        value(Token::Question, char('?')),
        value(Token::At, char('@')),
        value(Token::LParen, char('(')),
        value(Token::RParen, char(')')),
        value(Token::LBracket, char('[')),
        value(Token::RBracket, char(']')),
        value(Token::LBrace, char('{')),
        value(Token::RBrace, char('}')),
        value(Token::Plus, char('+')),
        value(Token::Minus, char('-')),
        value(Token::Star, char('*')),
        value(Token::Slash, char('/')),
        value(Token::Percent, char('%')),
        value(Token::Lt, char('<')),
        value(Token::Gt, char('>')),
        value(Token::Assign, char('=')),
    ))
    .parse(input)
}

fn string_literal(input: &str) -> IResult<&str, Token> {
    let (input, s) = delimited(
        char('"'),
        escaped_transform(
            none_of("\\\""),
            '\\',
            alt((
                value('\\', char('\\')),
                value('"', char('"')),
                value('\n', char('n')),
                value('\r', char('r')),
                value('\t', char('t')),
                value('/', char('/')),
            )),
        ),
        char('"'),
    )
    .parse(input)?;
    Ok((input, Token::String(s)))
}

#[inline]
fn number_literal(input: &str) -> IResult<&str, Token> {
    let (input, num) = recognize((
        opt(char('-')),
        alt((
            recognize(pair(char('0'), opt(pair(char('.'), digit1)))),
            recognize((
                take_while1(|c: char| c.is_ascii_digit() && c != '0'),
                take_while(|c: char| c.is_ascii_digit()),
                opt(pair(char('.'), digit1)),
            )),
            recognize(pair(digit1, opt(pair(char('.'), digit1)))),
        )),
        opt((one_of("eE"), opt(one_of("+-")), digit1)),
    ))
    .parse(input)?;
    Ok((input, Token::Number(num.into())))
}

#[inline]
fn variable(input: &str) -> IResult<&str, Token> {
    let (input, _) = char('$').parse(input)?;
    let (input, name) = identifier_str(input)?;
    Ok((input, Token::Var(name.into())))
}

#[inline]
fn field_access(input: &str) -> IResult<&str, Token> {
    let (input, _) = char('.').parse(input)?;
    let (input, name) = identifier_str(input)?;
    Ok((input, Token::Field(name.into())))
}

#[inline]
fn identifier(input: &str) -> IResult<&str, Token> {
    let (input, name) = identifier_str(input)?;
    if let Some(kw) = Token::is_keyword(name) {
        Ok((input, kw))
    } else {
        Ok((input, Token::Ident(name.into())))
    }
}

#[inline]
fn identifier_str(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        take_while1(|c: char| c.is_alphabetic() || c == '_'),
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    ))
    .parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_identity() {
        let tokens = lex(".").unwrap();
        assert_eq!(tokens, vec![Token::Dot]);
    }

    #[test]
    fn test_lex_field() {
        let tokens = lex(".foo").unwrap();
        assert_eq!(tokens, vec![Token::Field("foo".to_string())]);
    }

    #[test]
    fn test_lex_pipe() {
        let tokens = lex(". | .foo").unwrap();
        assert_eq!(
            tokens,
            vec![Token::Dot, Token::Pipe, Token::Field("foo".to_string())]
        );
    }

    #[test]
    fn test_lex_number() {
        let tokens = lex("42").unwrap();
        assert_eq!(tokens, vec![Token::Number("42".to_string())]);
    }

    #[test]
    fn test_lex_string() {
        let tokens = lex("\"hello\"").unwrap();
        assert_eq!(tokens, vec![Token::String("hello".to_string())]);
    }

    #[test]
    fn test_lex_keywords() {
        let tokens = lex("if then else end").unwrap();
        assert_eq!(
            tokens,
            vec![Token::If, Token::Then, Token::Else, Token::End]
        );
    }
}
