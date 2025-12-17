//! Token definitions for jq lexer

/// Token types for jq
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Literals
    Null,
    True,
    False,
    Number(String),
    String(String),

    // Identifiers
    Ident(String),
    Var(String),   // $name
    Field(String), // .name

    // Keywords
    If,
    Then,
    Else,
    Elif,
    End,
    As,
    Def,
    Reduce,
    Foreach,
    Try,
    Catch,
    And,
    Or,
    Not,
    Import,
    Include,
    Module,
    Label,
    Break,

    // Operators
    Dot,          // .
    DotDot,       // ..
    Pipe,         // |
    Semicolon,    // ;
    Comma,        // ,
    Colon,        // :
    Question,     // ?
    At,           // @

    // Brackets
    LParen,   // (
    RParen,   // )
    LBracket, // [
    RBracket, // ]
    LBrace,   // {
    RBrace,   // }

    // Arithmetic
    Plus,     // +
    Minus,    // -
    Star,     // *
    Slash,    // /
    Percent,  // %

    // Comparison
    Eq,       // ==
    Ne,       // !=
    Lt,       // <
    Le,       // <=
    Gt,       // >
    Ge,       // >=

    // Assignment
    Assign,   // =
    PipeEq,   // |=
    PlusEq,   // +=
    MinusEq,  // -=
    StarEq,   // *=
    SlashEq,  // /=
    PercentEq,// %=
    AltEq,    // //=

    // Other
    Alt,      // //
    Optional, // ?//

    // End of input
    Eof,
}

impl Token {
    pub fn is_keyword(s: &str) -> Option<Token> {
        match s {
            "null" => Some(Token::Null),
            "true" => Some(Token::True),
            "false" => Some(Token::False),
            "if" => Some(Token::If),
            "then" => Some(Token::Then),
            "else" => Some(Token::Else),
            "elif" => Some(Token::Elif),
            "end" => Some(Token::End),
            "as" => Some(Token::As),
            "def" => Some(Token::Def),
            "reduce" => Some(Token::Reduce),
            "foreach" => Some(Token::Foreach),
            "try" => Some(Token::Try),
            "catch" => Some(Token::Catch),
            "and" => Some(Token::And),
            "or" => Some(Token::Or),
            "not" => Some(Token::Not),
            "import" => Some(Token::Import),
            "include" => Some(Token::Include),
            "module" => Some(Token::Module),
            "label" => Some(Token::Label),
            "break" => Some(Token::Break),
            _ => None,
        }
    }
}
