//! Error types for rust-jq

use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum JqError {
    #[error("Parse error: {0}")]
    Parse(String),

    #[error("Type error: {0}")]
    Type(String),

    #[error("Index out of bounds: {index} (length {length})")]
    IndexOutOfBounds { index: i64, length: usize },

    #[error("Cannot index {value_type} with {index_type}")]
    InvalidIndex {
        value_type: String,
        index_type: String,
    },

    #[error("Cannot iterate over {0}")]
    NotIterable(String),

    #[error("Object key must be a string, got {0}")]
    InvalidKey(String),

    #[error("Undefined variable: ${0}")]
    UndefinedVariable(String),

    #[error("Undefined function: {0}")]
    UndefinedFunction(String),

    #[error("Division by zero")]
    DivisionByZero,

    #[error("Invalid regex: {0}")]
    InvalidRegex(String),

    #[error("{0}")]
    Custom(String),
}

pub type Result<T> = std::result::Result<T, JqError>;
