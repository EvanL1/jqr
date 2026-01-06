//! Builtin functions for jq
//!
//! Most builtins are implemented in interpreter/mod.rs
//! This module provides additional utilities and documentation.

// Re-export the builtin function evaluator
pub use crate::interpreter::eval;

/// List of all builtin function names
pub const BUILTINS: &[&str] = &[
    // Type functions
    "type",
    "null",
    "true",
    "false",
    "isnull",
    "isnumber",
    "isstring",
    "isarray",
    "isobject",
    "isboolean",
    // Length and size
    "length",
    // Keys and values
    "keys",
    "keys_unsorted",
    "values",
    // Array functions
    "first",
    "last",
    "reverse",
    "sort",
    "sort_by",
    "unique",
    "unique_by",
    "flatten",
    "add",
    "min",
    "max",
    "group_by",
    "nth",
    "range",
    // Object functions
    "has",
    "in",
    "to_entries",
    "from_entries",
    "with_entries",
    // String functions
    "tostring",
    "tonumber",
    "ascii_downcase",
    "ascii_upcase",
    "ltrimstr",
    "rtrimstr",
    "split",
    "join",
    "contains",
    "inside",
    "startswith",
    "endswith",
    "test",
    "match",
    "capture",
    "scan",
    "sub",
    "gsub",
    // Math functions
    "floor",
    "ceil",
    "round",
    "sqrt",
    "fabs",
    "log",
    "log2",
    "log10",
    "exp",
    "exp2",
    "exp10",
    "pow",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    // Control flow
    "select",
    "map",
    "map_values",
    "any",
    "all",
    "recurse",
    "walk",
    "empty",
    "error",
    "not",
    // Path functions
    "path",
    "getpath",
    "setpath",
    "delpaths",
    "leaf_paths",
    // Misc
    "env",
    "now",
    "debug",
    "input",
    "inputs",
];

/// Check if a name is a builtin function
#[inline]
pub fn is_builtin(name: &str) -> bool {
    BUILTINS.contains(&name)
}
