//! jqr: A jq implementation in Rust
//!
//! This crate provides a Rust implementation of the jq JSON processor.
//!
//! # Example
//!
//! ```
//! use jqr::{run, Value};
//!
//! let input = r#"{"name": "Alice", "age": 30}"#;
//! let filter = ".name";
//!
//! let results = run(filter, input).unwrap();
//! assert_eq!(results, vec![Value::string("Alice")]);
//! ```

pub mod ast;
pub mod builtins;
pub mod error;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod value;

pub use error::{JqError, Result};
pub use interpreter::Context;
pub use parser::parse;
pub use value::{Number, Value};

/// Run a jq filter on JSON input
pub fn run(filter: &str, input: &str) -> Result<Vec<Value>> {
    let expr = parse(filter)?;
    let json: serde_json::Value = serde_json::from_str(input)
        .map_err(|e| JqError::Parse(format!("Invalid JSON: {}", e)))?;
    let input_value = Value::from_json(json);
    let ctx = Context::new();
    interpreter::eval(&expr, &ctx, input_value)
}

/// Run a jq filter on a Value
pub fn run_value(filter: &str, input: Value) -> Result<Vec<Value>> {
    let expr = parse(filter)?;
    let ctx = Context::new();
    interpreter::eval(&expr, &ctx, input)
}

/// Run a jq filter on multiple JSON inputs
pub fn run_multi(filter: &str, inputs: &[&str]) -> Result<Vec<Value>> {
    let expr = parse(filter)?;
    let ctx = Context::new();
    let mut results = Vec::new();
    for input in inputs {
        let json: serde_json::Value = serde_json::from_str(input)
            .map_err(|e| JqError::Parse(format!("Invalid JSON: {}", e)))?;
        let input_value = Value::from_json(json);
        results.extend(interpreter::eval(&expr, &ctx, input_value)?);
    }
    Ok(results)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identity() {
        let results = run(".", r#"{"a": 1}"#).unwrap();
        assert_eq!(results.len(), 1);
    }

    #[test]
    fn test_field_access() {
        let results = run(".name", r#"{"name": "Alice"}"#).unwrap();
        assert_eq!(results, vec![Value::string("Alice")]);
    }

    #[test]
    fn test_array_index() {
        let results = run(".[0]", r#"[1, 2, 3]"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(1))]);
    }

    #[test]
    fn test_pipe() {
        let results = run(".users | .[0] | .name", r#"{"users": [{"name": "Alice"}]}"#).unwrap();
        assert_eq!(results, vec![Value::string("Alice")]);
    }

    #[test]
    fn test_iterator() {
        let results = run(".[]", r#"[1, 2, 3]"#).unwrap();
        assert_eq!(results.len(), 3);
    }

    #[test]
    fn test_object_construction() {
        let results = run("{a: .x, b: .y}", r#"{"x": 1, "y": 2}"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Object(obj) => {
                assert_eq!(obj.get("a"), Some(&Value::Number(Number::Int(1))));
                assert_eq!(obj.get("b"), Some(&Value::Number(Number::Int(2))));
            }
            _ => panic!("Expected object"),
        }
    }

    #[test]
    fn test_array_construction() {
        let results = run("[.a, .b]", r#"{"a": 1, "b": 2}"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 2);
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_addition() {
        let results = run(".a + .b", r#"{"a": 1, "b": 2}"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Float(3.0))]);
    }

    #[test]
    fn test_select() {
        let results = run(".[] | select(. > 2)", r#"[1, 2, 3, 4]"#).unwrap();
        assert_eq!(results.len(), 2);
    }

    #[test]
    fn test_map() {
        let results = run("map(. * 2)", r#"[1, 2, 3]"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 3);
                assert_eq!(arr[0], Value::Number(Number::Float(2.0)));
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_reduce() {
        let results = run("reduce .[] as $x (0; . + $x)", r#"[1, 2, 3]"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Float(6.0))]);
    }

    #[test]
    fn test_if_then_else() {
        let results = run("if . > 5 then \"big\" else \"small\" end", "10").unwrap();
        assert_eq!(results, vec![Value::string("big")]);
    }

    #[test]
    fn test_keys() {
        let results = run("keys", r#"{"b": 2, "a": 1}"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 2);
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_length() {
        let results = run("length", r#"[1, 2, 3]"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(3))]);
    }

    #[test]
    fn test_type() {
        let results = run("type", r#"{"a": 1}"#).unwrap();
        assert_eq!(results, vec![Value::string("object")]);
    }

    #[test]
    fn test_sort() {
        let results = run("sort", r#"[3, 1, 2]"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr[0], Value::Number(Number::Int(1)));
                assert_eq!(arr[1], Value::Number(Number::Int(2)));
                assert_eq!(arr[2], Value::Number(Number::Int(3)));
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_reverse() {
        let results = run("reverse", r#"[1, 2, 3]"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr[0], Value::Number(Number::Int(3)));
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_unique() {
        let results = run("unique", r#"[1, 2, 1, 3, 2]"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 3);
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_to_entries() {
        let results = run("to_entries", r#"{"a": 1, "b": 2}"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 2);
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_split() {
        let results = run(r#"split(",")"#, r#""a,b,c""#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 3);
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_join() {
        let results = run(r#"join("-")"#, r#"["a", "b", "c"]"#).unwrap();
        assert_eq!(results, vec![Value::string("a-b-c")]);
    }

    #[test]
    fn test_try_catch() {
        let results = run("try .foo.bar catch \"error\"", r#"null"#).unwrap();
        // Since .foo on null returns null, and .bar on null returns null, no error
        assert_eq!(results, vec![Value::Null]);
    }

    #[test]
    fn test_alternative() {
        let results = run(".foo // \"default\"", r#"{"bar": 1}"#).unwrap();
        assert_eq!(results, vec![Value::string("default")]);
    }

    #[test]
    fn test_recursive_descent() {
        let results = run(".. | numbers", r#"{"a": 1, "b": {"c": 2}}"#).unwrap();
        assert_eq!(results.len(), 2);
    }

    #[test]
    fn test_range() {
        let results = run("range(3)", "null").unwrap();
        assert_eq!(results.len(), 3);
        assert_eq!(results[0], Value::Number(Number::Int(0)));
        assert_eq!(results[1], Value::Number(Number::Int(1)));
        assert_eq!(results[2], Value::Number(Number::Int(2)));
    }

    #[test]
    fn test_function_definition() {
        let results = run("def double: . * 2; [1, 2] | map(double)", "null").unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr[0], Value::Number(Number::Float(2.0)));
                assert_eq!(arr[1], Value::Number(Number::Float(4.0)));
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_regex_test() {
        let results = run(r#"test("hello")"#, r#""hello world""#).unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);

        let results = run(r#"test("^Hello"; "i")"#, r#""hello world""#).unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);

        let results = run(r#"test("xyz")"#, r#""hello world""#).unwrap();
        assert_eq!(results, vec![Value::Bool(false)]);
    }

    #[test]
    fn test_regex_match() {
        let results = run(r#"match("(\\w+)")"#, r#""hello world""#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Object(obj) => {
                assert_eq!(obj.get("string"), Some(&Value::string("hello")));
                assert_eq!(obj.get("offset"), Some(&Value::Number(Number::Int(0))));
            }
            _ => panic!("Expected object"),
        }
    }

    #[test]
    fn test_regex_sub() {
        let results = run(r#"sub("world"; "jq")"#, r#""hello world""#).unwrap();
        assert_eq!(results, vec![Value::string("hello jq")]);
    }

    #[test]
    fn test_regex_gsub() {
        let results = run(r#"gsub("o"; "0")"#, r#""hello world""#).unwrap();
        assert_eq!(results, vec![Value::string("hell0 w0rld")]);
    }

    #[test]
    fn test_regex_scan() {
        let results = run(r#"scan("\\w+")"#, r#""a b c""#).unwrap();
        assert_eq!(results.len(), 3);
        assert_eq!(results[0], Value::string("a"));
        assert_eq!(results[1], Value::string("b"));
        assert_eq!(results[2], Value::string("c"));
    }

    #[test]
    fn test_regex_capture() {
        let results = run(r#"capture("(?P<name>\\w+)")"#, r#""hello""#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Object(obj) => {
                assert_eq!(obj.get("name"), Some(&Value::string("hello")));
            }
            _ => panic!("Expected object"),
        }
    }
}
