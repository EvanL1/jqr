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
    let json: serde_json::Value =
        serde_json::from_str(input).map_err(|e| JqError::Parse(format!("Invalid JSON: {}", e)))?;
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

    // =========== Phase 5: Additional comprehensive tests ===========

    // Math function tests
    #[test]
    fn test_floor() {
        let results = run("floor", "3.7").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(3))]);
    }

    #[test]
    fn test_ceil() {
        let results = run("ceil", "3.2").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(4))]);
    }

    #[test]
    fn test_round() {
        let results = run("round", "3.5").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(4))]);
    }

    #[test]
    fn test_sqrt() {
        let results = run("sqrt", "16").unwrap();
        match &results[0] {
            Value::Number(Number::Float(f)) => assert!((f - 4.0).abs() < 0.0001),
            _ => panic!("Expected float"),
        }
    }

    #[test]
    fn test_fabs() {
        let results = run("fabs", "-5.5").unwrap();
        match &results[0] {
            Value::Number(Number::Float(f)) => assert!((f - 5.5).abs() < 0.0001),
            _ => panic!("Expected float"),
        }
    }

    // String function tests
    #[test]
    fn test_ltrimstr() {
        let results = run(r#"ltrimstr("hello ")"#, r#""hello world""#).unwrap();
        assert_eq!(results, vec![Value::string("world")]);
    }

    #[test]
    fn test_rtrimstr() {
        let results = run(r#"rtrimstr(" world")"#, r#""hello world""#).unwrap();
        assert_eq!(results, vec![Value::string("hello")]);
    }

    #[test]
    fn test_startswith() {
        let results = run(r#"startswith("hello")"#, r#""hello world""#).unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_endswith() {
        let results = run(r#"endswith("world")"#, r#""hello world""#).unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_ascii_downcase() {
        let results = run("ascii_downcase", r#""HELLO""#).unwrap();
        assert_eq!(results, vec![Value::string("hello")]);
    }

    #[test]
    fn test_ascii_upcase() {
        let results = run("ascii_upcase", r#""hello""#).unwrap();
        assert_eq!(results, vec![Value::string("HELLO")]);
    }

    #[test]
    fn test_tostring() {
        let results = run("tostring", "42").unwrap();
        assert_eq!(results, vec![Value::string("42")]);
    }

    #[test]
    fn test_tonumber() {
        let results = run("tonumber", r#""42""#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(42))]);
    }

    // Array function tests
    #[test]
    fn test_min() {
        let results = run("min", r#"[3, 1, 2]"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(1))]);
    }

    #[test]
    fn test_max() {
        let results = run("max", r#"[3, 1, 2]"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(3))]);
    }

    #[test]
    fn test_add() {
        let results = run("add", r#"[1, 2, 3]"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(6))]);
    }

    #[test]
    fn test_add_strings() {
        let results = run("add", r#"["a", "b", "c"]"#).unwrap();
        assert_eq!(results, vec![Value::string("abc")]);
    }

    #[test]
    fn test_flatten() {
        let results = run("flatten", r#"[[1, 2], [3, [4, 5]]]"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 5);
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_flatten_depth() {
        let results = run("flatten(1)", r#"[[1, [2]], [3]]"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 3);
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_first() {
        let results = run("first", r#"[1, 2, 3]"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(1))]);
    }

    #[test]
    fn test_last() {
        let results = run("last", r#"[1, 2, 3]"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(3))]);
    }

    #[test]
    fn test_nth() {
        let results = run("nth(1)", r#"[10, 20, 30]"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(20))]);
    }

    #[test]
    fn test_group_by() {
        let results = run(
            "group_by(.type)",
            r#"[{"type":"a"},{"type":"b"},{"type":"a"}]"#,
        )
        .unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 2);
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_sort_by() {
        let results = run("sort_by(.x)", r#"[{"x":2},{"x":1},{"x":3}]"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                if let Value::Object(obj) = &arr[0] {
                    assert_eq!(obj.get("x"), Some(&Value::Number(Number::Int(1))));
                }
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_unique_by() {
        let results = run("unique_by(.x)", r#"[{"x":1,"y":1},{"x":1,"y":2},{"x":2}]"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 2);
            }
            _ => panic!("Expected array"),
        }
    }

    // Object function tests
    #[test]
    fn test_has() {
        let results = run(r#"has("a")"#, r#"{"a": 1}"#).unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);

        let results = run(r#"has("b")"#, r#"{"a": 1}"#).unwrap();
        assert_eq!(results, vec![Value::Bool(false)]);
    }

    #[test]
    fn test_in() {
        let results = run(r#"in({"a": 1})"#, r#""a""#).unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_contains() {
        let results = run(r#"contains("wor")"#, r#""hello world""#).unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_inside() {
        let results = run(r#"inside("hello world")"#, r#""wor""#).unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_from_entries() {
        let results = run(
            "from_entries",
            r#"[{"key":"a","value":1},{"key":"b","value":2}]"#,
        )
        .unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Object(obj) => {
                assert_eq!(obj.get("a"), Some(&Value::Number(Number::Int(1))));
                assert_eq!(obj.get("b"), Some(&Value::Number(Number::Int(2))));
            }
            _ => panic!("Expected object"),
        }
    }

    // Control flow tests
    #[test]
    fn test_empty() {
        let results = run("empty", "null").unwrap();
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_not() {
        let results = run("not .", "true").unwrap();
        assert_eq!(results, vec![Value::Bool(false)]);

        let results = run("not .", "false").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_any() {
        let results = run("any", r#"[false, true, false]"#).unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_all() {
        let results = run("all", r#"[true, true, true]"#).unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);

        let results = run("all", r#"[true, false, true]"#).unwrap();
        assert_eq!(results, vec![Value::Bool(false)]);
    }

    // Edge cases
    #[test]
    fn test_null_field_access() {
        let results = run(".foo", "null").unwrap();
        assert_eq!(results, vec![Value::Null]);
    }

    #[test]
    fn test_null_index() {
        let results = run(".[0]", "null").unwrap();
        assert_eq!(results, vec![Value::Null]);
    }

    #[test]
    fn test_negative_index() {
        let results = run(".[-1]", r#"[1, 2, 3]"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(3))]);
    }

    #[test]
    fn test_slice() {
        let results = run(".[1:3]", r#"[0, 1, 2, 3, 4]"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 2);
                assert_eq!(arr[0], Value::Number(Number::Int(1)));
                assert_eq!(arr[1], Value::Number(Number::Int(2)));
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_string_slice() {
        let results = run(".[1:4]", r#""hello""#).unwrap();
        assert_eq!(results, vec![Value::string("ell")]);
    }

    #[test]
    fn test_integer_arithmetic() {
        // Test that integer arithmetic is preserved
        let results = run(". + 1", "5").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(6))]);

        let results = run(". - 1", "5").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(4))]);

        let results = run(". * 2", "5").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(10))]);
    }

    #[test]
    fn test_modulo() {
        let results = run(". % 3", "7").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(1))]);
    }

    #[test]
    fn test_comparison_operators() {
        let results = run(". == 5", "5").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);

        let results = run(". != 5", "5").unwrap();
        assert_eq!(results, vec![Value::Bool(false)]);

        let results = run(". < 10", "5").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);

        let results = run(". <= 5", "5").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);

        let results = run(". > 3", "5").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);

        let results = run(". >= 5", "5").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_logical_operators() {
        let results = run("true and true", "null").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);

        let results = run("true and false", "null").unwrap();
        assert_eq!(results, vec![Value::Bool(false)]);

        let results = run("true or false", "null").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_map_values() {
        let results = run("map_values(. + 1)", r#"{"a": 1, "b": 2}"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Object(obj) => {
                assert_eq!(obj.get("a"), Some(&Value::Number(Number::Int(2))));
                assert_eq!(obj.get("b"), Some(&Value::Number(Number::Int(3))));
            }
            _ => panic!("Expected object"),
        }
    }

    #[test]
    fn test_values() {
        let results = run("values", r#"{"a": 1, "b": 2}"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 2);
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_type_selectors() {
        let results = run("nulls", "null").unwrap();
        assert_eq!(results, vec![Value::Null]);

        let results = run("booleans", "true").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);

        let results = run("numbers", "42").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(42))]);

        let results = run("strings", r#""hello""#).unwrap();
        assert_eq!(results, vec![Value::string("hello")]);

        let results = run("arrays", "[]").unwrap();
        assert_eq!(results.len(), 1);

        let results = run("objects", "{}").unwrap();
        assert_eq!(results.len(), 1);
    }

    #[test]
    fn test_recurse() {
        let results = run("recurse(.a?) | .b?", r#"{"a": {"a": {"b": 1}}, "b": 0}"#).unwrap();
        // Should find all .b values in the recursive structure
        assert!(!results.is_empty());
    }

    #[test]
    fn test_range_with_step() {
        let results = run("range(0; 10; 2)", "null").unwrap();
        assert_eq!(results.len(), 5);
        assert_eq!(results[0], Value::Number(Number::Int(0)));
        assert_eq!(results[1], Value::Number(Number::Int(2)));
        assert_eq!(results[4], Value::Number(Number::Int(8)));
    }

    #[test]
    fn test_object_merge() {
        let results = run(". + {c: 3}", r#"{"a": 1, "b": 2}"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Object(obj) => {
                assert_eq!(obj.len(), 3);
            }
            _ => panic!("Expected object"),
        }
    }

    #[test]
    fn test_array_concatenation() {
        let results = run(". + [4, 5]", r#"[1, 2, 3]"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 5);
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_string_concatenation() {
        let results = run(r#". + " world""#, r#""hello""#).unwrap();
        assert_eq!(results, vec![Value::string("hello world")]);
    }

    #[test]
    fn test_string_multiplication() {
        let results = run(". * 3", r#""ab""#).unwrap();
        assert_eq!(results, vec![Value::string("ababab")]);
    }

    #[test]
    fn test_string_division() {
        let results = run(r#". / ",""#, r#""a,b,c""#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 3);
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_array_subtraction() {
        let results = run(". - [2, 4]", r#"[1, 2, 3, 4, 5]"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 3);
                assert_eq!(arr[0], Value::Number(Number::Int(1)));
                assert_eq!(arr[1], Value::Number(Number::Int(3)));
                assert_eq!(arr[2], Value::Number(Number::Int(5)));
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_optional_field() {
        let results = run(".foo?", r#"{"bar": 1}"#).unwrap();
        assert_eq!(results, vec![Value::Null]);
    }

    #[test]
    fn test_optional_iterator() {
        let results = run(".[]?", "null").unwrap();
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_comma_operator() {
        let results = run(".a, .b", r#"{"a": 1, "b": 2}"#).unwrap();
        assert_eq!(results.len(), 2);
        assert_eq!(results[0], Value::Number(Number::Int(1)));
        assert_eq!(results[1], Value::Number(Number::Int(2)));
    }

    // =========== Phase 5: Round 2 - 50+ additional tests ===========

    // Edge case tests for numbers
    #[test]
    fn test_large_integer() {
        let results = run(".", "9223372036854775807").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(i64::MAX))]);
    }

    #[test]
    fn test_negative_integer() {
        let results = run(". * -1", "42").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(-42))]);
    }

    #[test]
    fn test_float_precision() {
        let results = run(". + 0.1", "0.2").unwrap();
        match &results[0] {
            Value::Number(Number::Float(f)) => assert!((f - 0.3).abs() < 0.0001),
            _ => panic!("Expected float"),
        }
    }

    // More string tests
    #[test]
    fn test_empty_string() {
        let results = run("length", r#""""#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(0))]);
    }

    #[test]
    fn test_unicode_string() {
        let results = run("length", r#""你好世界""#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(4))]);
    }

    #[test]
    fn test_string_special_chars() {
        // Test that we can work with strings containing special chars
        let results = run(r#"split(",")"#, r#""a,b,c""#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => assert_eq!(arr.len(), 3),
            _ => panic!("Expected array"),
        }
    }

    // Array edge cases
    #[test]
    fn test_empty_array() {
        let results = run("length", "[]").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(0))]);
    }

    #[test]
    fn test_nested_array() {
        let results = run(".[][] ", "[[1,2],[3,4]]").unwrap();
        assert_eq!(results.len(), 4);
    }

    #[test]
    fn test_array_index_out_of_bounds() {
        let results = run(".[100]", "[1,2,3]").unwrap();
        assert_eq!(results, vec![Value::Null]);
    }

    #[test]
    fn test_array_negative_slice() {
        let results = run(".[-2:]", "[1,2,3,4,5]").unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => assert_eq!(arr.len(), 2),
            _ => panic!("Expected array"),
        }
    }

    // Object edge cases
    #[test]
    fn test_empty_object() {
        let results = run("keys", "{}").unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => assert_eq!(arr.len(), 0),
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_nested_object_access() {
        let results = run(".a.b.c", r#"{"a":{"b":{"c":42}}}"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(42))]);
    }

    #[test]
    fn test_object_with_array_value() {
        let results = run(".items[0]", r#"{"items":[1,2,3]}"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(1))]);
    }

    // Pipe and composition tests
    #[test]
    fn test_multiple_pipes() {
        let results = run(".a | .b | .c", r#"{"a":{"b":{"c":1}}}"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(1))]);
    }

    #[test]
    fn test_pipe_with_filter() {
        let results = run(".[] | select(. > 2)", "[1,2,3,4]").unwrap();
        assert_eq!(results.len(), 2);
    }

    // Conditional tests
    #[test]
    fn test_if_without_else() {
        let results = run("if . > 0 then . end", "5").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(5))]);
    }

    #[test]
    fn test_nested_if() {
        let results = run(
            "if . > 0 then (if . > 5 then \"big\" else \"small\" end) else \"negative\" end",
            "3",
        )
        .unwrap();
        assert_eq!(results, vec![Value::string("small")]);
    }

    // Error handling tests
    #[test]
    fn test_try_with_error() {
        let results = run("try error catch \"caught\"", "null").unwrap();
        assert_eq!(results, vec![Value::string("caught")]);
    }

    #[test]
    fn test_optional_on_error() {
        let results = run("(.foo.bar.baz)?", "{}").unwrap();
        // Optional should not error
        assert!(results.len() <= 1);
    }

    // More builtin function tests
    #[test]
    fn test_nested_field_access() {
        let results = run(".a.b", r#"{"a":{"b":1}}"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(1))]);
    }

    #[test]
    fn test_has_nested() {
        let results = run(".a | has(\"b\")", r#"{"a":{"b":1}}"#).unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_min_empty() {
        let results = run("min", "[]").unwrap();
        assert_eq!(results, vec![Value::Null]);
    }

    #[test]
    fn test_max_empty() {
        let results = run("max", "[]").unwrap();
        assert_eq!(results, vec![Value::Null]);
    }

    #[test]
    fn test_add_empty() {
        let results = run("add", "[]").unwrap();
        assert_eq!(results, vec![Value::Null]);
    }

    #[test]
    fn test_indices() {
        let results = run("keys", "[10,20,30]").unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 3);
                assert_eq!(arr[0], Value::Number(Number::Int(0)));
            }
            _ => panic!("Expected array"),
        }
    }

    // Variable and function tests
    #[test]
    fn test_variable_binding() {
        // Test simple variable binding with reduce
        let results = run("reduce .[] as $x (0; . + $x)", "[1,2,3]").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(6))]);
    }

    #[test]
    fn test_function_simple() {
        // Test simple function without parameters
        let results = run("def double: . * 2; 5 | double", "null").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(10))]);
    }

    #[test]
    fn test_recursive_function() {
        let results = run(
            "def fact: if . <= 1 then 1 else . * ((. - 1) | fact) end; 5 | fact",
            "null",
        )
        .unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(120))]);
    }

    // Type checking tests
    #[test]
    fn test_isnull() {
        let results = run("isnull", "null").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);

        let results = run("isnull", "1").unwrap();
        assert_eq!(results, vec![Value::Bool(false)]);
    }

    #[test]
    fn test_isstring() {
        let results = run("isstring", r#""hello""#).unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_isarray() {
        let results = run("isarray", "[]").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_isobject() {
        let results = run("isobject", "{}").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_isnumber() {
        let results = run("isnumber", "42").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_isboolean() {
        let results = run("isboolean", "true").unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    // More complex expressions
    #[test]
    fn test_object_construction_computed_key() {
        let results = run(r#"{(.k): .v}"#, r#"{"k":"foo","v":42}"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Object(obj) => {
                assert_eq!(obj.get("foo"), Some(&Value::Number(Number::Int(42))));
            }
            _ => panic!("Expected object"),
        }
    }

    #[test]
    fn test_array_collect_from_iterator() {
        let results = run("[.[] | . * 2]", "[1,2,3]").unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => {
                assert_eq!(arr.len(), 3);
                assert_eq!(arr[0], Value::Number(Number::Int(2)));
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_with_entries() {
        // Test to_entries and from_entries roundtrip
        let results = run("to_entries | from_entries", r#"{"a":1,"b":2}"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Object(obj) => {
                assert_eq!(obj.get("a"), Some(&Value::Number(Number::Int(1))));
                assert_eq!(obj.get("b"), Some(&Value::Number(Number::Int(2))));
            }
            _ => panic!("Expected object"),
        }
    }

    // Regex edge cases
    #[test]
    fn test_test_case_insensitive() {
        let results = run(r#"test("HELLO"; "i")"#, r#""hello world""#).unwrap();
        assert_eq!(results, vec![Value::Bool(true)]);
    }

    #[test]
    fn test_gsub_global() {
        let results = run(r#"gsub("a"; "b")"#, r#""banana""#).unwrap();
        assert_eq!(results, vec![Value::string("bbnbnb")]);
    }

    #[test]
    fn test_splits_regex() {
        let results = run(r#"splits("\\s+")"#, r#""a  b   c""#).unwrap();
        assert_eq!(results.len(), 3);
    }

    // Numeric edge cases
    #[test]
    fn test_division_result() {
        let results = run(". / 2", "5").unwrap();
        match &results[0] {
            Value::Number(Number::Float(f)) => assert!((f - 2.5).abs() < 0.0001),
            _ => panic!("Expected float"),
        }
    }

    #[test]
    fn test_integer_modulo() {
        let results = run(". % 3", "10").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(1))]);
    }

    // Scalars and iterables
    #[test]
    fn test_scalars() {
        let results = run("scalars", "42").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(42))]);

        let results = run("scalars", "[]").unwrap();
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_iterables() {
        let results = run("iterables", "[]").unwrap();
        assert_eq!(results.len(), 1);

        let results = run("iterables", "42").unwrap();
        assert_eq!(results.len(), 0);
    }

    // Alternative operator
    #[test]
    fn test_alternative_with_null() {
        let results = run("null // 42", "null").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(42))]);
    }

    #[test]
    fn test_alternative_with_false() {
        let results = run("false // 42", "null").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(42))]);
    }

    #[test]
    fn test_alternative_chain() {
        let results = run(".a // .b // .c", r#"{"c":3}"#).unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(3))]);
    }

    // More flatten tests
    #[test]
    fn test_flatten_empty() {
        let results = run("flatten", "[]").unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(arr) => assert_eq!(arr.len(), 0),
            _ => panic!("Expected array"),
        }
    }

    // Debug/env tests (basic)
    #[test]
    fn test_now() {
        let results = run("now", "null").unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Number(Number::Float(f)) => assert!(*f > 0.0),
            _ => panic!("Expected float"),
        }
    }

    // Update and assignment (if supported)
    #[test]
    fn test_object_update() {
        let results = run(". + {c:3}", r#"{"a":1,"b":2}"#).unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Object(obj) => assert_eq!(obj.len(), 3),
            _ => panic!("Expected object"),
        }
    }

    // Complex nested operations
    #[test]
    fn test_nested_map() {
        let results = run("map(map(. + 1))", "[[1,2],[3,4]]").unwrap();
        assert_eq!(results.len(), 1);
        match &results[0] {
            Value::Array(outer) => {
                assert_eq!(outer.len(), 2);
                match &outer[0] {
                    Value::Array(inner) => {
                        assert_eq!(inner[0], Value::Number(Number::Int(2)));
                    }
                    _ => panic!("Expected inner array"),
                }
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn test_reduce_sum() {
        let results = run("reduce .[] as $x (0; . + $x)", "[1,2,3,4,5]").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(15))]);
    }

    #[test]
    fn test_reduce_product() {
        let results = run("reduce .[] as $x (1; . * $x)", "[1,2,3,4]").unwrap();
        assert_eq!(results, vec![Value::Number(Number::Int(24))]);
    }

    // Edge case: empty input handling
    #[test]
    fn test_first_empty_array() {
        let result = run("first", "[]");
        assert!(result.is_err());
    }

    #[test]
    fn test_last_empty_array() {
        let result = run("last", "[]");
        assert!(result.is_err());
    }
}
