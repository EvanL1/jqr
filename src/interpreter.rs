//! Interpreter for jq expressions

pub use crate::value::{Number, Value};

use crate::ast::*;
use crate::error::{JqError, Result};
use indexmap::IndexMap;
use regex::Regex;
use std::collections::HashMap;

/// Execution context for jq expressions
#[derive(Clone)]
pub struct Context {
    /// Variable bindings
    vars: HashMap<String, Value>,
    /// Function definitions
    funcs: HashMap<String, FuncDef>,
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn with_var(&self, name: String, value: Value) -> Self {
        let mut ctx = self.clone();
        ctx.vars.insert(name, value);
        ctx
    }

    pub fn get_var(&self, name: &str) -> Option<&Value> {
        self.vars.get(name)
    }

    pub fn define_func(&mut self, def: FuncDef) {
        self.funcs.insert(def.name.clone(), def);
    }

    pub fn get_func(&self, name: &str) -> Option<&FuncDef> {
        self.funcs.get(name)
    }
}

/// Evaluate a jq expression
pub fn eval(expr: &Expr, ctx: &Context, input: Value) -> Result<Vec<Value>> {
    match expr {
        Expr::Identity => Ok(vec![input]),

        Expr::RecursiveDescent => {
            let mut results = vec![input.clone()];
            collect_recursive(&input, &mut results);
            Ok(results)
        }

        Expr::Literal(v) => Ok(vec![v.clone()]),

        Expr::Field(name) => match &input {
            Value::Object(obj) => Ok(vec![obj.get(name).cloned().unwrap_or(Value::Null)]),
            Value::Null => Ok(vec![Value::Null]),
            _ => Err(JqError::Type(format!(
                "Cannot index {} with string \"{}\"",
                input.type_name(),
                name
            ))),
        },

        Expr::OptionalField(name) => match &input {
            Value::Object(obj) => Ok(vec![obj.get(name).cloned().unwrap_or(Value::Null)]),
            _ => Ok(vec![]),
        },

        Expr::Index(idx_expr) => {
            let indices = eval(idx_expr, ctx, input.clone())?;
            let mut results = Vec::new();
            for idx in indices {
                results.push(input.index(&idx)?);
            }
            Ok(results)
        }

        Expr::OptionalIndex(idx_expr) => {
            let indices = eval(idx_expr, ctx, input.clone())?;
            let mut results = Vec::new();
            for idx in indices {
                if let Ok(v) = input.index(&idx) {
                    results.push(v);
                }
            }
            Ok(results)
        }

        Expr::Slice { start, end } => match &input {
            Value::Array(arr) => {
                let len = arr.len() as i64;
                let start_idx = if let Some(s) = start {
                    let vals = eval(s, ctx, input.clone())?;
                    match vals.first() {
                        Some(Value::Number(n)) => {
                            let i = n.as_i64().unwrap_or(0);
                            if i < 0 {
                                (len + i).max(0) as usize
                            } else {
                                i as usize
                            }
                        }
                        _ => 0,
                    }
                } else {
                    0
                };
                let end_idx = if let Some(e) = end {
                    let vals = eval(e, ctx, input.clone())?;
                    match vals.first() {
                        Some(Value::Number(n)) => {
                            let i = n.as_i64().unwrap_or(len);
                            if i < 0 {
                                (len + i).max(0) as usize
                            } else {
                                (i as usize).min(arr.len())
                            }
                        }
                        _ => arr.len(),
                    }
                } else {
                    arr.len()
                };
                if start_idx >= arr.len() || start_idx >= end_idx {
                    Ok(vec![Value::array(vec![])])
                } else {
                    Ok(vec![Value::array(arr[start_idx..end_idx].to_vec())])
                }
            }
            Value::String(s) => {
                let chars: Vec<char> = s.chars().collect();
                let len = chars.len() as i64;
                let start_idx = if let Some(st) = start {
                    let vals = eval(st, ctx, input.clone())?;
                    match vals.first() {
                        Some(Value::Number(n)) => {
                            let i = n.as_i64().unwrap_or(0);
                            if i < 0 {
                                (len + i).max(0) as usize
                            } else {
                                i as usize
                            }
                        }
                        _ => 0,
                    }
                } else {
                    0
                };
                let end_idx = if let Some(e) = end {
                    let vals = eval(e, ctx, input.clone())?;
                    match vals.first() {
                        Some(Value::Number(n)) => {
                            let i = n.as_i64().unwrap_or(len);
                            if i < 0 {
                                (len + i).max(0) as usize
                            } else {
                                (i as usize).min(chars.len())
                            }
                        }
                        _ => chars.len(),
                    }
                } else {
                    chars.len()
                };
                if start_idx >= chars.len() || start_idx >= end_idx {
                    Ok(vec![Value::string("")])
                } else {
                    Ok(vec![Value::string(
                        chars[start_idx..end_idx].iter().collect::<String>(),
                    )])
                }
            }
            Value::Null => Ok(vec![Value::Null]),
            _ => Err(JqError::Type(format!("Cannot slice {}", input.type_name()))),
        },

        Expr::Iterator => input.iter(),

        Expr::OptionalIterator => input.iter().or(Ok(vec![])),

        Expr::Pipe(left, right) => {
            let left_results = eval(left, ctx, input)?;
            let mut results = Vec::new();
            for v in left_results {
                results.extend(eval(right, ctx, v)?);
            }
            Ok(results)
        }

        Expr::Comma(left, right) => {
            let mut results = eval(left, ctx, input.clone())?;
            results.extend(eval(right, ctx, input)?);
            Ok(results)
        }

        Expr::Array(inner) => match inner {
            Some(expr) => {
                let items = eval(expr, ctx, input)?;
                Ok(vec![Value::array(items)])
            }
            None => Ok(vec![Value::array(vec![])]),
        },

        Expr::Object(entries) => {
            let mut results = vec![IndexMap::new()];
            for entry in entries {
                let mut new_results = Vec::new();
                for obj in &results {
                    match entry {
                        ObjectEntry::KeyValue { key, value } => {
                            let keys = match key {
                                ObjectKey::Literal(s) => vec![s.clone()],
                                ObjectKey::Ident(s) => vec![s.clone()],
                                ObjectKey::Computed(expr) => eval(expr, ctx, input.clone())?
                                    .into_iter()
                                    .filter_map(|v| match v {
                                        Value::String(s) => Some((*s).clone()),
                                        _ => None,
                                    })
                                    .collect(),
                            };
                            let values = eval(value, ctx, input.clone())?;
                            for k in &keys {
                                for v in &values {
                                    let mut new_obj = obj.clone();
                                    new_obj.insert(k.clone(), v.clone());
                                    new_results.push(new_obj);
                                }
                            }
                        }
                        ObjectEntry::Ident(name) => {
                            // {foo} means {foo: .foo}
                            let field_value = match &input {
                                Value::Object(inp_obj) => {
                                    inp_obj.get(name).cloned().unwrap_or(Value::Null)
                                }
                                _ => Value::Null,
                            };
                            let mut new_obj = obj.clone();
                            new_obj.insert(name.clone(), field_value);
                            new_results.push(new_obj);
                        }
                    }
                }
                results = if new_results.is_empty() {
                    results.to_vec()
                } else {
                    new_results
                };
            }
            Ok(results.into_iter().map(Value::object).collect())
        }

        Expr::BinOp(op, left, right) => {
            let left_vals = eval(left, ctx, input.clone())?;
            let right_vals = eval(right, ctx, input)?;
            let mut results = Vec::new();
            for l in &left_vals {
                for r in &right_vals {
                    results.push(eval_binop(op, l, r)?);
                }
            }
            Ok(results)
        }

        Expr::UnaryOp(op, expr) => {
            let vals = eval(expr, ctx, input)?;
            vals.into_iter().map(|v| eval_unaryop(op, &v)).collect()
        }

        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_vals = eval(cond, ctx, input.clone())?;
            let mut results = Vec::new();
            for cv in cond_vals {
                if cv.is_truthy() {
                    results.extend(eval(then_branch, ctx, input.clone())?);
                } else if let Some(else_expr) = else_branch {
                    results.extend(eval(else_expr, ctx, input.clone())?);
                } else {
                    results.push(Value::Null);
                }
            }
            Ok(results)
        }

        Expr::TryCatch {
            try_expr,
            catch_expr,
        } => match eval(try_expr, ctx, input.clone()) {
            Ok(vals) => Ok(vals),
            Err(_) => {
                if let Some(catch) = catch_expr {
                    eval(catch, ctx, input)
                } else {
                    Ok(vec![])
                }
            }
        },

        Expr::Reduce {
            expr,
            var,
            init,
            update,
        } => {
            let items = eval(expr, ctx, input.clone())?;
            let mut acc = eval(init, ctx, input.clone())?
                .into_iter()
                .next()
                .unwrap_or(Value::Null);
            for item in items {
                let new_ctx = ctx.with_var(var.clone(), item);
                acc = eval(update, &new_ctx, acc)?
                    .into_iter()
                    .next()
                    .unwrap_or(Value::Null);
            }
            Ok(vec![acc])
        }

        Expr::Var(name) => ctx
            .get_var(name)
            .cloned()
            .map(|v| vec![v])
            .ok_or_else(|| JqError::UndefinedVariable(name.clone())),

        Expr::As { expr, var, body } => {
            let vals = eval(expr, ctx, input.clone())?;
            let mut results = Vec::new();
            for v in vals {
                let new_ctx = ctx.with_var(var.clone(), v);
                results.extend(eval(body, &new_ctx, input.clone())?);
            }
            Ok(results)
        }

        Expr::FuncCall { name, args } => eval_builtin(name, args, ctx, input),

        Expr::FuncDef { def, body } => {
            let mut new_ctx = ctx.clone();
            new_ctx.define_func(def.clone());
            eval(body, &new_ctx, input)
        }

        Expr::Optional(expr) => eval(expr, ctx, input).or(Ok(vec![])),

        _ => Err(JqError::Custom(format!(
            "Unimplemented expression: {:?}",
            expr
        ))),
    }
}

fn collect_recursive(value: &Value, results: &mut Vec<Value>) {
    match value {
        Value::Array(arr) => {
            for item in arr.iter() {
                results.push(item.clone());
                collect_recursive(item, results);
            }
        }
        Value::Object(obj) => {
            for (_, v) in obj.iter() {
                results.push(v.clone());
                collect_recursive(v, results);
            }
        }
        _ => {}
    }
}

fn eval_binop(op: &BinOp, left: &Value, right: &Value) -> Result<Value> {
    match op {
        BinOp::Add => match (left, right) {
            (Value::Number(a), Value::Number(b)) => {
                Ok(Value::Number(Number::Float(a.as_f64() + b.as_f64())))
            }
            (Value::String(a), Value::String(b)) => Ok(Value::string(format!("{}{}", a, b))),
            (Value::Array(a), Value::Array(b)) => {
                let mut result = (**a).clone();
                result.extend(b.iter().cloned());
                Ok(Value::array(result))
            }
            (Value::Object(a), Value::Object(b)) => {
                let mut result = (**a).clone();
                for (k, v) in b.iter() {
                    result.insert(k.clone(), v.clone());
                }
                Ok(Value::object(result))
            }
            (Value::Null, other) | (other, Value::Null) => Ok(other.clone()),
            _ => Err(JqError::Type(format!(
                "Cannot add {} and {}",
                left.type_name(),
                right.type_name()
            ))),
        },
        BinOp::Sub => match (left, right) {
            (Value::Number(a), Value::Number(b)) => {
                Ok(Value::Number(Number::Float(a.as_f64() - b.as_f64())))
            }
            (Value::Array(a), Value::Array(b)) => {
                let result: Vec<Value> = a.iter().filter(|x| !b.contains(x)).cloned().collect();
                Ok(Value::array(result))
            }
            _ => Err(JqError::Type(format!(
                "Cannot subtract {} from {}",
                right.type_name(),
                left.type_name()
            ))),
        },
        BinOp::Mul => match (left, right) {
            (Value::Number(a), Value::Number(b)) => {
                Ok(Value::Number(Number::Float(a.as_f64() * b.as_f64())))
            }
            (Value::String(s), Value::Number(n)) | (Value::Number(n), Value::String(s)) => {
                let count = n.as_i64().unwrap_or(0).max(0) as usize;
                Ok(Value::string(s.repeat(count)))
            }
            (Value::Object(a), Value::Object(b)) => {
                // Recursive merge
                let mut result = (**a).clone();
                for (k, v) in b.iter() {
                    if let Some(existing) = result.get(k) {
                        if let (Value::Object(ea), Value::Object(eb)) = (existing, v) {
                            let merged = eval_binop(
                                &BinOp::Mul,
                                &Value::Object(ea.clone()),
                                &Value::Object(eb.clone()),
                            )?;
                            result.insert(k.clone(), merged);
                            continue;
                        }
                    }
                    result.insert(k.clone(), v.clone());
                }
                Ok(Value::object(result))
            }
            _ => Err(JqError::Type(format!(
                "Cannot multiply {} and {}",
                left.type_name(),
                right.type_name()
            ))),
        },
        BinOp::Div => match (left, right) {
            (Value::Number(a), Value::Number(b)) => {
                let bv = b.as_f64();
                if bv == 0.0 {
                    Err(JqError::DivisionByZero)
                } else {
                    Ok(Value::Number(Number::Float(a.as_f64() / bv)))
                }
            }
            (Value::String(s), Value::String(sep)) => {
                let parts: Vec<Value> = s
                    .split(&**sep)
                    .map(|p| Value::string(p.to_string()))
                    .collect();
                Ok(Value::array(parts))
            }
            _ => Err(JqError::Type(format!(
                "Cannot divide {} by {}",
                left.type_name(),
                right.type_name()
            ))),
        },
        BinOp::Mod => match (left, right) {
            (Value::Number(a), Value::Number(b)) => {
                let ai = a
                    .as_i64()
                    .ok_or_else(|| JqError::Type("Modulo requires integers".to_string()))?;
                let bi = b
                    .as_i64()
                    .ok_or_else(|| JqError::Type("Modulo requires integers".to_string()))?;
                if bi == 0 {
                    Err(JqError::DivisionByZero)
                } else {
                    Ok(Value::Number(Number::Int(ai % bi)))
                }
            }
            _ => Err(JqError::Type(format!(
                "Cannot compute {} % {}",
                left.type_name(),
                right.type_name()
            ))),
        },
        BinOp::Eq => Ok(Value::Bool(left == right)),
        BinOp::Ne => Ok(Value::Bool(left != right)),
        BinOp::Lt => Ok(Value::Bool(left < right)),
        BinOp::Le => Ok(Value::Bool(left <= right)),
        BinOp::Gt => Ok(Value::Bool(left > right)),
        BinOp::Ge => Ok(Value::Bool(left >= right)),
        BinOp::And => Ok(Value::Bool(left.is_truthy() && right.is_truthy())),
        BinOp::Or => Ok(Value::Bool(left.is_truthy() || right.is_truthy())),
        BinOp::Alt => {
            if left.is_null() || matches!(left, Value::Bool(false)) {
                Ok(right.clone())
            } else {
                Ok(left.clone())
            }
        }
    }
}

fn eval_unaryop(op: &UnaryOp, value: &Value) -> Result<Value> {
    match op {
        UnaryOp::Neg => match value {
            Value::Number(n) => Ok(Value::Number(Number::Float(-n.as_f64()))),
            _ => Err(JqError::Type(format!(
                "Cannot negate {}",
                value.type_name()
            ))),
        },
        UnaryOp::Not => Ok(Value::Bool(!value.is_truthy())),
    }
}

/// Evaluate builtin functions
fn eval_builtin(name: &str, args: &[Expr], ctx: &Context, input: Value) -> Result<Vec<Value>> {
    match name {
        // Type functions
        "type" => Ok(vec![Value::string(input.type_name().to_string())]),
        "null" => Ok(vec![Value::Null]),
        "true" => Ok(vec![Value::Bool(true)]),
        "false" => Ok(vec![Value::Bool(false)]),
        "isnull" => Ok(vec![Value::Bool(input.is_null())]),
        "isnumber" => Ok(vec![Value::Bool(matches!(input, Value::Number(_)))]),
        "isstring" => Ok(vec![Value::Bool(matches!(input, Value::String(_)))]),
        "isarray" => Ok(vec![Value::Bool(matches!(input, Value::Array(_)))]),
        "isobject" => Ok(vec![Value::Bool(matches!(input, Value::Object(_)))]),
        "isboolean" => Ok(vec![Value::Bool(matches!(input, Value::Bool(_)))]),

        // Length and size
        "length" => {
            let len = input.length()?;
            Ok(vec![Value::Number(Number::Int(len as i64))])
        }

        // Keys and values
        "keys" | "keys_unsorted" => {
            let keys = input.keys()?;
            Ok(vec![Value::array(keys)])
        }
        "values" => {
            let values = input.values()?;
            Ok(vec![Value::array(values)])
        }

        // Type selectors
        "nulls" => {
            if input.is_null() {
                Ok(vec![input])
            } else {
                Ok(vec![])
            }
        }
        "booleans" => {
            if matches!(input, Value::Bool(_)) {
                Ok(vec![input])
            } else {
                Ok(vec![])
            }
        }
        "numbers" => {
            if matches!(input, Value::Number(_)) {
                Ok(vec![input])
            } else {
                Ok(vec![])
            }
        }
        "strings" => {
            if matches!(input, Value::String(_)) {
                Ok(vec![input])
            } else {
                Ok(vec![])
            }
        }
        "arrays" => {
            if matches!(input, Value::Array(_)) {
                Ok(vec![input])
            } else {
                Ok(vec![])
            }
        }
        "objects" => {
            if matches!(input, Value::Object(_)) {
                Ok(vec![input])
            } else {
                Ok(vec![])
            }
        }
        "iterables" => {
            if matches!(input, Value::Array(_) | Value::Object(_)) {
                Ok(vec![input])
            } else {
                Ok(vec![])
            }
        }
        "scalars" => {
            if !matches!(input, Value::Array(_) | Value::Object(_)) {
                Ok(vec![input])
            } else {
                Ok(vec![])
            }
        }

        // Array functions
        "first" => {
            if args.is_empty() {
                match &input {
                    Value::Array(arr) if !arr.is_empty() => Ok(vec![arr[0].clone()]),
                    _ => Err(JqError::Type(
                        "Cannot get first of empty or non-array".to_string(),
                    )),
                }
            } else {
                let vals = eval(&args[0], ctx, input)?;
                Ok(vals.into_iter().take(1).collect())
            }
        }
        "last" => {
            if args.is_empty() {
                match &input {
                    Value::Array(arr) if !arr.is_empty() => Ok(vec![arr.last().unwrap().clone()]),
                    _ => Err(JqError::Type(
                        "Cannot get last of empty or non-array".to_string(),
                    )),
                }
            } else {
                let vals = eval(&args[0], ctx, input)?;
                Ok(vals.into_iter().last().into_iter().collect())
            }
        }
        "reverse" => match input {
            Value::Array(arr) => {
                let mut vec = (*arr).clone();
                vec.reverse();
                Ok(vec![Value::array(vec)])
            }
            _ => Err(JqError::Type(format!(
                "Cannot reverse {}",
                input.type_name()
            ))),
        },
        "sort" => match input {
            Value::Array(arr) => {
                let mut vec = (*arr).clone();
                vec.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
                Ok(vec![Value::array(vec)])
            }
            _ => Err(JqError::Type(format!("Cannot sort {}", input.type_name()))),
        },
        "unique" => match input {
            Value::Array(arr) => {
                let mut seen = Vec::new();
                for item in arr.iter() {
                    if !seen.contains(item) {
                        seen.push(item.clone());
                    }
                }
                Ok(vec![Value::array(seen)])
            }
            _ => Err(JqError::Type(format!(
                "Cannot unique {}",
                input.type_name()
            ))),
        },
        "flatten" => {
            fn flatten_impl(arr: &[Value], depth: i64) -> Vec<Value> {
                if depth == 0 {
                    return arr.to_vec();
                }
                let mut result = Vec::new();
                for item in arr {
                    match item {
                        Value::Array(inner) => {
                            result.extend(flatten_impl(inner, depth - 1));
                        }
                        other => result.push(other.clone()),
                    }
                }
                result
            }
            match input {
                Value::Array(arr) => {
                    let depth = if args.is_empty() {
                        i64::MAX
                    } else {
                        match eval(&args[0], ctx, Value::Null)?.first() {
                            Some(Value::Number(n)) => n.as_i64().unwrap_or(i64::MAX),
                            _ => i64::MAX,
                        }
                    };
                    Ok(vec![Value::array(flatten_impl(&arr, depth))])
                }
                _ => Err(JqError::Type(format!(
                    "Cannot flatten {}",
                    input.type_name()
                ))),
            }
        }
        "add" => match input {
            Value::Array(arr) if arr.is_empty() => Ok(vec![Value::Null]),
            Value::Array(arr) => {
                let mut result = arr[0].clone();
                for item in arr.iter().skip(1) {
                    result = eval_binop(&BinOp::Add, &result, item)?;
                }
                Ok(vec![result])
            }
            _ => Err(JqError::Type(format!("Cannot add {}", input.type_name()))),
        },
        "min" => match input {
            Value::Array(arr) if arr.is_empty() => Ok(vec![Value::Null]),
            Value::Array(arr) => {
                let min = arr
                    .iter()
                    .cloned()
                    .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
                    .unwrap();
                Ok(vec![min])
            }
            _ => Err(JqError::Type(format!(
                "Cannot get min of {}",
                input.type_name()
            ))),
        },
        "max" => match input {
            Value::Array(arr) if arr.is_empty() => Ok(vec![Value::Null]),
            Value::Array(arr) => {
                let max = arr
                    .iter()
                    .cloned()
                    .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
                    .unwrap();
                Ok(vec![max])
            }
            _ => Err(JqError::Type(format!(
                "Cannot get max of {}",
                input.type_name()
            ))),
        },

        // Object functions
        "has" => {
            if args.is_empty() {
                return Err(JqError::Custom("has requires 1 argument".to_string()));
            }
            let keys = eval(&args[0], ctx, input.clone())?;
            let mut results = Vec::new();
            for key in keys {
                let has = match (&input, &key) {
                    (Value::Object(obj), Value::String(k)) => obj.contains_key(k.as_str()),
                    (Value::Array(arr), Value::Number(n)) => {
                        let i = n.as_i64().unwrap_or(-1);
                        i >= 0 && (i as usize) < arr.len()
                    }
                    _ => false,
                };
                results.push(Value::Bool(has));
            }
            Ok(results)
        }
        "in" => {
            if args.is_empty() {
                return Err(JqError::Custom("in requires 1 argument".to_string()));
            }
            let containers = eval(&args[0], ctx, input.clone())?;
            let mut results = Vec::new();
            for container in containers {
                let has = match (&input, &container) {
                    (Value::String(k), Value::Object(obj)) => obj.contains_key(k.as_str()),
                    (Value::Number(n), Value::Array(arr)) => {
                        let i = n.as_i64().unwrap_or(-1);
                        i >= 0 && (i as usize) < arr.len()
                    }
                    _ => false,
                };
                results.push(Value::Bool(has));
            }
            Ok(results)
        }
        "to_entries" => match input {
            Value::Object(obj) => {
                let entries: Vec<Value> = obj
                    .iter()
                    .map(|(k, v)| {
                        let mut entry = IndexMap::new();
                        entry.insert("key".to_string(), Value::string(k.clone()));
                        entry.insert("value".to_string(), v.clone());
                        Value::object(entry)
                    })
                    .collect();
                Ok(vec![Value::array(entries)])
            }
            _ => Err(JqError::Type(format!(
                "Cannot convert {} to entries",
                input.type_name()
            ))),
        },
        "from_entries" => match input {
            Value::Array(arr) => {
                let mut obj = IndexMap::new();
                for entry in arr.iter() {
                    if let Value::Object(e) = entry {
                        let key = e
                            .get("key")
                            .or_else(|| e.get("k"))
                            .or_else(|| e.get("name"));
                        let value = e
                            .get("value")
                            .or_else(|| e.get("v"))
                            .cloned()
                            .unwrap_or(Value::Null);
                        if let Some(Value::String(k)) = key {
                            obj.insert((**k).clone(), value);
                        }
                    }
                }
                Ok(vec![Value::object(obj)])
            }
            _ => Err(JqError::Type(format!(
                "Cannot convert {} from entries",
                input.type_name()
            ))),
        },

        // String functions
        "tostring" => Ok(vec![Value::string(match &input {
            Value::String(s) => (**s).clone(),
            Value::Null => "null".to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Number(n) => match n {
                Number::Int(i) => i.to_string(),
                Number::Float(f) => f.to_string(),
            },
            _ => serde_json::to_string(&input.to_json()).unwrap_or_default(),
        })]),
        "tonumber" => match &input {
            Value::Number(_) => Ok(vec![input]),
            Value::String(s) => {
                if let Ok(i) = s.parse::<i64>() {
                    Ok(vec![Value::Number(Number::Int(i))])
                } else if let Ok(f) = s.parse::<f64>() {
                    Ok(vec![Value::Number(Number::Float(f))])
                } else {
                    Err(JqError::Type(format!("Cannot parse \"{}\" as number", s)))
                }
            }
            _ => Err(JqError::Type(format!(
                "Cannot convert {} to number",
                input.type_name()
            ))),
        },
        "ascii_downcase" => match input {
            Value::String(s) => Ok(vec![Value::string(s.to_lowercase())]),
            _ => Err(JqError::Type(format!(
                "Cannot lowercase {}",
                input.type_name()
            ))),
        },
        "ascii_upcase" => match input {
            Value::String(s) => Ok(vec![Value::string(s.to_uppercase())]),
            _ => Err(JqError::Type(format!(
                "Cannot uppercase {}",
                input.type_name()
            ))),
        },
        "ltrimstr" => {
            if args.is_empty() {
                return Err(JqError::Custom("ltrimstr requires 1 argument".to_string()));
            }
            let prefixes = eval(&args[0], ctx, input.clone())?;
            let mut results = Vec::new();
            for prefix in prefixes {
                match (&input, &prefix) {
                    (Value::String(s), Value::String(p)) => {
                        results.push(Value::string(
                            s.strip_prefix(p.as_str()).unwrap_or(s).to_string(),
                        ));
                    }
                    _ => results.push(input.clone()),
                }
            }
            Ok(results)
        }
        "rtrimstr" => {
            if args.is_empty() {
                return Err(JqError::Custom("rtrimstr requires 1 argument".to_string()));
            }
            let suffixes = eval(&args[0], ctx, input.clone())?;
            let mut results = Vec::new();
            for suffix in suffixes {
                match (&input, &suffix) {
                    (Value::String(s), Value::String(p)) => {
                        results.push(Value::string(
                            s.strip_suffix(p.as_str()).unwrap_or(s).to_string(),
                        ));
                    }
                    _ => results.push(input.clone()),
                }
            }
            Ok(results)
        }
        "split" => {
            if args.is_empty() {
                return Err(JqError::Custom("split requires 1 argument".to_string()));
            }
            let seps = eval(&args[0], ctx, input.clone())?;
            let mut results = Vec::new();
            for sep in seps {
                match (&input, &sep) {
                    (Value::String(s), Value::String(sep)) => {
                        let parts: Vec<Value> = s
                            .split(sep.as_str())
                            .map(|p| Value::string(p.to_string()))
                            .collect();
                        results.push(Value::array(parts));
                    }
                    _ => return Err(JqError::Type("split requires strings".to_string())),
                }
            }
            Ok(results)
        }
        "join" => {
            if args.is_empty() {
                return Err(JqError::Custom("join requires 1 argument".to_string()));
            }
            let seps = eval(&args[0], ctx, input.clone())?;
            let mut results = Vec::new();
            for sep in seps {
                match (&input, &sep) {
                    (Value::Array(arr), Value::String(sep)) => {
                        let parts: Vec<String> = arr
                            .iter()
                            .filter_map(|v| match v {
                                Value::String(s) => Some((**s).clone()),
                                Value::Null => Some(String::new()),
                                _ => None,
                            })
                            .collect();
                        results.push(Value::string(parts.join(sep.as_str())));
                    }
                    _ => return Err(JqError::Type("join requires array and string".to_string())),
                }
            }
            Ok(results)
        }
        "contains" => {
            if args.is_empty() {
                return Err(JqError::Custom("contains requires 1 argument".to_string()));
            }
            let needles = eval(&args[0], ctx, input.clone())?;
            let mut results = Vec::new();
            for needle in needles {
                results.push(Value::Bool(value_contains(&input, &needle)));
            }
            Ok(results)
        }
        "inside" => {
            if args.is_empty() {
                return Err(JqError::Custom("inside requires 1 argument".to_string()));
            }
            let haystacks = eval(&args[0], ctx, input.clone())?;
            let mut results = Vec::new();
            for haystack in haystacks {
                results.push(Value::Bool(value_contains(&haystack, &input)));
            }
            Ok(results)
        }
        "startswith" => {
            if args.is_empty() {
                return Err(JqError::Custom(
                    "startswith requires 1 argument".to_string(),
                ));
            }
            let prefixes = eval(&args[0], ctx, input.clone())?;
            let mut results = Vec::new();
            for prefix in prefixes {
                match (&input, &prefix) {
                    (Value::String(s), Value::String(p)) => {
                        results.push(Value::Bool(s.starts_with(p.as_str())));
                    }
                    _ => return Err(JqError::Type("startswith requires strings".to_string())),
                }
            }
            Ok(results)
        }
        "endswith" => {
            if args.is_empty() {
                return Err(JqError::Custom("endswith requires 1 argument".to_string()));
            }
            let suffixes = eval(&args[0], ctx, input.clone())?;
            let mut results = Vec::new();
            for suffix in suffixes {
                match (&input, &suffix) {
                    (Value::String(s), Value::String(p)) => {
                        results.push(Value::Bool(s.ends_with(p.as_str())));
                    }
                    _ => return Err(JqError::Type("endswith requires strings".to_string())),
                }
            }
            Ok(results)
        }

        // Regex functions
        "test" => {
            if args.is_empty() {
                return Err(JqError::Custom("test requires 1 argument".to_string()));
            }
            let patterns = eval(&args[0], ctx, input.clone())?;
            let flags = if args.len() > 1 {
                eval(&args[1], ctx, input.clone())?
                    .into_iter()
                    .next()
                    .and_then(|v| match v {
                        Value::String(s) => Some(s),
                        _ => None,
                    })
            } else {
                None
            };
            let mut results = Vec::new();
            for pattern in patterns {
                match (&input, &pattern) {
                    (Value::String(s), Value::String(p)) => {
                        let regex_pattern =
                            build_regex_pattern(p, flags.as_deref().map(|s| s.as_str()))?;
                        let re = Regex::new(&regex_pattern)
                            .map_err(|e| JqError::Custom(format!("Invalid regex: {}", e)))?;
                        results.push(Value::Bool(re.is_match(s)));
                    }
                    _ => {
                        return Err(JqError::Type(
                            "test requires string and pattern".to_string(),
                        ))
                    }
                }
            }
            Ok(results)
        }
        "match" => {
            if args.is_empty() {
                return Err(JqError::Custom("match requires 1 argument".to_string()));
            }
            let patterns = eval(&args[0], ctx, input.clone())?;
            let flags = if args.len() > 1 {
                eval(&args[1], ctx, input.clone())?
                    .into_iter()
                    .next()
                    .and_then(|v| match v {
                        Value::String(s) => Some(s),
                        _ => None,
                    })
            } else {
                None
            };
            let global = flags.as_ref().map(|f| f.contains('g')).unwrap_or(false);
            let mut results = Vec::new();
            for pattern in patterns {
                match (&input, &pattern) {
                    (Value::String(s), Value::String(p)) => {
                        let regex_pattern =
                            build_regex_pattern(p, flags.as_deref().map(|s| s.as_str()))?;
                        let re = Regex::new(&regex_pattern)
                            .map_err(|e| JqError::Custom(format!("Invalid regex: {}", e)))?;

                        if global {
                            for cap in re.captures_iter(s) {
                                results.push(build_match_object(&cap, s));
                            }
                        } else if let Some(cap) = re.captures(s) {
                            results.push(build_match_object(&cap, s));
                        }
                    }
                    _ => {
                        return Err(JqError::Type(
                            "match requires string and pattern".to_string(),
                        ))
                    }
                }
            }
            Ok(results)
        }
        "capture" => {
            if args.is_empty() {
                return Err(JqError::Custom("capture requires 1 argument".to_string()));
            }
            let patterns = eval(&args[0], ctx, input.clone())?;
            let flags = if args.len() > 1 {
                eval(&args[1], ctx, input.clone())?
                    .into_iter()
                    .next()
                    .and_then(|v| match v {
                        Value::String(s) => Some(s),
                        _ => None,
                    })
            } else {
                None
            };
            let mut results = Vec::new();
            for pattern in patterns {
                match (&input, &pattern) {
                    (Value::String(s), Value::String(p)) => {
                        let regex_pattern =
                            build_regex_pattern(p, flags.as_deref().map(|s| s.as_str()))?;
                        let re = Regex::new(&regex_pattern)
                            .map_err(|e| JqError::Custom(format!("Invalid regex: {}", e)))?;

                        if let Some(cap) = re.captures(s) {
                            let mut obj = IndexMap::new();
                            for name in re.capture_names().flatten() {
                                if let Some(m) = cap.name(name) {
                                    obj.insert(name.to_string(), Value::string(m.as_str()));
                                }
                            }
                            results.push(Value::object(obj));
                        }
                    }
                    _ => {
                        return Err(JqError::Type(
                            "capture requires string and pattern".to_string(),
                        ))
                    }
                }
            }
            Ok(results)
        }
        "splits" => {
            if args.is_empty() {
                return Err(JqError::Custom("splits requires 1 argument".to_string()));
            }
            let patterns = eval(&args[0], ctx, input.clone())?;
            let flags = if args.len() > 1 {
                eval(&args[1], ctx, input.clone())?
                    .into_iter()
                    .next()
                    .and_then(|v| match v {
                        Value::String(s) => Some(s),
                        _ => None,
                    })
            } else {
                None
            };
            let mut results = Vec::new();
            for pattern in patterns {
                match (&input, &pattern) {
                    (Value::String(s), Value::String(p)) => {
                        let regex_pattern =
                            build_regex_pattern(p, flags.as_deref().map(|s| s.as_str()))?;
                        let re = Regex::new(&regex_pattern)
                            .map_err(|e| JqError::Custom(format!("Invalid regex: {}", e)))?;
                        for part in re.split(s) {
                            results.push(Value::string(part.to_string()));
                        }
                    }
                    _ => {
                        return Err(JqError::Type(
                            "splits requires string and pattern".to_string(),
                        ))
                    }
                }
            }
            Ok(results)
        }
        "sub" => {
            if args.len() < 2 {
                return Err(JqError::Custom("sub requires 2 arguments".to_string()));
            }
            let patterns = eval(&args[0], ctx, input.clone())?;
            let replacements = eval(&args[1], ctx, input.clone())?;
            let flags = if args.len() > 2 {
                eval(&args[2], ctx, input.clone())?
                    .into_iter()
                    .next()
                    .and_then(|v| match v {
                        Value::String(s) => Some(s),
                        _ => None,
                    })
            } else {
                None
            };
            let mut results = Vec::new();
            for pattern in &patterns {
                for replacement in &replacements {
                    match (&input, pattern, replacement) {
                        (Value::String(s), Value::String(p), Value::String(r)) => {
                            let regex_pattern =
                                build_regex_pattern(p, flags.as_deref().map(|s| s.as_str()))?;
                            let re = Regex::new(&regex_pattern)
                                .map_err(|e| JqError::Custom(format!("Invalid regex: {}", e)))?;
                            // Convert jq replacement syntax to regex replacement syntax
                            let replacement_str = convert_jq_replacement(r);
                            let result = re.replace(s, replacement_str.as_str()).to_string();
                            results.push(Value::string(result));
                        }
                        _ => return Err(JqError::Type("sub requires strings".to_string())),
                    }
                }
            }
            Ok(results)
        }
        "gsub" => {
            if args.len() < 2 {
                return Err(JqError::Custom("gsub requires 2 arguments".to_string()));
            }
            let patterns = eval(&args[0], ctx, input.clone())?;
            let replacements = eval(&args[1], ctx, input.clone())?;
            let flags = if args.len() > 2 {
                eval(&args[2], ctx, input.clone())?
                    .into_iter()
                    .next()
                    .and_then(|v| match v {
                        Value::String(s) => Some(s),
                        _ => None,
                    })
            } else {
                None
            };
            let mut results = Vec::new();
            for pattern in &patterns {
                for replacement in &replacements {
                    match (&input, pattern, replacement) {
                        (Value::String(s), Value::String(p), Value::String(r)) => {
                            let regex_pattern =
                                build_regex_pattern(p, flags.as_deref().map(|s| s.as_str()))?;
                            let re = Regex::new(&regex_pattern)
                                .map_err(|e| JqError::Custom(format!("Invalid regex: {}", e)))?;
                            // Convert jq replacement syntax to regex replacement syntax
                            let replacement_str = convert_jq_replacement(r);
                            let result = re.replace_all(s, replacement_str.as_str()).to_string();
                            results.push(Value::string(result));
                        }
                        _ => return Err(JqError::Type("gsub requires strings".to_string())),
                    }
                }
            }
            Ok(results)
        }
        "scan" => {
            if args.is_empty() {
                return Err(JqError::Custom("scan requires 1 argument".to_string()));
            }
            let patterns = eval(&args[0], ctx, input.clone())?;
            let flags = if args.len() > 1 {
                eval(&args[1], ctx, input.clone())?
                    .into_iter()
                    .next()
                    .and_then(|v| match v {
                        Value::String(s) => Some(s),
                        _ => None,
                    })
            } else {
                None
            };
            let mut results = Vec::new();
            for pattern in patterns {
                match (&input, &pattern) {
                    (Value::String(s), Value::String(p)) => {
                        let regex_pattern =
                            build_regex_pattern(p, flags.as_deref().map(|s| s.as_str()))?;
                        let re = Regex::new(&regex_pattern)
                            .map_err(|e| JqError::Custom(format!("Invalid regex: {}", e)))?;

                        for cap in re.captures_iter(s) {
                            if cap.len() > 1 {
                                // Has capture groups - return array of captured strings
                                let captures: Vec<Value> = cap
                                    .iter()
                                    .skip(1) // Skip the full match
                                    .map(|m| {
                                        m.map(|m| Value::string(m.as_str())).unwrap_or(Value::Null)
                                    })
                                    .collect();
                                results.push(Value::array(captures));
                            } else {
                                // No capture groups - return the full match
                                results.push(Value::string(cap[0].to_string()));
                            }
                        }
                    }
                    _ => {
                        return Err(JqError::Type(
                            "scan requires string and pattern".to_string(),
                        ))
                    }
                }
            }
            Ok(results)
        }

        // Math functions
        "floor" => match &input {
            Value::Number(n) => Ok(vec![Value::Number(Number::Int(n.as_f64().floor() as i64))]),
            _ => Err(JqError::Type(format!("Cannot floor {}", input.type_name()))),
        },
        "ceil" => match &input {
            Value::Number(n) => Ok(vec![Value::Number(Number::Int(n.as_f64().ceil() as i64))]),
            _ => Err(JqError::Type(format!("Cannot ceil {}", input.type_name()))),
        },
        "round" => match &input {
            Value::Number(n) => Ok(vec![Value::Number(Number::Int(n.as_f64().round() as i64))]),
            _ => Err(JqError::Type(format!("Cannot round {}", input.type_name()))),
        },
        "sqrt" => match &input {
            Value::Number(n) => Ok(vec![Value::Number(Number::Float(n.as_f64().sqrt()))]),
            _ => Err(JqError::Type(format!("Cannot sqrt {}", input.type_name()))),
        },
        "fabs" => match &input {
            Value::Number(n) => Ok(vec![Value::Number(Number::Float(n.as_f64().abs()))]),
            _ => Err(JqError::Type(format!("Cannot fabs {}", input.type_name()))),
        },

        // Misc
        "empty" => Ok(vec![]),
        "error" => {
            if args.is_empty() {
                Err(JqError::Custom("error".to_string()))
            } else {
                let msgs = eval(&args[0], ctx, input)?;
                match msgs.first() {
                    Some(Value::String(s)) => Err(JqError::Custom((**s).clone())),
                    Some(v) => Err(JqError::Custom(format!("{}", v))),
                    None => Err(JqError::Custom("error".to_string())),
                }
            }
        }
        "not" => Ok(vec![Value::Bool(!input.is_truthy())]),
        "select" => {
            if args.is_empty() {
                return Err(JqError::Custom("select requires 1 argument".to_string()));
            }
            let conds = eval(&args[0], ctx, input.clone())?;
            if conds.iter().any(|v| v.is_truthy()) {
                Ok(vec![input])
            } else {
                Ok(vec![])
            }
        }
        "map" => {
            if args.is_empty() {
                return Err(JqError::Custom("map requires 1 argument".to_string()));
            }
            match input {
                Value::Array(arr) => {
                    let mut results = Vec::new();
                    for item in arr.iter() {
                        results.extend(eval(&args[0], ctx, item.clone())?);
                    }
                    Ok(vec![Value::array(results)])
                }
                _ => Err(JqError::Type(format!(
                    "Cannot map over {}",
                    input.type_name()
                ))),
            }
        }
        "map_values" => {
            if args.is_empty() {
                return Err(JqError::Custom(
                    "map_values requires 1 argument".to_string(),
                ));
            }
            match input {
                Value::Array(arr) => {
                    let mut results = Vec::new();
                    for item in arr.iter() {
                        let mapped = eval(&args[0], ctx, item.clone())?;
                        if let Some(v) = mapped.into_iter().next() {
                            results.push(v);
                        }
                    }
                    Ok(vec![Value::array(results)])
                }
                Value::Object(obj) => {
                    let mut result = IndexMap::new();
                    for (k, v) in obj.iter() {
                        let mapped = eval(&args[0], ctx, v.clone())?;
                        if let Some(new_v) = mapped.into_iter().next() {
                            result.insert(k.clone(), new_v);
                        }
                    }
                    Ok(vec![Value::object(result)])
                }
                _ => Err(JqError::Type(format!(
                    "Cannot map_values over {}",
                    input.type_name()
                ))),
            }
        }
        "group_by" => {
            if args.is_empty() {
                return Err(JqError::Custom("group_by requires 1 argument".to_string()));
            }
            match input {
                Value::Array(arr) => {
                    let mut groups: IndexMap<String, Vec<Value>> = IndexMap::new();
                    for item in arr.iter() {
                        let keys = eval(&args[0], ctx, item.clone())?;
                        let key = keys.into_iter().next().unwrap_or(Value::Null);
                        let key_str = serde_json::to_string(&key.to_json()).unwrap_or_default();
                        groups.entry(key_str).or_default().push(item.clone());
                    }
                    let result: Vec<Value> = groups.into_values().map(Value::array).collect();
                    Ok(vec![Value::array(result)])
                }
                _ => Err(JqError::Type(format!(
                    "Cannot group_by {}",
                    input.type_name()
                ))),
            }
        }
        "sort_by" => {
            if args.is_empty() {
                return Err(JqError::Custom("sort_by requires 1 argument".to_string()));
            }
            match input {
                Value::Array(arr) => {
                    let mut arr = (*arr).clone();
                    // Get sort keys for all items
                    let mut keyed: Vec<(Value, Value)> = arr
                        .drain(..)
                        .map(|item| {
                            let key = eval(&args[0], ctx, item.clone())
                                .ok()
                                .and_then(|v| v.into_iter().next())
                                .unwrap_or(Value::Null);
                            (key, item)
                        })
                        .collect();
                    keyed.sort_by(|(a, _), (b, _)| {
                        a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
                    });
                    Ok(vec![Value::array(
                        keyed.into_iter().map(|(_, v)| v).collect(),
                    )])
                }
                _ => Err(JqError::Type(format!(
                    "Cannot sort_by {}",
                    input.type_name()
                ))),
            }
        }
        "unique_by" => {
            if args.is_empty() {
                return Err(JqError::Custom("unique_by requires 1 argument".to_string()));
            }
            match input {
                Value::Array(arr) => {
                    let mut seen_keys = Vec::new();
                    let mut result = Vec::new();
                    for item in arr.iter() {
                        let key = eval(&args[0], ctx, item.clone())
                            .ok()
                            .and_then(|v| v.into_iter().next())
                            .unwrap_or(Value::Null);
                        if !seen_keys.contains(&key) {
                            seen_keys.push(key);
                            result.push(item.clone());
                        }
                    }
                    Ok(vec![Value::array(result)])
                }
                _ => Err(JqError::Type(format!(
                    "Cannot unique_by {}",
                    input.type_name()
                ))),
            }
        }
        "any" => match input {
            Value::Array(arr) => {
                if args.is_empty() {
                    Ok(vec![Value::Bool(arr.iter().any(|v| v.is_truthy()))])
                } else {
                    let result = arr.iter().cloned().any(|item| {
                        eval(&args[0], ctx, item)
                            .map(|vals| vals.iter().any(|v| v.is_truthy()))
                            .unwrap_or(false)
                    });
                    Ok(vec![Value::Bool(result)])
                }
            }
            _ => Err(JqError::Type(format!(
                "Cannot any over {}",
                input.type_name()
            ))),
        },
        "all" => match input {
            Value::Array(arr) => {
                if args.is_empty() {
                    Ok(vec![Value::Bool(arr.iter().all(|v| v.is_truthy()))])
                } else {
                    let result = arr.iter().cloned().all(|item| {
                        eval(&args[0], ctx, item)
                            .map(|vals| vals.iter().all(|v| v.is_truthy()))
                            .unwrap_or(false)
                    });
                    Ok(vec![Value::Bool(result)])
                }
            }
            _ => Err(JqError::Type(format!(
                "Cannot all over {}",
                input.type_name()
            ))),
        },
        "range" => {
            let (from, to, step) = match args.len() {
                1 => {
                    let to = eval(&args[0], ctx, input.clone())?
                        .into_iter()
                        .next()
                        .and_then(|v| match v {
                            Value::Number(n) => n.as_i64(),
                            _ => None,
                        })
                        .unwrap_or(0);
                    (0, to, 1)
                }
                2 => {
                    let from = eval(&args[0], ctx, input.clone())?
                        .into_iter()
                        .next()
                        .and_then(|v| match v {
                            Value::Number(n) => n.as_i64(),
                            _ => None,
                        })
                        .unwrap_or(0);
                    let to = eval(&args[1], ctx, input.clone())?
                        .into_iter()
                        .next()
                        .and_then(|v| match v {
                            Value::Number(n) => n.as_i64(),
                            _ => None,
                        })
                        .unwrap_or(0);
                    (from, to, 1)
                }
                _ => {
                    let from = eval(&args[0], ctx, input.clone())?
                        .into_iter()
                        .next()
                        .and_then(|v| match v {
                            Value::Number(n) => n.as_i64(),
                            _ => None,
                        })
                        .unwrap_or(0);
                    let to = eval(&args[1], ctx, input.clone())?
                        .into_iter()
                        .next()
                        .and_then(|v| match v {
                            Value::Number(n) => n.as_i64(),
                            _ => None,
                        })
                        .unwrap_or(0);
                    let step = eval(&args[2], ctx, input)?
                        .into_iter()
                        .next()
                        .and_then(|v| match v {
                            Value::Number(n) => n.as_i64(),
                            _ => None,
                        })
                        .unwrap_or(1);
                    (from, to, step)
                }
            };
            if step == 0 {
                return Err(JqError::Custom("range step cannot be 0".to_string()));
            }
            let mut results = Vec::new();
            let mut i = from;
            if step > 0 {
                while i < to {
                    results.push(Value::Number(Number::Int(i)));
                    i += step;
                }
            } else {
                while i > to {
                    results.push(Value::Number(Number::Int(i)));
                    i += step;
                }
            }
            Ok(results)
        }
        "nth" => {
            if args.is_empty() {
                return Err(JqError::Custom(
                    "nth requires at least 1 argument".to_string(),
                ));
            }
            let n = eval(&args[0], ctx, input.clone())?
                .into_iter()
                .next()
                .and_then(|v| match v {
                    Value::Number(num) => num.as_i64(),
                    _ => None,
                })
                .ok_or_else(|| JqError::Type("nth requires a number".to_string()))?;
            if n < 0 {
                return Ok(vec![]);
            }
            if args.len() > 1 {
                let vals = eval(&args[1], ctx, input)?;
                Ok(vals.into_iter().nth(n as usize).into_iter().collect())
            } else {
                match input {
                    Value::Array(arr) => Ok(arr.get(n as usize).cloned().into_iter().collect()),
                    _ => Err(JqError::Type("nth requires array".to_string())),
                }
            }
        }
        "recurse" => {
            let filter = if args.is_empty() {
                Expr::Pipe(
                    Box::new(Expr::Iterator),
                    Box::new(Expr::Optional(Box::new(Expr::Identity))),
                )
            } else {
                args[0].clone()
            };
            let mut results = vec![input.clone()];
            let mut stack = vec![input];
            while let Some(current) = stack.pop() {
                if let Ok(children) = eval(&filter, ctx, current) {
                    for child in children {
                        if !results.contains(&child) {
                            results.push(child.clone());
                            stack.push(child);
                        }
                    }
                }
            }
            Ok(results)
        }
        "env" => {
            let mut env_obj = IndexMap::new();
            for (key, value) in std::env::vars() {
                env_obj.insert(key, Value::string(value));
            }
            Ok(vec![Value::object(env_obj)])
        }
        "now" => {
            use std::time::{SystemTime, UNIX_EPOCH};
            let duration = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default();
            Ok(vec![Value::Number(Number::Float(duration.as_secs_f64()))])
        }
        "debug" => {
            eprintln!("[DEBUG] {}", input);
            Ok(vec![input])
        }

        // User-defined function
        _ => {
            if let Some(func) = ctx.get_func(name) {
                if func.params.len() != args.len() {
                    return Err(JqError::Custom(format!(
                        "Function {} expects {} arguments, got {}",
                        name,
                        func.params.len(),
                        args.len()
                    )));
                }
                let mut new_ctx = ctx.clone();
                for (param, arg) in func.params.iter().zip(args.iter()) {
                    // For jq, function arguments are filters, not values
                    // But for simplicity, we'll evaluate them first
                    let vals = eval(arg, ctx, input.clone())?;
                    if let Some(v) = vals.into_iter().next() {
                        new_ctx = new_ctx.with_var(param.clone(), v);
                    }
                }
                eval(&func.body, &new_ctx, input)
            } else {
                Err(JqError::UndefinedFunction(name.to_string()))
            }
        }
    }
}

fn value_contains(haystack: &Value, needle: &Value) -> bool {
    match (haystack, needle) {
        (Value::String(h), Value::String(n)) => h.contains(n.as_str()),
        (Value::Array(h), Value::Array(n)) => {
            n.iter().all(|ni| h.iter().any(|hi| value_contains(hi, ni)))
        }
        (Value::Object(h), Value::Object(n)) => n
            .iter()
            .all(|(k, nv)| h.get(k).map(|hv| value_contains(hv, nv)).unwrap_or(false)),
        (h, n) => h == n,
    }
}

/// Build a regex pattern with optional flags
fn build_regex_pattern(pattern: &str, flags: Option<&str>) -> Result<String> {
    let mut result = String::new();
    if let Some(f) = flags {
        if f.contains('i') {
            result.push_str("(?i)");
        }
        if f.contains('m') {
            result.push_str("(?m)");
        }
        if f.contains('s') {
            result.push_str("(?s)");
        }
        if f.contains('x') {
            result.push_str("(?x)");
        }
    }
    result.push_str(pattern);
    Ok(result)
}

/// Build a match object from regex captures (jq-compatible format)
fn build_match_object(cap: &regex::Captures, _input: &str) -> Value {
    let full_match = cap.get(0).unwrap();
    let mut obj = IndexMap::new();

    // offset - byte offset of the match
    obj.insert(
        "offset".to_string(),
        Value::Number(Number::Int(full_match.start() as i64)),
    );
    // length - length of the match
    obj.insert(
        "length".to_string(),
        Value::Number(Number::Int(full_match.len() as i64)),
    );
    // string - the matched string
    obj.insert("string".to_string(), Value::string(full_match.as_str()));
    // name - the capture group name (null for unnamed)
    obj.insert("name".to_string(), Value::Null);

    // captures - array of capture groups
    let captures: Vec<Value> = cap
        .iter()
        .skip(1) // Skip the full match
        .map(|m| {
            let mut capture_obj = IndexMap::new();
            if let Some(m) = m {
                capture_obj.insert(
                    "offset".to_string(),
                    Value::Number(Number::Int(m.start() as i64)),
                );
                capture_obj.insert(
                    "length".to_string(),
                    Value::Number(Number::Int(m.len() as i64)),
                );
                capture_obj.insert("string".to_string(), Value::string(m.as_str()));
                // Try to get named capture
                capture_obj.insert("name".to_string(), Value::Null);
            } else {
                capture_obj.insert("offset".to_string(), Value::Number(Number::Int(-1)));
                capture_obj.insert("length".to_string(), Value::Number(Number::Int(0)));
                capture_obj.insert("string".to_string(), Value::Null);
                capture_obj.insert("name".to_string(), Value::Null);
            }
            Value::object(capture_obj)
        })
        .collect();
    obj.insert("captures".to_string(), Value::array(captures));

    Value::object(obj)
}

/// Convert jq replacement syntax to regex replacement syntax
/// jq uses \(.name) for named groups and \(0), \(1) for numbered groups
/// Rust regex uses $name and $1, $2 etc.
fn convert_jq_replacement(replacement: &str) -> String {
    let mut result = String::new();
    let chars: Vec<char> = replacement.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '\\' && i + 1 < chars.len() {
            match chars[i + 1] {
                '(' => {
                    // Start of jq interpolation \(...)
                    let mut depth = 1;
                    let mut j = i + 2;
                    let mut content = String::new();
                    while j < chars.len() && depth > 0 {
                        if chars[j] == '(' {
                            depth += 1;
                            content.push(chars[j]);
                        } else if chars[j] == ')' {
                            depth -= 1;
                            if depth > 0 {
                                content.push(chars[j]);
                            }
                        } else {
                            content.push(chars[j]);
                        }
                        j += 1;
                    }
                    // Convert content to regex syntax
                    if content.chars().all(|c| c.is_ascii_digit()) {
                        // Numbered group \(1) -> $1
                        result.push('$');
                        result.push_str(&content);
                    } else if let Some(name) = content.strip_prefix('.') {
                        // Named group \(.name) -> ${name}
                        result.push_str("${");
                        result.push_str(name);
                        result.push('}');
                    } else {
                        // Just pass through
                        result.push('$');
                        result.push_str(&content);
                    }
                    i = j;
                    continue;
                }
                'n' => {
                    result.push('\n');
                    i += 2;
                    continue;
                }
                't' => {
                    result.push('\t');
                    i += 2;
                    continue;
                }
                'r' => {
                    result.push('\r');
                    i += 2;
                    continue;
                }
                '\\' => {
                    result.push('\\');
                    i += 2;
                    continue;
                }
                c if c.is_ascii_digit() => {
                    // Numbered backreference \1 -> $1
                    result.push('$');
                    result.push(c);
                    i += 2;
                    continue;
                }
                _ => {
                    result.push(chars[i]);
                    i += 1;
                }
            }
        } else {
            result.push(chars[i]);
            i += 1;
        }
    }

    result
}
