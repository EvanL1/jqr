//! JSON Value type for jq

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt;

use crate::error::{JqError, Result};

/// Represents a JSON value in jq
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Value {
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Object(IndexMap<String, Value>),
}

/// Number type supporting both integers and floats
#[derive(Clone, Debug)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl Number {
    pub fn as_f64(&self) -> f64 {
        match self {
            Number::Int(i) => *i as f64,
            Number::Float(f) => *f,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Number::Int(i) => Some(*i),
            Number::Float(f) => {
                if f.fract() == 0.0 && *f >= i64::MIN as f64 && *f <= i64::MAX as f64 {
                    Some(*f as i64)
                } else {
                    None
                }
            }
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Number::Int(_) => true,
            Number::Float(f) => f.fract() == 0.0,
        }
    }
}

impl Serialize for Number {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Number::Int(i) => serializer.serialize_i64(*i),
            Number::Float(f) => serializer.serialize_f64(*f),
        }
    }
}

impl<'de> Deserialize<'de> for Number {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = serde_json::Number::deserialize(deserializer)?;
        if let Some(i) = value.as_i64() {
            Ok(Number::Int(i))
        } else if let Some(f) = value.as_f64() {
            Ok(Number::Float(f))
        } else {
            Err(serde::de::Error::custom("invalid number"))
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.as_f64() == other.as_f64()
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_f64().partial_cmp(&other.as_f64())
    }
}

impl Value {
    /// Get the type name of this value
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Null => "null",
            Value::Bool(_) => "boolean",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Object(_) => "object",
        }
    }

    /// Check if value is null
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

    /// Check if value is truthy (jq semantics: false and null are falsy)
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Null | Value::Bool(false))
    }

    /// Get length of value
    pub fn length(&self) -> Result<usize> {
        match self {
            Value::Null => Ok(0),
            Value::String(s) => Ok(s.chars().count()),
            Value::Array(arr) => Ok(arr.len()),
            Value::Object(obj) => Ok(obj.len()),
            _ => Err(JqError::Type(format!(
                "{} has no length",
                self.type_name()
            ))),
        }
    }

    /// Get keys of object
    pub fn keys(&self) -> Result<Vec<Value>> {
        match self {
            Value::Object(obj) => Ok(obj.keys().cloned().map(Value::String).collect()),
            Value::Array(arr) => Ok((0..arr.len() as i64).map(|i| Value::Number(Number::Int(i))).collect()),
            _ => Err(JqError::Type(format!(
                "{} has no keys",
                self.type_name()
            ))),
        }
    }

    /// Get values of object/array
    pub fn values(&self) -> Result<Vec<Value>> {
        match self {
            Value::Object(obj) => Ok(obj.values().cloned().collect()),
            Value::Array(arr) => Ok(arr.clone()),
            _ => Err(JqError::Type(format!(
                "{} has no values",
                self.type_name()
            ))),
        }
    }

    /// Index into value
    pub fn index(&self, idx: &Value) -> Result<Value> {
        match (self, idx) {
            (Value::Array(arr), Value::Number(n)) => {
                let i = n.as_i64().ok_or_else(|| {
                    JqError::Type("array index must be integer".to_string())
                })?;
                let len = arr.len() as i64;
                let actual_idx = if i < 0 { len + i } else { i };
                if actual_idx < 0 || actual_idx >= len {
                    Ok(Value::Null)
                } else {
                    Ok(arr[actual_idx as usize].clone())
                }
            }
            (Value::Object(obj), Value::String(key)) => {
                Ok(obj.get(key).cloned().unwrap_or(Value::Null))
            }
            (Value::Null, _) => Ok(Value::Null),
            _ => Err(JqError::InvalidIndex {
                value_type: self.type_name().to_string(),
                index_type: idx.type_name().to_string(),
            }),
        }
    }

    /// Iterate over value
    pub fn iter(&self) -> Result<Vec<Value>> {
        match self {
            Value::Array(arr) => Ok(arr.clone()),
            Value::Object(obj) => Ok(obj.values().cloned().collect()),
            Value::Null => Ok(vec![]),
            _ => Err(JqError::NotIterable(self.type_name().to_string())),
        }
    }

    /// Convert from serde_json::Value
    pub fn from_json(json: serde_json::Value) -> Self {
        match json {
            serde_json::Value::Null => Value::Null,
            serde_json::Value::Bool(b) => Value::Bool(b),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Value::Number(Number::Int(i))
                } else {
                    Value::Number(Number::Float(n.as_f64().unwrap_or(0.0)))
                }
            }
            serde_json::Value::String(s) => Value::String(s),
            serde_json::Value::Array(arr) => {
                Value::Array(arr.into_iter().map(Value::from_json).collect())
            }
            serde_json::Value::Object(obj) => {
                Value::Object(obj.into_iter().map(|(k, v)| (k, Value::from_json(v))).collect())
            }
        }
    }

    /// Convert to serde_json::Value
    pub fn to_json(&self) -> serde_json::Value {
        match self {
            Value::Null => serde_json::Value::Null,
            Value::Bool(b) => serde_json::Value::Bool(*b),
            Value::Number(n) => match n {
                Number::Int(i) => serde_json::Value::Number((*i).into()),
                Number::Float(f) => {
                    serde_json::Number::from_f64(*f)
                        .map(serde_json::Value::Number)
                        .unwrap_or(serde_json::Value::Null)
                }
            },
            Value::String(s) => serde_json::Value::String(s.clone()),
            Value::Array(arr) => {
                serde_json::Value::Array(arr.iter().map(|v| v.to_json()).collect())
            }
            Value::Object(obj) => {
                serde_json::Value::Object(
                    obj.iter().map(|(k, v)| (k.clone(), v.to_json())).collect()
                )
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Object(a), Value::Object(b)) => a == b,
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // jq ordering: null < false < true < numbers < strings < arrays < objects
        match (self, other) {
            (Value::Null, Value::Null) => Some(Ordering::Equal),
            (Value::Null, _) => Some(Ordering::Less),
            (_, Value::Null) => Some(Ordering::Greater),
            (Value::Bool(a), Value::Bool(b)) => a.partial_cmp(b),
            (Value::Bool(_), _) => Some(Ordering::Less),
            (_, Value::Bool(_)) => Some(Ordering::Greater),
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            (Value::Number(_), _) => Some(Ordering::Less),
            (_, Value::Number(_)) => Some(Ordering::Greater),
            (Value::String(a), Value::String(b)) => a.partial_cmp(b),
            (Value::String(_), _) => Some(Ordering::Less),
            (_, Value::String(_)) => Some(Ordering::Greater),
            (Value::Array(a), Value::Array(b)) => a.partial_cmp(b),
            (Value::Array(_), _) => Some(Ordering::Less),
            (_, Value::Array(_)) => Some(Ordering::Greater),
            (Value::Object(_), Value::Object(_)) => None, // Objects are not ordered
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Number(n) => match n {
                Number::Int(i) => write!(f, "{}", i),
                Number::Float(fl) => write!(f, "{}", fl),
            },
            Value::String(s) => write!(f, "{:?}", s),
            Value::Array(_) | Value::Object(_) => {
                write!(f, "{}", serde_json::to_string(&self.to_json()).unwrap_or_default())
            }
        }
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Number(Number::Int(i))
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Number(Number::Float(f))
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::String(s.to_string())
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(v: Vec<T>) -> Self {
        Value::Array(v.into_iter().map(Into::into).collect())
    }
}
