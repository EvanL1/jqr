//! JSON Value type for jq - using Rc for zero-copy sharing

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;

use crate::error::{JqError, Result};

/// Represents a JSON value in jq
/// Uses Rc for String/Array/Object to enable O(1) cloning
#[derive(Clone, Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Number(Number),
    String(Rc<String>),
    Array(Rc<Vec<Value>>),
    Object(Rc<IndexMap<String, Value>>),
}

/// Number type supporting both integers and floats
#[derive(Clone, Debug)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl Number {
    #[inline(always)]
    pub fn as_f64(&self) -> f64 {
        match self {
            Number::Int(i) => *i as f64,
            Number::Float(f) => *f,
        }
    }

    #[inline(always)]
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

    #[inline(always)]
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
    // Constructors for convenience - inlined for hot paths
    #[inline(always)]
    pub fn string(s: impl Into<String>) -> Self {
        Value::String(Rc::new(s.into()))
    }

    #[inline(always)]
    pub fn array(items: Vec<Value>) -> Self {
        Value::Array(Rc::new(items))
    }

    #[inline(always)]
    pub fn object(map: IndexMap<String, Value>) -> Self {
        Value::Object(Rc::new(map))
    }

    /// Get the type name of this value
    #[inline(always)]
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
    #[inline(always)]
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

    /// Check if value is truthy (jq semantics: false and null are falsy)
    #[inline(always)]
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Null | Value::Bool(false))
    }

    /// Get length of value
    #[inline]
    pub fn length(&self) -> Result<usize> {
        match self {
            Value::Null => Ok(0),
            Value::String(s) => Ok(s.chars().count()),
            Value::Array(arr) => Ok(arr.len()),
            Value::Object(obj) => Ok(obj.len()),
            _ => Err(JqError::Type(format!("{} has no length", self.type_name()))),
        }
    }

    /// Get keys of object
    #[inline]
    pub fn keys(&self) -> Result<Vec<Value>> {
        match self {
            Value::Object(obj) => {
                let mut keys = Vec::with_capacity(obj.len());
                for k in obj.keys() {
                    keys.push(Value::string(k.as_str()));
                }
                Ok(keys)
            }
            Value::Array(arr) => {
                let mut keys = Vec::with_capacity(arr.len());
                for i in 0..arr.len() {
                    keys.push(Value::Number(Number::Int(i as i64)));
                }
                Ok(keys)
            }
            _ => Err(JqError::Type(format!("{} has no keys", self.type_name()))),
        }
    }

    /// Get values of object/array
    #[inline]
    pub fn values(&self) -> Result<Vec<Value>> {
        match self {
            Value::Object(obj) => {
                let mut values = Vec::with_capacity(obj.len());
                values.extend(obj.values().cloned());
                Ok(values)
            }
            Value::Array(arr) => {
                let mut values = Vec::with_capacity(arr.len());
                values.extend(arr.iter().cloned());
                Ok(values)
            }
            _ => Err(JqError::Type(format!("{} has no values", self.type_name()))),
        }
    }

    /// Index into value
    #[inline]
    pub fn index(&self, idx: &Value) -> Result<Value> {
        match (self, idx) {
            (Value::Array(arr), Value::Number(n)) => {
                let i = n
                    .as_i64()
                    .ok_or_else(|| JqError::Type("array index must be integer".to_string()))?;
                let len = arr.len() as i64;
                let actual_idx = if i < 0 { len + i } else { i };
                if actual_idx < 0 || actual_idx >= len {
                    Ok(Value::Null)
                } else {
                    Ok(arr[actual_idx as usize].clone())
                }
            }
            (Value::Object(obj), Value::String(key)) => {
                Ok(obj.get(key.as_str()).cloned().unwrap_or(Value::Null))
            }
            (Value::Null, _) => Ok(Value::Null),
            _ => Err(JqError::InvalidIndex {
                value_type: self.type_name().to_string(),
                index_type: idx.type_name().to_string(),
            }),
        }
    }

    /// Iterate over value
    #[inline]
    pub fn iter(&self) -> Result<Vec<Value>> {
        match self {
            Value::Array(arr) => {
                let mut result = Vec::with_capacity(arr.len());
                result.extend(arr.iter().cloned());
                Ok(result)
            }
            Value::Object(obj) => {
                let mut result = Vec::with_capacity(obj.len());
                result.extend(obj.values().cloned());
                Ok(result)
            }
            Value::Null => Ok(Vec::new()),
            _ => Err(JqError::NotIterable(self.type_name().to_string())),
        }
    }

    /// Convert from serde_json::Value
    #[inline]
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
            serde_json::Value::String(s) => Value::string(s),
            serde_json::Value::Array(arr) => {
                Value::array(arr.into_iter().map(Value::from_json).collect())
            }
            serde_json::Value::Object(obj) => Value::object(
                obj.into_iter()
                    .map(|(k, v)| (k, Value::from_json(v)))
                    .collect(),
            ),
        }
    }

    /// Convert to serde_json::Value
    pub fn to_json(&self) -> serde_json::Value {
        match self {
            Value::Null => serde_json::Value::Null,
            Value::Bool(b) => serde_json::Value::Bool(*b),
            Value::Number(n) => match n {
                Number::Int(i) => serde_json::Value::Number((*i).into()),
                Number::Float(f) => serde_json::Number::from_f64(*f)
                    .map(serde_json::Value::Number)
                    .unwrap_or(serde_json::Value::Null),
            },
            Value::String(s) => serde_json::Value::String(s.as_ref().clone()),
            Value::Array(arr) => {
                let mut result = Vec::with_capacity(arr.len());
                for v in arr.iter() {
                    result.push(v.to_json());
                }
                serde_json::Value::Array(result)
            }
            Value::Object(obj) => {
                let mut map = serde_json::Map::with_capacity(obj.len());
                for (k, v) in obj.iter() {
                    map.insert(k.clone(), v.to_json());
                }
                serde_json::Value::Object(map)
            }
        }
    }

    /// Get inner string reference
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s.as_str()),
            _ => None,
        }
    }

    /// Get inner array reference
    pub fn as_array(&self) -> Option<&Vec<Value>> {
        match self {
            Value::Array(arr) => Some(arr.as_ref()),
            _ => None,
        }
    }

    /// Get inner object reference
    pub fn as_object(&self) -> Option<&IndexMap<String, Value>> {
        match self {
            Value::Object(obj) => Some(obj.as_ref()),
            _ => None,
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
            Value::String(s) => write!(f, "{:?}", &**s),
            Value::Array(_) | Value::Object(_) => {
                write!(
                    f,
                    "{}",
                    serde_json::to_string(&self.to_json()).unwrap_or_default()
                )
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
        Value::string(s)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::string(s)
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(v: Vec<T>) -> Self {
        Value::array(v.into_iter().map(Into::into).collect())
    }
}

// Serde support - manual implementation since we use Rc
impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_json().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let json = serde_json::Value::deserialize(deserializer)?;
        Ok(Value::from_json(json))
    }
}
