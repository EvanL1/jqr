//! Abstract Syntax Tree for jq expressions

use crate::value::Value;

/// A jq program consisting of optional definitions and a main expression
#[derive(Clone, Debug)]
pub struct Program {
    pub defs: Vec<FuncDef>,
    pub expr: Expr,
}

/// A function definition
#[derive(Clone, Debug)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Expr>,
}

/// Binary operators
#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    // Logical
    And,
    Or,

    // Alternative
    Alt, // //
}

/// Unary operators
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

/// Update operators
#[derive(Clone, Debug, PartialEq)]
pub enum UpdateOp {
    Assign, // =
    Update, // |=
    AddEq,  // +=
    SubEq,  // -=
    MulEq,  // *=
    DivEq,  // /=
    ModEq,  // %=
    AltEq,  // //=
}

/// jq expression AST
#[derive(Clone, Debug)]
pub enum Expr {
    /// Identity: .
    Identity,

    /// Recursive descent: ..
    RecursiveDescent,

    /// Literal value
    Literal(Value),

    /// Field access: .foo or .["foo"]
    Field(String),

    /// Optional field access: .foo?
    OptionalField(String),

    /// Index: .[expr]
    Index(Box<Expr>),

    /// Optional index: .[expr]?
    OptionalIndex(Box<Expr>),

    /// Slice: .[start:end]
    Slice {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
    },

    /// Iterator: .[]
    Iterator,

    /// Optional iterator: .[]?
    OptionalIterator,

    /// Pipe: expr | expr
    Pipe(Box<Expr>, Box<Expr>),

    /// Comma: expr, expr
    Comma(Box<Expr>, Box<Expr>),

    /// Array construction: [expr]
    Array(Option<Box<Expr>>),

    /// Object construction: {key: value, ...}
    Object(Vec<ObjectEntry>),

    /// Binary operation: expr op expr
    BinOp(BinOp, Box<Expr>, Box<Expr>),

    /// Unary operation: op expr
    UnaryOp(UnaryOp, Box<Expr>),

    /// Conditional: if cond then t else f end
    If {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },

    /// Try-catch: try expr catch handler
    TryCatch {
        try_expr: Box<Expr>,
        catch_expr: Option<Box<Expr>>,
    },

    /// Reduce: reduce expr as $var (init; update)
    Reduce {
        expr: Box<Expr>,
        var: String,
        init: Box<Expr>,
        update: Box<Expr>,
    },

    /// Foreach: foreach expr as $var (init; update; extract)
    Foreach {
        expr: Box<Expr>,
        var: String,
        init: Box<Expr>,
        update: Box<Expr>,
        extract: Option<Box<Expr>>,
    },

    /// Function call: name or name(args)
    FuncCall { name: String, args: Vec<Expr> },

    /// Variable reference: $var
    Var(String),

    /// Variable binding: expr as $var | body
    As {
        expr: Box<Expr>,
        var: String,
        body: Box<Expr>,
    },

    /// Update expression: path op= expr
    Update {
        path: Box<Expr>,
        op: UpdateOp,
        value: Box<Expr>,
    },

    /// String interpolation: "text \(expr) more"
    StringInterpolation(Vec<StringPart>),

    /// Optional wrapper: expr?
    Optional(Box<Expr>),

    /// Label-break: label $name | expr
    Label { name: String, body: Box<Expr> },

    /// Break: break $name
    Break(String),

    /// Error: error or error(msg)
    Error(Option<Box<Expr>>),

    /// Format string: @base64, @uri, etc.
    Format(String),

    /// Path expression: path(expr)
    Path(Box<Expr>),

    /// Get path value: getpath(path)
    GetPath(Box<Expr>),

    /// Set path value: setpath(path; value)
    SetPath { path: Box<Expr>, value: Box<Expr> },

    /// Delete path: delpaths(paths)
    DelPaths(Box<Expr>),

    /// Limit: limit(n; expr)
    Limit { n: Box<Expr>, expr: Box<Expr> },

    /// First: first(expr) or first
    First(Option<Box<Expr>>),

    /// Last: last(expr) or last
    Last(Option<Box<Expr>>),

    /// Nth: nth(n; expr) or nth(n)
    Nth {
        n: Box<Expr>,
        expr: Option<Box<Expr>>,
    },

    /// Range: range(n) or range(from; to) or range(from; to; step)
    Range {
        from: Box<Expr>,
        to: Option<Box<Expr>>,
        step: Option<Box<Expr>>,
    },

    /// While: while(cond; update)
    While { cond: Box<Expr>, update: Box<Expr> },

    /// Until: until(cond; update)
    Until { cond: Box<Expr>, update: Box<Expr> },

    /// Recurse: recurse or recurse(f)
    Recurse(Option<Box<Expr>>),

    /// Walk: walk(f)
    Walk(Box<Expr>),

    /// Env: env or $ENV
    Env,

    /// Debug: debug or debug(msg)
    Debug(Option<Box<Expr>>),

    /// Input: input
    Input,

    /// Inputs: inputs
    Inputs,

    /// Function definition in expression
    FuncDef { def: FuncDef, body: Box<Expr> },
}

/// Object entry for object construction
#[derive(Clone, Debug)]
pub enum ObjectEntry {
    /// Regular key-value: "key": value or (expr): value
    KeyValue { key: ObjectKey, value: Box<Expr> },
    /// Identifier shorthand: {foo} means {foo: .foo}
    Ident(String),
}

/// Object key
#[derive(Clone, Debug)]
pub enum ObjectKey {
    /// Literal string key
    Literal(String),
    /// Computed key from expression
    Computed(Box<Expr>),
    /// Identifier key (unquoted)
    Ident(String),
}

/// Part of a string interpolation
#[derive(Clone, Debug)]
pub enum StringPart {
    /// Literal string content
    Literal(String),
    /// Interpolated expression
    Expr(Box<Expr>),
}
