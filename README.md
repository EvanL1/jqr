# jqr

A jq implementation in Rust.

## Installation

```bash
curl -fsSL https://raw.githubusercontent.com/EvanL1/jqr/master/install.sh | sh
```

Or build from source:

```bash
cargo install --path .
```

## Usage

```bash
echo '{"name": "Alice", "age": 30}' | jqr '.name'
# "Alice"

echo '[1, 2, 3]' | jqr 'map(. * 2)'
# [2, 4, 6]

echo '"hello world"' | jqr 'gsub("world"; "jqr")'
# "hello jqr"
```

## Supported Features

- Basic: `.`, `.foo`, `.[0]`, `.[]`, `|`, `,`
- Operators: `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `>`, `and`, `or`, `not`, `//`
- Constructors: `[]`, `{}`
- Conditionals: `if-then-else`, `try-catch`
- Variables: `as $x`, `$x`
- Functions: `def f: ...;`
- Builtins: `map`, `select`, `keys`, `values`, `length`, `type`, `sort`, `unique`, `group_by`, `flatten`, `reverse`, `contains`, `split`, `join`, `to_entries`, `from_entries`, `range`, `reduce`, `recurse`, `add`, `min`, `max`, `floor`, `ceil`, `round`, `sqrt`, etc.
- Regex: `test`, `match`, `capture`, `scan`, `sub`, `gsub`, `splits`

## License

MIT
