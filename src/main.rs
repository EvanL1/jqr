//! jqr: A jq implementation in Rust

use clap::Parser;
use jqr::{run, Value};
use std::io::{self, BufRead, Read};

#[derive(Parser)]
#[command(name = "jqr")]
#[command(author = "jqr authors")]
#[command(version = "0.1.0")]
#[command(about = "Command-line JSON processor", long_about = None)]
struct Args {
    /// The jq filter to apply
    #[arg(default_value = ".")]
    filter: String,

    /// Input files (use - for stdin)
    #[arg(default_value = "-")]
    files: Vec<String>,

    /// Output raw strings, not JSON encoded
    #[arg(short, long)]
    raw_output: bool,

    /// Read raw strings, not JSON
    #[arg(short = 'R', long)]
    raw_input: bool,

    /// Slurp all inputs into an array
    #[arg(short, long)]
    slurp: bool,

    /// Compact output (no pretty printing)
    #[arg(short, long)]
    compact_output: bool,

    /// Don't output newline after each output
    #[arg(short = 'j', long)]
    join_output: bool,

    /// Colorize output (auto/always/never)
    #[arg(short = 'C', long, default_value = "auto")]
    color: String,

    /// Monochrome output (no colors)
    #[arg(short = 'M', long)]
    monochrome: bool,

    /// Sort object keys in output
    #[arg(short = 'S', long)]
    sort_keys: bool,

    /// Read each line as a JSON value
    #[arg(short = 'n', long)]
    null_input: bool,

    /// Exit with error if result is false or null
    #[arg(short, long)]
    exit_status: bool,
}

fn main() {
    let args = Args::parse();

    if let Err(e) = run_jq(&args) {
        eprintln!("jqr: {}", e);
        std::process::exit(1);
    }
}

fn run_jq(args: &Args) -> Result<(), Box<dyn std::error::Error>> {
    let inputs = read_inputs(args)?;
    let filter = &args.filter;

    let mut exit_code = 0;
    let mut first = true;

    for input in inputs {
        match run(filter, &input) {
            Ok(results) => {
                for result in results {
                    if args.exit_status {
                        match &result {
                            Value::Null | Value::Bool(false) => exit_code = 1,
                            _ => {}
                        }
                    }

                    if !first && !args.join_output {
                        // Already printed separator
                    }
                    first = false;

                    print_value(&result, args);

                    if !args.join_output {
                        println!();
                    }
                }
            }
            Err(e) => {
                eprintln!("jqr: error: {}", e);
                exit_code = 1;
            }
        }
    }

    if exit_code != 0 {
        std::process::exit(exit_code);
    }

    Ok(())
}

fn read_inputs(args: &Args) -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let mut inputs = Vec::new();

    if args.null_input {
        return Ok(vec!["null".to_string()]);
    }

    for file in &args.files {
        let content = if file == "-" {
            let mut buffer = String::new();
            io::stdin().read_to_string(&mut buffer)?;
            buffer
        } else {
            std::fs::read_to_string(file)?
        };

        if args.raw_input {
            // Read as raw strings (one per line)
            for line in content.lines() {
                inputs.push(format!("{:?}", line));
            }
        } else if args.slurp {
            // Parse all JSON values and wrap in array
            let values: Vec<serde_json::Value> = parse_json_stream(&content)?;
            let array = serde_json::Value::Array(values);
            inputs.push(serde_json::to_string(&array)?);
        } else {
            // Parse as JSON stream
            for json_str in split_json_values(&content)? {
                inputs.push(json_str);
            }
        }
    }

    Ok(inputs)
}

fn parse_json_stream(input: &str) -> Result<Vec<serde_json::Value>, Box<dyn std::error::Error>> {
    let mut values = Vec::new();
    let mut reader = io::Cursor::new(input);
    let mut buf = String::new();

    loop {
        buf.clear();
        let bytes_read = reader.read_line(&mut buf)?;
        if bytes_read == 0 {
            break;
        }
        let trimmed = buf.trim();
        if trimmed.is_empty() {
            continue;
        }
        let value: serde_json::Value = serde_json::from_str(trimmed)?;
        values.push(value);
    }

    Ok(values)
}

fn split_json_values(input: &str) -> Result<Vec<String>, Box<dyn std::error::Error>> {
    // Simple approach: try to parse whitespace-separated JSON values
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return Ok(Vec::new());
    }

    // Try parsing as a single value first
    if serde_json::from_str::<serde_json::Value>(trimmed).is_ok() {
        return Ok(vec![trimmed.to_string()]);
    }

    // Otherwise, try line-by-line
    let mut values = Vec::new();
    for line in trimmed.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        // Validate JSON
        serde_json::from_str::<serde_json::Value>(line)?;
        values.push(line.to_string());
    }

    Ok(values)
}

fn print_value(value: &Value, args: &Args) {
    match value {
        Value::String(s) if args.raw_output => {
            print!("{}", s);
        }
        _ => {
            let json = value.to_json();
            let output = if args.compact_output {
                serde_json::to_string(&json).unwrap_or_default()
            } else {
                serde_json::to_string_pretty(&json).unwrap_or_default()
            };

            // Handle sort_keys if needed
            if args.sort_keys {
                if let Ok(sorted) = sort_json_keys(&json) {
                    let sorted_output = if args.compact_output {
                        serde_json::to_string(&sorted).unwrap_or_default()
                    } else {
                        serde_json::to_string_pretty(&sorted).unwrap_or_default()
                    };
                    print!("{}", sorted_output);
                    return;
                }
            }

            print!("{}", output);
        }
    }
}

fn sort_json_keys(value: &serde_json::Value) -> Result<serde_json::Value, ()> {
    match value {
        serde_json::Value::Object(obj) => {
            let mut sorted: serde_json::Map<String, serde_json::Value> = serde_json::Map::new();
            let mut keys: Vec<&String> = obj.keys().collect();
            keys.sort();
            for key in keys {
                if let Some(v) = obj.get(key) {
                    sorted.insert(key.clone(), sort_json_keys(v)?);
                }
            }
            Ok(serde_json::Value::Object(sorted))
        }
        serde_json::Value::Array(arr) => {
            let sorted: Vec<serde_json::Value> =
                arr.iter().map(sort_json_keys).collect::<Result<_, _>>()?;
            Ok(serde_json::Value::Array(sorted))
        }
        other => Ok(other.clone()),
    }
}
