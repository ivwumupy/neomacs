//! XML and compression stubs for the Elisp interpreter.
//!
//! Provides stub implementations for:
//! - `libxml-parse-html-region`, `libxml-parse-xml-region`, `libxml-available-p`
//! - `zlib-available-p`, `zlib-decompress-region`
//!
//! These are stubbed because libxml and zlib are not available in pure Rust Elisp yet.

use super::error::{signal, EvalResult, Flow};
use super::value::*;

// ---------------------------------------------------------------------------
// Argument helpers
// ---------------------------------------------------------------------------

fn expect_args(name: &str, args: &[Value], n: usize) -> Result<(), Flow> {
    if args.len() != n {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_min_args(name: &str, args: &[Value], min: usize) -> Result<(), Flow> {
    if args.len() < min {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_max_args(name: &str, args: &[Value], max: usize) -> Result<(), Flow> {
    if args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// (libxml-parse-html-region START END &optional BASE-URL DISCARD-COMMENTS)
/// Stub: returns nil (libxml not available in pure Rust yet).
pub(crate) fn builtin_libxml_parse_html_region(args: Vec<Value>) -> EvalResult {
    expect_min_args("libxml-parse-html-region", &args, 2)?;
    expect_max_args("libxml-parse-html-region", &args, 4)?;
    // Stub: libxml parsing not implemented
    Ok(Value::Nil)
}

/// (libxml-parse-xml-region START END &optional BASE-URL DISCARD-COMMENTS)
/// Stub: returns nil (libxml not available in pure Rust yet).
pub(crate) fn builtin_libxml_parse_xml_region(args: Vec<Value>) -> EvalResult {
    expect_min_args("libxml-parse-xml-region", &args, 2)?;
    expect_max_args("libxml-parse-xml-region", &args, 4)?;
    // Stub: libxml parsing not implemented
    Ok(Value::Nil)
}

/// (libxml-available-p)
/// Returns nil (libxml not available in pure Rust yet).
pub(crate) fn builtin_libxml_available_p(args: Vec<Value>) -> EvalResult {
    expect_args("libxml-available-p", &args, 0)?;
    Ok(Value::Nil)
}

/// (zlib-available-p)
/// Returns nil (zlib not available in pure Rust yet).
pub(crate) fn builtin_zlib_available_p(args: Vec<Value>) -> EvalResult {
    expect_args("zlib-available-p", &args, 0)?;
    Ok(Value::Nil)
}

/// (zlib-decompress-region START END)
/// Stub: returns nil (zlib decompression not implemented in pure Rust yet).
pub(crate) fn builtin_zlib_decompress_region(args: Vec<Value>) -> EvalResult {
    expect_args("zlib-decompress-region", &args, 2)?;
    // Stub: zlib decompression not implemented
    Ok(Value::Nil)
}
