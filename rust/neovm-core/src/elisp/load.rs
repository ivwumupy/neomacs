//! File loading and module system (require/provide/load).

use std::path::{Path, PathBuf};
use super::error::EvalError;
use super::value::Value;

/// Search for a file in the load path.
pub fn find_file_in_load_path(name: &str, load_path: &[String]) -> Option<PathBuf> {
    let name_el = if name.ends_with(".el") || name.ends_with(".elc") {
        name.to_string()
    } else {
        format!("{}.el", name)
    };

    // Try absolute path first
    let path = Path::new(&name_el);
    if path.is_absolute() && path.exists() {
        return Some(path.to_path_buf());
    }

    // Search load-path
    for dir in load_path {
        let full = Path::new(dir).join(&name_el);
        if full.exists() {
            return Some(full);
        }
        // Also try without .el extension (exact name)
        if !name.ends_with(".el") && !name.ends_with(".elc") {
            let exact = Path::new(dir).join(name);
            if exact.exists() {
                return Some(exact);
            }
        }
    }

    None
}

/// Extract `load-path` from the evaluator's obarray as a Vec<String>.
pub fn get_load_path(obarray: &super::symbol::Obarray) -> Vec<String> {
    let val = obarray.symbol_value("load-path").cloned().unwrap_or(Value::Nil);
    super::value::list_to_vec(&val)
        .unwrap_or_default()
        .into_iter()
        .filter_map(|v| v.as_str().map(|s| s.to_string()))
        .collect()
}

/// Load and evaluate a file. Returns the last result.
pub fn load_file(
    eval: &mut super::eval::Evaluator,
    path: &Path,
) -> Result<Value, EvalError> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| EvalError::Signal {
            symbol: "file-error".to_string(),
            data: vec![Value::string(format!("Cannot read file: {}: {}", path.display(), e))],
        })?;

    // Check for lexical-binding file variable
    let first_line: &str = content.lines().next().unwrap_or("");
    if first_line.contains("lexical-binding: t") {
        eval.set_lexical_binding(true);
    }

    // Save and set load-file-name
    let old_load_file = eval.obarray().symbol_value("load-file-name").cloned();
    eval.set_variable("load-file-name", Value::string(path.to_string_lossy().to_string()));

    let forms = super::parser::parse_forms(&content)
        .map_err(|e| EvalError::Signal {
            symbol: "invalid-read-syntax".to_string(),
            data: vec![Value::string(format!("Parse error in {}: {:?}", path.display(), e))],
        })?;

    let mut last = Value::Nil;
    for form in &forms {
        last = eval.eval_expr(form)?;
    }

    // Restore load-file-name
    if let Some(old) = old_load_file {
        eval.set_variable("load-file-name", old);
    } else {
        eval.set_variable("load-file-name", Value::Nil);
    }

    Ok(last)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_file_nonexistent() {
        assert!(find_file_in_load_path("nonexistent", &[]).is_none());
    }

    #[test]
    fn load_path_extraction() {
        let mut ob = super::super::symbol::Obarray::new();
        ob.set_symbol_value("load-path", Value::list(vec![
            Value::string("/usr/share/emacs/lisp"),
            Value::string("/home/user/.emacs.d"),
        ]));
        let paths = get_load_path(&ob);
        assert_eq!(paths, vec!["/usr/share/emacs/lisp", "/home/user/.emacs.d"]);
    }
}
