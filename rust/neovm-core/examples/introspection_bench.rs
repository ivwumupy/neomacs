use neovm_core::elisp::{parse_forms, Evaluator};
use std::time::Instant;

const INTROSPECTION_FORMS: &[&str] = &[
    "(fboundp 'car)",
    "(fboundp 'when)",
    "(symbol-function 'car)",
    "(symbol-function 'when)",
    "(indirect-function 'car)",
    "(functionp 'car)",
    "(macrop 'when)",
    "(special-form-p 'if)",
    "(func-arity 'car)",
    "(condition-case err (funcall nil) (void-function (car err)))",
];

fn parse_iterations(arg: Option<&String>) -> Result<usize, String> {
    match arg {
        None => Ok(100_000),
        Some(raw) => raw
            .parse::<usize>()
            .map_err(|e| format!("invalid iterations '{}': {}", raw, e))
            .and_then(|n| {
                if n == 0 {
                    Err("iterations must be > 0".to_string())
                } else {
                    Ok(n)
                }
            }),
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 2 {
        eprintln!("usage: introspection_bench [iterations]");
        std::process::exit(2);
    }

    let iterations = match parse_iterations(args.get(1)) {
        Ok(n) => n,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(2);
        }
    };

    let source = INTROSPECTION_FORMS.join("\n");
    let forms = match parse_forms(&source) {
        Ok(forms) => forms,
        Err(err) => {
            eprintln!("failed to parse benchmark forms: {err}");
            std::process::exit(1);
        }
    };

    let mut evaluator = Evaluator::new();
    let total_ops = iterations.saturating_mul(forms.len());
    let start = Instant::now();

    for _ in 0..iterations {
        for form in &forms {
            if let Err(err) = evaluator.eval_expr(form) {
                eprintln!("benchmark form evaluation failed: {err}");
                std::process::exit(1);
            }
        }
    }

    let elapsed = start.elapsed();
    let elapsed_ms = elapsed.as_secs_f64() * 1000.0;
    let ns_per_op = (elapsed.as_secs_f64() * 1_000_000_000.0) / total_ops as f64;
    let ops_per_sec = total_ops as f64 / elapsed.as_secs_f64();

    println!("iterations: {iterations}");
    println!("forms_per_iteration: {}", forms.len());
    println!("total_ops: {total_ops}");
    println!("elapsed_ms: {:.3}", elapsed_ms);
    println!("ns_per_op: {:.1}", ns_per_op);
    println!("ops_per_sec: {:.0}", ops_per_sec);
}
