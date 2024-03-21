use std::{
    env, fs,
    io::{stdin, stdout, Write},
    process::exit,
};

use ariadne::{Report, Source};
use language::{hir::Hir, parser::parse, span::Span, types::TypeChecker};

// TODO:
// - Parser resilience
// - HIR & type table formatting
// - Code generation

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() - 1 {
        0 => {
            let mut stdout = stdout();
            print!(
                "{} v{}\n> ",
                env!("CARGO_PKG_NAME"),
                env!("CARGO_PKG_VERSION")
            );
            stdout.flush().expect("failed to flush stdout");

            for line in stdin().lines() {
                let input = line.expect("failed to read line");
                run(&input);

                print!("> ");
                stdout.flush().expect("failed to flush stdout");
            }
        }
        1 => {
            let input = fs::read_to_string(&args[1]).expect("failed to read file");
            run(&input);
        }
        _ => {
            eprintln!("Usage: {} [FILE?]", &args[0]);
            exit(1);
        }
    }
}

fn run(input: &str) {
    let ast = match parse(input) {
        Ok(ast) => ast,
        Err(errors) => {
            report(input, errors);
            return;
        }
    };

    dbg!(&ast);

    let mut builder = Hir::builder();
    builder.lower(ast);
    let hir = match builder.finish() {
        Ok(hir) => hir,
        Err(errors) => {
            report(input, errors);
            return;
        }
    };

    dbg!(&hir);

    let mut typechecker = TypeChecker::new(&hir);
    typechecker.check();
    let types = typechecker.finish().expect("type error");
    dbg!(&types);
}

fn report<'a>(input: &'a str, errors: Vec<Report<'a, Span>>) {
    for error in errors {
        error
            .eprint(Source::from(input))
            .expect("failed to print error diagnostic");
    }
}
