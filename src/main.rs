use std::error::Error;

use language::Compiler;

fn main() -> Result<(), Box<dyn Error>> {
    let mut compiler = Compiler::new()?;
    compiler.compile("input")?;

    Ok(())
}
