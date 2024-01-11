use std::error::Error;

use language::Compiler;

fn main() -> Result<(), Box<dyn Error>> {
    let compiler = Compiler::new()?;

    Ok(())
}
