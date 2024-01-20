use crate::{codemap::CodeMap, fs::FileSystem};

#[derive(Debug)]
pub struct Resolver<'a, F> {
    codemap: &'a mut CodeMap<F>,
}

impl<'a, F: FileSystem> Resolver<'a, F> {
    pub fn new(codemap: &'a mut CodeMap<F>) -> Self {
        Resolver { codemap }
    }
}
