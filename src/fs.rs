use std::{
    env,
    fs::File,
    io::{Error, Read, Write},
    path::{Path, PathBuf},
};

pub trait FileSystem {
    type Error: std::fmt::Debug + std::error::Error;

    fn read(&mut self, path: &Path) -> Result<Vec<u8>, Self::Error>;
    fn write(&mut self, path: &Path, contents: &[u8]) -> Result<(), Self::Error>;
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LocalFs {
    root: PathBuf,
}

impl LocalFs {
    pub fn new(root: impl AsRef<Path>) -> Self {
        LocalFs {
            root: root.as_ref().to_path_buf(),
        }
    }

    pub fn new_in_current_dir() -> Result<Self, Error> {
        Ok(LocalFs {
            root: env::current_dir()?,
        })
    }
}

impl FileSystem for LocalFs {
    type Error = Error;

    fn read(&mut self, path: &Path) -> Result<Vec<u8>, Self::Error> {
        let mut file = File::open(self.root.join(path))?;
        let mut buf = Vec::with_capacity(file.metadata()?.len() as usize);
        file.read_to_end(&mut buf)?;
        Ok(buf)
    }

    fn write(&mut self, path: &Path, contents: &[u8]) -> Result<(), Self::Error> {
        let mut file = File::create(self.root.join(path))?;
        file.write_all(contents)?;
        Ok(())
    }
}

#[cfg(test)]
pub(crate) use test_fs::TestFs;

#[cfg(test)]
mod test_fs {
    use std::{collections::HashMap, path::PathBuf};

    use thiserror::Error;

    use super::*;

    #[derive(Debug, Default, Clone, PartialEq, Eq)]
    pub struct TestFs {
        files: HashMap<PathBuf, Vec<u8>>,
    }

    impl TestFs {
        pub fn new() -> Self {
            TestFs::default()
        }
    }

    #[derive(Debug, Error)]
    #[error("file {0} not found")]
    pub struct NotFound(PathBuf);

    impl FileSystem for TestFs {
        type Error = NotFound;

        fn read(&mut self, path: &Path) -> Result<Vec<u8>, Self::Error> {
            self.files
                .get(path)
                .cloned()
                .ok_or_else(|| NotFound(path.to_path_buf()))
        }

        fn write(&mut self, path: &Path, contents: &[u8]) -> Result<(), Self::Error> {
            self.files.insert(path.to_path_buf(), contents.to_vec());
            Ok(())
        }
    }
}
