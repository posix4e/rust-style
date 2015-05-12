use std::fs::{self, Metadata, ReadDir};
use std::io;
use std::path::{Path, PathBuf};


pub trait PathExts {
    fn _metadata(&self) -> io::Result<Metadata>;
    // fn _symlink_metadata(&self) -> io::Result<Metadata>;
    fn _canonicalize(&self) -> io::Result<PathBuf>;
    fn _read_link(&self) -> io::Result<PathBuf>;
    fn _read_dir(&self) -> io::Result<ReadDir>;
    fn _exists(&self) -> bool;
    fn _is_file(&self) -> bool;
    fn _is_dir(&self) -> bool;
}
impl PathExts for Path {
    fn _metadata(&self) -> io::Result<Metadata> { fs::metadata(self) }
    // fn _symlink_metadata(&self) -> io::Result<Metadata> { fs::symlink_metadata(self) }
    // fn _canonicalize(&self) -> io::Result<PathBuf> { fs::canonicalize(self) }
    fn _canonicalize(&self) -> io::Result<PathBuf> { unimplemented!() }

    fn _read_link(&self) -> io::Result<PathBuf> { fs::read_link(self) }
    fn _read_dir(&self) -> io::Result<ReadDir> { fs::read_dir(self) }
    fn _exists(&self) -> bool { fs::metadata(self).is_ok() }

    fn _is_file(&self) -> bool {
        fs::metadata(self).map(|s| s.is_file()).unwrap_or(false)
    }

    fn _is_dir(&self) -> bool {
        fs::metadata(self).map(|s| s.is_dir()).unwrap_or(false)
    }
}
