use std::path::PathBuf;

use tokio::{fs, io};

fn is_solidity_file(path: &str) -> bool {
    if let Some((i, _)) = path.char_indices().rev().nth(3) {
        let extension = &path[i..];

        if extension != ".sol" {
            return false;
        }

        return true;
    } else {
        return false;
    }
}

pub async fn read_from_file(path: PathBuf) -> io::Result<String> {
    // TODO: Use custom error instead.
    if let Some(path_str) = path.to_str() {
        if !is_solidity_file(path_str) {
            panic!("Files other than .sol extension are not supported!");
        }
    } else {
        panic!("Invalid path!");
    }

    let contents = fs::read_to_string(path).await?;

    return Ok(contents);
}

pub async fn write_to_file(path: &str, content: &[u8]) -> io::Result<()> {
    return Ok(fs::write(path, content).await?);
}
