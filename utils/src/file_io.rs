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

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;

    const DIR_NAME: &str = "temp";
    const FILE_NAME: &str = "Test.sol";
    const FILE_CONTENT: &[u8] = b"contract Test{}";

    async fn create_temp_file() -> io::Result<()> {
        let file_path = Path::new(DIR_NAME).join(FILE_NAME);

        if fs::try_exists(DIR_NAME).await? || fs::try_exists(&file_path).await? {
            return Ok(());
        }

        fs::create_dir(DIR_NAME).await?;

        {
            fs::File::create(&file_path).await?;
            fs::write(file_path, FILE_CONTENT).await?;
        }

        Ok(())
    }

    async fn remove_temp_file() -> io::Result<()> {
        let file_path = Path::new(DIR_NAME).join(FILE_NAME);

        fs::remove_file(file_path).await?;
        fs::remove_dir(DIR_NAME).await?;

        Ok(())
    }

    #[test]
    fn test_solidity_file() {
        let mut path = "foo.sol";

        assert_eq!(is_solidity_file(path), true);

        path = "foo.ml";

        assert_eq!(is_solidity_file(path), false);
    }

    #[tokio::test]
    async fn test_read_from_file() -> io::Result<()> {
        create_temp_file().await?;

        let file_path = Path::new(DIR_NAME).join(FILE_NAME);

        let contents = read_from_file(file_path).await?;

        assert_eq!(contents.as_bytes(), FILE_CONTENT);

        remove_temp_file().await?;

        Ok(())
    }
}
