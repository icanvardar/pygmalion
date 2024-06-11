use std::path::Path;

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

pub async fn read_from_file(path: &Path) -> io::Result<String> {
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

pub async fn write_to_file(path: &Path, content: &[u8]) -> io::Result<()> {
    check_file_existency(path).await?;

    return Ok(fs::write(path, content).await?);
}

async fn check_file_existency(path: &Path) -> io::Result<()> {
    if fs::try_exists(path).await? {
        return Ok(());
    }

    Err(io::Error::new(
        io::ErrorKind::NotFound,
        "No input or destination provided",
    ))
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;

    const PSEUDO_FILE_CONTENT: &[u8] = b"contract Test{}";

    async fn create_temp_file(dir_name: &str, file_name: &str) -> io::Result<()> {
        let file_path = Path::new(dir_name).join(file_name);

        if fs::try_exists(dir_name).await? || fs::try_exists(&file_path).await? {
            return Ok(());
        }

        fs::create_dir(dir_name).await?;

        {
            fs::File::create(&file_path).await?;
            fs::write(file_path, PSEUDO_FILE_CONTENT).await?;
        }

        Ok(())
    }

    async fn remove_temp_file(dir_name: &str, file_name: &str) -> io::Result<()> {
        let file_path = Path::new(dir_name).join(file_name);

        fs::remove_file(file_path).await?;
        fs::remove_dir(dir_name).await?;

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
        let dir_name = "temp_1";
        let file_name = "temp_1.sol";

        let file_path = Path::new(dir_name).join(file_name);

        create_temp_file(dir_name, file_name).await?;

        let contents = read_from_file(&file_path).await?;

        assert_eq!(contents.as_bytes(), PSEUDO_FILE_CONTENT);

        remove_temp_file(dir_name, file_name).await?;

        Ok(())
    }

    #[tokio::test]
    async fn test_check_file_existency() -> io::Result<()> {
        // NOTE: I think Rust recognizes paths on workspace basis.
        // I need to keep in my mind that paths start from utils.
        let true_path = Path::new("./src/lib.rs").canonicalize()?;
        println!("absolute path: {:?}", true_path);

        check_file_existency(&true_path).await?;

        let false_path = Path::new("some_wrong_path.js");
        let e = check_file_existency(&false_path).await.unwrap_err();

        assert_eq!(e.kind(), io::ErrorKind::NotFound);

        Ok(())
    }

    #[tokio::test]
    async fn test_write_to_file() -> io::Result<()> {
        let dir_name = "temp_2";
        let file_name = "temp_2.sol";

        let file_path = Path::new(dir_name).join(file_name);

        create_temp_file(dir_name, file_name).await?;

        let new_content = b"contract Foo {}";
        write_to_file(&file_path, new_content).await?;

        let contents = read_from_file(&file_path).await?;

        assert_eq!(contents.as_bytes(), new_content);

        remove_temp_file(dir_name, file_name).await?;

        Ok(())
    }
}
