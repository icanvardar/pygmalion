use tokio::{fs, io};

fn is_solidity_file(path: &str) -> bool {
    if let Some((i, _)) = path.char_indices().rev().nth(4) {
        let extension = &path[i..];

        if extension != ".sol" {
            return false;
        }

        return true;
    } else {
        return false;
    }
}

pub async fn read_from_file(path: &str) -> io::Result<String> {
    if !is_solidity_file(path) {
        panic!("Files other than .sol extension are not supported!");
    }

    let contents = fs::read_to_string(path).await?;

    return Ok(contents);
}
