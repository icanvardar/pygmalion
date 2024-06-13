use std::path::Path;

use super::file_io::read_from_file;
use clap::Parser;
use tokio::io;

/// Pygmalion, yet another solidity lexer and stuff!
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// Path of Solidity file
    #[arg(short, long)]
    path: Option<String>,

    /// Short input to analyze  
    #[arg(short, long)]
    code: Option<String>,

    /// Lexical analysis output destination
    #[arg(short, long)]
    out: Option<String>,
}

pub async fn get_app_args(cli: Cli) -> io::Result<(String, String)> {
    let input: String;

    if let Some(code) = cli.code {
        input = code;
    } else if let Some(path) = cli.path {
        let file_path = Path::new(&path).canonicalize()?;
        input = read_from_file(&file_path).await?;
    } else {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            "No input or destination provided",
        ));
    }

    Ok((input, cli.out.unwrap_or_default()))
}

#[cfg(test)]
mod tests {
    use std::ffi::OsString;

    use tokio::fs;

    use super::*;

    const PSEUDO_PATH: &str = "./temp/Test.sol";
    const PSEUDO_CODE: &str = "contract Test {\
            bool test = true;
        }";
    const PSEUDO_OUT: &str = "./temp/test_out.sol";

    async fn create_temp_file(dir_name: &str, file_name: &str) -> io::Result<()> {
        let file_path = Path::new(dir_name).join(file_name);

        if fs::try_exists(dir_name).await? || fs::try_exists(&file_path).await? {
            return Ok(());
        }

        fs::create_dir(dir_name).await?;

        {
            fs::File::create(&file_path).await?;
            fs::write(file_path, PSEUDO_CODE).await?;
        }

        Ok(())
    }

    async fn remove_temp_file(dir_name: &str, file_name: &str) -> io::Result<()> {
        let file_path = Path::new(dir_name).join(file_name);

        fs::remove_file(file_path).await?;
        fs::remove_dir(dir_name).await?;

        Ok(())
    }

    pub fn create_mock_cli<I, T>(itr: I) -> Result<Cli, Box<dyn std::error::Error>>
    where
        I: IntoIterator<Item = T>,
        T: Into<OsString> + Clone,
    {
        return Ok(Cli::try_parse_from(itr)?);
    }

    #[test]
    fn should_get_args() -> Result<(), Box<dyn std::error::Error>> {
        let cli = create_mock_cli(&["pygmalion", "--path", PSEUDO_PATH])?;
        assert_eq!(cli.path, Some(PSEUDO_PATH.to_string()));

        Ok(())
    }

    #[tokio::test]
    async fn should_get_app_args_code_only() -> Result<(), Box<dyn std::error::Error>> {
        let cli = create_mock_cli(&["pygmalion", "--code", PSEUDO_CODE, "--out", PSEUDO_OUT])?;
        let (input, out) = get_app_args(cli).await?;

        assert_eq!(input, PSEUDO_CODE);
        assert_eq!(out, PSEUDO_OUT);

        Ok(())
    }

    #[tokio::test]
    async fn should_get_app_args_path_only() -> Result<(), Box<dyn std::error::Error>> {
        let dir_name = "temp";
        let file_name = "Test.sol";
        create_temp_file(dir_name, file_name).await?;
        let cli = create_mock_cli(&["pygmalion", "--path", PSEUDO_PATH])?;
        let (input, _out) = get_app_args(cli).await?;

        assert_eq!(input, PSEUDO_CODE);

        remove_temp_file(dir_name, file_name).await?;

        Ok(())
    }

    #[tokio::test]
    async fn should_panic_for_code_and_path() -> Result<(), Box<dyn std::error::Error>> {
        let cli = create_mock_cli(&["pygmalion"])?;
        let e = get_app_args(cli).await.unwrap_err();

        assert_eq!(e.kind(), io::ErrorKind::NotFound);

        Ok(())
    }
}
