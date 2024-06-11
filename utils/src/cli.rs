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
        let file_path = Path::new(&path);
        input = read_from_file(file_path).await?;
    } else {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            "No input or destination provided",
        ));
    }

    return Ok((input, cli.out.unwrap_or_else(|| "".to_string())));
}

#[cfg(test)]
mod tests {
    use std::ffi::OsString;

    use super::*;

    pub fn create_cli<I, T>(itr: I) -> Result<Cli, Box<dyn std::error::Error>>
    where
        I: IntoIterator<Item = T>,
        T: Into<OsString> + Clone,
    {
        return Ok(Cli::try_parse_from(itr)?);
    }

    #[test]
    fn should_get_args() -> Result<(), Box<dyn std::error::Error>> {
        let path = "./temp/Test.sol";
        let cli = create_cli(&["pygmalion", "--path", path])?;
        assert_eq!(cli.path, Some(path.to_string()));

        Ok(())
    }
}
