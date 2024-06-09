use std::path::Path;

use super::file_io::read_from_file;
use clap::Parser;
use tokio::io;

/// Pygmalion, yet another solidity lexer and stuff!
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
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

pub async fn get_app_args() -> io::Result<(String, String)> {
    let cli = Cli::parse();

    let input: String;

    if let Some(code) = cli.code {
        input = code;
    } else if let Some(path) = cli.path {
        let file_path = Path::new(&path);
        input = read_from_file(file_path.to_path_buf()).await?;
    } else {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            "No input or destination provided",
        ));
    }

    return Ok((input, cli.out.unwrap_or_else(|| "".to_string())));
}
