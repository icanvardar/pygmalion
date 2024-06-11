use std::path::Path;

use clap::Parser;
use lexer::token::Token;
use logos::Logos;
use utils::{
    cli::{self, get_app_args},
    file_io,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = cli::Cli::parse();

    let (source, out) = get_app_args(cli).await?;

    let _lex = Token::lexer(&source);

    // TODO: Format lexical analysis data
    let output = b"You shall not pass!";

    if !out.is_empty() {
        // NOTE: Write formatted data to disk
        file_io::write_to_file(&Path::new(&out).canonicalize()?, output).await?;
    } else {
        // TODO: Show formatted data on console
    }

    Ok(())
}
