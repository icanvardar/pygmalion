use std::path::Path;

use clap::Parser;
use logos::Logos;
use pygmalion_lexer::token::{LexerFormatter, Token};
use pygmalion_utils::{
    cli::{self, get_app_args},
    file_io,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = cli::Cli::parse();

    let (source, out) = get_app_args(cli).await?;

    let lex = Token::lexer(&source);

    let output = lex.stringify();

    if !out.is_empty() {
        file_io::write_to_file(&Path::new(&out).canonicalize()?, output.as_bytes()).await?;
    } else {
        println!("Analysis Result:\n {}", &output);
    }

    Ok(())
}
