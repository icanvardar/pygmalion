use std::path::Path;

use clap::Parser;
use logos::Logos;
use pygmalion_lexer::token::{LexerFormatter, Token};
use pygmalion_utils::{
    cli::{self, get_app_args},
    file_io::{self, write_to_file},
};
use tokio::{fs, io};

const PSEUDO_FILE_CONTENT: &[u8] = b"contract Test{}";
const PSEUDO_OUTPUT: &str = "\
        Token: Contract, Mode: Normal, Identifier: contract\n\
        Token: Identifier, Mode: Normal, Identifier: Test\n\
        Token: LBrace, Mode: Normal, Identifier: {\n\
        Token: RBrace, Mode: Normal, Identifier: }\n\
        ";

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

#[tokio::test]
async fn should_print_out_analyze_result() -> Result<(), Box<dyn std::error::Error>> {
    let cli = cli::Cli::try_parse_from([
        "pygmalion",
        "--code",
        String::from_utf8(PSEUDO_FILE_CONTENT.to_vec())?.as_str(),
    ])?;

    let (source, _) = get_app_args(cli).await?;

    let lex = Token::lexer(&source);

    let output = lex.stringify();

    assert_eq!(output, PSEUDO_OUTPUT);

    Ok(())
}

#[tokio::test]
async fn should_write_analyze_result_to_file() -> Result<(), Box<dyn std::error::Error>> {
    let cli = cli::Cli::try_parse_from([
        "pygmalion",
        "--code",
        String::from_utf8(PSEUDO_FILE_CONTENT.to_vec())?.as_str(),
        "--out",
        "./tmp2/out.txt",
    ])?;

    let (source, out) = get_app_args(cli).await?;

    let lex = Token::lexer(&source);

    fs::create_dir("tmp2").await?;
    create_temp_file("tmp2", "out.txt").await?;

    let output = lex.stringify();

    let out_path = Path::new(&out);
    println!("{:?}", out_path);

    file_io::write_to_file(&out_path, output.as_bytes()).await?;

    let written_content = fs::read_to_string(out_path).await?;

    remove_temp_file("tmp2", "out.txt").await?;

    assert_eq!(written_content, PSEUDO_OUTPUT);

    Ok(())
}

#[tokio::test]
async fn should_get_data_from_file() -> Result<(), Box<dyn std::error::Error>> {
    let file_path = Path::new("temp").join("Test.sol");
    create_temp_file("temp", "Test.sol").await?;
    write_to_file(&file_path, PSEUDO_FILE_CONTENT).await?;

    let cli = cli::Cli::try_parse_from(["pygmalion", "--path", "./temp/Test.sol"])?;

    let (source, _) = get_app_args(cli).await?;

    assert_eq!(source.as_bytes(), PSEUDO_FILE_CONTENT);

    remove_temp_file("temp", "Test.sol").await?;

    Ok(())
}
