# Pygmalion: A Solidity Lexer in Rust

Pygmalion, yet another Solidity lexer written in Rust! This project leverages the power of the Rust programming language and the `logos` library to provide a robust and efficient lexical analyzer for Solidity source code. 

## Features

- **Lexical Analysis**: Pygmalion can tokenize Solidity source code, recognizing a wide variety of keywords, literals, operators, and more.
- **Mode Switching**: The lexer intelligently switches modes for different contexts like `pragma`, `assembly`, and `yul`.
- **Asynchronous Operations**: Utilizes asynchronous file I/O for reading from and writing to files.
- **CLI Support**: Includes a command-line interface (CLI) for easy integration into your workflow.

## Usage

Pygmalion can be used through its CLI. You can either provide a Solidity file or a short snippet of Solidity code for analysis. The lexer will output the tokenized version of the input.

## Installation

You can add Pygmalion to your project using Cargo. Run the following command:

```sh
cargo install pygmalion
```

### Command Line Interface

Here are the available CLI options:

- `-p, --path <PATH>`: Path of the Solidity file to analyze.
- `-c, --code <CODE>`: Short Solidity code snippet to analyze.
- `-o, --out <OUT>`: Output file for the lexical analysis results.

### Examples

Analyze a Solidity file:

```sh
pygmalion -p path/to/your/file.sol
```

Analyze a short snippet of Solidity code:

```sh
pygmalion -c "pragma solidity ^0.8.0; contract HelloWorld { }"
```

Save the analysis result to a file:

```sh
pygmalion -p path/to/your/file.sol -o output.txt
```

## Contributing

To contribute to Pygmalion, follow these steps:

1. Fork the repository.
2. Create a new branch for your feature or bugfix.
3. Make your changes.
4. Test thoroughly.
5. Submit a pull request.

## Code Structure

- **main.rs**: Entry point of the application.
- **cli.rs**: Command-line interface definition and argument parsing.
- **file_io.rs**: Asynchronous file input/output functions.
- **lexer.rs**: Lexer implementation, including token definitions and mode handling.

## License

Pygmalion is licensed under the MIT License. See the [LICENSE](https://github.com/icanvardar/pygmalion/blob/main/LICENSE) file for more details.

## Acknowledgements

This project uses the following crates:
- [`logos`](https://crates.io/crates/logos) for lexical analysis.
- [`tokio`](https://crates.io/crates/tokio) for asynchronous I/O operations.
- [`clap`](https://crates.io/crates/clap) for command-line argument parsing.

---

Thank you for using Pygmalion! If you have any questions or feedback, feel free to open an issue on GitHub. Peace!
