use std::fmt::{self, Debug};

use logos::{Lexer, Logos};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum Mode {
    #[default]
    Normal,
    Pragma,
    Assembly,
    Yul,
}

#[derive(Debug)]
pub struct LexerState {
    pub mode: Mode,
    pub depth: u8,
}

impl Default for LexerState {
    fn default() -> Self {
        Self {
            mode: Mode::Normal,
            depth: 0,
        }
    }
}

trait StateManager {
    fn switch_mode(&mut self, mode: Mode);
    fn inc_depth(&mut self);
    fn dec_depth(&mut self);
    fn is(&mut self, mode: Mode) -> bool;
}

impl StateManager for LexerState {
    fn switch_mode(&mut self, mode: Mode) {
        self.mode = mode;
    }
    fn inc_depth(&mut self) {
        self.depth += 1;
    }
    fn dec_depth(&mut self) {
        if self.depth > 0 {
            self.depth -= 1;
        }
    }
    fn is(&mut self, mode: Mode) -> bool {
        if self.mode == mode {
            return true;
        };

        return false;
    }
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

fn switch_mode(lex: &mut Lexer<Token>) {
    match lex.slice().as_bytes() {
        b"pragma" => {
            lex.extras.switch_mode(Mode::Pragma);
        }
        b"assembly" => {
            lex.extras.switch_mode(Mode::Assembly);
        }
        b"{" => {
            if lex.extras.is(Mode::Assembly) {
                lex.extras.switch_mode(Mode::Yul);
            } else if lex.extras.is(Mode::Yul) {
                lex.extras.inc_depth();
            }
        }
        b"}" => {
            if lex.extras.is(Mode::Yul) {
                if lex.extras.depth > 0 {
                    lex.extras.dec_depth();
                } else {
                    lex.extras.switch_mode(Mode::Normal);
                }
            }
        }
        b";" => {
            if lex.extras.mode == Mode::Pragma {
                lex.extras.switch_mode(Mode::Normal);
            }
        }
        _ => {
            panic!("No matching literal!");
        }
    };
}

fn check_reserve_word(lex: &mut Lexer<Token>) -> Token {
    let cur_token = lex.slice();

    match cur_token.as_bytes() {
        b"case" => {
            if lex.extras.mode != Mode::Yul {
                return Token::ReservedKeyword;
            }

            return Token::YulCase;
        }
        _ => {
            return Token::Illegal;
        }
    }
}

pub trait LexerFormatter {
    fn stringify(self) -> String;
}

impl<'source> LexerFormatter for logos::Lexer<'source, Token> {
    fn stringify(mut self) -> String {
        let mut result = String::new();
        while let Some(token) = self.next() {
            let token_str = format!("{:?}", token.unwrap());
            let mode_str = format!("{:?}", &self.extras.mode);
            let identifier = self.slice();

            result.push_str(&format!(
                "Token: {}, Mode: {}, Identifier: {}\n",
                token_str, mode_str, identifier
            ));
        }

        result
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(extras = LexerState)]
pub enum Token {
    #[end]
    Eof,

    Illegal,

    #[regex(r"after|alias|apply|auto|byte|copyof|default|define|final")]
    #[regex(r"implements|in|inline|let|macro|match|mutable|null|of|partial")]
    #[regex(r"promise|reference|relocatable|sealed|sizeof|static|supports")]
    #[regex(r"switch|typedef|typeof|var")]
    ReservedKeyword,

    #[token("abstract")]
    Abstract,

    #[token("address")]
    Address,

    #[token("anonymous")]
    Anonymous,

    #[token("as")]
    As,

    #[token("assembly", switch_mode)]
    Assembly,

    #[token("bool")]
    Bool,

    #[token("break", switch_mode)]
    Break,

    #[token("bytes")]
    Bytes,

    #[token("calldata")]
    Calldata,

    #[token("catch")]
    Catch,

    #[token("constant")]
    Constant,

    #[token("constructor")]
    Constructor,

    #[token("continue")]
    Continue,

    #[token("contract")]
    Contract,

    #[token("delete")]
    Delete,

    #[token("do")]
    Do,

    #[token("else")]
    Else,

    #[token("emit")]
    Emit,

    #[token("enum")]
    Enum,

    #[token("error")]
    Error,

    #[token("event")]
    Event,

    #[token("external")]
    External,

    #[token("fallback")]
    Fallback,

    #[token("false")]
    False,

    #[token("fixed")]
    Fixed,

    #[regex(r"fixed[1-9][0-9]*x[1-9][0-9]*")]
    FixedWithSize,

    #[regex(r"bytes[1-9]|bytes[1-2][0-9]|bytes3[0-2]")]
    FixedBytes,

    #[token("for")]
    For,

    #[token("from")]
    From,

    #[token("function")]
    Function,

    #[token("global")]
    Global,

    #[token("hex")]
    Hex,

    #[token("if")]
    If,

    #[token("immutable")]
    Immutable,

    #[token("import")]
    Import,

    #[token("indexed")]
    Indexed,

    #[token("interface")]
    Interface,

    #[token("internal")]
    Internal,

    #[token("is")]
    Is,

    #[token("library")]
    Library,

    #[token("mapping")]
    Mapping,

    #[token("memory")]
    Memory,

    #[token("modifier")]
    Modifier,

    #[token("new")]
    New,

    #[regex(r"wei|gwei|ether|seconds|minutes|hours|days|weeks|years")]
    SubDenomination,

    #[token("override")]
    Override,

    #[token("payable")]
    Payable,

    #[token("pragma", switch_mode)]
    Pragma,

    #[token("private")]
    Private,

    #[token("public")]
    Public,

    #[token("pure")]
    Pure,

    #[token("receive")]
    Receive,

    #[token("require")]
    Require,

    #[token("return")]
    Return,

    #[token("returns")]
    Returns,

    #[token("revert")]
    Revert,

    #[regex("int(8|16|24|32|40|48|56|64|72|80|88|96|104|112|120|128|136|144)")]
    #[regex("int(152|160|168|176|184|192|200|208|216|224|232|240|248|256)")]
    SignedIntegerType,

    #[token("storage")]
    Storage,

    #[token("string")]
    String,

    #[token("struct")]
    Struct,

    #[token("super")]
    Super,

    #[token("this")]
    This,

    #[token("true")]
    True,

    #[token("try")]
    Try,

    #[token("type")]
    Type,

    #[token("ufixed")]
    Ufixed,

    #[regex(r"ufixed[1-9][0-9]*x[1-9][0-9]*")]
    UfixedWithSize,

    #[token("unchecked")]
    Unchecked,

    #[token("unicode")]
    Unicode,

    #[regex("uint(8|16|24|32|40|48|56|64|72|80|88|96|104|112|120|128|136|144)")]
    #[regex("uint(152|160|168|176|184|192|200|208|216|224|232|240|248|256)")]
    UnsignedIntegerType,

    #[token("using")]
    Using,

    #[token("view")]
    View,

    #[token("virtual")]
    Virtual,

    #[token("while")]
    While,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBrack,

    #[token("]")]
    RBrack,

    #[token("{", switch_mode)]
    LBrace,

    #[token("}", switch_mode)]
    RBrace,

    #[token(":")]
    Colon,

    #[token(";", switch_mode)]
    Semicolon,

    #[token(".")]
    Period,

    #[token("?")]
    Conditional,

    #[token("=>")]
    DoubleArrow,

    #[token("->")]
    RightArrow,

    #[token("=")]
    Assign,

    #[token("|=")]
    AssignBitOr,

    #[token("^=")]
    AssignBitXor,

    #[token("&=")]
    AssignBitAnd,

    #[token("<<=")]
    AssignShl,

    #[token(">>=")]
    AssignSar,

    #[token(">>>=")]
    AssignShr,

    #[token("+=")]
    AssignAdd,

    #[token("-=")]
    AssignSub,

    #[token("*=")]
    AssignMul,

    #[token("/=")]
    AssignDiv,

    #[token("%=")]
    AssignMod,

    #[token(",")]
    Comma,

    #[token("||")]
    Or,

    #[token("&&")]
    And,

    #[token("|")]
    BitOr,

    #[token("^")]
    BitXor,

    #[token("&")]
    BitAnd,

    #[token("<<")]
    Shl,

    #[token(">>")]
    Sar,

    #[token(">>>")]
    Shr,

    #[token("+")]
    Add,

    #[token("-")]
    Sub,

    #[token("*")]
    Mul,

    #[token("/")]
    Div,

    #[token("%")]
    Mod,

    #[token("**")]
    Exp,

    #[token("==")]
    Equal,

    #[token("!=")]
    NotEqual,

    #[token("<")]
    LessThan,

    #[token(">")]
    GreaterThan,

    #[token("<=")]
    LessThanOrEqual,

    #[token(">=")]
    GreaterThanOrEqual,

    #[token("!")]
    Not,

    #[token("~")]
    BitNot,

    #[token("++")]
    Inc,

    #[token("--")]
    Dec,

    #[regex("\"")]
    DoubleQuote,

    #[regex("'")]
    SingleQuote,

    #[regex("\"(\\\"|[^\"\\\\])*\"", priority = 5)]
    NonEmptyStringLiteral,

    #[regex("\"\"")]
    EmptyStringLiteral,

    #[regex("'(\\\\'|[^'\\\\])*'")]
    UnicodeStringLiteral,

    #[regex("hex(\"(\\\\\"|[^\"\\\\])*\"|'(\\\\'|[^'\\\\])*')")]
    HexString,

    #[regex("0x[0-9a-fA-F]+")]
    HexNumber,

    #[regex("'0'[0-7]+(\\.[0-9]+)?")]
    OctalNumber,

    #[regex("[0-9]+")]
    Integer,

    #[regex(r"[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+")]
    Rational,

    #[regex("[a-zA-Z$_][a-zA-Z0-9$_]*")]
    Identifier,

    #[token("case", check_reserve_word)]
    YulCase,

    #[token("default")]
    YulDefault,

    #[token("leave")]
    YulLeave,

    #[token("let")]
    AssemblyLet,

    #[token("switch")]
    AssemblySwitch,

    #[regex("stop|add|sub|mul|div|sdiv|mod|smod|exp|not|lt|gt|slt|sgt|eq|iszero|and|or")]
    #[regex("xor|byte|shl|shr|sar|addmod|mulmod|signextend|keccak256|pop|mload|mstore|mstore8")]
    #[regex("sload|sstore|tload|tstore|msize|gas|address|balance|selfbalance|caller|callvalue")]
    #[regex("calldataload|calldatasize|calldatacopy|extcodesize|extcodecopy|returndatasize")]
    #[regex("returndatacopy|mcopy|extcodehash|create|create2|call|callcode|delegatecall")]
    #[regex("staticcall|return|revert|selfdestruct|invalid|log0|log1|log2|log3|log4|chainid")]
    #[regex("origin|gasprice|blockhash|blobhash|coinbase|timestamp|number|difficulty|prevrandao")]
    #[regex("gaslimit|basefee|blobbasefee")]
    AssemblyBuiltin,

    #[token("=:")]
    AssemblyAssign,

    #[token(":=")]
    AssemblyBind,

    #[regex("[ \\t\\r\\n\\u{000C}]+", logos::skip)]
    Whitespace,

    #[regex("/\\*([^*]|\\*[^/])*\\*/")]
    Comment,

    #[regex("//.*")]
    LineComment,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pragma_mode() {
        let mut lex = Token::lexer("pragma solidity ^0.8.16;");

        let tests = vec![
            (Token::Pragma, Mode::Pragma, "pragma"),
            (Token::Identifier, Mode::Pragma, "solidity"),
            (Token::BitXor, Mode::Pragma, "^"),
            (Token::Integer, Mode::Pragma, "0"),
            (Token::Period, Mode::Pragma, "."),
            (Token::Integer, Mode::Pragma, "8"),
            (Token::Period, Mode::Pragma, "."),
            (Token::Integer, Mode::Pragma, "16"),
            (Token::Semicolon, Mode::Normal, ";"),
        ];

        for i in 0..tests.len() {
            let (tok, mode, literal) = tests[i].clone();

            assert_eq!(Some(Ok(tok)), lex.next());
            assert_eq!(mode, lex.extras.mode);
            assert_eq!(literal.as_bytes(), lex.slice().as_bytes());
        }
    }

    #[test]
    fn test_assembly_mode() {
        let mut lex = Token::lexer(
            "assembly {\
                let size := extcodesize(addr)
            }",
        );

        let tests = vec![
            (Token::Assembly, Mode::Assembly, "assembly"),
            (Token::LBrace, Mode::Yul, "{"),
            (Token::AssemblyLet, Mode::Yul, "let"),
            (Token::Identifier, Mode::Yul, "size"),
            (Token::AssemblyBind, Mode::Yul, ":="),
            (Token::AssemblyBuiltin, Mode::Yul, "extcodesize"),
            (Token::LParen, Mode::Yul, "("),
            (Token::Identifier, Mode::Yul, "addr"),
            (Token::RParen, Mode::Yul, ")"),
            (Token::RBrace, Mode::Normal, "}"),
        ];

        for i in 0..tests.len() {
            let (tok, mode, literal) = tests[i].clone();

            assert_eq!(Some(Ok(tok)), lex.next());
            assert_eq!(mode, lex.extras.mode);
            assert_eq!(literal.as_bytes(), lex.slice().as_bytes());
        }
    }

    #[test]
    fn test_normal_mode() {
        let mut lex = Token::lexer(
            "contract Pygmalion {\
                uint256 counter;

                constructor() {
                    counter = 5;
                }

                event o7();

                function inc() public {
                    if (counter == 1881) {
                        emit o7();                        
                    }

                    counter += 1;                    
                }
            }",
        );

        let tests = vec![
            (Token::Contract, Mode::Normal, "contract"),
            (Token::Identifier, Mode::Normal, "Pygmalion"),
            (Token::LBrace, Mode::Normal, "{"),
            (Token::UnsignedIntegerType, Mode::Normal, "uint256"),
            (Token::Identifier, Mode::Normal, "counter"),
            (Token::Semicolon, Mode::Normal, ";"),
            (Token::Constructor, Mode::Normal, "constructor"),
            (Token::LParen, Mode::Normal, "("),
            (Token::RParen, Mode::Normal, ")"),
            (Token::LBrace, Mode::Normal, "{"),
            (Token::Identifier, Mode::Normal, "counter"),
            (Token::Assign, Mode::Normal, "="),
            (Token::Integer, Mode::Normal, "5"),
            (Token::Semicolon, Mode::Normal, ";"),
            (Token::RBrace, Mode::Normal, "}"),
            (Token::Event, Mode::Normal, "event"),
            (Token::Identifier, Mode::Normal, "o7"),
            (Token::LParen, Mode::Normal, "("),
            (Token::RParen, Mode::Normal, ")"),
            (Token::Semicolon, Mode::Normal, ";"),
            (Token::Function, Mode::Normal, "function"),
            (Token::Identifier, Mode::Normal, "inc"),
            (Token::LParen, Mode::Normal, "("),
            (Token::RParen, Mode::Normal, ")"),
            (Token::Public, Mode::Normal, "public"),
            (Token::LBrace, Mode::Normal, "{"),
            (Token::If, Mode::Normal, "if"),
            (Token::LParen, Mode::Normal, "("),
            (Token::Identifier, Mode::Normal, "counter"),
            (Token::Equal, Mode::Normal, "=="),
            (Token::Integer, Mode::Normal, "1881"),
            (Token::RParen, Mode::Normal, ")"),
            (Token::LBrace, Mode::Normal, "{"),
            (Token::Emit, Mode::Normal, "emit"),
            (Token::Identifier, Mode::Normal, "o7"),
            (Token::LParen, Mode::Normal, "("),
            (Token::RParen, Mode::Normal, ")"),
            (Token::Semicolon, Mode::Normal, ";"),
            (Token::RBrace, Mode::Normal, "}"),
            (Token::Identifier, Mode::Normal, "counter"),
            (Token::AssignAdd, Mode::Normal, "+="),
            (Token::Integer, Mode::Normal, "1"),
            (Token::Semicolon, Mode::Normal, ";"),
            (Token::RBrace, Mode::Normal, "}"),
            (Token::RBrace, Mode::Normal, "}"),
        ];

        for i in 0..tests.len() {
            let (tok, mode, literal) = tests[i].clone();

            assert_eq!(Some(Ok(tok)), lex.next());
            assert_eq!(mode, lex.extras.mode);
            assert_eq!(literal.as_bytes(), lex.slice().as_bytes());
        }
    }

    #[test]
    fn should_stringify_lexer() {
        let lex = Token::lexer("contract UwU {}");

        assert_eq!(
            lex.stringify(),
            "\
                Token: Contract, Mode: Normal, Identifier: contract\n\
                Token: Identifier, Mode: Normal, Identifier: UwU\n\
                Token: LBrace, Mode: Normal, Identifier: {\n\
                Token: RBrace, Mode: Normal, Identifier: }\n\
                "
        );
    }
}
