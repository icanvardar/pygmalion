use std::fmt;

use logos::{Lexer, Logos};
use regex::bytes::RegexSet;

pub enum Mode {
    Normal,
    Pragma,
    Assembly,
    Yul,
}

pub struct LexerState {
    pub cur_mode: Mode,
    pub depth: u32,
}

// if mode yul ise hic cikma
impl LexerState {
    fn switch_mode(&mut self, mode: Mode) {
        self.cur_mode = mode;
    }

    fn inc_depth(&mut self) {
        self.depth += 1;
    }

    fn dec_depth(&mut self) {
        self.depth -= 1;
    }
}

impl Default for LexerState {
    fn default() -> Self {
        Self {
            cur_mode: Mode::Normal,
            depth: 0,
        }
    }
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Debug for LexerState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "mode: {}, y: {}", self.cur_mode, self.depth)
    }
}

// TODO: skip if conditions met
fn switch_mode(lex: &mut Lexer<Token>, mode: Mode) {
    let current_token = lex.slice().as_bytes();

    match current_token {
        b"pragma" => {
            if lex.extras.depth == 0 {
                lex.extras.switch_mode(Mode::Pragma)
            }
        }
        b"assembly" => lex.extras.switch_mode(Mode::Assembly),
        _ => println!("fuck man"),
    }

    let assembly_set = RegexSet::new(&[
        r"{",                       // AssemblyLBrace
        r"(",                       // AssemblyBlockLParen,
        r")",                       // AssemblyBlockRParen,
        r",",                       // AssemblyBlockComma,
        r"[ \\t\\r\\n\\u{000C}]+",  // AssemblyBlockWhitespace
        r"/\\*([^*]|\\*[^/])*\\*/", // AssemblyBlockComment
        r"//.*",                    // AssemblyBlockLineComment
    ]);

    let yul_set = RegexSet::new(&[
        r"break",                    // YulBreak
        r"false",                    // YulFalse
        r"function",                 // YulFunction
        r"if",                       // YulIf
        r"true",                     // YulTrue
        r"hex",                      // YulHex
        r"{",                        // YulLBrace
        r"}",                        // YulRBrace
        r"(",                        // YulLParen
        r")",                        // YulRParen
        r".",                        // YulPeriod
        r",",                        // YulComma
        r"->",                       // YulArrow
        r"[a-zA-Z$_][a-zA-Z0-9$_]*", // YulIdentifier
        r"0[xX][0-9a-fA-F]+",        // YulHexNumber
        r"0|([1-9][0-9]*)",          // YulDecimalNumber
        r"[ \t\r\n\u{000C}]+",       // YulWhitespace
        r"/\*.*?\*/",                // YulComment
        r"//.*",                     // YulLineComment
    ]);

    let pragma_set = RegexSet::new(&[
        r"[^;]+",              // PragmaToken
        r";",                  // PragmaSemicolon
        r"[ \t\r\n\u{000C}]+", // PragmaWhitespace
        r"/\*.*?\*/",          // PragmaComment
        r"//.*",               // PragmaLineComment
    ]);
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = LexerState)]
pub enum Token {
    #[end]
    Eof,
    #[regex(
        r"after|alias|apply|auto|byte|case|copyof|default|define|final|implements|in|inline|let|macro|match|mutable|null|of|partial|promise|reference|relocatable|sealed|sizeof|static|supports|switch|typedef|typeof|var",
        |lex| lex.slice().to_string()
    )]
    ReservedKeyword(String),
    #[token("abstract")]
    Abstract,
    #[token("address")]
    Address,
    #[token("anonymous")]
    Anonymous,
    #[token("as")]
    As,
    #[token("assembly", |lex| switch_mode(lex, Mode::Assembly))]
    Assembly,
    #[token("bool")]
    Bool,
    #[token("break")]
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
    #[token("continue", priority = 20)]
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
    #[token("pragma")]
    Pragma,
    #[token("private")]
    Private,
    #[token("public")]
    Public,
    #[token("pure")]
    Pure,
    #[token("receive")]
    Receive,
    #[token("return")]
    Return,
    #[token("returns")]
    Returns,
    #[token("revert")]
    Revert,
    #[regex(r"int|int8|int16|int24|int32|int40|int48|int56|int64|int72|int80|int88|int96|int104|int112|int120|int128|int136|int144|int152|int160|int168|int176|int184|int192|int200|int208|int216|int224|int232|int240|int248|int256")]
    SignedIntegerType,
    #[token("storage")]
    Storage,
    #[token("string")]
    String,
    #[token("struct")]
    Struct,
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
    #[regex(r"uint|uint8|uint16|uint24|uint32|uint40|uint48|uint56|uint64|uint72|uint80|uint88|uint96|uint104|uint112|uint120|uint128|uint136|uint144|uint152|uint160|uint168|uint176|uint184|uint192|uint200|uint208|uint216|uint224|uint232|uint240|uint248|uint256")]
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
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(":")]
    Colon,
    #[token(";")]
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
    #[regex("0[0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?|[1-9][0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?|0|([1-9][0-9]*([eE][-+]?[0-9]+)?)")]
    DecimalNumber,
    #[regex("(0[0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?|[1-9][0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?|0|([1-9][0-9]*([eE][-+]?[0-9]+)?)) [a-zA-Z$_][a-zA-Z0-9$_]*")]
    DecimalNumberFollowedByIdentifier,
    #[regex("[a-zA-Z$_][a-zA-Z0-9$_]*")]
    Identifier,
    #[regex("[ \\t\\r\\n\\u{000C}]+", logos::skip)]
    Whitespace,
    #[regex("/\\*([^*]|\\*[^/])*\\*/", logos::skip)]
    Comment,
    #[regex("//.*", logos::skip)]
    LineComment,

    // NOTE: Mode::Assembly tokens
    #[regex("\"evmasm\"")]
    AssemblyDialect,
    AssemblyLBrace,
    AssemblyFlagString, // If token NonEmptyStringLiteral and mode is assembly
    AssemblyBlockLParen,
    AssemblyBlockRParen,
    AssemblyBlockComma,
    AssemblyBlockWhitespace,
    AssemblyBlockComment,
    AssemblyBlockLineComment,

    // NOTE: Mode::Yul tokens
    YulBreak,
    #[token("case")]
    YulCase,
    #[token("continue")]
    YulContinue,
    #[token("default")]
    YulDefault,
    YulFalse,
    YulFor,
    YulFunction,
    YulIf,
    #[token("leave")]
    YulLeave,
    #[token("let")]
    YulLet,
    #[token("switch")]
    YulSwitch,
    YulTrue,
    YulHex,
    #[regex("stop|add|sub|mul|div|sdiv|mod|smod|exp|not|lt|gt|slt|sgt|eq|iszero|and|or|xor|byte|shl|shr|sar|addmod|mulmod|signextend|keccak256|pop|mload|mstore|mstore8|sload|sstore|tload|tstore|msize|gas|address|balance|selfbalance|caller|callvalue|calldataload|calldatasize|calldatacopy|extcodesize|extcodecopy|returndatasize|returndatacopy|mcopy|extcodehash|create|create2|call|callcode|delegatecall|staticcall|return|revert|selfdestruct|invalid|log0|log1|log2|log3|log4|chainid|origin|gasprice|blockhash|blobhash|coinbase|timestamp|number|difficulty|prevrandao|gaslimit|basefee|blobbasefee", priority=5)]
    YulEVMBuiltin,
    YulLBrace,
    YulRBrace,
    YulLParen,
    YulRParen,
    #[token(":=")]
    YulAssign,
    YulPeriod,
    YulComma,
    YulArrow,
    YulIdentifier,
    YulHexNumber,
    YulDecimalNumber,
    YulStringLiteral,
    YulHexStringLiteral, // TODO: if current token is HexString and mode is Yul
    YulWhitespace,
    YulComment,
    YulLineComment,

    // NOTE: Mode::Pragma tokens
    PragmaToken,
    PragmaSemicolon,
    PragmaWhitespace,
    PragmaComment,
    PragmaLineComment,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reserve_keyword_works() {
        let mut lex = Token::lexer("as as as");

        // assert_eq!(
        //     lex.next(),
        //     Some(Ok(Token::ReservedKeyword("after".to_string())))
        // )

        assert_eq!(lex.next(), Some(Ok(Token::LParen)))

        //        loop {
        //            match lex.next() {
        //
        //                _ => {
        //                    break;
        //                }
        //            }
        //        }
    }

    #[test]
    fn sub_denomination_works() {
        let mut lex = Token::lexer("wei");

        assert_eq!(lex.next(), Some(Ok(Token::SubDenomination)))
    }
}
