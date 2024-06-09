use std::fmt;

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
        _ => {}
    };
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(extras = LexerState)]
pub enum Token {
    #[end]
    Eof,
    Illegal,
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
    #[regex(r"int|int8|int16|int24|int32|int40|int48|int56|int64|int72|int80|int88|int96|int104|int112|int120|int128|int136|int144|int152|int160|int168|int176|int184|int192|int200|int208|int216|int224|int232|int240|int248|int256")]
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
    #[regex("0[0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?|[1-9][0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?|0|([1-9][0-9]*([eE][-+]?[0-9]+)?)")]
    DecimalNumber,
    #[regex("(0[0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?|[1-9][0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?|0|([1-9][0-9]*([eE][-+]?[0-9]+)?)) [a-zA-Z$_][a-zA-Z0-9$_]*")]
    DecimalNumberFollowedByIdentifier,
    #[regex("[a-zA-Z$_][a-zA-Z0-9$_]*")]
    Identifier,
    #[regex("[ \\t\\r\\n\\u{000C}]+", logos::skip)]
    Whitespace,
    #[regex("/\\*([^*]|\\*[^/])*\\*/")]
    Comment,
    #[regex("//.*")]
    LineComment,
    #[token("case")]
    YulCase,
    #[token("default")]
    YulDefault,
    #[token("leave")]
    YulLeave,
    #[token("let")]
    AssemblyLet,
    #[token("switch")]
    AssemblySwitch,
    #[regex("stop|add|sub|mul|div|sdiv|mod|smod|exp|not|lt|gt|slt|sgt|eq|iszero|and|or|xor|byte|shl|shr|sar|addmod|mulmod|signextend|keccak256|pop|mload|mstore|mstore8|sload|sstore|tload|tstore|msize|gas|address|balance|selfbalance|caller|callvalue|calldataload|calldatasize|calldatacopy|extcodesize|extcodecopy|returndatasize|returndatacopy|mcopy|extcodehash|create|create2|call|callcode|delegatecall|staticcall|return|revert|selfdestruct|invalid|log0|log1|log2|log3|log4|chainid|origin|gasprice|blockhash|blobhash|coinbase|timestamp|number|difficulty|prevrandao|gaslimit|basefee|blobbasefee", priority=5)]
    AssemblyBuiltin,
    #[token(":=")]
    AssemblyAssign,
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
            (Token::DecimalNumber, Mode::Pragma, "0.8"),
            (Token::Period, Mode::Pragma, "."),
        ];

        assert_eq!(lex.next(), Some(Ok(Token::Pragma)));
        assert_eq!(lex.extras.mode, Mode::Pragma);

        while let Some(y) = lex.next() {
            println!("{}", lex.slice());
            println!("{:?}", y);
            println!("mode: {:?}", lex.extras.mode);
        }
    }

    #[test]
    fn test_assembly_mode() {}

    #[test]
    fn test_normal_mode() {}
}
