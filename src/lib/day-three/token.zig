pub const Token = struct {
    tokenType: TokenType,
    start: usize,
};

pub const TokenType = enum {
    Garbage,
    Mul,
    Num,
    LParen,
    RParen,
    EOF,
};
