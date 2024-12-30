// I don't really have regex so...
const token = @import("token.zig");
const std = @import("std");

const startsWith = std.mem.startsWith;
const Token = token.Token;

pub fn lexer(input: []const u8, allocator: std.mem.Allocator) Lexer {
    return .{
        .input = input,
        .idx = 0,
        .state = Lexer.LexerState.LexGarbage,
        .allocator = allocator,
    };
}

const Lexer = struct {
    input: []const u8,
    idx: usize,
    state: LexerState,
    allocator: std.mem.Allocator,

    const Self = @This();

    // What I want is this
    // const LexerStateFn = fn (*Self) ?LexerStateFn;

    pub const LexerState = enum(usize) {
        LexGarbage = 0,
    };

    const LexerStateFn = fn (*Self) ?LexerState;

    fn lexGarbage(self: *Self) ?LexerState {
        _ = self;

        return lexGarbage;
    }

    const stateTable = []LexerStateFn{
        lexGarbage,
    };

    //    fn next(self: *Self) !LexerState {
    //        std.debug.print("State: idx={d}, input=[{s}]\n", .{ self.idx, self.input });
    //        if (self.idx >= self.input.len) {
    //            return LexerState.LexDone;
    //        }
    //
    //        // One Width
    //        const one_width_token = switch (self.input[self.idx]) {
    //            '(' => .{ .tokenType = TokenType.LP,
    //            else => null,
    //        };
    //
    //        if (one_width_token != null) {
    //            self.idx += 1;
    //        }
    //
    //        // Keyword Tokens
    //        if (startsWith(u8, self.input[self.idx..], "mul")) {
    //            self.idx += "mul".len;
    //            return LexerState.LexMulExpr;
    //        }
    //
    //        return LexerError.UnexpectedToken;
    //    }

    fn peek(self: *Self) u8 {
        return self.input[self.idx];
    }

    fn run(self: *Self) void {
        while (self.state) {
            self.state = self.stateTable[self.state]();
        }
    }
};

test "Lexer scans mul" {
    const input = "mul(";

    const lex = lexer(input, std.testing.allocator);
    _ = lex;
}
