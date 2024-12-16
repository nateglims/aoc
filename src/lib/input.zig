const std = @import("std");
const fs = std.fs;

pub const Input = struct {
    buf: [][]const u8,

    pub fn from_file(filename: []const u8, allocator: std.mem.Allocator) !Input {
        var lines = std.ArrayList([]const u8).init(allocator);
        errdefer lines.deinit();

        var f: fs.File = try fs.cwd().openFile(filename, .{});
        defer f.close();

        var fr = f.reader();

        var buf: [1024]u8 = undefined;
        while (try fr.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            const allocated_line = try allocator.dupe(u8, line);
            try lines.append(allocated_line);
            // allocator.free(allocated_line);
        }

        return Input{ .buf = try lines.toOwnedSlice() };
    }

    pub fn from_sample(data: []const []const u8, allocator: std.mem.Allocator) !Input {
        var lines = std.ArrayList([]const u8).init(allocator);
        errdefer lines.deinit();

        for (data) |line| {
            try lines.append(try allocator.dupe(u8, line));
        }

        return Input{ .buf = try lines.toOwnedSlice() };
    }

    pub fn from_string(input: []const u8, allocator: std.mem.Allocator) !Input {
        var lines = std.ArrayList([]const u8).init(allocator);
        errdefer lines.deinit();

        var n: usize = 0;
        var start: usize = 0;
        while (n < input.len) : (n += 1) {
            if (input[n] == '\n') {
                try lines.append(try allocator.dupe(u8, input[start..n]));
                start = n + 1;
            }
        }
        try lines.append(try allocator.dupe(u8, input[start..n]));

        return Input{ .buf = try lines.toOwnedSlice() };
    }

    pub fn deinit(self: *const Input, allocator: std.mem.Allocator) void {
        for (self.buf) |line| {
            allocator.free(line);
        }
        allocator.free(self.buf);
    }
};

test "Try reading a file." {
    const ir = try Input.from_file("testdata/input.txt", std.testing.allocator);
    defer ir.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings("line1", ir.buf[0]);
}

test "Try reading a sample input." {
    const input = [_][]const u8{ "3  4", "43  3" };
    const ir = try Input.from_sample(&input, std.testing.allocator);
    defer ir.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings("3  4", ir.buf[0]);
}

test "Try reading a multiline string input." {
    const input =
        \\line2
        \\line3
    ;
    const ir = try Input.from_string(input, std.testing.allocator);
    defer ir.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings("line2", ir.buf[0]);
    try std.testing.expectEqualStrings("line3", ir.buf[1]);
}
