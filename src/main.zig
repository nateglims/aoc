const std = @import("std");
const util = @import("util.zig");
const day1 = @import("aoc/day1.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    try day1.run(alloc);
}
