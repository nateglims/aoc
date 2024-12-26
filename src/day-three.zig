const std = @import("std");
const in = @import("lib/input.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) @panic("MEM LEAK");
    }

    const allocator = gpa.allocator();

    const ir = try in.Input.from_file("inputs/day3.input", allocator);
    defer ir.deinit(allocator);
}

test "Day 3, Part 1, Sample Input" {
    const sample_input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
    _ = sample_input;
    const expected = 161;
    _ = expected;
}
