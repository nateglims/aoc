const std = @import("std");
const util = @import("../util.zig");

pub fn run(allocator: std.mem.Allocator) !void {
    const input = try util.loadFile(allocator, "day1.input");

    var calories = util.SortedArray(u64).init(allocator);

    var acc: u64 = 0;
    var elfNum: usize = 1;
    for (input) |line| {
        if (line.len == 0) {
            elfNum += 1;
            try calories.insert(acc);
            acc = 0;
            continue;
        }
        var val = try std.fmt.parseInt(u64, line, 10);
        acc += val;
    }
    allocator.free(input);

    const result = calories.toOwnedSlice();
    defer allocator.free(result);

    std.debug.print("part 1: {d}\n", .{result[0]});
    std.debug.print("part 2: {d}\n", .{result[0] + result[1] + result[2]});
}
