const std = @import("std");
const in = @import("lib/input.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) @panic("MEM LEAK");
    }

    const allocator = gpa.allocator();

    const ir = try in.Input.from_file("inputs/day1.input", allocator);
    defer ir.deinit(allocator);

    //
    var left = std.ArrayList(i32).init(allocator);
    var right = std.ArrayList(i32).init(allocator);
    defer left.deinit();
    defer right.deinit();

    for (ir.buf) |line| {
        var iter = std.mem.splitSequence(u8, line, "   ");
        const val = iter.next().?;
        try left.append(try std.fmt.parseInt(i32, val, 10));
        const valr = iter.next().?;
        try right.append(try std.fmt.parseInt(i32, valr, 10));
    }
    std.mem.sort(i32, left.items, {}, lessThan);
    std.mem.sort(i32, right.items, {}, lessThan);

    //
    const res = try day1_part1(left, right);
    std.debug.print("Part 1 Result: {d}\n", .{res});

    const res_part2 = try day1_part2(left, right);
    std.debug.print("Part 2 Result: {d}\n", .{res_part2});
}

fn day1_part1(left: std.ArrayList(i32), right: std.ArrayList(i32)) !i32 {
    var total: i32 = 0;
    for (left.items, right.items) |lval, rval| {
        total += abs(@TypeOf(lval), lval - rval);
    }

    return total;
}

fn day1_part2(left: std.ArrayList(i32), right: std.ArrayList(i32)) !i32 {
    var total: i32 = 0;
    for (left.items) |lval| {
        total += lval * count_item(i32, right.items, lval);
    }

    return total;
}

fn lessThan(_: void, lhs: i32, rhs: i32) bool {
    return lhs < rhs;
}

fn abs(comptime T: type, val: T) T {
    return if (val > 0) val else -val;
}

fn count_item(comptime T: type, list: []T, val: T) i32 {
    var count: i32 = 0;
    for (list) |item| {
        if (item > val) {
            break;
        } else if (item == val) {
            count += 1;
        }
    }
    return count;
}

test "Part 1 from Sample Input" {
    const input =
        \\3   4
        \\4   3
        \\2   5
        \\1   3
        \\3   9
        \\3   3
    ;
    const ir = try in.Input.from_string(input, std.testing.allocator);
    defer ir.deinit(std.testing.allocator);
    var left = std.ArrayList(i32).init(std.testing.allocator);
    var right = std.ArrayList(i32).init(std.testing.allocator);
    defer left.deinit();
    defer right.deinit();

    for (ir.buf) |line| {
        var iter = std.mem.splitSequence(u8, line, "   ");
        const val = iter.next().?;
        try left.append(try std.fmt.parseInt(i32, val, 10));
        const valr = iter.next().?;
        try right.append(try std.fmt.parseInt(i32, valr, 10));
    }
    std.mem.sort(i32, left.items, {}, lessThan);
    std.mem.sort(i32, right.items, {}, lessThan);

    const res = try day1_part1(left, right);
    try std.testing.expectEqual(11, res);
}

test "Part 2 from Sample Input" {
    const input =
        \\3   4
        \\4   3
        \\2   5
        \\1   3
        \\3   9
        \\3   3
    ;
    const ir = try in.Input.from_string(input, std.testing.allocator);
    defer ir.deinit(std.testing.allocator);
    var left = std.ArrayList(i32).init(std.testing.allocator);
    var right = std.ArrayList(i32).init(std.testing.allocator);
    defer left.deinit();
    defer right.deinit();

    for (ir.buf) |line| {
        var iter = std.mem.splitSequence(u8, line, "   ");
        const val = iter.next().?;
        try left.append(try std.fmt.parseInt(i32, val, 10));
        const valr = iter.next().?;
        try right.append(try std.fmt.parseInt(i32, valr, 10));
    }
    std.mem.sort(i32, left.items, {}, lessThan);
    std.mem.sort(i32, right.items, {}, lessThan);

    const res = try day1_part2(left, right);
    try std.testing.expectEqual(31, res);
}
