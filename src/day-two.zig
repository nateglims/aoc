const std = @import("std");
const in = @import("lib/input.zig");
const iterators = @import("lib/iterator.zig");

const skipIter = iterators.skippingIterator;

const print = std.debug.print;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) @panic("MEM LEAK");
    }

    const allocator = gpa.allocator();

    const ir = try in.Input.from_file("inputs/day2.input", allocator);
    defer ir.deinit(allocator);

    const part1 = try day2_part1(allocator, ir);
    print("Part 1 Result: {d}\n", .{part1});

    const part2 = try day2_part2(allocator, ir);
    print("Part 2 Result: {d}\n", .{part2});
}

fn day2_part1(allocator: std.mem.Allocator, input: in.Input) !i32 {
    var total: i32 = 0;
    for (input.buf) |line| {
        if (try check_iterator_safe(allocator, line, 0)) {
            total += 1;
        }
    }
    return total;
}

fn day2_part2(allocator: std.mem.Allocator, input: in.Input) !i32 {
    var total: i32 = 0;
    for (input.buf) |line| {
        if (try check_iterator_safe(allocator, line, 1)) {
            total += 1;
        } else {}
    }
    return total;
}

fn check_iterator_safe(
    allocator: std.mem.Allocator,
    line: []const u8,
    tolerance: usize,
) !bool {
    var row = std.ArrayList(i32).init(allocator);
    defer row.deinit();

    var iter = std.mem.splitSequence(u8, line, " ");

    try slurp_line(&row, &iter);

    return is_safe(row.items, decreasing, tolerance) or is_safe(row.items, increasing, tolerance);
}

fn slurp_line(
    al: *std.ArrayList(i32),
    iter: *in.StrIterator,
) !void {
    while (iter.next()) |next| {
        try al.append(try std.fmt.parseInt(i32, next, 10));
    }
}

const CompFn = fn (val: i32) bool;

fn is_safe(v: []i32, comptime compFn: CompFn, tol: usize) bool {
    if (tol > 0) {
        for (0..v.len) |i| {
            // Brute force, but I am trying not to reallocate the buffer in any
            // loops though this iterator struct is probably larger than the
            // longest buffer. At least it's not on the heap ¯\_(ツ)_/¯
            var iter = skipIter(i32, v, i);

            while (iter.next()) |n| {
                if (!compFn(n[0] - n[1])) {
                    break;
                }
            } else return true;
        } else return false;
    } else {
        for (0..v.len - 1) |i| {
            if (!compFn(v[i] - v[i + 1])) {
                return false;
            }
        }
        return true;
    }
    return false;
}

fn decreasing(val: i32) bool {
    return val >= 1 and val <= 3;
}

fn increasing(val: i32) bool {
    return val >= -3 and val <= -1;
}

test "Part 1 from Sample Input" {
    const sample =
        \\7 6 4 2 1
        \\1 2 7 8 9
        \\9 7 6 2 1
        \\1 3 2 4 5
        \\8 6 4 4 1
        \\1 3 6 7 9
    ;

    print("\n", .{});
    const ir = try in.Input.from_string(sample, std.testing.allocator);
    defer ir.deinit(std.testing.allocator);
    const result = day2_part1(std.testing.allocator, ir);
    try std.testing.expectEqual(2, result);
    print("\n", .{});
}

test "Part 2 from Sample Input" {
    const sample =
        \\7 6 4 2 1
        \\1 2 7 8 9
        \\9 7 6 2 1
        \\1 3 2 4 5
        \\8 6 4 4 1
        \\1 3 6 7 9
    ;

    print("\n=== PART 2 ===\n", .{});
    const ir = try in.Input.from_string(sample, std.testing.allocator);
    defer ir.deinit(std.testing.allocator);
    const result = day2_part2(std.testing.allocator, ir);
    try std.testing.expectEqual(4, result);
}

test "Part 2, Split" {
    print("\n", .{});
    const test_cases: [8]struct { line: []const u8, expected: bool } = .{
        .{ .line = "7 6 4 2 1", .expected = true },
        .{ .line = "1 2 7 8 9", .expected = false },
        .{ .line = "9 7 6 2 1", .expected = false },
        .{ .line = "1 3 2 4 5", .expected = true },
        .{ .line = "8 6 4 4 1", .expected = true },
        .{ .line = "1 3 6 7 9", .expected = true },
        // A special case where the first number has to be rejected.
        .{ .line = "3 1 2 3 4 5", .expected = true },
        // And one where it's the last number.
        .{ .line = "1 2 3 4 5 9", .expected = true },
    };

    for (test_cases) |tc| {
        print("Testing Line {s}\n", .{tc.line});
        const result = try check_iterator_safe(std.testing.allocator, tc.line, 1);

        try std.testing.expectEqual(tc.expected, result);
    }
}
