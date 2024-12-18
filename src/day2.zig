const std = @import("std");
const in = @import("lib/input.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) @panic("MEM LEAK");
    }

    const allocator = gpa.allocator();

    const ir = try in.Input.from_file("inputs/day2.input", allocator);
    defer ir.deinit(allocator);
}

fn day2_part1(input: in.Input) !i32 {
    var total: i32 = 0;
    for (input.buf) |line| {
        var iter = std.mem.splitSequence(u8, line, " ");
        if (try check_iterator_safe(&iter)) {
            total += 1;
        }
    }
    return total;
}

fn check_iterator_safe(iter: *std.mem.SplitIterator(u8, std.mem.DelimiterType.sequence)) !bool {
    const first_val = iter.next().?;
    const second_val = iter.next().?;

    const first: i32 = try std.fmt.parseInt(i32, first_val, 10);
    const second: i32 = try std.fmt.parseInt(i32, second_val, 10);
    var prev = second;
    // Increasing
    if (first < second) {
        while (iter.next()) |nexts| {
            const next = try std.fmt.parseInt(i32, nexts, 10);
            if ((next - prev) < 4) {
                return false;
            }
            prev = next;
        }
        return true;
        // Decreasing
    } else if (second > first) {
        while (iter.next()) |nexts| {
            const next = try std.fmt.parseInt(i32, nexts, 10);
            if ((prev - next) < 4) {
                return false;
            }
            prev = next;
        }
        return true;
    } else {
        return false;
    }
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

    const ir = try in.Input.from_string(sample, std.testing.allocator);
    defer ir.deinit(std.testing.allocator);
    const result = day2_part1(ir);
    try std.testing.expectEqual(2, result);
}
