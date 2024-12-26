const std = @import("std");
const assert = std.debug.assert;

pub fn skippingIterator(comptime T: type, input: []T, skip_index: usize) SkippingIterator(T) {
    assert(skip_index < input.len);
    return .{
        .input = input,
        .lidx = 0,
        .ridx = 1,
        .sidx = skip_index,
    };
}

/// Iterate through a list, skipping a value at an index.
/// Used in Day 2 Part 2.
pub fn SkippingIterator(comptime T: type) type {
    return struct {
        input: []T,
        lidx: usize,
        ridx: usize,
        sidx: usize,

        const Self = @This();

        fn first(self: *Self) struct { T, T } {
            // We should probably figure out if we are actually indexed for
            // "first"
            return self.next().?;
        }

        pub fn next(self: *Self) ?struct { T, T } {
            assert(self.lidx != self.ridx);
            if (self.lidx == self.sidx) {
                // If we are left indexed on the first item, we have to also
                // advance the right index.
                if (self.lidx == 0) {
                    self.ridx += 1;
                }

                self.lidx += 1;
            }
            if (self.ridx == self.sidx) {
                self.ridx += 1;
            }

            if (self.ridx >= self.input.len or self.lidx >= self.input.len) {
                return null;
            }
            const l = self.input[self.lidx];
            const r = self.input[self.ridx];

            self.lidx = self.ridx;
            self.ridx += 1;

            return .{ l, r };
        }

        pub fn skip(self: *Self) void {
            self.lidx -= 1;
        }
    };
}

test "SkippingIterator skips a value." {
    var test_buf = [_]i32{ 1, 2, 3, 4, 5, 6 };

    var iut = skippingIterator(i32, &test_buf, 3);

    const val = iut.next();
    try std.testing.expectEqual(1, val.?[0]);
}

test "SkippingIterator skips first value." {
    var test_buf = [_]i32{ 1, 2, 3, 4, 5, 6 };

    var iut = skippingIterator(i32, &test_buf, 0);

    const val = iut.next().?;
    try std.testing.expectEqual(2, val[0]);
    try std.testing.expectEqual(3, val[1]);
}

test "SkippingIterator skips last value." {
    var test_buf = [_]i32{ 1, 2, 3, 4, 5, 9 };

    var iut = skippingIterator(i32, &test_buf, test_buf.len - 1);

    var val = iut.next().?;
    try std.testing.expectEqual(1, val[0]);
    try std.testing.expectEqual(2, val[1]);
    val = iut.next().?;
    try std.testing.expectEqual(2, val[0]);
    try std.testing.expectEqual(3, val[1]);
    val = iut.next().?;
    try std.testing.expectEqual(3, val[0]);
    try std.testing.expectEqual(4, val[1]);
    val = iut.next().?;
    try std.testing.expectEqual(4, val[0]);
    try std.testing.expectEqual(5, val[1]);
    const lastval = iut.next();
    try std.testing.expectEqual(null, lastval);
}
