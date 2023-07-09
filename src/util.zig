const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn loadFile(allocator: Allocator, filename: []const u8) ![][]const u8 {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var contents = try file.readToEndAlloc(allocator, (try file.stat()).size);
    //    defer allocator.free(contents);

    var iter = std.mem.split(u8, contents, "\n");
    var lines = std.ArrayList([]const u8).init(allocator);

    while (iter.next()) |line| {
        try lines.append(line);
    }

    return lines.toOwnedSlice();
}

pub fn SortedArray(comptime T: type) type {
    return struct {
        inner: ArrayList(T),

        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) Self {
            var inner = ArrayList(T).init(allocator);

            return .{ .inner = inner };
        }

        pub fn insert(self: *Self, val: T) !void {
            if (self.inner.items.len == 0) {
                return self.inner.insert(0, val);
            }

            for (self.inner.items) |item, n| {
                if (val > item) {
                    return self.inner.insert(n, val);
                }
            }

            return self.inner.append(val);
        }

        pub fn toOwnedSlice(self: *Self) []T {
            return self.inner.toOwnedSlice();
        }
    };
}

test "load file" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "sorted array" {
    var sa = SortedArray(usize).init(std.testing.allocator);

    try sa.insert(4);
    try sa.insert(5);
    try sa.insert(1);

    const actual = sa.toOwnedSlice();
    defer std.testing.allocator.free(actual);

    const expect = [3]usize{ 5, 4, 1 };
    try std.testing.expectEqualSlices(usize, &expect, actual);
}
