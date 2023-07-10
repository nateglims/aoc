const std = @import("std");
const util = @import("../util.zig");

const Machine = struct {
    accumulator: i64,
    clockCycle: i64,
    captureCycles: []const i64,
    powerAcc: i64,

    const Self = @This();

    fn init(captureCycles: []const i64) Self {
        return .{ .accumulator = 0, .clockCycle = 0, .captureCycles = captureCycles, .powerAcc = 0 };
    }

    fn incClock(self: *Self) void {
        self.clockCycle += 1;

        for (self.captureCycles) |cyc| {
            if (self.clockCycle == cyc) {
                self.powerAcc += self.accumulator * self.clockCycle;
            }
        }
    }

    fn parseInstruction(self: *Self, instruction: []const u8) !void {
        std.debug.print("inst: {s}\n", .{instruction});
        if (std.mem.startsWith(u8, instruction, "noop")) {
            self.incClock();
        } else {
            self.incClock();
            self.incClock();
        }
    }
};

pub fn run(allocator: std.mem.Allocator) !void {
    const input = try util.loadFile(allocator, "day10.input");

    const captureCycles = [2]i64{ 20, 60 };
    var machine = Machine.init(&captureCycles);
    for (input) |instruction| {
        try machine.parseInstruction(instruction);
    }
    allocator.free(input);
}

test "day10 part1 sample" {
    const input = [_][]const u8{
        "addx 15",
        "addx -11",
        "addx 6",
        "addx -3",
        "addx 5",
        "addx -1",
        "addx -8",
        "addx 13",
        "addx 4",
        "noop",
        "addx -1",
        "addx 5",
        "addx -1",
        "addx 5",
        "addx -1",
        "addx 5",
        "addx -1",
        "addx 5",
        "addx -1",
        "addx -35",
        "addx 1",
        "addx 24",
        "addx -19",
        "addx 1",
        "addx 16",
        "addx -11",
        "noop",
        "noop",
        "addx 21",
        "addx -15",
        "noop",
        "noop",
        "addx -3",
        "addx 9",
        "addx 1",
        "addx -3",
        "addx 8",
        "addx 1",
        "addx 5",
        "noop",
        "noop",
        "noop",
        "noop",
        "noop",
        "addx -36",
        "noop",
        "addx 1",
        "addx 7",
        "noop",
        "noop",
        "noop",
        "addx 2",
        "addx 6",
        "noop",
        "noop",
        "noop",
        "noop",
        "noop",
        "addx 1",
        "noop",
        "noop",
        "addx 7",
        "addx 1",
        "noop",
        "addx -13",
        "addx 13",
        "addx 7",
        "noop",
        "addx 1",
        "addx -33",
        "noop",
        "noop",
        "noop",
        "addx 2",
        "noop",
        "noop",
        "noop",
        "addx 8",
        "noop",
        "addx -1",
        "addx 2",
        "addx 1",
        "noop",
        "addx 17",
        "addx -9",
        "addx 1",
        "addx 1",
        "addx -3",
        "addx 11",
        "noop",
        "noop",
        "addx 1",
        "noop",
        "addx 1",
        "noop",
        "noop",
        "addx -13",
        "addx -19",
        "addx 1",
        "addx 3",
        "addx 26",
        "addx -30",
        "addx 12",
        "addx -1",
        "addx 3",
        "addx 1",
        "noop",
        "noop",
        "noop",
        "addx -9",
        "addx 18",
        "addx 1",
        "addx 2",
        "noop",
        "noop",
        "addx 9",
        "noop",
        "noop",
        "noop",
        "addx -1",
        "addx 2",
        "addx -37",
        "addx 1",
        "addx 3",
        "noop",
        "addx 15",
        "addx -21",
        "addx 22",
        "addx -6",
        "addx 1",
        "noop",
        "addx 2",
        "addx 1",
        "noop",
        "addx -10",
        "noop",
        "noop",
        "addx 20",
        "addx 1",
        "addx 2",
        "addx 2",
        "addx -6",
        "addx -11",
        "noop",
        "noop",
        "noop",
    };

    const captureCycles = [_]i64{ 20, 60, 100, 140, 180, 220 };
    var machine = Machine.init(&captureCycles);
    for (input) |instruction| {
        try machine.parseInstruction(instruction);
    }
    try std.testing.expectEqual(machine.clockCycle, 240);
    try std.testing.expectEqual(machine.powerAcc, 13140);
}
