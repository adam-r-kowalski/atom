const std = @import("std");

pub const Indent = struct {
    value: u64,

    pub fn add(self: Indent, n: u64) Indent {
        return Indent{ .value = self.value + n };
    }

    pub fn format(self: Indent, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        if (self.value > 0) try writer.writeAll("\n");
        for (0..self.value) |_| try writer.writeAll("    ");
    }
};
