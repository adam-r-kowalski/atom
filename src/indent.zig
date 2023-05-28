pub const Indent = struct {
    value: u64,

    pub fn increment(self: Indent) Indent {
        return Indent{ .value = self.value + 1 };
    }

    pub fn toString(self: Indent, writer: anytype) !void {
        if (self.value > 0) try writer.writeAll("\n");
        for (0..self.value) |_| try writer.writeAll("    ");
    }
};
