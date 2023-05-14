const std = @import("std");
const Allocator = std.mem.Allocator;

const types = @import("types.zig");
const Constraints = types.Constraints;
const Substitution = types.Substitution;
const Equal = types.Equal;

fn equal(substitution: *Substitution, e: Equal) !void {
    switch (e.left) {
        .typevar => |t| try substitution.putNoClobber(t, e.right),
        else => std.debug.panic("\nUnsupported type in equal: {} {}\n", .{ e.left, e.right }),
    }
}

pub fn solve(allocator: Allocator, c: Constraints) !Substitution {
    var substitution = Substitution.init(allocator);
    for (c.equal.items) |e| try equal(&substitution, e);
    return substitution;
}
