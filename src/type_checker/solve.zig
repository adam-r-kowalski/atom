const std = @import("std");
const Allocator = std.mem.Allocator;

const types = @import("types.zig");
const Constraints = types.Constraints;
const Substitution = types.Substitution;
const Equal = types.Equal;

fn equal(substitution: *Substitution, e: Equal) !void {
    const left_tag = std.meta.activeTag(e.left);
    const right_tag = std.meta.activeTag(e.right);
    if (left_tag == .typevar)
        return try substitution.putNoClobber(e.left.typevar, e.right);
    if (right_tag == .typevar)
        return try substitution.putNoClobber(e.right.typevar, e.left);
    if (left_tag == right_tag)
        return;
    std.debug.panic("\nUnsupported type in equal: {} {}\n", .{ e.left, e.right });
}

pub fn solve(allocator: Allocator, c: Constraints) !Substitution {
    var substitution = Substitution.init(allocator);
    for (c.equal.items) |e| try equal(&substitution, e);
    return substitution;
}
