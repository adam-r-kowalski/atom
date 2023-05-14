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
    if (left_tag == .i32 and right_tag == .int_literal)
        return;
    if (left_tag == .f32 and right_tag == .int_literal)
        return;
    if (left_tag == .bool and right_tag == .bool_literal)
        return;
    std.debug.panic("\nUnsupported type in equal: {} {}\n", .{ e.left, e.right });
}

fn simplify(substitution: *Substitution) !void {
    var iterator = substitution.iterator();
    while (iterator.next()) |entry| {
        switch (entry.value_ptr.*) {
            .typevar => |t| {
                if (substitution.get(t)) |v|
                    entry.value_ptr.* = v;
            },
            else => {},
        }
    }
}

pub fn solve(allocator: Allocator, c: Constraints) !Substitution {
    var substitution = Substitution.init(allocator);
    for (c.equal.items) |e| try equal(&substitution, e);
    try simplify(&substitution);
    return substitution;
}
