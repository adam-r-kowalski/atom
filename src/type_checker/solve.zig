const std = @import("std");
const Allocator = std.mem.Allocator;

const types = @import("types.zig");
const TypeVar = types.TypeVar;
const MonoType = types.MonoType;
const Constraints = types.Constraints;
const Substitution = types.Substitution;
const Equal = types.Equal;

fn set(substitution: *Substitution, t: TypeVar, m: MonoType) !void {
    const result = try substitution.getOrPut(t);
    if (result.found_existing) {
        if (std.meta.eql(result.value_ptr.*, m)) return;
        switch (m) {
            .typevar => |t1| try set(substitution, t1, result.value_ptr.*),
            else => std.debug.panic("\nType mismatch: {} != {}\n", .{ result.value_ptr.*, m }),
        }
    }
    result.value_ptr.* = m;
}

fn equal(substitution: *Substitution, e: Equal) !void {
    const left_tag = std.meta.activeTag(e.left);
    const right_tag = std.meta.activeTag(e.right);
    if (left_tag == .typevar)
        return try set(substitution, e.left.typevar, e.right);
    if (right_tag == .typevar)
        return try set(substitution, e.right.typevar, e.left);
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

fn simplify(substitution: *Substitution) u64 {
    var count: u64 = 0;
    var iterator = substitution.iterator();
    while (iterator.next()) |entry| {
        switch (entry.value_ptr.*) {
            .typevar => |t| {
                if (substitution.get(t)) |v| {
                    entry.value_ptr.* = v;
                    count += 1;
                }
            },
            else => {},
        }
    }
    return count;
}

pub fn solve(allocator: Allocator, c: Constraints) !Substitution {
    var substitution = Substitution.init(allocator);
    for (c.equal.items) |e| try equal(&substitution, e);
    var max_attemps: u64 = 3;
    while (simplify(&substitution) > 0 and max_attemps != 0) : (max_attemps -= 1) {}
    return substitution;
}
