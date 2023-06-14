const std = @import("std");
const Map = std.AutoHashMap;
const types = @import("types.zig");
const Errors = @import("../error_reporter.zig").types.Errors;

pub fn set(s: *types.Substitution, t: types.TypeVar, m: types.MonoType) !void {
    const result = try s.map.getOrPut(t);
    if (result.found_existing) {
        if (std.meta.eql(result.value_ptr.*, m)) return;
        switch (m) {
            .typevar => |t1| try set(s, t1, result.value_ptr.*),
            else => switch (result.value_ptr.*) {
                .typevar => |t1| try set(s, t1, m),
                else => return error.CompileError,
            },
        }
    }
    result.value_ptr.* = m;
}

pub fn equalConstraint(equal: types.EqualConstraint, s: *types.Substitution, errors: *Errors) !void {
    const left_tag = std.meta.activeTag(equal.left.type);
    const right_tag = std.meta.activeTag(equal.right.type);
    if (left_tag == .typevar)
        return set(s, equal.left.type.typevar, equal.right.type) catch |e| switch (e) {
            error.CompileError => {
                const left = if (s.map.get(equal.left.type.typevar)) |t|
                    types.TypedSpan{ .type = t, .span = equal.left.span }
                else
                    equal.left;
                try errors.errors.append(.{
                    .type_error = .{
                        .left = left,
                        .right = equal.right,
                    },
                });
                return error.CompileError;
            },
            else => return e,
        };
    if (right_tag == .typevar)
        return set(s, equal.right.type.typevar, equal.left.type) catch |e| switch (e) {
            error.CompileError => {
                const right = if (s.map.get(equal.right.type.typevar)) |t|
                    types.TypedSpan{ .type = t, .span = equal.right.span }
                else
                    equal.right;
                try errors.errors.append(.{
                    .type_error = .{
                        .left = equal.left,
                        .right = right,
                    },
                });
                return error.CompileError;
            },
            else => return e,
        };
    if (left_tag == .function and right_tag == .function) {
        if (equal.left.type.function.len != equal.right.type.function.len)
            std.debug.panic("\nFunction arity mismatch: {} != {}\n", .{
                equal.left.type.function.len,
                equal.right.type.function.len,
            });
        for (equal.left.type.function, 0..) |left, i| {
            const right = equal.right.type.function[i];
            const constraint = types.EqualConstraint{
                .left = .{ .type = left, .span = null },
                .right = .{ .type = right, .span = null },
            };
            try equalConstraint(constraint, s, errors);
        }
        return;
    }
    if (left_tag == right_tag)
        return;
    try errors.errors.append(.{
        .type_error = .{
            .left = equal.left,
            .right = equal.right,
        },
    });
    return error.CompileError;
}

pub fn simplify(s: *types.Substitution) u64 {
    var count: u64 = 0;
    var iterator = s.map.iterator();
    while (iterator.next()) |entry| {
        switch (entry.value_ptr.*) {
            .typevar => |t| {
                if (s.map.get(t)) |v| {
                    entry.value_ptr.* = v;
                    count += 1;
                }
            },
            else => {},
        }
    }
    return count;
}

pub fn constraints(allocator: std.mem.Allocator, cs: types.Constraints, errors: *Errors) !types.Substitution {
    var s = types.Substitution{
        .map = Map(types.TypeVar, types.MonoType).init(allocator),
    };
    for (cs.equal.items) |e| try equalConstraint(e, &s, errors);
    var max_attemps: u64 = 3;
    while (simplify(&s) > 0 and max_attemps != 0) : (max_attemps -= 1) {}
    return s;
}
