const std = @import("std");
const Map = std.AutoHashMap;
const types = @import("types.zig");
const Errors = @import("../error_reporter.zig").types.Errors;
const monotype = @import("monotype.zig");

fn exactEqual(a: types.MonoType, b: types.MonoType) bool {
    switch (a) {
        .void, .u8, .i32, .i64, .f32, .f64, .bool => {
            return std.meta.activeTag(a) == std.meta.activeTag(b);
        },
        .typevar => {
            return std.meta.activeTag(a) == std.meta.activeTag(b) and a.typevar.value == b.typevar.value;
        },
        .function => |f1| switch (b) {
            .function => |f2| {
                if (f1.parameters.len != f2.parameters.len) return false;
                for (f1.parameters, f2.parameters) |t1, t2| {
                    if (t1.mutable != t2.mutable) return false;
                    if (!exactEqual(t1.type, t2.type)) return false;
                }
                return exactEqual(f1.return_type.*, f2.return_type.*);
            },
            else => return false,
        },
        .array => |a1| switch (b) {
            .array => |a2| return a1.rank == a2.rank and exactEqual(a1.element_type.*, a2.element_type.*),
            else => return false,
        },
    }
}

pub fn set(s: *types.Substitution, t: types.TypeVar, m: types.MonoType) !void {
    const result = try s.getOrPut(t);
    if (result.found_existing) {
        if (exactEqual(result.value_ptr.*, m)) return;
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
    const left_tag = std.meta.activeTag(equal.left);
    const right_tag = std.meta.activeTag(equal.right);
    if (left_tag == .typevar) {
        return set(s, equal.left.typevar, equal.right) catch |e| switch (e) {
            error.CompileError => {
                const left = if (s.get(equal.left.typevar)) |t|
                    t
                else
                    equal.left;
                try errors.type_mismatch.append(.{ .left = left, .right = equal.right });
                return error.CompileError;
            },
            else => return e,
        };
    }
    if (right_tag == .typevar) {
        return set(s, equal.right.typevar, equal.left) catch |e| switch (e) {
            error.CompileError => {
                const right = if (s.get(equal.right.typevar)) |t|
                    t
                else
                    equal.right;
                try errors.type_mismatch.append(.{ .left = equal.left, .right = right });
                return error.CompileError;
            },
            else => return e,
        };
    }
    if (left_tag == .function and right_tag == .function) {
        if (equal.left.function.parameters.len != equal.right.function.parameters.len)
            std.debug.panic("\nFunction arity mismatch: {} != {}\n", .{
                equal.left.function.parameters.len,
                equal.right.function.parameters.len,
            });
        for (equal.left.function.parameters, equal.right.function.parameters) |left, right| {
            const constraint = types.EqualConstraint{ .left = left.type, .right = right.type };
            try equalConstraint(constraint, s, errors);
            if (left.mutable != right.mutable) {
                try errors.mutability_mismatch.append(.{
                    .left = .{ .mutable = left.mutable, .span = monotype.span(left.type) },
                    .right = .{ .mutable = right.mutable, .span = monotype.span(right.type) },
                });
                return error.CompileError;
            }
        }
        const constraint = types.EqualConstraint{
            .left = equal.left.function.return_type.*,
            .right = equal.right.function.return_type.*,
        };
        try equalConstraint(constraint, s, errors);
        return;
    }
    if (left_tag == right_tag) {
        return;
    }
    try errors.type_mismatch.append(.{ .left = equal.left, .right = equal.right });
    return error.CompileError;
}

pub fn simplify(s: *types.Substitution) u64 {
    var count: u64 = 0;
    var iterator = s.map.iterator();
    while (iterator.next()) |entry| {
        switch (entry.value_ptr.*) {
            .typevar => |t| {
                if (s.get(t)) |v| {
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
        .map = Map(u64, types.MonoType).init(allocator),
    };
    for (cs.equal.items) |e| try equalConstraint(e, &s, errors);
    var max_attemps: u64 = 3;
    while (simplify(&s) > 0 and max_attemps != 0) : (max_attemps -= 1) {}
    return s;
}
