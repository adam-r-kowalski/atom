const std = @import("std");
const Map = std.AutoHashMap;
const types = @import("types.zig");
const Errors = @import("../error_reporter.zig").types.Errors;
const monotype = @import("monotype.zig");
const apply_substitution = @import("apply_substitution.zig");

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
            .array => |a2| return exactEqual(a1.element_type.*, a2.element_type.*),
            else => return false,
        },
        .structure => |s1| switch (b) {
            .structure => |s2| return s1.name.eql(s2.name),
            else => return false,
        },
        .enumeration => |e1| switch (b) {
            .enumeration => |e2| {
                for (e1.variants, e2.variants) |v1, v2| if (!v1.eql(v2)) return false;
                return true;
            },
            else => return false,
        },
        .enumeration_instance => |e1| switch (b) {
            .enumeration_instance => |e2| return e1.name.eql(e2.name),
            else => return false,
        },
        else => |k| std.debug.panic("Unhandled type: {}\n", .{k}),
    }
}

pub fn set(s: *types.Substitution, t: types.TypeVar, m: types.MonoType, errors: *Errors) !void {
    const result = try s.getOrPut(t);
    if (result.found_existing) {
        if (exactEqual(result.value_ptr.*, m)) return;
        switch (m) {
            .typevar => |t1| try set(s, t1, result.value_ptr.*, errors),
            .structure => {
                switch (result.value_ptr.*) {
                    .typevar => |t1| try set(s, t1, m, errors),
                    else => return error.CompileError,
                }
            },
            .array => |a1| {
                switch (result.value_ptr.*) {
                    .typevar => |t1| try set(s, t1, m, errors),
                    .array => |a2| {
                        const constraint = .{ .left = a1.element_type.*, .right = a2.element_type.* };
                        try equalConstraint(constraint, s, errors);
                    },
                    else => return error.CompileError,
                }
            },
            else => switch (result.value_ptr.*) {
                .typevar => |t1| try set(s, t1, m, errors),
                else => return error.CompileError,
            },
        }
    }
    result.value_ptr.* = m;
}

pub fn equalConstraint(equal: types.EqualConstraint, s: *types.Substitution, errors: *Errors) error{ CompileError, OutOfMemory }!void {
    const left_tag = std.meta.activeTag(equal.left);
    const right_tag = std.meta.activeTag(equal.right);
    if (left_tag == .typevar) {
        return set(s, equal.left.typevar, equal.right, errors) catch |e| switch (e) {
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
        return set(s, equal.right.typevar, equal.left, errors) catch |e| switch (e) {
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
    if (left_tag == .function and right_tag == .call) {
        const call = equal.right.call;
        const total_arguments = call.arguments.len + call.named_arguments.count();
        const func = equal.left.function;
        if (func.parameters.len != total_arguments)
            std.debug.panic("\nFunction arity mismatch: {} != {}\n", .{
                func.parameters.len,
                total_arguments,
            });
        for (func.parameters[0..call.arguments.len], call.arguments) |left, right| {
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
        var iterator = call.named_arguments.iterator();
        while (iterator.next()) |entry| {
            const name = entry.key_ptr.*;
            for (func.parameters[call.arguments.len..]) |left| {
                if (left.name.eql(name)) {
                    const right = entry.value_ptr.*;
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
            }
        }
        const constraint = types.EqualConstraint{
            .left = func.return_type.*,
            .right = call.return_type.*,
        };
        try equalConstraint(constraint, s, errors);
        return;
    }
    if (left_tag == .structure and right_tag == .call) {
        const call = equal.right.call;
        const total_arguments = call.arguments.len + call.named_arguments.count();
        const structure = equal.left.structure;
        if (structure.order.len != total_arguments)
            std.debug.panic("\nFunction arity mismatch: {} != {}\n", .{
                structure.order.len,
                total_arguments,
            });
        for (structure.order[0..call.arguments.len], call.arguments) |name, right| {
            const left = structure.fields.get(name).?;
            const constraint = types.EqualConstraint{ .left = left, .right = right.type };
            try equalConstraint(constraint, s, errors);
            if (right.mutable) {
                try errors.mutability_mismatch.append(.{
                    .left = .{ .mutable = false, .span = null },
                    .right = .{ .mutable = right.mutable, .span = monotype.span(right.type) },
                });
                return error.CompileError;
            }
        }
        var iterator = call.named_arguments.iterator();
        while (iterator.next()) |entry| {
            const right_name = entry.key_ptr.*;
            for (structure.order[call.arguments.len..]) |left_name| {
                if (left_name.eql(right_name)) {
                    const left = structure.fields.get(left_name).?;
                    const right = entry.value_ptr.*;
                    const constraint = types.EqualConstraint{ .left = left, .right = right.type };
                    try equalConstraint(constraint, s, errors);
                    if (right.mutable) {
                        try errors.mutability_mismatch.append(.{
                            .left = .{ .mutable = false, .span = null },
                            .right = .{ .mutable = right.mutable, .span = monotype.span(right.type) },
                        });
                        return error.CompileError;
                    }
                }
            }
        }
        const constraint = types.EqualConstraint{
            .left = equal.left,
            .right = call.return_type.*,
        };
        try equalConstraint(constraint, s, errors);
        return;
    }
    if (left_tag == .array and right_tag == .array) {
        const constraint = types.EqualConstraint{
            .left = equal.left.array.element_type.*,
            .right = equal.right.array.element_type.*,
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

pub fn fieldOfConstraint(allocator: std.mem.Allocator, field_of: types.FieldOfConstraint, s: *types.Substitution, errors: *Errors) error{ CompileError, OutOfMemory }!void {
    const value_type = try apply_substitution.monotype(allocator, s.*, field_of.value);
    const field_type = try apply_substitution.monotype(allocator, s.*, field_of.field);
    switch (value_type) {
        .structure => |structure| {
            const field = structure.fields.get(field_of.name).?;
            if (exactEqual(field, field_type)) return;
            try errors.type_mismatch.append(.{ .left = field, .right = field_type });
            return error.CompileError;
        },
        .enumeration => |e1| {
            {
                var contains = false;
                for (e1.variants) |v| {
                    if (v.eql(field_of.name)) {
                        contains = true;
                        break;
                    }
                }
                if (!contains)
                    std.debug.panic("enumeration {} does not contain {}", .{ e1.name, field_of.name });
            }
            switch (field_type) {
                .enumeration => {
                    if (!exactEqual(value_type, field_type)) {
                        try errors.type_mismatch.append(.{ .left = value_type, .right = field_type });
                        return error.CompileError;
                    }
                },
                .typevar => {
                    const constraint = .{ .left = value_type, .right = field_type };
                    try equalConstraint(constraint, s, errors);
                },
                else => std.debug.panic("cannot access field {} of type {}", .{ field_of.name, field_type }),
            }
        },
        else => std.debug.panic("cannot access field {} of type {}", .{ field_of.name, value_type }),
    }
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
    for (cs.field_of.items) |f| try fieldOfConstraint(allocator, f, &s, errors);
    var max_attemps: u64 = 3;
    while (simplify(&s) > 0 and max_attemps != 0) : (max_attemps -= 1) {}
    return s;
}
