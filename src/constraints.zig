const std = @import("std");
const List = std.ArrayList;
const Allocator = std.mem.Allocator;

const substitution = @import("substitution.zig");
const Substitution = substitution.Substitution;
const TypedSpan = substitution.TypedSpan;
const MonoType = substitution.MonoType;
const TypeVar = substitution.TypeVar;
const Indent = @import("indent.zig").Indent;
const errors = @import("compile_errors.zig");
const CompileErrors = errors.CompileErrors;
const Span = @import("span.zig").Span;

pub const Equal = struct {
    left: TypedSpan,
    right: TypedSpan,

    fn solve(self: Equal, s: *Substitution, compile_errors: *CompileErrors) !void {
        const left_tag = std.meta.activeTag(self.left.type);
        const right_tag = std.meta.activeTag(self.right.type);
        if (left_tag == .typevar)
            return s.set(self.left.type.typevar, self.right.type) catch |e| switch (e) {
                error.CompileError => {
                    const left = if (s.get(self.left.type.typevar)) |t| TypedSpan{ .type = t, .span = self.left.span } else self.left;
                    try compile_errors.errors.append(.{
                        .type_error = .{
                            .left = left,
                            .right = self.right,
                        },
                    });
                    return error.CompileError;
                },
                else => return e,
            };
        if (right_tag == .typevar)
            return s.set(self.right.type.typevar, self.left.type) catch |e| switch (e) {
                error.CompileError => {
                    const right = if (s.get(self.right.type.typevar)) |t| TypedSpan{ .type = t, .span = self.right.span } else self.right;
                    try compile_errors.errors.append(.{
                        .type_error = .{
                            .left = self.left,
                            .right = right,
                        },
                    });
                    return error.CompileError;
                },
                else => return e,
            };
        if (left_tag == .function and right_tag == .function) {
            if (self.left.type.function.len != self.right.type.function.len)
                std.debug.panic("\nFunction arity mismatch: {} != {}\n", .{
                    self.left.type.function.len,
                    self.right.type.function.len,
                });
            for (self.left.type.function, 0..) |left, i| {
                const right = self.right.type.function[i];
                const constraint = Equal{
                    .left = .{ .type = left, .span = null },
                    .right = .{ .type = right, .span = null },
                };
                try constraint.solve(s, compile_errors);
            }
            return;
        }
        if (left_tag == right_tag)
            return;
        if (left_tag == .global) {
            const constraint = Equal{
                .left = .{ .type = self.left.type.global.*, .span = self.left.span },
                .right = self.right,
            };
            return try constraint.solve(s, compile_errors);
        }
        if (right_tag == .global) {
            const constraint = Equal{
                .left = self.left,
                .right = .{ .type = self.right.type.global.*, .span = self.right.span },
            };
            return try constraint.solve(s, compile_errors);
        }
        try compile_errors.errors.append(.{
            .type_error = .{
                .left = self.left,
                .right = self.right,
            },
        });
        return error.CompileError;
    }

    pub fn format(self: Equal, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll("equal = {}");
        try writer.print("{}left = {}", .{ Indent{ .value = 1 }, self.left.type });
        try writer.print("{}right = {}", .{ Indent{ .value = 1 }, self.right.type });
    }
};

pub const Constraints = struct {
    equal: List(Equal),
    next_type_var: TypeVar,
    compile_errors: *CompileErrors,

    pub fn init(allocator: Allocator, compile_errors: *CompileErrors) Constraints {
        return Constraints{
            .equal = List(Equal).init(allocator),
            .next_type_var = TypeVar{ .value = 0 },
            .compile_errors = compile_errors,
        };
    }

    pub fn solve(self: Constraints, allocator: Allocator) !Substitution {
        var s = Substitution.init(allocator);
        for (self.equal.items) |e| try e.solve(&s, self.compile_errors);
        var max_attemps: u64 = 3;
        while (s.simplify() > 0 and max_attemps != 0) : (max_attemps -= 1) {}
        return s;
    }

    pub fn freshTypeVar(self: *Constraints) MonoType {
        const typevar = self.next_type_var;
        self.next_type_var = TypeVar{ .value = self.next_type_var.value + 1 };
        return .{ .typevar = typevar };
    }

    pub fn format(self: Constraints, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll("\n\n=== Constraints ===");
        for (self.equal.items) |e| try writer.print("\n{}", .{e});
    }
};
