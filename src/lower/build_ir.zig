const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const types = @import("types.zig");
const IR = types.IR;
const Function = types.Function;
const Parameter = types.Parameter;
const Type = types.Type;
const Expression = types.Expression;
const Intern = @import("../interner.zig").Intern;
const type_checker_types = @import("../type_checker/types.zig");
const Module = type_checker_types.Module;
const MonoType = type_checker_types.MonoType;
const Int = type_checker_types.Int;
const Float = type_checker_types.Float;

fn mapType(monotype: MonoType) Type {
    switch (monotype) {
        .i32 => return .i32,
        .f32 => return .f32,
        else => std.debug.panic("\nMonotype {} not yet supported", .{monotype}),
    }
}

fn int(i: Int) !Expression {
    switch (i.type) {
        .i32 => return .{ .i32 = i.value },
        .f32 => return .{ .f32 = i.value },
        else => std.debug.panic("\nInt type {} not yet supported", .{i.type}),
    }
}

fn float(f: Float) !Expression {
    switch (f.type) {
        .f32 => return .{ .f32 = f.value },
        else => std.debug.panic("\nFloat type {} not yet supported", .{f.type}),
    }
}

fn expression(e: type_checker_types.Expression) !Expression {
    switch (e) {
        .int => |i| return try int(i),
        .float => |f| return try float(f),
        else => std.debug.panic("\nExpression {} not yet supported", .{e}),
    }
}

fn function(allocator: Allocator, f: type_checker_types.Function) !Function {
    const parameters = try allocator.alloc(Parameter, f.parameters.len);
    for (f.parameters) |p, i| {
        parameters[i] = Parameter{
            .name = p.value,
            .type = mapType(p.type),
        };
    }
    var body = List(Expression).init(allocator);
    for (f.body) |e| {
        const expr = try expression(e);
        try body.append(expr);
    }
    return Function{
        .name = f.name.value,
        .parameters = parameters,
        .return_type = mapType(f.return_type),
        .body = body.toOwnedSlice(),
    };
}

pub fn buildIr(allocator: Allocator, module: Module) !IR {
    var functions = std.ArrayList(Function).init(allocator);
    for (module.order) |name| {
        if (module.typed.get(name)) |top_level| {
            switch (top_level) {
                .function => |f| {
                    const lowered = try function(allocator, f);
                    try functions.append(lowered);
                },
                else => |e| std.debug.panic("\nTop level kind {} no yet supported", .{e}),
            }
        } else {
            std.debug.panic("\nCould not find {} in module\n", .{name});
        }
    }
    return IR{ .functions = functions.toOwnedSlice() };
}
