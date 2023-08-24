const std = @import("std");
const List = std.ArrayList;
const Map = std.AutoHashMap;
const Allocator = std.mem.Allocator;
const interner = @import("../interner.zig");
const Interned = interner.Interned;
const Intern = interner.Intern;
const types = @import("types.zig");
const Builtins = @import("../builtins.zig").Builtins;

pub fn concat(allocator: Allocator, builtins: Builtins, intern: *Intern, count: usize, name: Interned) !types.Function {
    const parameters = try allocator.alloc(types.Parameter, count);
    for (parameters, 0..) |*p, i| {
        const param_name = try std.fmt.allocPrint(allocator, "s{}", .{i});
        const interned = try intern.store(param_name);
        p.* = types.Parameter{ .name = interned, .type = .i32 };
    }
    var locals = List(types.Local).init(allocator);
    const ptr = try intern.store("ptr");
    const len = try intern.store("len");
    try locals.appendSlice(&.{
        .{ .name = ptr, .type = .i32 },
        .{ .name = len, .type = .i32 },
    });
    const exprs = try allocator.alloc(types.Expression, parameters.len + 3);
    {
        const get_arena = try allocator.create(types.Expression);
        get_arena.* = .{ .global_get = .{ .name = builtins.core_arena } };
        exprs[0] = .{ .local_set = .{ .name = ptr, .value = get_arena } };
    }
    const func = try intern.store("str/concat/fragment");
    const get_ptr = try allocator.create(types.Expression);
    get_ptr.* = .{ .local_get = .{ .name = ptr } };
    const get_len = try allocator.create(types.Expression);
    get_len.* = .{ .local_get = .{ .name = len } };
    for (parameters, 0..) |p, i| {
        const args = try allocator.alloc(types.Expression, 3);
        args[0] = get_ptr.*;
        args[1] = .{ .local_get = .{ .name = p.name } };
        const call_func = try allocator.create(types.Expression);
        call_func.* = .{ .call = .{ .function = func, .arguments = args } };
        exprs[i + 1] = .{ .local_set = .{ .name = len, .value = call_func } };
        args[2] = if (i == 0) .{ .literal = .{ .i32 = 0 } } else get_len.*;
    }
    {
        const i32_add = try allocator.create(types.Expression);
        i32_add.* = .{ .binary_op = .{ .kind = .i32_add, .left = get_ptr, .right = get_len } };
        exprs[parameters.len + 1] = .{ .global_set = .{ .name = builtins.core_arena, .value = i32_add } };
    }
    {
        const args = try allocator.alloc(types.Expression, 2);
        args[0] = get_ptr.*;
        args[1] = get_len.*;
        exprs[parameters.len + 2] = .{ .call = .{ .function = builtins.str, .arguments = args } };
    }
    return types.Function{
        .name = name,
        .parameters = parameters,
        .return_type = .i32,
        .locals = try locals.toOwnedSlice(),
        .pointers = &.{},
        .body = .{ .expressions = exprs },
    };
}

pub fn concatFragment(allocator: Allocator, intern: *Intern) !types.Function {
    const parameters = try allocator.alloc(types.Parameter, 3);
    const destination = try intern.store("destination");
    const source = try intern.store("source");
    const total_length = try intern.store("total_length");
    parameters[0] = types.Parameter{ .name = destination, .type = .i32 };
    parameters[1] = types.Parameter{ .name = source, .type = .i32 };
    parameters[2] = types.Parameter{ .name = total_length, .type = .i32 };
    var locals = List(types.Local).init(allocator);
    const len = try intern.store("len");
    try locals.append(.{ .name = len, .type = .i32 });
    const exprs = try allocator.alloc(types.Expression, 3);
    const get_source = try allocator.create(types.Expression);
    get_source.* = .{ .local_get = .{ .name = source } };
    {
        const func = try intern.store("str/len");
        const arguments = try allocator.alloc(types.Expression, 1);
        arguments[0] = get_source.*;
        const call_func = try allocator.create(types.Expression);
        call_func.* = .{ .call = .{ .function = func, .arguments = arguments } };
        exprs[0] = .{ .local_set = .{ .name = len, .value = call_func } };
    }
    const get_len = try allocator.create(types.Expression);
    get_len.* = .{ .local_get = .{ .name = len } };
    const get_total_length = try allocator.create(types.Expression);
    get_total_length.* = .{ .local_get = .{ .name = total_length } };
    {
        const get_destination = try allocator.create(types.Expression);
        get_destination.* = .{ .local_get = .{ .name = destination } };
        const copy_destination = try allocator.create(types.Expression);
        copy_destination.* = .{ .binary_op = .{
            .kind = .i32_add,
            .left = get_destination,
            .right = get_total_length,
        } };
        const copy_source = try allocator.create(types.Expression);
        const func = try intern.store("str/ptr");
        const arguments = try allocator.alloc(types.Expression, 1);
        arguments[0] = get_source.*;
        copy_source.* = .{ .call = .{ .function = func, .arguments = arguments } };
        exprs[1] = .{ .memory_copy = .{
            .destination = copy_destination,
            .source = copy_source,
            .size = get_len,
        } };
    }
    exprs[2] = .{ .binary_op = .{ .kind = .i32_add, .left = get_len, .right = get_total_length } };
    return types.Function{
        .name = try intern.store("str/concat/fragment"),
        .parameters = parameters,
        .return_type = .i32,
        .locals = try locals.toOwnedSlice(),
        .pointers = &.{},
        .body = .{ .expressions = exprs },
    };
}

pub fn pointer(allocator: Allocator, intern: *Intern) !types.Function {
    const parameters = try allocator.alloc(types.Parameter, 1);
    const s = try intern.store("s");
    parameters[0] = types.Parameter{ .name = s, .type = .i32 };
    const exprs = try allocator.alloc(types.Expression, 1);
    const value = try allocator.create(types.Expression);
    value.* = .{ .local_get = .{ .name = s } };
    exprs[0] = .{ .unary_op = .{ .kind = .i32_load, .expression = value } };
    return types.Function{
        .name = try intern.store("str/ptr"),
        .parameters = parameters,
        .return_type = .i32,
        .locals = &.{},
        .pointers = &.{},
        .body = .{ .expressions = exprs },
    };
}

pub fn length(allocator: Allocator, intern: *Intern) !types.Function {
    const parameters = try allocator.alloc(types.Parameter, 1);
    const s = try intern.store("s");
    parameters[0] = types.Parameter{ .name = s, .type = .i32 };
    const exprs = try allocator.alloc(types.Expression, 1);
    const local_get = try allocator.create(types.Expression);
    local_get.* = .{ .local_get = .{ .name = s } };
    const i32_const = try allocator.create(types.Expression);
    i32_const.* = .{ .literal = .{ .i32 = 4 } };
    const i32_add = try allocator.create(types.Expression);
    i32_add.* = .{ .binary_op = .{ .kind = .i32_add, .left = local_get, .right = i32_const } };
    exprs[0] = .{ .unary_op = .{ .kind = .i32_load, .expression = i32_add } };
    return types.Function{
        .name = try intern.store("str/len"),
        .parameters = parameters,
        .return_type = .i32,
        .locals = &.{},
        .pointers = &.{},
        .body = .{ .expressions = exprs },
    };
}
