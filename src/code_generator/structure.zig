const std = @import("std");
const Allocator = std.mem.Allocator;
const Builtins = @import("../builtins.zig").Builtins;
const types = @import("types.zig");
const type_checker = @import("../type_checker.zig");
const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const List = std.ArrayList;
const Map = std.AutoHashMap;
const size_of = @import("size_of.zig");
const mapType = @import("map_type.zig").mapType;

pub const Constructors = std.AutoHashMap(Interned, type_checker.monotype.Structure);
pub const StructName = Interned;
pub const FieldName = Interned;
pub const LocalName = Interned;
pub const PointerName = Interned;

pub const Accesses = struct {
    structure: type_checker.monotype.Structure,
    fields: List(FieldName),
};

pub const FieldAccesses = Map(StructName, Accesses);

pub fn constructor(
    allocator: Allocator,
    intern: *Intern,
    uses_memory: *bool,
    name: Interned,
    s: type_checker.monotype.Structure,
) !types.Function {
    uses_memory.* = true;
    var locals = List(types.Local).init(allocator);
    var pointers = List(types.LocalPointer).init(allocator);
    const local = try intern.store("result");
    try pointers.append(.{ .name = local, .size = size_of.structure(s) });
    const exprs = try allocator.alloc(types.Expression, s.order.len + 1);
    const base = try allocator.create(types.Expression);
    base.* = .{ .local_get = .{ .name = local } };
    var offset: u32 = 0;
    const parameters = try allocator.alloc(types.Parameter, s.order.len);
    for (exprs[0..s.order.len], parameters, s.order) |*e, *ir_p, param_name| {
        const typed_p = s.fields.get(param_name).?;
        ir_p.* = types.Parameter{
            .name = param_name,
            .type = mapType(typed_p),
        };
        const field_offset = try allocator.create(types.Expression);
        field_offset.* = .{ .literal = .{ .u32 = offset } };
        const field_address = blk: {
            if (offset > 0) {
                const ptr = try allocator.create(types.Expression);
                ptr.* = .{ .binary_op = .{ .kind = .i32_add, .left = base, .right = field_offset } };
                break :blk ptr;
            }
            break :blk base;
        };
        const result = try allocator.create(types.Expression);
        result.* = .{ .local_get = .{ .name = param_name } };
        switch (typed_p) {
            .u8 => e.* = .{ .binary_op = .{ .kind = .i32_store8, .left = field_address, .right = result } },
            .u32 => e.* = .{ .binary_op = .{ .kind = .i32_store, .left = field_address, .right = result } },
            .u64 => e.* = .{ .binary_op = .{ .kind = .i64_store, .left = field_address, .right = result } },
            .i32 => e.* = .{ .binary_op = .{ .kind = .i32_store, .left = field_address, .right = result } },
            .i64 => e.* = .{ .binary_op = .{ .kind = .i64_store, .left = field_address, .right = result } },
            .array => {
                const size_expr = try allocator.create(types.Expression);
                size_expr.* = .{ .literal = .{ .u32 = 8 } };
                e.* = .{ .memory_copy = .{
                    .destination = field_address,
                    .source = result,
                    .size = size_expr,
                } };
                offset += 8;
            },
            else => |k| std.debug.panic("\nField type {} not allowed", .{k}),
        }
    }
    exprs[s.order.len] = .{ .local_get = .{ .name = local } };
    return types.Function{
        .name = name,
        .parameters = parameters,
        .return_type = .i32,
        .locals = try locals.toOwnedSlice(),
        .pointers = try pointers.toOwnedSlice(),
        .body = .{ .expressions = exprs },
    };
}

fn contains(fields: []Interned, field: Interned) bool {
    for (fields) |name| {
        if (name.eql(field)) return true;
    }
    return false;
}

pub fn fieldAccess(allocator: Allocator, intern: *Intern, functions: *List(types.Function), accesses: Accesses) !void {
    var offset: u32 = 0;
    for (accesses.structure.order) |order| {
        const field = accesses.structure.fields.get(order).?;
        const size = size_of.monotype(field);
        if (!contains(accesses.fields.items, order)) {
            offset += size;
            continue;
        }
        const function_name = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ accesses.structure.name.string(), order.string() });
        const func = try intern.store(function_name);
        const result_type = mapType(field);
        const struct_type = mapType(.{ .structure = accesses.structure });
        const parameters = try allocator.alloc(types.Parameter, 1);
        const param_name = try allocator.alloc(u8, 1);
        param_name[0] = std.ascii.toLower(accesses.structure.name.string()[0]);
        const param = try intern.store(param_name);
        parameters[0] = types.Parameter{ .name = param, .type = struct_type };
        const local_get = try allocator.create(types.Expression);
        local_get.* = .{ .local_get = .{ .name = param } };
        const exprs = try allocator.alloc(types.Expression, 1);
        if (offset == 0) {
            exprs[0] = local_get.*;
        } else {
            const offset_expr = try allocator.create(types.Expression);
            offset_expr.* = .{ .literal = .{ .u32 = offset } };
            exprs[0] = .{ .binary_op = .{ .kind = .i32_add, .left = local_get, .right = offset_expr } };
        }
        switch (field) {
            .u8 => {
                const value = try allocator.create(types.Expression);
                value.* = exprs[0];
                exprs[0] = .{ .unary_op = .{ .kind = .i32_load8_u, .expression = value } };
            },
            .u32 => {
                const value = try allocator.create(types.Expression);
                value.* = exprs[0];
                exprs[0] = .{ .unary_op = .{ .kind = .i32_load, .expression = value } };
            },
            .u64 => {
                const value = try allocator.create(types.Expression);
                value.* = exprs[0];
                exprs[0] = .{ .unary_op = .{ .kind = .i64_load, .expression = value } };
            },
            .i32 => {
                const value = try allocator.create(types.Expression);
                value.* = exprs[0];
                exprs[0] = .{ .unary_op = .{ .kind = .i32_load, .expression = value } };
            },
            .i64 => {
                const value = try allocator.create(types.Expression);
                value.* = exprs[0];
                exprs[0] = .{ .unary_op = .{ .kind = .i64_load, .expression = value } };
            },
            .array => {},
            else => |k| std.debug.panic("\ninvalid field type {}\n", .{k}),
        }
        try functions.append(types.Function{
            .name = func,
            .parameters = parameters,
            .return_type = result_type,
            .locals = &.{},
            .pointers = &.{},
            .body = .{ .expressions = exprs },
        });
        offset += size;
    }
}
