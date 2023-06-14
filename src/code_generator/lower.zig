const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const type_checker = @import("../type_checker.zig");
const Builtins = @import("../builtins.zig").Builtins;
const types = @import("types.zig");

const Context = struct {
    allocator: Allocator,
    builtins: Builtins,
    locals: *List(types.Local),
    next_local: *u32,
    data_segment: *types.DataSegment,
    intern: *Intern,
};

fn fresh_local(context: Context, t: types.Type) !Interned {
    const text = try std.fmt.allocPrint(context.allocator, "{}", .{context.next_local.*});
    const interned = try context.intern.store(text);
    context.next_local.* += 1;
    try context.locals.append(.{ .name = interned, .type = t });
    return interned;
}

fn mapType(monotype: type_checker.types.MonoType) types.Type {
    switch (monotype) {
        .i32 => return .i32,
        .i64 => return .i64,
        .f32 => return .f32,
        .f64 => return .f64,
        .bool => return .i32,
        .void => return .void,
        .array => return .i32,
        else => std.debug.panic("\nMonotype {} not yet supported", .{monotype}),
    }
}

fn int(i: type_checker.types.Int) !types.Expression {
    switch (i.type) {
        .i32 => return .{ .literal = .{ .i32 = try std.fmt.parseInt(i32, i.value.string(), 10) } },
        .i64 => return .{ .literal = .{ .i64 = try std.fmt.parseInt(i64, i.value.string(), 10) } },
        .f32 => return .{ .literal = .{ .f32 = try std.fmt.parseFloat(f32, i.value.string()) } },
        .f64 => return .{ .literal = .{ .f64 = try std.fmt.parseFloat(f64, i.value.string()) } },
        else => |k| std.debug.panic("\nInt type {} not yet supported", .{k}),
    }
}

fn float(f: type_checker.types.Float) !types.Expression {
    switch (f.type) {
        .f32 => return .{ .literal = .{ .f32 = try std.fmt.parseFloat(f32, f.value.string()) } },
        .f64 => return .{ .literal = .{ .f64 = try std.fmt.parseFloat(f64, f.value.string()) } },
        else => |k| std.debug.panic("\nFloat type {} not yet supported", .{k}),
    }
}

fn boolean(b: type_checker.types.Bool) !types.Expression {
    return types.Expression{ .literal = types.Literal{ .bool = b.value } };
}

fn expressions(context: Context, b: type_checker.types.Block) !types.Expressions {
    const exprs = try context.allocator.alloc(types.Expression, b.expressions.len);
    for (b.expressions, exprs) |expr, *ir_expr| {
        ir_expr.* = try expression(context, expr);
    }
    return types.Expressions{ .expressions = exprs };
}

fn add(context: Context, b: type_checker.types.BinaryOp) !types.Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (type_checker.type_of.expression(b.left.*)) {
        .i32 => return .{ .binary_op = .{ .kind = .i32_add, .left = left, .right = right } },
        .i64 => return .{ .binary_op = .{ .kind = .i64_add, .left = left, .right = right } },
        .f32 => return .{ .binary_op = .{ .kind = .f32_add, .left = left, .right = right } },
        .f64 => return .{ .binary_op = .{ .kind = .f64_add, .left = left, .right = right } },
        else => |k| std.debug.panic("\nAdd type {} not yet supported", .{k}),
    }
}

fn subtract(context: Context, b: type_checker.types.BinaryOp) !types.Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (type_checker.type_of.expression(b.left.*)) {
        .i32 => return .{ .binary_op = .{ .kind = .i32_sub, .left = left, .right = right } },
        .i64 => return .{ .binary_op = .{ .kind = .i64_sub, .left = left, .right = right } },
        .f32 => return .{ .binary_op = .{ .kind = .f32_sub, .left = left, .right = right } },
        .f64 => return .{ .binary_op = .{ .kind = .f64_sub, .left = left, .right = right } },
        else => |k| std.debug.panic("\nSubtract type {} not yet supported", .{k}),
    }
}

fn multiply(context: Context, b: type_checker.types.BinaryOp) !types.Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (type_checker.type_of.expression(b.left.*)) {
        .i32 => return .{ .binary_op = .{ .kind = .i32_mul, .left = left, .right = right } },
        .i64 => return .{ .binary_op = .{ .kind = .i64_mul, .left = left, .right = right } },
        .f32 => return .{ .binary_op = .{ .kind = .f32_mul, .left = left, .right = right } },
        .f64 => return .{ .binary_op = .{ .kind = .f64_mul, .left = left, .right = right } },
        else => |k| std.debug.panic("\nMultiply type {} not yet supported", .{k}),
    }
}

fn divide(context: Context, b: type_checker.types.BinaryOp) !types.Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (type_checker.type_of.expression(b.left.*)) {
        .i32 => return .{ .binary_op = .{ .kind = .i32_div_s, .left = left, .right = right } },
        .i64 => return .{ .binary_op = .{ .kind = .i64_div_s, .left = left, .right = right } },
        .f32 => return .{ .binary_op = .{ .kind = .f32_div, .left = left, .right = right } },
        .f64 => return .{ .binary_op = .{ .kind = .f64_div, .left = left, .right = right } },
        else => |k| std.debug.panic("\nDivide type {} not yet supported", .{k}),
    }
}

fn modulo(context: Context, b: type_checker.types.BinaryOp) !types.Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (type_checker.type_of.expression(b.left.*)) {
        .i32 => return .{ .binary_op = .{ .kind = .i32_rem_s, .left = left, .right = right } },
        .i64 => return .{ .binary_op = .{ .kind = .i64_rem_s, .left = left, .right = right } },
        else => |k| std.debug.panic("\nModulo type {} not yet supported", .{k}),
    }
}

fn equal(context: Context, b: type_checker.types.BinaryOp) !types.Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (type_checker.type_of.expression(b.left.*)) {
        .i32 => return .{ .binary_op = .{ .kind = .i32_eq, .left = left, .right = right } },
        .i64 => return .{ .binary_op = .{ .kind = .i64_eq, .left = left, .right = right } },
        .f32 => return .{ .binary_op = .{ .kind = .f32_eq, .left = left, .right = right } },
        .f64 => return .{ .binary_op = .{ .kind = .f64_eq, .left = left, .right = right } },
        else => |k| std.debug.panic("\nEqual type {} not yet supported", .{k}),
    }
}

fn binaryOr(context: Context, b: type_checker.types.BinaryOp) !types.Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (type_checker.type_of.expression(b.left.*)) {
        .bool => return .{ .binary_op = .{ .kind = .i32_or, .left = left, .right = right } },
        else => |k| std.debug.panic("\nOr type {} not yet supported", .{k}),
    }
}

fn greater(context: Context, b: type_checker.types.BinaryOp) !types.Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (type_checker.type_of.expression(b.left.*)) {
        .i32 => return .{ .binary_op = .{ .kind = .i32_gt_s, .left = left, .right = right } },
        .i64 => return .{ .binary_op = .{ .kind = .i64_gt_s, .left = left, .right = right } },
        .f32 => return .{ .binary_op = .{ .kind = .f32_gt, .left = left, .right = right } },
        .f64 => return .{ .binary_op = .{ .kind = .f64_gt, .left = left, .right = right } },
        else => |k| std.debug.panic("\nGreater type {} not yet supported", .{k}),
    }
}

fn less(context: Context, b: type_checker.types.BinaryOp) !types.Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (type_checker.type_of.expression(b.left.*)) {
        .i32 => return .{ .binary_op = .{ .kind = .i32_lt_s, .left = left, .right = right } },
        .i64 => return .{ .binary_op = .{ .kind = .i64_lt_s, .left = left, .right = right } },
        .f32 => return .{ .binary_op = .{ .kind = .f32_lt, .left = left, .right = right } },
        .f64 => return .{ .binary_op = .{ .kind = .f64_lt, .left = left, .right = right } },
        else => |k| std.debug.panic("\nGreater type {} not yet supported", .{k}),
    }
}

fn binaryOp(context: Context, b: type_checker.types.BinaryOp) !types.Expression {
    switch (b.kind) {
        .add => return try add(context, b),
        .subtract => return try subtract(context, b),
        .multiply => return try multiply(context, b),
        .divide => return try divide(context, b),
        .modulo => return try modulo(context, b),
        .equal => return try equal(context, b),
        .or_ => return try binaryOr(context, b),
        .greater => return try greater(context, b),
        .less => return try less(context, b),
        else => |k| std.debug.panic("\nBinary op {} not yet supported", .{k}),
    }
}

fn symbol(s: type_checker.types.Symbol) types.Expression {
    if (s.global) return types.Expression{ .global_get = .{ .name = s.value } };
    return .{ .local_get = .{ .name = s.value } };
}

fn call(context: Context, c: type_checker.types.Call) !types.Expression {
    switch (c.function.*) {
        .symbol => |s| {
            const arguments = try context.allocator.alloc(types.Expression, c.arguments.len);
            for (c.arguments, arguments) |arg, *ir_arg| {
                ir_arg.* = try expression(context, arg);
            }
            return .{
                .call = .{
                    .function = s.value,
                    .arguments = arguments,
                },
            };
        },
        else => |k| std.debug.panic("\nCall function type {} not yet supported", .{k}),
    }
}

fn intrinsic(context: Context, i: type_checker.types.Intrinsic) !types.Expression {
    if (i.function.eql(context.builtins.sqrt)) {
        const expr = try expressionAlloc(context, i.arguments[0]);
        switch (i.type) {
            .f32 => return .{ .unary_op = .{ .kind = .f32_sqrt, .expression = expr } },
            .f64 => return .{ .unary_op = .{ .kind = .f64_sqrt, .expression = expr } },
            else => |k| std.debug.panic("\nSqrt type {} not yet supported", .{k}),
        }
    }
    std.debug.panic("\nIntrinsic {} not yet supported", .{i.function});
}

fn branch(context: Context, b: type_checker.types.Branch) !types.Expression {
    const len = b.arms.len;
    const last_arm = b.arms[len - 1];
    const result_type = mapType(b.type);
    var result = types.Expression{
        .if_ = .{
            .result = result_type,
            .condition = try expressionAlloc(context, last_arm.condition),
            .then = try expressions(context, last_arm.then),
            .else_ = try expressions(context, b.else_),
        },
    };
    var iterator = std.mem.reverseIterator(b.arms[0 .. len - 1]);
    while (iterator.next()) |arm| {
        const else_ = try context.allocator.alloc(types.Expression, 1);
        else_[0] = result;
        result = types.Expression{
            .if_ = .{
                .result = result_type,
                .condition = try expressionAlloc(context, arm.condition),
                .then = try expressions(context, arm.then),
                .else_ = types.Expressions{ .expressions = else_ },
            },
        };
    }
    return result;
}

fn define(context: Context, d: type_checker.types.Define) !types.Expression {
    const name = d.name.value;
    try context.locals.append(types.Local{ .name = name, .type = mapType(d.name.type) });
    switch (d.value.*) {
        .undefined => return .nop,
        else => {
            const value = try expressionAlloc(context, d.value.*);
            return types.Expression{ .local_set = .{ .name = name, .value = value } };
        },
    }
}

fn addAssign(context: Context, a: type_checker.types.AddAssign) !types.Expression {
    var left = type_checker.types.Expression{ .symbol = a.name };
    const binary_op = type_checker.types.BinaryOp{
        .kind = .add,
        .left = &left,
        .right = a.value,
        .span = a.span,
        .type = type_checker.type_of.expression(a.value.*),
    };
    const value = try context.allocator.create(types.Expression);
    value.* = try binaryOp(context, binary_op);
    return .{ .local_set = .{ .name = a.name.value, .value = value } };
}

fn convert(context: Context, c: type_checker.types.Convert) !types.Expression {
    const value = try expressionAlloc(context, c.value.*);
    switch (type_checker.type_of.expression(c.value.*)) {
        .i32 => switch (c.type) {
            .f32 => return .{ .unary_op = .{ .kind = .f32_convert_i32_s, .expression = value } },
            else => |k| std.debug.panic("\nConvert type i32 to {} not yet supported", .{k}),
        },
        .f32 => switch (c.type) {
            .i32 => return .{ .unary_op = .{ .kind = .i32_trunc_f32_s, .expression = value } },
            else => |k| std.debug.panic("\nConvert type f32 to {} not yet supported", .{k}),
        },
        .i64 => switch (c.type) {
            .f64 => return .{ .unary_op = .{ .kind = .f64_convert_i64_s, .expression = value } },
            else => |k| std.debug.panic("\nConvert type i64 to {} not yet supported", .{k}),
        },
        .f64 => switch (c.type) {
            .i64 => return .{ .unary_op = .{ .kind = .i64_trunc_f64_s, .expression = value } },
            else => |k| std.debug.panic("\nConvert type f64 to {} not yet supported", .{k}),
        },
        else => |k| std.debug.panic("\nConvert type {} not yet supported", .{k}),
    }
}

fn storeArenaInLocal(context: Context, local: Interned) !types.Expression {
    const global_get = try context.allocator.create(types.Expression);
    global_get.* = .{ .global_get = .{ .name = context.builtins.arena } };
    return .{ .local_set = .{ .name = local, .value = global_get } };
}

fn storeStringOffsetInArena(context: Context, local: Interned, offset: u32) !types.Expression {
    const left = try context.allocator.create(types.Expression);
    left.* = .{ .local_get = .{ .name = local } };
    const right = try context.allocator.create(types.Expression);
    right.* = .{ .literal = .{ .u32 = offset } };
    return .{ .binary_op = .{ .kind = .i32_store, .left = left, .right = right } };
}

fn addLocalAndU32(context: Context, local: Interned, value: u32) !*const types.Expression {
    const left = try context.allocator.create(types.Expression);
    left.* = .{ .local_get = .{ .name = local } };
    const right = try context.allocator.create(types.Expression);
    right.* = .{ .literal = .{ .u32 = value } };
    const result = try context.allocator.create(types.Expression);
    result.* = .{ .binary_op = .{ .kind = .i32_add, .left = left, .right = right } };
    return result;
}

fn storeStringLengthInArena(context: Context, local: Interned, offset: u32) !types.Expression {
    const left = try addLocalAndU32(context, local, 4);
    const right = try context.allocator.create(types.Expression);
    right.* = .{ .literal = .{ .u32 = context.data_segment.offset - offset } };
    return .{ .binary_op = .{ .kind = .i32_store, .left = left, .right = right } };
}

pub fn putStringInDataSegment(data_segment: *types.DataSegment, s: type_checker.types.String) !types.Offset {
    const bytes = s.value.string();
    const offset = data_segment.offset;
    try data_segment.data.append(.{ .offset = offset, .bytes = bytes });
    data_segment.offset += @intCast(u32, bytes.len - 2);
    return offset;
}

fn string(context: Context, s: type_checker.types.String) !types.Expression {
    const local = try fresh_local(context, .i32);
    const exprs = try context.allocator.alloc(types.Expression, 5);
    const offset = try putStringInDataSegment(context.data_segment, s);
    exprs[0] = try storeArenaInLocal(context, local);
    exprs[1] = try storeStringOffsetInArena(context, local, offset);
    exprs[2] = try storeStringLengthInArena(context, local, offset);
    exprs[3] = .{ .global_set = .{
        .name = context.builtins.arena,
        .value = try addLocalAndU32(context, local, 8),
    } };
    exprs[4] = .{ .local_get = .{ .name = local } };
    return .{ .block = types.Block{ .result = .i32, .expressions = exprs } };
}

fn expression(context: Context, e: type_checker.types.Expression) error{ OutOfMemory, InvalidCharacter, Overflow }!types.Expression {
    switch (e) {
        .int => |i| return try int(i),
        .float => |f| return try float(f),
        .bool => |b| return try boolean(b),
        .block => |b| return .{ .expressions = try expressions(context, b) },
        .binary_op => |b| return try binaryOp(context, b),
        .symbol => |s| return symbol(s),
        .call => |c| return try call(context, c),
        .intrinsic => |i| return try intrinsic(context, i),
        .branch => |b| return try branch(context, b),
        .define => |d| return try define(context, d),
        .add_assign => |a| return try addAssign(context, a),
        .convert => |c| return try convert(context, c),
        .string => |s| return try string(context, s),
        else => |k| std.debug.panic("\ntypes.Expression {} not yet supported", .{k}),
    }
}

fn expressionAlloc(context: Context, e: type_checker.types.Expression) !*const types.Expression {
    const ptr = try context.allocator.create(types.Expression);
    ptr.* = try expression(context, e);
    return ptr;
}

fn function(allocator: Allocator, builtins: Builtins, data_segment: *types.DataSegment, intern: *Intern, name: Interned, f: type_checker.types.Function) !types.Function {
    const parameters = try allocator.alloc(types.Parameter, f.parameters.len);
    for (f.parameters, parameters) |typed_p, *ir_p| {
        ir_p.* = types.Parameter{
            .name = typed_p.value,
            .type = mapType(typed_p.type),
        };
    }
    var locals = List(types.Local).init(allocator);
    var next_local: u32 = 0;
    const context = Context{
        .allocator = allocator,
        .builtins = builtins,
        .locals = &locals,
        .next_local = &next_local,
        .data_segment = data_segment,
        .intern = intern,
    };
    const body = try expressions(context, f.body);
    return types.Function{
        .name = name,
        .parameters = parameters,
        .return_type = mapType(f.return_type),
        .locals = try locals.toOwnedSlice(),
        .body = body,
    };
}

fn foreignImport(allocator: Allocator, name: Interned, i: type_checker.types.ForeignImport) !types.ForeignImport {
    switch (i.type) {
        .function => |f| {
            const path = [2]Interned{ i.module, i.name };
            const function_type = try allocator.alloc(types.Type, f.len);
            for (f, function_type) |t, *ir_t| ir_t.* = mapType(t);
            return types.ForeignImport{
                .name = name,
                .path = path,
                .type = .{ .function = function_type },
            };
        },
        else => |k| std.debug.panic("\nForeign import type {} not yet supported", .{k}),
    }
}

pub fn module(allocator: Allocator, builtins: Builtins, m: type_checker.types.Module, intern: *Intern) !types.Module {
    var functions = std.ArrayList(types.Function).init(allocator);
    var imports = std.ArrayList(types.ForeignImport).init(allocator);
    var data_segment = types.DataSegment{
        .offset = 0,
        .data = List(types.Data).init(allocator),
    };
    var globals = std.ArrayList(types.Global).init(allocator);
    var exports = std.ArrayList(types.ForeignExport).init(allocator);
    for (m.order) |name| {
        if (m.typed.get(name)) |top_level| {
            switch (top_level) {
                .define => |d| {
                    const name_symbol = d.name.value;
                    switch (d.value.*) {
                        .function => |f| {
                            const lowered = try function(allocator, builtins, &data_segment, intern, name_symbol, f);
                            try functions.append(lowered);
                        },
                        .foreign_import => |i| {
                            const lowered = try foreignImport(allocator, name_symbol, i);
                            try imports.append(lowered);
                        },
                        .int => |i| {
                            const lowered = try int(i);
                            try globals.append(.{ .name = name_symbol, .type = mapType(d.name.type), .value = lowered });
                        },
                        else => |e| std.debug.panic("\nTop level kind {} no yet supported", .{e}),
                    }
                },
                .foreign_export => |e| {
                    const name_string = e.name.string();
                    const trimmed = try intern.store(name_string[1 .. name_string.len - 1]);
                    switch (e.value.*) {
                        .function => |f| {
                            const lowered = try function(allocator, builtins, &data_segment, intern, trimmed, f);
                            try functions.append(lowered);
                        },
                        .symbol => {},
                        else => |k| std.debug.panic("\nForeign export kind {} no yet supported", .{k}),
                    }
                    try exports.append(.{ .name = trimmed, .alias = trimmed });
                },
                else => std.debug.panic("\nTop level kind {} no yet supported", .{top_level}),
            }
        }
    }
    return types.Module{
        .functions = try functions.toOwnedSlice(),
        .foreign_imports = try imports.toOwnedSlice(),
        .globals = try globals.toOwnedSlice(),
        .data_segment = data_segment,
        .foreign_exports = try exports.toOwnedSlice(),
    };
}
