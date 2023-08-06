const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;
const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const type_checker = @import("../type_checker.zig");
const Builtins = @import("../builtins.zig").Builtins;
const types = @import("types.zig");
const size_of = @import("size_of.zig");

const LocalName = Interned;
const PointerName = Interned;

const Context = struct {
    allocator: Allocator,
    builtins: Builtins,
    locals: *List(types.Local),
    pointers: *List(types.LocalPointer),
    pointer_map: *Map(LocalName, PointerName),
    next_local: *u32,
    uses_memory: *bool,
    data_segment: *types.DataSegment,
    intern: *Intern,
    intrinsics: *types.Intrinsics,
};

fn freshLocal(context: Context, t: types.Type) !Interned {
    const text = try std.fmt.allocPrint(context.allocator, "{}", .{context.next_local.*});
    const interned = try context.intern.store(text);
    context.next_local.* += 1;
    try context.locals.append(.{ .name = interned, .type = t });
    return interned;
}

fn freshLocalPointer(context: Context, size: u32) !Interned {
    const text = try std.fmt.allocPrint(context.allocator, "{}", .{context.next_local.*});
    const interned = try context.intern.store(text);
    context.next_local.* += 1;
    try context.pointers.append(.{ .name = interned, .size = size });
    context.uses_memory.* = true;
    return interned;
}

fn mapType(monotype: type_checker.types.MonoType) types.Type {
    switch (monotype) {
        .u8 => return .i32,
        .i32 => return .i32,
        .i64 => return .i64,
        .f32 => return .f32,
        .f64 => return .f64,
        .bool => return .i32,
        .void => return .void,
        .array => return .i32,
        .enumeration => |e| {
            switch (size_of.enumeration(e)) {
                0...4 => return .i32,
                5...8 => return .i64,
                else => std.debug.panic("\nEnumeration with {} variants not yet supported", .{e.variants.len}),
            }
        },
        .structure => return .i32,
        else => std.debug.panic("\nMonotype {} not yet supported", .{monotype}),
    }
}

fn int(i: type_checker.types.Int) !types.Expression {
    switch (i.type) {
        .u8 => return .{ .literal = .{ .i32 = try std.fmt.parseInt(u8, i.value.string(), 10) } },
        .u32 => return .{ .literal = .{ .u32 = try std.fmt.parseInt(u32, i.value.string(), 10) } },
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
        .enumeration => |e| {
            switch (size_of.enumeration(e)) {
                0...4 => return .{ .binary_op = .{ .kind = .i32_eq, .left = left, .right = right } },
                5...8 => return .{ .binary_op = .{ .kind = .i64_eq, .left = left, .right = right } },
                else => std.debug.panic("\nEnumeration with {} variants not yet supported", .{e.variants.len}),
            }
        },
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
    if (s.binding.global) return types.Expression{ .global_get = .{ .name = s.value } };
    return .{ .local_get = .{ .name = s.value } };
}

fn getOrCreatePointer(context: Context, s: type_checker.types.Symbol) !Interned {
    const result = try context.pointer_map.getOrPut(s.value);
    if (result.found_existing) return result.value_ptr.*;
    const name = try std.fmt.allocPrint(context.allocator, "{s}/ptr", .{s.value.string()});
    const interned = try context.intern.store(name);
    try context.pointers.append(.{ .name = interned, .size = 4 });
    result.value_ptr.* = interned;
    context.uses_memory.* = true;
    return interned;
}

fn call(context: Context, c: type_checker.types.Call) !types.Expression {
    switch (c.function.*) {
        .symbol => |s| {
            var exprs = List(types.Expression).init(context.allocator);
            var deferred = List(types.Expression).init(context.allocator);
            var arguments = List(types.Expression).init(context.allocator);
            for (c.arguments) |arg| {
                if (arg.mutable) {
                    switch (arg.value) {
                        .symbol => |symbol_arg| {
                            switch (symbol_arg.type) {
                                .i32 => {
                                    const pointer = try getOrCreatePointer(context, symbol_arg);
                                    const local_get_ptr = try context.allocator.create(types.Expression);
                                    local_get_ptr.* = .{ .local_get = .{ .name = pointer } };
                                    const local_get = try context.allocator.create(types.Expression);
                                    local_get.* = .{ .local_get = .{ .name = symbol_arg.value } };
                                    try exprs.append(.{ .binary_op = .{ .kind = .i32_store, .left = local_get_ptr, .right = local_get } });
                                    const i32_load = try context.allocator.create(types.Expression);
                                    i32_load.* = .{ .unary_op = .{ .kind = .i32_load, .expression = local_get_ptr } };
                                    try deferred.append(.{ .local_set = .{ .name = symbol_arg.value, .value = i32_load } });
                                    try arguments.append(.{ .local_get = .{ .name = pointer } });
                                },
                                .array => try arguments.append(try expression(context, arg.value)),
                                else => |k| std.debug.panic("\nMutable argument type {} not yet supported", .{k}),
                            }
                        },
                        else => |k| std.debug.panic("\nMutable argument type {} not yet supported", .{k}),
                    }
                } else {
                    try arguments.append(try expression(context, arg.value));
                }
            }
            const f_type = type_checker.type_of.expression(c.function.*).function;
            for (c.named_arguments_order) |name| {
                for (f_type.parameters) |param| {
                    if (param.name.eql(name)) {
                        const arg = c.named_arguments.get(name).?;
                        try arguments.append(try expression(context, arg.value));
                    }
                }
            }
            try exprs.append(.{
                .call = .{
                    .function = s.value,
                    .arguments = try arguments.toOwnedSlice(),
                },
            });
            for (deferred.items) |expr| {
                try exprs.append(expr);
            }
            return .{ .expressions = .{ .expressions = try exprs.toOwnedSlice() } };
        },
        else => |k| std.debug.panic("\nCall function type {} not yet supported", .{k}),
    }
}

fn intrinsic(context: Context, i: type_checker.types.Intrinsic) !types.Expression {
    if (i.function.eql(context.builtins.sqrt)) {
        const expr = try expressionAlloc(context, i.arguments[0].value);
        switch (i.type) {
            .f32 => return .{ .unary_op = .{ .kind = .f32_sqrt, .expression = expr } },
            .f64 => return .{ .unary_op = .{ .kind = .f64_sqrt, .expression = expr } },
            else => |k| std.debug.panic("\nSqrt type {} not yet supported", .{k}),
        }
    }
    if (i.function.eql(context.builtins.empty)) {
        try context.intrinsics.put(.empty, void{});
        context.uses_memory.* = true;
        const arguments = try context.allocator.alloc(types.Expression, i.arguments.len);
        for (i.arguments, arguments) |arg, *ir_arg| {
            ir_arg.* = try expression(context, arg.value);
        }
        return .{
            .call_intrinsic = .{
                .intrinsic = .empty,
                .arguments = arguments,
            },
        };
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

fn drop(context: Context, d: type_checker.types.Drop) !types.Expression {
    switch (d.value.*) {
        .undefined => return .nop,
        else => {
            const value = try expressionAlloc(context, d.value.*);
            return types.Expression{ .drop = .{ .expression = value } };
        },
    }
}

fn plusEqual(context: Context, a: type_checker.types.PlusEqual) !types.Expression {
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

fn timesEqual(context: Context, a: type_checker.types.TimesEqual) !types.Expression {
    var left = type_checker.types.Expression{ .symbol = a.name };
    const binary_op = type_checker.types.BinaryOp{
        .kind = .multiply,
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

fn string(context: Context, s: type_checker.types.String) !types.Expression {
    const local = try freshLocalPointer(context, 8);
    const bytes = s.value.string();
    const offset = context.data_segment.offset;
    try context.data_segment.data.append(.{ .offset = offset, .bytes = bytes });
    context.data_segment.offset += @intCast(bytes.len);
    const exprs = try context.allocator.alloc(types.Expression, 3);
    const local_get = try context.allocator.create(types.Expression);
    local_get.* = .{ .local_get = .{ .name = local } };
    const literal_offset = try context.allocator.create(types.Expression);
    literal_offset.* = .{ .literal = .{ .u32 = offset } };
    exprs[0] = .{ .binary_op = .{ .kind = .i32_store, .left = local_get, .right = literal_offset } };
    const literal_4 = try context.allocator.create(types.Expression);
    literal_4.* = .{ .literal = .{ .u32 = 4 } };
    const binary_add = try context.allocator.create(types.Expression);
    binary_add.* = .{ .binary_op = .{ .kind = .i32_add, .left = local_get, .right = literal_4 } };
    const literal_length = try context.allocator.create(types.Expression);
    literal_length.* = .{ .literal = .{ .u32 = context.data_segment.offset - offset } };
    exprs[1] = .{ .binary_op = .{ .kind = .i32_store, .left = binary_add, .right = literal_length } };
    exprs[2] = .{ .local_get = .{ .name = local } };
    return .{ .block = types.Block{ .result = .i32, .expressions = exprs } };
}

fn variant(v: type_checker.types.Variant) !types.Expression {
    switch (v.type) {
        .enumeration => |e| {
            switch (size_of.enumeration(e)) {
                0...4 => return .{ .literal = .{ .u32 = @intCast(v.index) } },
                5...8 => return .{ .literal = .{ .u64 = v.index } },
                else => |k| std.debug.panic("\nVariant type {} not allowed", .{k}),
            }
        },
        else => |k| std.debug.panic("\nVariant type {} not allowed", .{k}),
    }
}

fn structLiteral(context: Context, s: type_checker.types.StructLiteral) !types.Expression {
    const size = size_of.monotype(s.type);
    const local = try freshLocalPointer(context, size);
    const exprs = try context.allocator.alloc(types.Expression, s.order.len + 1);
    const structure = s.type.structure_literal.structure.structure;
    const base = try context.allocator.create(types.Expression);
    base.* = .{ .local_get = .{ .name = local } };
    var offset: u32 = 0;
    for (structure.order, exprs[0..structure.order.len]) |o, *e| {
        const field = s.fields.get(o).?;
        const result = try expressionAlloc(context, field.value);
        const field_offset = try context.allocator.create(types.Expression);
        field_offset.* = .{ .literal = .{ .u32 = offset } };
        const field_address = blk: {
            if (offset > 0) {
                const ptr = try context.allocator.create(types.Expression);
                ptr.* = .{ .binary_op = .{ .kind = .i32_add, .left = base, .right = field_offset } };
                break :blk ptr;
            }
            break :blk base;
        };
        switch (type_checker.type_of.expression(field.value)) {
            .u8 => e.* = .{ .binary_op = .{ .kind = .i32_store8, .left = field_address, .right = result } },
            .array => {
                const size_expr = try context.allocator.create(types.Expression);
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
    exprs[s.order.len] = base.*;
    return .{ .block = types.Block{ .result = .i32, .expressions = exprs } };
}

fn array(context: Context, a: type_checker.types.Array) !types.Expression {
    const element_type = a.type.array.element_type;
    const size = size_of.monotype(element_type.*);
    const len = @as(u32, @intCast(a.expressions.len));
    const array_local = try freshLocalPointer(context, size * len);
    const exprs = try context.allocator.alloc(types.Expression, a.expressions.len + 3);
    const array_base = try context.allocator.create(types.Expression);
    array_base.* = .{ .local_get = .{ .name = array_local } };
    var offset: u32 = 0;
    for (a.expressions, exprs[0..len]) |e, *ir| {
        const result = try expressionAlloc(context, e);
        const field_offset = try context.allocator.create(types.Expression);
        field_offset.* = .{ .literal = .{ .u32 = offset } };
        const field_address = blk: {
            if (offset > 0) {
                const ptr = try context.allocator.create(types.Expression);
                ptr.* = .{ .binary_op = .{ .kind = .i32_add, .left = array_base, .right = field_offset } };
                break :blk ptr;
            }
            break :blk array_base;
        };
        switch (element_type.*) {
            .bool => ir.* = .{ .binary_op = .{ .kind = .i32_store8, .left = field_address, .right = result } },
            .u8 => ir.* = .{ .binary_op = .{ .kind = .i32_store8, .left = field_address, .right = result } },
            .i32 => ir.* = .{ .binary_op = .{ .kind = .i32_store, .left = field_address, .right = result } },
            .i64 => ir.* = .{ .binary_op = .{ .kind = .i64_store, .left = field_address, .right = result } },
            .f32 => ir.* = .{ .binary_op = .{ .kind = .f32_store, .left = field_address, .right = result } },
            .f64 => ir.* = .{ .binary_op = .{ .kind = .f64_store, .left = field_address, .right = result } },
            else => |k| std.debug.panic("\nField type {} not allowed", .{k}),
        }
        offset += size;
    }
    const local = try freshLocalPointer(context, 8);
    const local_get = try context.allocator.create(types.Expression);
    local_get.* = .{ .local_get = .{ .name = local } };
    exprs[len] = .{ .binary_op = .{ .kind = .i32_store, .left = local_get, .right = array_base } };
    const binary_add = try context.allocator.create(types.Expression);
    const literal_4 = try context.allocator.create(types.Expression);
    literal_4.* = .{ .literal = .{ .u32 = 4 } };
    binary_add.* = .{ .binary_op = .{ .kind = .i32_add, .left = local_get, .right = literal_4 } };
    const literal_size = try context.allocator.create(types.Expression);
    literal_size.* = .{ .literal = .{ .u32 = len } };
    exprs[len + 1] = .{ .binary_op = .{ .kind = .i32_store, .left = binary_add, .right = literal_size } };
    exprs[len + 2] = .{ .local_get = .{ .name = local } };
    return .{ .block = types.Block{ .result = .i32, .expressions = exprs } };
}

fn index(context: Context, i: type_checker.types.Index) !types.Expression {
    const local_get = try expressionAlloc(context, i.expression.*);
    const base = try context.allocator.create(types.Expression);
    base.* = .{ .unary_op = .{ .kind = .i32_load, .expression = local_get } };
    const first_index = try expressionAlloc(context, i.indices[0]);
    const literal_4 = try context.allocator.create(types.Expression);
    literal_4.* = .{ .literal = .{ .u32 = 4 } };
    const binary_add = try context.allocator.create(types.Expression);
    binary_add.* = .{ .binary_op = .{ .kind = .i32_add, .left = local_get, .right = literal_4 } };
    const length = try context.allocator.create(types.Expression);
    length.* = .{ .unary_op = .{ .kind = .i32_load, .expression = binary_add } };
    const out_of_bounds = try context.allocator.create(types.Expression);
    out_of_bounds.* = .{ .binary_op = .{ .kind = .i32_ge_u, .left = first_index, .right = length } };
    const size = try context.allocator.create(types.Expression);
    size.* = .{ .literal = .{ .u32 = size_of.monotype(i.type) } };
    const offset = try context.allocator.create(types.Expression);
    offset.* = .{ .binary_op = .{ .kind = .i32_mul, .left = first_index, .right = size } };
    const address = try context.allocator.create(types.Expression);
    address.* = .{ .binary_op = .{ .kind = .i32_add, .left = base, .right = offset } };
    const result_type = mapType(i.type);
    const result: types.Expression = switch (i.type) {
        .bool => .{ .unary_op = .{ .kind = .i32_load8_u, .expression = address } },
        .u8 => .{ .unary_op = .{ .kind = .i32_load8_u, .expression = address } },
        .i32 => .{ .unary_op = .{ .kind = .i32_load, .expression = address } },
        .i64 => .{ .unary_op = .{ .kind = .i64_load, .expression = address } },
        .f32 => .{ .unary_op = .{ .kind = .f32_load, .expression = address } },
        .f64 => .{ .unary_op = .{ .kind = .f64_load, .expression = address } },
        else => |k| std.debug.panic("\nIndex type {} not allowed", .{k}),
    };
    const then = try context.allocator.alloc(types.Expression, 1);
    then[0] = .unreachable_;
    const else_ = try context.allocator.alloc(types.Expression, 1);
    else_[0] = result;
    return types.Expression{
        .if_ = .{
            .result = result_type,
            .condition = out_of_bounds,
            .then = .{ .expressions = then },
            .else_ = .{ .expressions = else_ },
        },
    };
}

fn templateLiteral(context: Context, t: type_checker.types.TemplateLiteral) !types.Expression {
    if (t.strings.len == 1) {
        return try string(context, t.strings[0]);
    }
    const result = try freshLocalPointer(context, 8);
    const locals = try context.allocator.alloc(Interned, t.strings.len + t.arguments.len);
    for (locals) |*l| l.* = try freshLocal(context, .i32);
    var exprs = List(types.Expression).init(context.allocator);
    {
        const value = try context.allocator.create(types.Expression);
        value.* = try string(context, t.strings[0]);
        try exprs.append(.{ .local_set = .{ .name = locals[0], .value = value } });
    }
    var local_index: u64 = 1;
    for (t.strings[1..], t.arguments) |s, a| {
        switch (type_checker.type_of.expression(a)) {
            .array => |arr| {
                switch (arr.element_type.*) {
                    .u8 => {
                        const value = try context.allocator.create(types.Expression);
                        value.* = try expression(context, a);
                        try exprs.append(.{ .local_set = .{ .name = locals[local_index], .value = value } });
                    },
                    else => |k| std.debug.panic("\nTemplate literal array with element type {} not yet supported", .{k}),
                }
            },
            else => |k| std.debug.panic("\nTemplate literal argument type {} not yet supported", .{k}),
        }
        const value = try context.allocator.create(types.Expression);
        value.* = try string(context, s);
        try exprs.append(.{ .local_set = .{ .name = locals[local_index + 1], .value = value } });
        local_index += 2;
    }
    const destination = try freshLocal(context, .i32);
    const get_arena = try context.allocator.create(types.Expression);
    get_arena.* = .{ .global_get = .{ .name = context.builtins.core_arena } };
    try exprs.append(.{ .local_set = .{ .name = destination, .value = get_arena } });
    const get_destination = try context.allocator.create(types.Expression);
    get_destination.* = .{ .local_get = .{ .name = destination } };
    const total_length = try freshLocal(context, .i32);
    const get_total_length = try context.allocator.create(types.Expression);
    get_total_length.* = .{ .local_get = .{ .name = total_length } };
    const four = try context.allocator.create(types.Expression);
    four.* = .{ .literal = .{ .u32 = 4 } };
    const current_length = try freshLocal(context, .i32);
    const get_current_length = try context.allocator.create(types.Expression);
    get_current_length.* = .{ .local_get = .{ .name = current_length } };
    for (locals, 0..) |l, i| {
        const base = try context.allocator.create(types.Expression);
        base.* = .{ .local_get = .{ .name = l } };
        const ptr = try context.allocator.create(types.Expression);
        ptr.* = .{ .unary_op = .{ .kind = .i32_load, .expression = base } };
        const binary_op = try context.allocator.create(types.Expression);
        binary_op.* = .{ .binary_op = .{ .kind = .i32_add, .left = base, .right = four } };
        const length = try context.allocator.create(types.Expression);
        length.* = .{ .unary_op = .{ .kind = .i32_load, .expression = binary_op } };
        try exprs.append(.{ .local_set = .{ .name = current_length, .value = length } });
        if (i == 0) {
            try exprs.append(.{ .memory_copy = .{
                .destination = get_destination,
                .source = ptr,
                .size = get_current_length,
            } });
            try exprs.append(.{ .local_set = .{ .name = total_length, .value = get_current_length } });
        } else {
            const new_length = try context.allocator.create(types.Expression);
            new_length.* = .{ .binary_op = .{ .kind = .i32_add, .left = get_total_length, .right = get_current_length } };
            const new_destination = try context.allocator.create(types.Expression);
            new_destination.* = .{ .binary_op = .{ .kind = .i32_add, .left = get_destination, .right = get_total_length } };
            try exprs.append(.{ .memory_copy = .{
                .destination = new_destination,
                .source = ptr,
                .size = get_current_length,
            } });
            try exprs.append(.{ .local_set = .{ .name = total_length, .value = new_length } });
        }
    }
    const get_result = try context.allocator.create(types.Expression);
    get_result.* = .{ .local_get = .{ .name = result } };
    try exprs.append(.{ .binary_op = .{ .kind = .i32_store, .left = get_result, .right = get_destination } });
    const length_address = try context.allocator.create(types.Expression);
    length_address.* = .{ .binary_op = .{ .kind = .i32_add, .left = get_result, .right = four } };
    try exprs.append(.{ .binary_op = .{ .kind = .i32_store, .left = length_address, .right = get_total_length } });
    const new_arena = try context.allocator.create(types.Expression);
    new_arena.* = .{ .binary_op = .{ .kind = .i32_add, .left = get_destination, .right = get_total_length } };
    try exprs.append(.{ .global_set = .{ .name = context.builtins.core_arena, .value = new_arena } });
    try exprs.append(.{ .local_get = .{ .name = result } });
    return .{ .block = types.Block{ .result = .i32, .expressions = try exprs.toOwnedSlice() } };
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
        .drop => |d| return try drop(context, d),
        .plus_equal => |a| return try plusEqual(context, a),
        .times_equal => |a| return try timesEqual(context, a),
        .convert => |c| return try convert(context, c),
        .string => |s| return try string(context, s),
        .variant => |v| return try variant(v),
        .struct_literal => |s| return try structLiteral(context, s),
        .array => |a| return try array(context, a),
        .index => |i| return try index(context, i),
        .template_literal => |t| return try templateLiteral(context, t),
        else => |k| std.debug.panic("\ntypes.Expression {} not yet supported", .{k}),
    }
}

fn expressionAlloc(context: Context, e: type_checker.types.Expression) !*const types.Expression {
    const ptr = try context.allocator.create(types.Expression);
    ptr.* = try expression(context, e);
    return ptr;
}

fn function(allocator: Allocator, builtins: Builtins, data_segment: *types.DataSegment, uses_memory: *bool, intrinsics: *types.Intrinsics, intern: *Intern, name: Interned, f: type_checker.types.Function) !types.Function {
    var locals = List(types.Local).init(allocator);
    const parameters = try allocator.alloc(types.Parameter, f.parameters.len);
    var body = List(types.Expression).init(allocator);
    var deferred = List(types.Expression).init(allocator);
    for (f.parameters, parameters) |typed_p, *ir_p| {
        if (typed_p.mutable) {
            const parameter_name = try std.fmt.allocPrint(allocator, "{s}/ptr", .{typed_p.name.value.string()});
            const interned = try intern.store(parameter_name);
            ir_p.* = types.Parameter{
                .name = interned,
                .type = .i32,
            };
            try locals.append(.{ .name = typed_p.name.value, .type = mapType(typed_p.name.type) });
            const local_get_ptr = try allocator.create(types.Expression);
            local_get_ptr.* = .{ .local_get = .{ .name = interned } };
            const i32_load = try allocator.create(types.Expression);
            i32_load.* = .{ .unary_op = .{ .kind = .i32_load, .expression = local_get_ptr } };
            try body.append(.{ .local_set = .{ .name = typed_p.name.value, .value = i32_load } });
            const local_get = try allocator.create(types.Expression);
            local_get.* = .{ .local_get = .{ .name = typed_p.name.value } };
            try deferred.append(.{ .binary_op = .{
                .kind = .i32_store,
                .left = local_get_ptr,
                .right = local_get,
            } });
            uses_memory.* = true;
        } else {
            ir_p.* = types.Parameter{
                .name = typed_p.name.value,
                .type = mapType(typed_p.name.type),
            };
        }
    }
    var next_local: u32 = 0;
    var pointers = List(types.LocalPointer).init(allocator);
    var pointer_map = Map(LocalName, PointerName).init(allocator);
    const context = Context{
        .allocator = allocator,
        .builtins = builtins,
        .locals = &locals,
        .next_local = &next_local,
        .pointers = &pointers,
        .pointer_map = &pointer_map,
        .uses_memory = uses_memory,
        .data_segment = data_segment,
        .intern = intern,
        .intrinsics = intrinsics,
    };
    const last = f.body.expressions.len - 1;
    for (f.body.expressions, 0..) |expr, i| {
        const e = try expression(context, expr);
        if (i != last) {
            switch (type_checker.type_of.expression(expr)) {
                .void => try body.append(e),
                else => {
                    const value = try allocator.create(types.Expression);
                    value.* = e;
                    try body.append(.{ .drop = .{ .expression = value } });
                },
            }
        } else {
            try body.append(e);
        }
    }
    for (deferred.items) |expr| {
        try body.append(expr);
    }
    return types.Function{
        .name = name,
        .parameters = parameters,
        .return_type = mapType(f.return_type),
        .locals = try locals.toOwnedSlice(),
        .pointers = try pointers.toOwnedSlice(),
        .body = .{ .expressions = try body.toOwnedSlice() },
    };
}

fn foreignImport(allocator: Allocator, name: Interned, i: type_checker.types.ForeignImport) !types.ForeignImport {
    switch (i.type) {
        .function => |f| {
            const path = [2]Interned{ i.module, i.name };
            const parameters = try allocator.alloc(types.Type, f.parameters.len);
            for (f.parameters, parameters) |t, *ir_t| ir_t.* = mapType(t.type);
            const return_type = try allocator.create(types.Type);
            return_type.* = mapType(f.return_type.*);
            return types.ForeignImport{
                .name = name,
                .path = path,
                .type = .{ .function = .{ .parameters = parameters, .return_type = return_type } },
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
    var uses_memory = false;
    var intrinsics = types.Intrinsics.init(allocator);
    for (m.order) |name| {
        if (m.typed.get(name)) |top_level| {
            switch (top_level) {
                .define => |d| {
                    const name_symbol = d.name.value;
                    switch (d.value.*) {
                        .function => |f| {
                            const lowered = try function(allocator, builtins, &data_segment, &uses_memory, &intrinsics, intern, name_symbol, f);
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
                    const trimmed = try intern.store(name_string);
                    switch (e.value.*) {
                        .function => |f| {
                            const lowered = try function(allocator, builtins, &data_segment, &uses_memory, &intrinsics, intern, trimmed, f);
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
        .uses_memory = uses_memory,
        .foreign_exports = try exports.toOwnedSlice(),
        .intrinsics = intrinsics,
    };
}
