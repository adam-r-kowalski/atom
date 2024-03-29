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
const str = @import("str.zig");
const mapType = @import("map_type.zig").mapType;
const structure = @import("structure.zig");
const LocalName = structure.LocalName;
const PointerName = structure.PointerName;
const FieldName = structure.FieldName;
const Constructors = structure.Constructors;
const FieldAccesses = structure.FieldAccesses;
const Accesses = structure.Accesses;
const primitive = @import("primitive.zig");

const StrConcat = Map(usize, Interned);

const Context = struct {
    allocator: Allocator,
    builtins: Builtins,
    locals: *List(types.Local),
    pointers: *List(types.LocalPointer),
    pointer_map: *Map(LocalName, PointerName),
    next_local: *u32,
    uses_memory: *bool,
    uses_string: *bool,
    data_segment: *types.DataSegment,
    intern: *Intern,
    intrinsics: *types.Intrinsics,
    constructors: *Constructors,
    str_concat: *StrConcat,
    field_accesses: *FieldAccesses,
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
            for (c.arguments.positional) |arg| {
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
            switch (type_checker.type_of.expression(c.function.*)) {
                .function => |f_type| {
                    for (c.arguments.named_order) |name| {
                        for (f_type.parameters) |param| {
                            if (param.name.eql(name)) {
                                const arg = c.arguments.named.get(name).?;
                                try arguments.append(try expression(context, arg.value));
                            }
                        }
                    }
                },
                .structure => |s_type| {
                    for (c.arguments.named_order) |name| {
                        for (s_type.order) |field_name| {
                            if (field_name.eql(name)) {
                                const arg = c.arguments.named.get(name).?;
                                try arguments.append(try expression(context, arg.value));
                            }
                        }
                    }
                    try context.constructors.put(s_type.name, s_type);
                },
                else => |k| std.debug.panic("\nCall function type {} not yet supported", .{k}),
            }
            try exprs.append(.{
                .call = .{
                    .function = s.value,
                    .arguments = try arguments.toOwnedSlice(),
                },
            });
            for (deferred.items) |expr| try exprs.append(expr);
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
    context.uses_string.* = true;
    const bytes = s.value.string();
    const offset = context.data_segment.offset;
    try context.data_segment.data.append(.{ .offset = offset, .bytes = bytes });
    context.data_segment.offset += @intCast(bytes.len);
    const arguments = try context.allocator.alloc(types.Expression, 2);
    arguments[0] = .{ .literal = .{ .u32 = offset } };
    arguments[1] = .{ .literal = .{ .u32 = context.data_segment.offset - offset } };
    return .{
        .call = .{ .function = context.builtins.str, .arguments = arguments },
    };
}

fn enumerationIndex(comptime T: type, e: type_checker.monotype.Enumeration, name: Interned) T {
    var i: T = 0;
    for (e.variants) |variant| {
        if (variant.eql(name)) return i;
        i += 1;
    }
    std.debug.panic("\nEnumeration {} does not have variant {}", .{ e.name, name });
}

fn addFieldAccess(field_accesses: *FieldAccesses, s: type_checker.monotype.Structure, f: FieldName) !void {
    const accesses = try field_accesses.getOrPut(s.name);
    if (!accesses.found_existing) {
        accesses.value_ptr.* = Accesses{
            .structure = s,
            .fields = List(FieldName).init(field_accesses.allocator),
        };
    }
    try accesses.value_ptr.fields.append(f);
}

fn dot(context: Context, d: type_checker.types.Dot) !types.Expression {
    switch (type_checker.type_of.expression(d.left.*)) {
        .enumeration => |e| {
            switch (size_of.enumeration(e)) {
                0...4 => return .{ .literal = .{ .u32 = enumerationIndex(u32, e, d.right.value) } },
                5...8 => return .{ .literal = .{ .u64 = enumerationIndex(u64, e, d.right.value) } },
                else => |k| std.debug.panic("\nVariant type {} not allowed", .{k}),
            }
        },
        .structure => |s| {
            const field_name = d.right.value;
            const function_name = try std.fmt.allocPrint(context.allocator, "{}/{}", .{ s.name, field_name });
            const func = try context.intern.store(function_name);
            const arguments = try context.allocator.alloc(types.Expression, 1);
            arguments[0] = try expression(context, d.left.*);
            try addFieldAccess(context.field_accesses, s, field_name);
            return .{ .call = .{ .function = func, .arguments = arguments } };
        },
        else => |k| std.debug.panic("\nDot type {} not allowed", .{k}),
    }
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

fn strConcatName(context: Context, count: usize) !Interned {
    const result = try context.str_concat.getOrPut(count);
    if (result.found_existing) return result.value_ptr.*;
    const name = try std.fmt.allocPrint(context.allocator, "str/concat/{}", .{count});
    const interned = try context.intern.store(name);
    result.value_ptr.* = interned;
    return interned;
}

fn templateLiteral(context: Context, t: type_checker.types.TemplateLiteral) !types.Expression {
    if (t.strings.len == 1) {
        return try string(context, t.strings[0]);
    }
    const count = t.strings.len + t.arguments.len;
    var arguments = try context.allocator.alloc(types.Expression, count);
    arguments[0] = try string(context, t.strings[0]);
    var i: usize = 1;
    for (t.strings[1..], t.arguments) |s, a| {
        switch (type_checker.type_of.expression(a)) {
            .array => |arr| {
                switch (arr.element_type.*) {
                    .u8 => arguments[i] = try expression(context, a),
                    else => |k| std.debug.panic("\nTemplate literal array with element type {} not yet supported", .{k}),
                }
            },
            else => |k| std.debug.panic("\nTemplate literal argument type {} not yet supported", .{k}),
        }
        arguments[i + 1] = try string(context, s);
        i += 2;
    }
    const func = try strConcatName(context, count);
    return .{ .call = .{ .function = func, .arguments = arguments } };
}

fn expression(context: Context, e: type_checker.types.Expression) error{ OutOfMemory, InvalidCharacter, Overflow }!types.Expression {
    switch (e) {
        .int => |i| return try primitive.int(i),
        .float => |f| return try primitive.float(f),
        .bool => |b| return try primitive.boolean(b),
        .block => |b| return .{ .expressions = try expressions(context, b) },
        .binary_op => |b| return try binaryOp(context, b),
        .symbol => |s| return primitive.symbol(s),
        .call => |c| return try call(context, c),
        .intrinsic => |i| return try intrinsic(context, i),
        .branch => |b| return try branch(context, b),
        .define => |d| return try define(context, d),
        .drop => |d| return try drop(context, d),
        .plus_equal => |a| return try plusEqual(context, a),
        .times_equal => |a| return try timesEqual(context, a),
        .convert => |c| return try convert(context, c),
        .string => |s| return try string(context, s),
        .dot => |d| return try dot(context, d),
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

fn function(
    allocator: Allocator,
    builtins: Builtins,
    data_segment: *types.DataSegment,
    uses_memory: *bool,
    uses_string: *bool,
    intrinsics: *types.Intrinsics,
    intern: *Intern,
    constructors: *Constructors,
    str_concat: *StrConcat,
    field_accesses: *FieldAccesses,
    f: type_checker.types.Function,
) !types.Function {
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
        .uses_string = uses_string,
        .data_segment = data_segment,
        .intern = intern,
        .intrinsics = intrinsics,
        .constructors = constructors,
        .str_concat = str_concat,
        .field_accesses = field_accesses,
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
        .name = f.name.value,
        .parameters = parameters,
        .return_type = mapType(f.return_type),
        .locals = try locals.toOwnedSlice(),
        .pointers = try pointers.toOwnedSlice(),
        .body = .{ .expressions = try body.toOwnedSlice() },
    };
}

fn foreignImport(allocator: Allocator, f: type_checker.types.ForeignImport) !types.ForeignImport {
    const path = [2]Interned{ f.module, f.name };
    const parameters = try allocator.alloc(types.Type, f.prototype.parameters.len);
    for (f.prototype.parameters, parameters) |t, *ir_t| ir_t.* = mapType(t.name.type);
    const return_type = try allocator.create(types.Type);
    return_type.* = mapType(f.prototype.return_type);
    return types.ForeignImport{
        .name = f.prototype.name.value,
        .path = path,
        .type = .{ .function = .{ .parameters = parameters, .return_type = return_type } },
    };
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
    var uses_string = false;
    var intrinsics = types.Intrinsics.init(allocator);
    var constructors = Constructors.init(allocator);
    var str_concat = StrConcat.init(allocator);
    var field_accesses = FieldAccesses.init(allocator);
    for (m.order) |name| {
        if (m.typed.get(name)) |top_level| {
            switch (top_level) {
                .define => |d| {
                    const name_symbol = d.name.value;
                    switch (d.value.*) {
                        .int => |i| {
                            const lowered = try primitive.int(i);
                            try globals.append(.{ .name = name_symbol, .type = mapType(d.name.type), .value = lowered });
                        },
                        else => |e| std.debug.panic("\nTop level kind {} no yet supported", .{e}),
                    }
                },
                .foreign_export => |e| {
                    const name_string = e.name.string();
                    const trimmed = try intern.store(name_string);
                    const lowered = try function(
                        allocator,
                        builtins,
                        &data_segment,
                        &uses_memory,
                        &uses_string,
                        &intrinsics,
                        intern,
                        &constructors,
                        &str_concat,
                        &field_accesses,
                        e.function,
                    );
                    try functions.append(lowered);
                    try exports.append(.{ .name = e.name, .alias = trimmed });
                },
                .foreign_import => |f| {
                    const lowered = try foreignImport(allocator, f);
                    try imports.append(lowered);
                },
                .function => |f| {
                    const lowered = try function(
                        allocator,
                        builtins,
                        &data_segment,
                        &uses_memory,
                        &uses_string,
                        &intrinsics,
                        intern,
                        &constructors,
                        &str_concat,
                        &field_accesses,
                        f,
                    );
                    try functions.append(lowered);
                },
                else => std.debug.panic("\nTop level kind {} no yet supported", .{top_level}),
            }
        }
    }
    {
        var iterator = constructors.iterator();
        while (iterator.next()) |entry| {
            try functions.append(try structure.constructor(
                allocator,
                intern,
                &uses_memory,
                entry.key_ptr.*,
                entry.value_ptr.*,
            ));
        }
    }
    if (uses_string) {
        try functions.append(try str.constructor(allocator, builtins, intern, &uses_memory));
    }
    {
        var iterator = str_concat.iterator();
        while (iterator.next()) |entry| {
            try functions.append(try str.concat(
                allocator,
                builtins,
                intern,
                entry.key_ptr.*,
                entry.value_ptr.*,
            ));
        }
    }
    if (str_concat.count() > 0) {
        try functions.append(try str.concatFragment(allocator, intern));
        try functions.append(try str.pointer(allocator, intern));
        try functions.append(try str.length(allocator, intern));
    }
    {
        var iterator = field_accesses.valueIterator();
        while (iterator.next()) |entry| {
            try structure.fieldAccess(allocator, intern, &functions, entry.*);
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
