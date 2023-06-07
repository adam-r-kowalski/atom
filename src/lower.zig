const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const typed_ast = @import("typed_ast.zig");
const Module = typed_ast.Module;
const MonoType = @import("substitution.zig").MonoType;
const Builtins = @import("builtins.zig").Builtins;
const ir = @import("ir.zig");
const Type = ir.Type;
const Expression = ir.Expression;
const Local = ir.Local;
const Function = ir.Function;
const Import = ir.Import;
const Global = ir.Global;
const DataSegment = ir.DataSegment;
const Export = ir.Export;
const Parameter = ir.Parameter;
const IR = ir.IR;
const Literal = ir.Literal;
const Expressions = ir.Expressions;
const Block = ir.Block;

const Context = struct {
    allocator: Allocator,
    builtins: Builtins,
    locals: *List(Local),
    next_local: *u32,
    data_segment: *DataSegment,
    intern: *Intern,

    fn fresh_local(self: Context, t: Type) !Interned {
        const text = try std.fmt.allocPrint(self.allocator, "{}", .{self.next_local.*});
        const interned = try self.intern.store(text);
        self.next_local.* += 1;
        try self.locals.append(.{ .name = interned, .type = t });
        return interned;
    }
};

fn mapType(monotype: MonoType) Type {
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

fn int(i: typed_ast.Int) !Expression {
    switch (i.type) {
        .i32 => return .{ .literal = Literal{ .i32 = try std.fmt.parseInt(i32, i.value.string(), 10) } },
        .i64 => return .{ .literal = Literal{ .i64 = try std.fmt.parseInt(i64, i.value.string(), 10) } },
        .f32 => return .{ .literal = Literal{ .f32 = try std.fmt.parseFloat(f32, i.value.string()) } },
        .f64 => return .{ .literal = Literal{ .f64 = try std.fmt.parseFloat(f64, i.value.string()) } },
        else => |k| std.debug.panic("\nInt type {} not yet supported", .{k}),
    }
}

fn float(f: typed_ast.Float) !Expression {
    switch (f.type) {
        .f32 => return .{ .literal = Literal{ .f32 = try std.fmt.parseFloat(f32, f.value.string()) } },
        .f64 => return .{ .literal = Literal{ .f64 = try std.fmt.parseFloat(f64, f.value.string()) } },
        else => |k| std.debug.panic("\nFloat type {} not yet supported", .{k}),
    }
}

fn boolean(b: typed_ast.Bool) !Expression {
    return Expression{ .literal = Literal{ .bool = b.value } };
}

fn expressions(context: Context, b: typed_ast.Block) !Expressions {
    const exprs = try context.allocator.alloc(Expression, b.expressions.len);
    for (b.expressions, exprs) |expr, *ir_expr| {
        ir_expr.* = try expression(context, expr);
    }
    return Expressions{ .expressions = exprs };
}

fn add(context: Context, b: typed_ast.BinaryOp) !Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .binary_op = .{ .kind = .i32_add, .left = left, .right = right } },
        .i64 => return Expression{ .binary_op = .{ .kind = .i64_add, .left = left, .right = right } },
        .f32 => return Expression{ .binary_op = .{ .kind = .f32_add, .left = left, .right = right } },
        .f64 => return Expression{ .binary_op = .{ .kind = .f64_add, .left = left, .right = right } },
        else => |k| std.debug.panic("\nAdd type {} not yet supported", .{k}),
    }
}

fn subtract(context: Context, b: typed_ast.BinaryOp) !Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .binary_op = .{ .kind = .i32_sub, .left = left, .right = right } },
        .i64 => return Expression{ .binary_op = .{ .kind = .i64_sub, .left = left, .right = right } },
        .f32 => return Expression{ .binary_op = .{ .kind = .f32_sub, .left = left, .right = right } },
        .f64 => return Expression{ .binary_op = .{ .kind = .f64_sub, .left = left, .right = right } },
        else => |k| std.debug.panic("\nSubtract type {} not yet supported", .{k}),
    }
}

fn multiply(context: Context, b: typed_ast.BinaryOp) !Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .binary_op = .{ .kind = .i32_mul, .left = left, .right = right } },
        .i64 => return Expression{ .binary_op = .{ .kind = .i64_mul, .left = left, .right = right } },
        .f32 => return Expression{ .binary_op = .{ .kind = .f32_mul, .left = left, .right = right } },
        .f64 => return Expression{ .binary_op = .{ .kind = .f64_mul, .left = left, .right = right } },
        else => |k| std.debug.panic("\nMultiply type {} not yet supported", .{k}),
    }
}

fn divide(context: Context, b: typed_ast.BinaryOp) !Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .binary_op = .{ .kind = .i32_div_s, .left = left, .right = right } },
        .i64 => return Expression{ .binary_op = .{ .kind = .i64_div_s, .left = left, .right = right } },
        .f32 => return Expression{ .binary_op = .{ .kind = .f32_div, .left = left, .right = right } },
        .f64 => return Expression{ .binary_op = .{ .kind = .f64_div, .left = left, .right = right } },
        else => |k| std.debug.panic("\nDivide type {} not yet supported", .{k}),
    }
}

fn modulo(context: Context, b: typed_ast.BinaryOp) !Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .binary_op = .{ .kind = .i32_rem_s, .left = left, .right = right } },
        .i64 => return Expression{ .binary_op = .{ .kind = .i64_rem_s, .left = left, .right = right } },
        else => |k| std.debug.panic("\nModulo type {} not yet supported", .{k}),
    }
}

fn equal(context: Context, b: typed_ast.BinaryOp) !Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .binary_op = .{ .kind = .i32_eq, .left = left, .right = right } },
        .i64 => return Expression{ .binary_op = .{ .kind = .i64_eq, .left = left, .right = right } },
        .f32 => return Expression{ .binary_op = .{ .kind = .f32_eq, .left = left, .right = right } },
        .f64 => return Expression{ .binary_op = .{ .kind = .f64_eq, .left = left, .right = right } },
        else => |k| std.debug.panic("\nEqual type {} not yet supported", .{k}),
    }
}

fn binaryOr(context: Context, b: typed_ast.BinaryOp) !Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (b.left.typeOf()) {
        .bool => return Expression{ .binary_op = .{ .kind = .i32_or, .left = left, .right = right } },
        else => |k| std.debug.panic("\nOr type {} not yet supported", .{k}),
    }
}

fn greater(context: Context, b: typed_ast.BinaryOp) !Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .binary_op = .{ .kind = .i32_gt_s, .left = left, .right = right } },
        .i64 => return Expression{ .binary_op = .{ .kind = .i64_gt_s, .left = left, .right = right } },
        .f32 => return Expression{ .binary_op = .{ .kind = .f32_gt, .left = left, .right = right } },
        .f64 => return Expression{ .binary_op = .{ .kind = .f64_gt, .left = left, .right = right } },
        else => |k| std.debug.panic("\nGreater type {} not yet supported", .{k}),
    }
}

fn less(context: Context, b: typed_ast.BinaryOp) !Expression {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .binary_op = .{ .kind = .i32_lt_s, .left = left, .right = right } },
        .i64 => return Expression{ .binary_op = .{ .kind = .i64_lt_s, .left = left, .right = right } },
        .f32 => return Expression{ .binary_op = .{ .kind = .f32_lt, .left = left, .right = right } },
        .f64 => return Expression{ .binary_op = .{ .kind = .f64_lt, .left = left, .right = right } },
        else => |k| std.debug.panic("\nGreater type {} not yet supported", .{k}),
    }
}

fn binaryOp(context: Context, b: typed_ast.BinaryOp) !Expression {
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

fn symbol(s: typed_ast.Symbol) Expression {
    if (s.binding.global) return Expression{ .global_get = .{ .name = s.value } };
    return Expression{ .local_get = .{ .name = s.value } };
}

fn call(context: Context, c: typed_ast.Call) !Expression {
    switch (c.function.*) {
        .symbol => |s| {
            const arguments = try context.allocator.alloc(Expression, c.arguments.len);
            for (c.arguments, arguments) |arg, *ir_arg| {
                ir_arg.* = try expression(context, arg);
            }
            return Expression{
                .call = .{
                    .function = s.value,
                    .arguments = arguments,
                },
            };
        },
        else => |k| std.debug.panic("\nCall function type {} not yet supported", .{k}),
    }
}

fn intrinsic(context: Context, i: typed_ast.Intrinsic) !Expression {
    if (i.function.eql(context.builtins.sqrt)) {
        const expr = try expressionAlloc(context, i.arguments[0]);
        switch (i.type) {
            .f32 => return Expression{ .unary_op = .{ .kind = .f32_sqrt, .expression = expr } },
            .f64 => return Expression{ .unary_op = .{ .kind = .f64_sqrt, .expression = expr } },
            else => |k| std.debug.panic("\nSqrt type {} not yet supported", .{k}),
        }
    }
    std.debug.panic("\nIntrinsic {} not yet supported", .{i.function});
}

fn branch(context: Context, b: typed_ast.Branch) !Expression {
    const len = b.arms.len;
    const last_arm = b.arms[len - 1];
    const result_type = mapType(b.type);
    var result = Expression{
        .if_ = .{
            .result = result_type,
            .condition = try expressionAlloc(context, last_arm.condition),
            .then = try expressions(context, last_arm.then),
            .else_ = try expressions(context, b.else_),
        },
    };
    var iterator = std.mem.reverseIterator(b.arms[0 .. len - 1]);
    while (iterator.next()) |arm| {
        const else_ = try context.allocator.alloc(Expression, 1);
        else_[0] = result;
        result = Expression{
            .if_ = .{
                .result = result_type,
                .condition = try expressionAlloc(context, arm.condition),
                .then = try expressions(context, arm.then),
                .else_ = Expressions{ .expressions = else_ },
            },
        };
    }
    return result;
}

fn define(context: Context, d: typed_ast.Define) !Expression {
    const name = d.name.value;
    try context.locals.append(Local{ .name = name, .type = mapType(d.name.type) });
    switch (d.value.*) {
        .undefined => return .nop,
        else => {
            const value = try expressionAlloc(context, d.value.*);
            return Expression{ .local_set = .{ .name = name, .value = value } };
        },
    }
}

fn convert(context: Context, c: typed_ast.Convert) !Expression {
    const value = try expressionAlloc(context, c.value.*);
    switch (c.value.typeOf()) {
        .i32 => switch (c.type) {
            .f32 => return Expression{ .unary_op = .{ .kind = .f32_convert_i32_s, .expression = value } },
            else => |k| std.debug.panic("\nConvert type i32 to {} not yet supported", .{k}),
        },
        .f32 => switch (c.type) {
            .i32 => return Expression{ .unary_op = .{ .kind = .i32_trunc_f32_s, .expression = value } },
            else => |k| std.debug.panic("\nConvert type f32 to {} not yet supported", .{k}),
        },
        .i64 => switch (c.type) {
            .f64 => return Expression{ .unary_op = .{ .kind = .f64_convert_i64_s, .expression = value } },
            else => |k| std.debug.panic("\nConvert type i64 to {} not yet supported", .{k}),
        },
        .f64 => switch (c.type) {
            .i64 => return Expression{ .unary_op = .{ .kind = .i64_trunc_f64_s, .expression = value } },
            else => |k| std.debug.panic("\nConvert type f64 to {} not yet supported", .{k}),
        },
        else => |k| std.debug.panic("\nConvert type {} not yet supported", .{k}),
    }
}

fn storeArenaInLocal(context: Context, local: Interned) !Expression {
    const global_get = try context.allocator.create(Expression);
    global_get.* = .{ .global_get = .{ .name = context.builtins.arena } };
    return .{ .local_set = .{ .name = local, .value = global_get } };
}

fn storeStringOffsetInArena(context: Context, local: Interned, offset: u32) !Expression {
    const left = try context.allocator.create(Expression);
    left.* = .{ .local_get = .{ .name = local } };
    const right = try context.allocator.create(Expression);
    right.* = .{ .literal = .{ .u32 = offset } };
    return .{ .binary_op = .{ .kind = .i32_store, .left = left, .right = right } };
}

fn addLocalAndU32(context: Context, local: Interned, value: u32) !*const Expression {
    const left = try context.allocator.create(Expression);
    left.* = .{ .local_get = .{ .name = local } };
    const right = try context.allocator.create(Expression);
    right.* = .{ .literal = .{ .u32 = value } };
    const result = try context.allocator.create(Expression);
    result.* = .{ .binary_op = .{ .kind = .i32_add, .left = left, .right = right } };
    return result;
}

fn storeStringLengthInArena(context: Context, local: Interned, offset: u32) !Expression {
    const left = try addLocalAndU32(context, local, 4);
    const right = try context.allocator.create(Expression);
    right.* = .{ .literal = .{ .u32 = context.data_segment.offset - offset } };
    return .{ .binary_op = .{ .kind = .i32_store, .left = left, .right = right } };
}

fn string(context: Context, s: typed_ast.String) !Expression {
    const local = try context.fresh_local(.i32);
    const exprs = try context.allocator.alloc(Expression, 5);
    const offset = try context.data_segment.string(s);
    exprs[0] = try storeArenaInLocal(context, local);
    exprs[1] = try storeStringOffsetInArena(context, local, offset);
    exprs[2] = try storeStringLengthInArena(context, local, offset);
    exprs[3] = .{ .global_set = .{
        .name = context.builtins.arena,
        .value = try addLocalAndU32(context, local, 8),
    } };
    exprs[4] = .{ .local_get = .{ .name = local } };
    return .{ .block = Block{ .result = .i32, .expressions = exprs } };
}

fn expression(context: Context, e: typed_ast.Expression) error{ OutOfMemory, InvalidCharacter, Overflow }!Expression {
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
        .convert => |c| return try convert(context, c),
        .string => |s| return try string(context, s),
        else => |k| std.debug.panic("\nExpression {} not yet supported", .{k}),
    }
}

fn expressionAlloc(context: Context, e: typed_ast.Expression) !*const Expression {
    const ptr = try context.allocator.create(Expression);
    ptr.* = try expression(context, e);
    return ptr;
}

fn function(allocator: Allocator, builtins: Builtins, data_segment: *DataSegment, intern: *Intern, name: Interned, f: typed_ast.Function) !Function {
    const parameters = try allocator.alloc(Parameter, f.parameters.len);
    for (f.parameters, parameters) |typed_p, *ir_p| {
        ir_p.* = Parameter{
            .name = typed_p.value,
            .type = mapType(typed_p.type),
        };
    }
    var locals = List(Local).init(allocator);
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
    return Function{
        .name = name,
        .parameters = parameters,
        .return_type = mapType(f.return_type),
        .locals = try locals.toOwnedSlice(),
        .body = body,
    };
}

fn foreignImport(allocator: Allocator, name: Interned, i: typed_ast.ForeignImport) !Import {
    switch (i.type) {
        .function => |f| {
            const path = [2]Interned{ i.module, i.name };
            const function_type = try allocator.alloc(Type, f.len);
            for (f, function_type) |t, *ir_t| ir_t.* = mapType(t);
            return Import{
                .name = name,
                .path = path,
                .type = .{ .function = function_type },
            };
        },
        else => |k| std.debug.panic("\nForeign import type {} not yet supported", .{k}),
    }
}

pub fn buildIr(allocator: Allocator, builtins: Builtins, module: Module) !IR {
    var functions = std.ArrayList(Function).init(allocator);
    var imports = std.ArrayList(Import).init(allocator);
    var data_segment = DataSegment.init(allocator);
    var globals = std.ArrayList(Global).init(allocator);
    var exports = std.ArrayList(Export).init(allocator);
    for (module.order) |name| {
        if (module.typed.get(name)) |top_level| {
            switch (top_level) {
                .define => |d| {
                    const name_symbol = d.name.value;
                    switch (d.value.*) {
                        .function => |f| {
                            const lowered = try function(allocator, builtins, &data_segment, module.intern, name_symbol, f);
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
                    const trimmed = try module.intern.store(name_string[1 .. name_string.len - 1]);
                    switch (e.value.*) {
                        .function => |f| {
                            const lowered = try function(allocator, builtins, &data_segment, module.intern, trimmed, f);
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
    return IR{
        .functions = try functions.toOwnedSlice(),
        .imports = try imports.toOwnedSlice(),
        .globals = try globals.toOwnedSlice(),
        .data_segment = data_segment,
        .exports = try exports.toOwnedSlice(),
    };
}
