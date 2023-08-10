const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;

const Interned = @import("../interner.zig").Interned;
const Intern = @import("../interner.zig").Intern;
const Builtins = @import("../builtins.zig").Builtins;
const types = @import("types.zig");
const spanOf = @import("span.zig").expression;
const typeOf = @import("type_of.zig").expression;
const parser = @import("../parser.zig");
const Errors = @import("../error_reporter.zig").types.Errors;
const monotype = @import("monotype.zig");
const MonoType = monotype.MonoType;

pub const WorkQueue = List(Interned);

pub const Scopes = struct {
    allocator: Allocator,
    base: types.Scope,
    active: *List(types.Scope),
    work_queue: *WorkQueue,
    errors: *Errors,
};

fn pushScope(scopes: *Scopes) !void {
    try scopes.active.append(types.Scope.init(scopes.allocator));
}

fn popScope(scopes: *Scopes) void {
    _ = scopes.active.pop();
}

fn putInScope(scopes: *Scopes, name: Interned, binding: types.Binding) !void {
    try scopes.active.items[scopes.active.items.len - 1].put(name, binding);
}

pub fn findInScope(scopes: Scopes, s: parser.types.Symbol) !types.Binding {
    var reverse_iterator = std.mem.reverseIterator(scopes.active.items);
    while (reverse_iterator.next()) |scope| {
        if (scope.get(s.value)) |binding| return binding;
    }
    if (scopes.base.get(s.value)) |binding| {
        try scopes.work_queue.append(s.value);
        return binding;
    }
    var in_scope = List(Interned).init(scopes.allocator);
    var base_iterator = scopes.base.keyIterator();
    while (base_iterator.next()) |key| try in_scope.append(key.*);
    for (scopes.active.items) |scope| {
        var scope_iterator = scope.keyIterator();
        while (scope_iterator.next()) |key| try in_scope.append(key.*);
    }
    try scopes.errors.undefined_variable.append(.{
        .symbol = s.value,
        .span = s.span,
        .in_scope = try in_scope.toOwnedSlice(),
    });
    return error.CompileError;
}

pub fn expressionToMonoType(allocator: Allocator, scope: types.Scope, builtins: Builtins, e: parser.types.Expression) !MonoType {
    switch (e) {
        .symbol => |s| {
            if (s.value.eql(builtins.u8)) return .{ .u8 = .{ .span = s.span } };
            if (s.value.eql(builtins.i32)) return .{ .i32 = .{ .span = s.span } };
            if (s.value.eql(builtins.i64)) return .{ .i64 = .{ .span = s.span } };
            if (s.value.eql(builtins.f32)) return .{ .f32 = .{ .span = s.span } };
            if (s.value.eql(builtins.f64)) return .{ .f64 = .{ .span = s.span } };
            if (s.value.eql(builtins.bool)) return .{ .bool = .{ .span = s.span } };
            if (s.value.eql(builtins.void)) return .{ .void = .{ .span = s.span } };
            if (s.value.eql(builtins.str)) {
                const element_type = try allocator.create(MonoType);
                element_type.* = .{ .u8 = .{ .span = null } };
                return .{ .array = .{
                    .rank = 1,
                    .element_type = element_type,
                    .span = s.span,
                } };
            }
            if (scope.get(s.value)) |binding| {
                return monotype.withSpan(binding.type, s.span);
            }
            std.debug.panic("\nCannot convert symbol {} to mono type", .{s});
        },
        .prototype => |p| {
            const len = p.parameters.len;
            const parameters = try allocator.alloc(monotype.Parameter, len);
            for (p.parameters, parameters) |param, *t| {
                const mono = try expressionToMonoType(allocator, scope, builtins, param.type);
                t.* = .{
                    .name = param.name.value,
                    .type = monotype.withSpan(mono, param.span),
                    .mutable = param.mutable,
                };
            }
            const return_type = try allocator.create(MonoType);
            return_type.* = try expressionToMonoType(allocator, scope, builtins, p.return_type.*);
            return MonoType{ .function = .{
                .parameters = parameters,
                .return_type = return_type,
                .span = p.span,
            } };
        },
        .index => |i| {
            switch (i.expression.*) {
                .symbol => |s| {
                    if (s.value.eql(builtins.vec)) {
                        std.debug.assert(i.indices.len == 1);
                        const element_type = try allocator.create(MonoType);
                        element_type.* = try expressionToMonoType(allocator, scope, builtins, i.indices[0]);
                        return .{ .array = .{
                            .rank = 1,
                            .element_type = element_type,
                            .span = s.span,
                        } };
                    }
                    std.debug.panic("\nCannot convert index {} to mono type", .{s.value});
                },
                else => |k| std.debug.panic("\nCannot convert index {} to mono type", .{k}),
            }
        },
        else => std.debug.panic("\nCannot convert expression {} to mono type", .{e}),
    }
}

const Context = struct {
    allocator: Allocator,
    builtins: Builtins,
    constraints: *types.Constraints,
    scopes: *Scopes,
    errors: *Errors,
};

fn symbol(scopes: Scopes, s: parser.types.Symbol) !types.Symbol {
    const binding = try findInScope(scopes, s);
    return types.Symbol{
        .value = s.value,
        .span = s.span,
        .type = monotype.withSpan(binding.type, s.span),
        .binding = binding,
    };
}

fn freshTypeVar(cs: *types.Constraints, span: ?types.Span) MonoType {
    const typevar = cs.next_type_var;
    cs.next_type_var += 1;
    return .{ .typevar = .{ .value = typevar, .span = span } };
}

fn int(context: Context, i: parser.types.Int) types.Int {
    return types.Int{
        .value = i.value,
        .span = i.span,
        .type = freshTypeVar(context.constraints, i.span),
    };
}

fn float(context: Context, f: parser.types.Float) types.Float {
    return types.Float{
        .value = f.value,
        .span = f.span,
        .type = freshTypeVar(context.constraints, f.span),
    };
}

fn string(context: Context, s: parser.types.String) !types.String {
    const element_type = try context.allocator.create(MonoType);
    element_type.* = .{ .u8 = .{ .span = null } };
    return types.String{
        .value = s.value,
        .span = s.span,
        .type = .{ .array = .{ .rank = 1, .element_type = element_type, .span = s.span } },
    };
}

fn boolean(b: parser.types.Bool) types.Bool {
    return types.Bool{
        .value = b.value,
        .span = b.span,
        .type = .{ .bool = .{ .span = b.span } },
    };
}

fn untypedUndefined(context: Context, u: parser.types.Undefined) types.Undefined {
    return types.Undefined{
        .span = u.span,
        .type = freshTypeVar(context.constraints, u.span),
    };
}

fn structLiteral(context: Context, s: parser.types.StructLiteral) !types.StructLiteral {
    var field_types = Map(Interned, MonoType).init(context.allocator);
    var fields = Map(Interned, types.Field).init(context.allocator);
    for (s.order) |o| {
        const field = s.fields.get(o).?;
        const expr = try expression(context, field.value);
        const mono = typeOf(expr);
        try field_types.putNoClobber(o, mono);
        try fields.putNoClobber(o, .{
            .name = .{
                .value = field.name.value,
                .span = field.name.span,
                .type = monotype.withSpan(mono, field.name.span),
                .binding = .{
                    .type = mono,
                    .global = false,
                    .mutable = false,
                    .span = field.name.span,
                },
            },
            .value = expr,
            .span = field.span,
        });
    }
    const structure = try context.allocator.create(MonoType);
    structure.* = freshTypeVar(context.constraints, s.span);
    const mono = MonoType{ .structure_literal = .{
        .fields = field_types,
        .order = s.order,
        .span = s.span,
        .structure = structure,
    } };
    return types.StructLiteral{
        .fields = fields,
        .order = s.order,
        .type = mono,
        .span = s.span,
    };
}

fn branch(context: Context, b: parser.types.Branch) !types.Branch {
    const arms = try context.allocator.alloc(types.Arm, b.arms.len);
    const result_type = freshTypeVar(context.constraints, b.span);
    for (arms, b.arms) |*typed_arm, untyped_arm| {
        const condition = try expression(context, untyped_arm.condition);
        const then = try block(context, untyped_arm.then);
        typed_arm.* = types.Arm{ .condition = condition, .then = then };
        try context.constraints.equal.appendSlice(&.{
            .{
                .left = typeOf(condition),
                .right = .{ .bool = .{ .span = null } },
            },
            .{ .left = then.type, .right = result_type },
        });
    }
    const else_ = try block(context, b.else_);
    try context.constraints.equal.append(.{ .left = else_.type, .right = result_type });
    return types.Branch{
        .arms = arms,
        .else_ = else_,
        .type = result_type,
        .span = b.span,
    };
}

fn dot(context: Context, b: parser.types.BinaryOp) !types.Expression {
    switch (b.right.*) {
        .symbol => |right| {
            switch (b.left.*) {
                .symbol => |left| {
                    const binding = try findInScope(context.scopes.*, left);
                    switch (binding.type) {
                        .enumeration => |e| {
                            for (e.variants, 0..) |v, i| {
                                if (v.eql(right.value)) {
                                    return types.Expression{
                                        .variant = .{
                                            .value = v,
                                            .index = i,
                                            .span = b.span,
                                            .type = monotype.withSpan(binding.type, b.span),
                                        },
                                    };
                                }
                            }
                            std.debug.panic("\nvariant {s} not found in enumeration {}", .{ right.value.string(), binding.type });
                        },
                        else => |k| std.debug.panic("Expected enumeration, got {}", .{k}),
                    }
                    std.debug.panic("\nfound binding {} for symbol {s}", .{ binding, left.value.string() });
                },
                else => |t| std.debug.panic("Expected symbol before dot, got {}", .{t}),
            }
        },
        else => |k| std.debug.panic("Expected symbol after dot, got {}", .{k}),
    }
}

fn pipeline(context: Context, b: parser.types.BinaryOp) !types.Expression {
    switch (b.right.*) {
        .call => |c| {
            const positional = try context.allocator.alloc(parser.types.Argument, c.arguments.positional.len + 1);
            positional[0] = .{
                .value = b.left.*,
                .mutable = false,
                .span = parser.span.expression(b.left.*),
            };
            @memcpy(positional[1..], c.arguments.positional);
            const arguments = parser.types.Arguments{
                .positional = positional,
                .named = Map(Interned, parser.types.Argument).init(context.allocator),
                .named_order = &.{},
                .span = c.arguments.span,
            };
            const new_call = parser.types.Call{
                .function = c.function,
                .arguments = arguments,
                .span = b.span,
            };
            return try call(context, new_call);
        },
        else => |k| std.debug.panic("Expected call after pipeline, got {}", .{k}),
    }
}

fn binaryOp(context: Context, b: parser.types.BinaryOp) !types.Expression {
    switch (b.kind) {
        .dot => return dot(context, b),
        .pipeline => return pipeline(context, b),
        .equal, .greater, .less => {
            const left = try expressionAlloc(context, b.left.*);
            const right = try expressionAlloc(context, b.right.*);
            try context.constraints.equal.append(.{ .left = typeOf(left.*), .right = typeOf(right.*) });
            return types.Expression{
                .binary_op = .{
                    .kind = b.kind,
                    .left = left,
                    .right = right,
                    .span = b.span,
                    .type = .{ .bool = .{ .span = b.span } },
                },
            };
        },
        else => {
            const left = try expressionAlloc(context, b.left.*);
            const right = try expressionAlloc(context, b.right.*);
            const left_type = typeOf(left.*);
            try context.constraints.equal.append(.{ .left = left_type, .right = typeOf(right.*) });
            const tvar = freshTypeVar(context.constraints, null);
            try context.constraints.equal.append(.{ .left = left_type, .right = tvar });
            return types.Expression{
                .binary_op = .{
                    .kind = b.kind,
                    .left = left,
                    .right = right,
                    .span = b.span,
                    .type = tvar,
                },
            };
        },
    }
}

fn define(context: Context, d: parser.types.Define) !types.Define {
    const value = try expressionAlloc(context, d.value.*);
    var mono = typeOf(value.*);
    if (d.type) |t| {
        const annotated_type = try expressionToMonoType(context.allocator, context.scopes.base, context.builtins, t.*);
        try context.constraints.equal.append(.{ .left = annotated_type, .right = mono });
        mono = annotated_type;
    }
    const binding = types.Binding{
        .type = mono,
        .global = false,
        .mutable = d.mutable,
        .span = d.name.span,
    };
    const name = types.Symbol{
        .value = d.name.value,
        .span = d.span,
        .type = mono,
        .binding = binding,
    };
    try putInScope(context.scopes, name.value, binding);
    return types.Define{
        .name = name,
        .value = value,
        .span = d.span,
        .mutable = d.mutable,
        .type = .{ .void = .{ .span = d.span } },
    };
}

fn drop(context: Context, d: parser.types.Drop) !types.Drop {
    const value = try expressionAlloc(context, d.value.*);
    var mono = typeOf(value.*);
    if (d.type) |t| {
        const annotated_type = try expressionToMonoType(context.allocator, context.scopes.base, context.builtins, t.*);
        try context.constraints.equal.append(.{ .left = annotated_type, .right = mono });
        mono = annotated_type;
    }
    return types.Drop{
        .value = value,
        .span = d.span,
        .type = .{ .void = .{ .span = d.span } },
    };
}

fn plusEqual(context: Context, p: parser.types.PlusEqual) !types.PlusEqual {
    const value = try expressionAlloc(context, p.value.*);
    const binding = try findInScope(context.scopes.*, p.name);
    if (binding.global) std.debug.panic("Cannot reassign global variable {s}", .{p.name.value.string()});
    if (!binding.mutable) {
        try context.errors.reassigning_immutable.append(.{
            .span = p.name.span,
            .name = p.name.value,
        });
        return error.CompileError;
    }
    try context.constraints.equal.append(.{ .left = typeOf(value.*), .right = binding.type });
    const name = types.Symbol{
        .value = p.name.value,
        .span = p.span,
        .type = binding.type,
        .binding = binding,
    };
    return types.PlusEqual{
        .name = name,
        .value = value,
        .span = p.span,
        .type = .{ .void = .{ .span = p.span } },
    };
}

fn timesEqual(context: Context, t: parser.types.TimesEqual) !types.TimesEqual {
    const value = try expressionAlloc(context, t.value.*);
    const binding = try findInScope(context.scopes.*, t.name);
    if (binding.global) std.debug.panic("Cannot reassign global variable {s}", .{t.name.value.string()});
    if (!binding.mutable) {
        try context.errors.reassigning_immutable.append(.{
            .span = t.name.span,
            .name = t.name.value,
        });
        return error.CompileError;
    }
    try context.constraints.equal.append(.{ .left = typeOf(value.*), .right = binding.type });
    const name = types.Symbol{
        .value = t.name.value,
        .span = t.span,
        .type = binding.type,
        .binding = binding,
    };
    return types.TimesEqual{
        .name = name,
        .value = value,
        .span = t.span,
        .type = .{ .void = .{ .span = t.span } },
    };
}

fn callConvert(context: Context, c: parser.types.Call) !types.Expression {
    if (c.arguments.positional.len != 2) std.debug.panic("convert takes 2 arguments", .{});
    const mono = try expressionToMonoType(context.allocator, context.scopes.base, context.builtins, c.arguments.positional[1].value);
    return types.Expression{
        .convert = .{
            .value = try expressionAlloc(context, c.arguments.positional[0].value),
            .span = c.span,
            .type = mono,
        },
    };
}

fn callSqrt(context: Context, c: parser.types.Call) !types.Expression {
    if (c.arguments.positional.len != 1) std.debug.panic("sqrt takes 1 arguments", .{});
    const positional = try context.allocator.alloc(types.Argument, c.arguments.positional.len);
    for (c.arguments.positional, positional) |untyped, *typed| {
        typed.* = .{
            .value = try expression(context, untyped.value),
            .mutable = untyped.mutable,
        };
    }
    return types.Expression{
        .intrinsic = .{
            .function = context.builtins.sqrt,
            .arguments = positional,
            .span = c.span,
            .type = typeOf(positional[0].value),
        },
    };
}

fn sizeOfMonoType(builtins: Builtins, m: MonoType) Interned {
    switch (m) {
        .u8 => return builtins.one,
        else => std.debug.panic("sizeOfMonoType: {} not yet implemented", .{m}),
    }
}

fn callEmpty(context: Context, c: parser.types.Call) !types.Expression {
    if (c.arguments.positional.len != 2) std.debug.panic("empty takes 2 arguments", .{});
    const arguments = try context.allocator.alloc(types.Argument, 2);
    const arg0 = c.arguments.positional[0];
    const mono = try expressionToMonoType(context.allocator, context.scopes.base, context.builtins, arg0.value);
    const arg_span = parser.span.expression(arg0.value);
    arguments[0] = .{
        .value = .{
            .int = .{
                .value = sizeOfMonoType(context.builtins, mono),
                .span = arg_span,
                .type = .{ .i32 = .{ .span = arg_span } },
            },
        },
        .mutable = arg0.mutable,
    };
    const arg1 = c.arguments.positional[1];
    arguments[1] = .{
        .value = try expression(context, arg1.value),
        .mutable = arg1.mutable,
    };
    try context.constraints.equal.append(.{
        .left = typeOf(arguments[1].value),
        .right = .{ .i32 = .{ .span = null } },
    });
    const element_type = try context.allocator.create(MonoType);
    element_type.* = mono;
    return types.Expression{ .intrinsic = .{
        .function = context.builtins.empty,
        .arguments = arguments,
        .span = c.span,
        .type = .{ .array = .{
            .rank = 1,
            .element_type = element_type,
            .span = c.span,
        } },
    } };
}

fn call(context: Context, c: parser.types.Call) !types.Expression {
    switch (c.function.*) {
        .symbol => |s| {
            if (s.value.eql(context.builtins.convert)) return try callConvert(context, c);
            if (s.value.eql(context.builtins.sqrt)) return try callSqrt(context, c);
            if (s.value.eql(context.builtins.empty)) return try callEmpty(context, c);
            const f = try symbol(context.scopes.*, s);
            const len = c.arguments.positional.len;
            const argument_types = try context.allocator.alloc(monotype.Argument, len);
            const arguments = try context.allocator.alloc(types.Argument, len);
            for (c.arguments.positional, arguments, argument_types) |untyped_arg, *typed_arg, *arg_type| {
                typed_arg.value = try expression(context, untyped_arg.value);
                if (untyped_arg.mutable) {
                    switch (typed_arg.value) {
                        .symbol => |sym| {
                            if (sym.binding.mutable == false) {
                                try context.errors.mutability_mismatch.append(.{
                                    .left = .{
                                        .mutable = false,
                                        .span = sym.binding.span,
                                    },
                                    .right = .{
                                        .mutable = true,
                                        .span = untyped_arg.span,
                                    },
                                });
                                return error.CompileError;
                            }
                        },
                        else => |k| std.debug.panic("Can only mutate symbol, but found {}", .{k}),
                    }
                }
                typed_arg.mutable = untyped_arg.mutable;
                arg_type.* = .{
                    .type = monotype.withSpan(typeOf(typed_arg.value), untyped_arg.span),
                    .mutable = untyped_arg.mutable,
                };
            }
            var named_argument_types = Map(Interned, monotype.Argument).init(context.allocator);
            var named_arguments = Map(Interned, types.Argument).init(context.allocator);
            for (c.arguments.named_order) |name| {
                const untyped_arg = c.arguments.named.get(name).?;
                const type_arg = try expression(context, untyped_arg.value);
                try named_argument_types.putNoClobber(name, .{
                    .type = monotype.withSpan(typeOf(type_arg), untyped_arg.span),
                    .mutable = untyped_arg.mutable,
                });
                try named_arguments.putNoClobber(name, .{
                    .value = type_arg,
                    .mutable = untyped_arg.mutable,
                });
            }
            const return_type = try context.allocator.create(MonoType);
            return_type.* = freshTypeVar(context.constraints, null);
            try context.constraints.equal.append(.{
                .left = f.type,
                .right = .{ .call = .{
                    .arguments = argument_types,
                    .named_arguments = named_argument_types,
                    .return_type = return_type,
                    .span = null,
                } },
            });
            return types.Expression{
                .call = .{
                    .function = try alloc(context.allocator, .{ .symbol = f }),
                    .arguments = .{
                        .positional = arguments,
                        .named = named_arguments,
                        .named_order = c.arguments.named_order,
                        .span = c.arguments.span,
                    },
                    .span = c.span,
                    .type = return_type.*,
                },
            };
        },
        else => |k| std.debug.panic("\nInvalid call function type {}", .{k}),
    }
}

fn foreignImport(context: Context, d: parser.types.Decorator) !types.Expression {
    if (d.arguments) |args|
        if (args.positional.len != 2) std.debug.panic("import takes 2 arguments", .{});
    const value = try expression(context, d.value.*);
    const proto = value.prototype;
    return types.Expression{
        .foreign_import = .{
            .module = d.arguments.?.positional[0].value.string.value,
            .name = d.arguments.?.positional[1].value.string.value,
            .prototype = proto,
            .span = d.span,
            .type = proto.type,
        },
    };
}

fn foreignExport(context: Context, d: parser.types.Decorator) !types.Expression {
    if (d.arguments) |args| {
        if (args.positional.len != 1) std.debug.panic("export takes 1 argument", .{});
        const value = try expression(context, d.value.*);
        const func = value.function;
        return types.Expression{
            .foreign_export = .{
                .name = args.positional[0].value.string.value,
                .function = func,
                .span = d.span,
                .type = func.type,
            },
        };
    }
    const value = try expression(context, d.value.*);
    const func = value.function;
    return types.Expression{
        .foreign_export = .{
            .name = func.name.value,
            .function = func,
            .span = d.span,
            .type = func.type,
        },
    };
}

fn decorator(context: Context, d: parser.types.Decorator) !types.Expression {
    if (d.attribute.value.eql(context.builtins.import))
        return try foreignImport(context, d);
    if (d.attribute.value.eql(context.builtins.export_))
        return try foreignExport(context, d);
    std.debug.panic("Invalid decorator {s}", .{d.attribute.value.string()});
}

fn function(context: Context, f: parser.types.Function) !types.Function {
    try pushScope(context.scopes);
    defer popScope(context.scopes);
    const len = f.parameters.len;
    const parameters = try context.allocator.alloc(types.Parameter, len);
    const function_parameters = try context.allocator.alloc(monotype.Parameter, len);
    for (f.parameters, parameters, function_parameters) |untyped_p, *typed_p, *t| {
        const name_symbol = untyped_p.name.value;
        const mono = try expressionToMonoType(context.allocator, context.scopes.base, context.builtins, untyped_p.type);
        const p_type = monotype.withSpan(mono, untyped_p.span);
        const binding = types.Binding{
            .type = mono,
            .global = false,
            .mutable = untyped_p.mutable,
            .span = untyped_p.name.span,
        };
        typed_p.* = types.Parameter{
            .name = types.Symbol{
                .value = name_symbol,
                .span = untyped_p.name.span,
                .type = p_type,
                .binding = binding,
            },
            .mutable = binding.mutable,
        };
        try putInScope(context.scopes, name_symbol, binding);
        t.* = .{
            .name = name_symbol,
            .type = p_type,
            .mutable = untyped_p.mutable,
        };
    }
    const return_type = try context.allocator.create(MonoType);
    return_type.* = try expressionToMonoType(context.allocator, context.scopes.base, context.builtins, f.return_type.*);
    const body = try block(context, f.body);
    try context.constraints.equal.append(.{
        .left = return_type.*,
        .right = body.type,
    });
    const mono = .{ .function = .{
        .parameters = function_parameters,
        .return_type = return_type,
        .span = f.span,
    } };
    const binding = types.Binding{
        .type = mono,
        .global = false,
        .mutable = false,
        .span = f.name.span,
    };
    const name = types.Symbol{
        .value = f.name.value,
        .span = f.name.span,
        .type = monotype.withSpan(mono, f.name.span),
        .binding = binding,
    };
    return types.Function{
        .name = name,
        .parameters = parameters,
        .return_type = return_type.*,
        .body = body,
        .span = f.span,
        .type = mono,
    };
}

fn prototype(context: Context, p: parser.types.Prototype) !types.Prototype {
    try pushScope(context.scopes);
    defer popScope(context.scopes);
    const len = p.parameters.len;
    const parameters = try context.allocator.alloc(types.Parameter, len);
    const function_parameters = try context.allocator.alloc(monotype.Parameter, len);
    for (p.parameters, parameters, function_parameters) |untyped_p, *typed_p, *t| {
        const name_symbol = untyped_p.name.value;
        const mono = try expressionToMonoType(context.allocator, context.scopes.base, context.builtins, untyped_p.type);
        const p_type = monotype.withSpan(mono, untyped_p.span);
        const binding = types.Binding{
            .type = mono,
            .global = false,
            .mutable = untyped_p.mutable,
            .span = untyped_p.name.span,
        };
        typed_p.* = types.Parameter{
            .name = types.Symbol{
                .value = name_symbol,
                .span = untyped_p.name.span,
                .type = p_type,
                .binding = binding,
            },
            .mutable = binding.mutable,
        };
        try putInScope(context.scopes, name_symbol, binding);
        t.* = .{
            .name = name_symbol,
            .type = p_type,
            .mutable = untyped_p.mutable,
        };
    }
    const return_type = try context.allocator.create(MonoType);
    return_type.* = try expressionToMonoType(context.allocator, context.scopes.base, context.builtins, p.return_type.*);
    const mono = .{ .function = .{
        .parameters = function_parameters,
        .return_type = return_type,
        .span = p.span,
    } };
    const binding = types.Binding{
        .type = mono,
        .global = false,
        .mutable = false,
        .span = p.name.span,
    };
    const name = types.Symbol{
        .value = p.name.value,
        .span = p.name.span,
        .type = monotype.withSpan(mono, p.name.span),
        .binding = binding,
    };
    return types.Prototype{
        .name = name,
        .parameters = parameters,
        .return_type = return_type.*,
        .span = p.span,
        .type = mono,
    };
}

fn block(context: Context, b: parser.types.Block) !types.Block {
    const len = b.expressions.len;
    const expressions = try context.allocator.alloc(types.Expression, len);
    for (b.expressions, expressions) |untyped_e, *typed_e| {
        typed_e.* = try expression(context, untyped_e);
    }
    if (len == 0) {
        return types.Block{
            .expressions = expressions,
            .span = b.span,
            .type = .{ .void = .{ .span = b.span } },
        };
    }
    const last_type = typeOf(expressions[len - 1]);
    const block_type = freshTypeVar(context.constraints, b.span);
    try context.constraints.equal.append(.{ .left = block_type, .right = last_type });
    return types.Block{
        .expressions = expressions,
        .span = b.span,
        .type = block_type,
    };
}

fn array(context: Context, a: parser.types.Array) !types.Array {
    const expressions = try context.allocator.alloc(types.Expression, a.expressions.len);
    const element_type = try context.allocator.create(MonoType);
    element_type.* = freshTypeVar(context.constraints, a.span);
    for (a.expressions, expressions) |untyped_e, *typed_e| {
        typed_e.* = try expression(context, untyped_e);
        try context.constraints.equal.append(.{
            .left = element_type.*,
            .right = typeOf(typed_e.*),
        });
    }
    return types.Array{
        .expressions = expressions,
        .span = a.span,
        .type = .{ .array = .{
            .rank = 1,
            .element_type = element_type,
            .span = a.span,
        } },
    };
}

fn index(context: Context, i: parser.types.Index) !types.Index {
    const expr = try expressionAlloc(context, i.expression.*);
    const indices = try context.allocator.alloc(types.Expression, i.indices.len);
    for (i.indices, indices) |untyped_i, *typed_i| {
        typed_i.* = try expression(context, untyped_i);
    }
    if (indices.len != 1) std.debug.panic("\nInvalid index length {}", .{indices.len});
    const element_type = try context.allocator.create(MonoType);
    element_type.* = freshTypeVar(context.constraints, i.span);
    try context.constraints.equal.appendSlice(&.{
        .{
            .left = typeOf(expr.*),
            .right = .{ .array = .{ .rank = 1, .element_type = element_type, .span = null } },
        },
        .{
            .left = typeOf(indices[0]),
            .right = .{ .u32 = .{ .span = null } },
        },
    });
    return types.Index{
        .expression = expr,
        .indices = indices,
        .span = i.span,
        .type = element_type.*,
    };
}

fn templateLiteral(context: Context, t: parser.types.TemplateLiteral) !types.TemplateLiteral {
    const strings = try context.allocator.alloc(types.String, t.strings.len);
    for (t.strings, strings) |untyped_s, *typed_s| {
        typed_s.* = try string(context, untyped_s);
    }
    const arguments = try context.allocator.alloc(types.Expression, t.arguments.len);
    for (t.arguments, arguments) |untyped_a, *typed_a| {
        typed_a.* = try expression(context, untyped_a);
    }
    const element_type = try context.allocator.create(MonoType);
    element_type.* = .{ .u8 = .{ .span = null } };
    const mono = .{ .array = .{ .rank = 1, .element_type = element_type, .span = t.span } };
    if (t.function) |f| {
        const argument_types = try context.allocator.alloc(monotype.Argument, t.arguments.len);
        for (arguments, argument_types) |a, *arg_type| {
            arg_type.* = .{ .type = typeOf(a), .mutable = false };
        }
        const return_type = try context.allocator.create(MonoType);
        return_type.* = mono;
        const f_type = .{ .call = .{
            .arguments = argument_types,
            .named_arguments = Map(Interned, monotype.Argument).init(context.allocator),
            .return_type = return_type,
            .span = null,
        } };
        const binding = .{
            .type = f_type,
            .global = false,
            .mutable = false,
            .span = f.span,
        };
        const sym = types.Symbol{
            .value = f.value,
            .span = f.span,
            .type = f_type,
            .binding = binding,
        };
        return types.TemplateLiteral{
            .function = sym,
            .strings = strings,
            .arguments = arguments,
            .type = mono,
            .span = t.span,
        };
    }
    return types.TemplateLiteral{
        .function = null,
        .strings = strings,
        .arguments = arguments,
        .type = mono,
        .span = t.span,
    };
}

fn expression(context: Context, e: parser.types.Expression) error{ OutOfMemory, CompileError }!types.Expression {
    switch (e) {
        .int => |i| return .{ .int = int(context, i) },
        .float => |f| return .{ .float = float(context, f) },
        .string => |s| return .{ .string = try string(context, s) },
        .symbol => |s| return .{ .symbol = try symbol(context.scopes.*, s) },
        .bool => |b| return .{ .bool = boolean(b) },
        .define => |d| return .{ .define = try define(context, d) },
        .drop => |d| return .{ .drop = try drop(context, d) },
        .plus_equal => |a| return .{ .plus_equal = try plusEqual(context, a) },
        .times_equal => |a| return .{ .times_equal = try timesEqual(context, a) },
        .function => |f| return .{ .function = try function(context, f) },
        .prototype => |p| return .{ .prototype = try prototype(context, p) },
        .binary_op => |b| return try binaryOp(context, b),
        .block => |b| return .{ .block = try block(context, b) },
        .branch => |b| return .{ .branch = try branch(context, b) },
        .call => |c| return try call(context, c),
        .decorator => |d| return try decorator(context, d),
        .undefined => |u| return .{ .undefined = untypedUndefined(context, u) },
        .struct_literal => |s| return .{ .struct_literal = try structLiteral(context, s) },
        .array => |a| return .{ .array = try array(context, a) },
        .index => |i| return .{ .index = try index(context, i) },
        .template_literal => |t| return .{ .template_literal = try templateLiteral(context, t) },
        else => |k| std.debug.panic("\nUnsupported expression {}", .{k}),
    }
}

fn alloc(allocator: Allocator, expr: types.Expression) !*types.Expression {
    const result = try allocator.create(types.Expression);
    result.* = expr;
    return result;
}

fn expressionAlloc(context: Context, expr: parser.types.Expression) !*types.Expression {
    return try alloc(context.allocator, try expression(context, expr));
}

pub fn topLevel(m: *types.Module, name: Interned, errors: *Errors) !void {
    var work_queue = WorkQueue.init(m.allocator);
    try work_queue.append(name);
    while (work_queue.items.len != 0) {
        const current = work_queue.pop();
        if (m.untyped.fetchRemove(current)) |entry| {
            var active = List(types.Scope).init(m.allocator);
            try active.append(types.Scope.init(m.allocator));
            var scopes = Scopes{
                .allocator = m.allocator,
                .work_queue = &work_queue,
                .active = &active,
                .base = m.scope,
                .errors = errors,
            };
            const context = Context{
                .allocator = m.allocator,
                .builtins = m.builtins,
                .constraints = m.constraints,
                .scopes = &scopes,
                .errors = errors,
            };
            const expr = try expression(context, entry.value);
            try m.typed.putNoClobber(current, expr);
        }
    }
}

pub fn module(allocator: Allocator, cs: *types.Constraints, builtins: Builtins, intern: *Intern, ast: parser.types.Module) !types.Module {
    _ = intern;
    var order = List(Interned).init(allocator);
    var untyped = types.Untyped.init(allocator);
    var typed = types.Typed.init(allocator);
    var scope = types.Scope.init(allocator);
    var foreign_exports = List(Interned).init(allocator);
    for (ast.enumerations) |e| {
        const variants = try allocator.alloc(Interned, e.enumeration.variants.len);
        for (e.enumeration.variants, variants) |v, *i| i.* = v.value;
        try scope.put(e.name.value, types.Binding{
            .type = .{ .enumeration = .{
                .variants = variants,
                .span = e.enumeration.span,
            } },
            .global = true,
            .mutable = false,
            .span = e.span,
        });
    }
    for (ast.structures) |s| {
        var fields = Map(Interned, MonoType).init(allocator);
        for (s.structure.order) |o| {
            const field = s.structure.fields.get(o).?;
            try fields.putNoClobber(o, try expressionToMonoType(allocator, scope, builtins, field.type));
        }
        try scope.put(s.name.value, types.Binding{
            .type = .{ .structure = .{
                .fields = fields,
                .order = s.structure.order,
                .span = s.structure.span,
            } },
            .global = true,
            .mutable = false,
            .span = s.span,
        });
    }
    for (ast.foreign_imports) |f| {
        const proto = f.value.prototype;
        const name = proto.name.value;
        try order.append(name);
        try untyped.putNoClobber(name, .{ .decorator = f });
        if (f.arguments) |args|
            if (args.positional.len != 2) std.debug.panic("import takes 2 arguments", .{});
        const len = proto.parameters.len;
        const parameters = try allocator.alloc(monotype.Parameter, len);
        for (proto.parameters, parameters) |p, *t| {
            const m = try expressionToMonoType(allocator, scope, builtins, p.type);
            t.* = .{
                .name = p.name.value,
                .type = monotype.withSpan(m, p.span),
                .mutable = p.mutable,
            };
        }
        const return_type = try allocator.create(MonoType);
        return_type.* = try expressionToMonoType(allocator, scope, builtins, proto.return_type.*);
        const mono = MonoType{ .function = .{
            .parameters = parameters,
            .return_type = return_type,
            .span = proto.span,
        } };
        try scope.put(name, types.Binding{
            .type = mono,
            .global = true,
            .mutable = false,
            .span = proto.name.span,
        });
    }
    for (ast.functions) |f| {
        const name = f.name.value;
        try order.append(name);
        try untyped.putNoClobber(name, .{ .function = f });
        const len = f.parameters.len;
        const parameters = try allocator.alloc(monotype.Parameter, len);
        for (f.parameters, parameters) |p, *t| {
            const m = try expressionToMonoType(allocator, scope, builtins, p.type);
            t.* = .{
                .name = p.name.value,
                .type = monotype.withSpan(m, p.span),
                .mutable = p.mutable,
            };
        }
        const return_type = try allocator.create(MonoType);
        return_type.* = try expressionToMonoType(allocator, scope, builtins, f.return_type.*);
        const mono = MonoType{ .function = .{
            .parameters = parameters,
            .return_type = return_type,
            .span = f.span,
        } };
        try scope.put(name, types.Binding{
            .type = mono,
            .global = true,
            .mutable = false,
            .span = f.name.span,
        });
    }
    for (ast.defines) |d| {
        const name = d.name.value;
        try order.append(name);
        try untyped.putNoClobber(name, .{ .define = d });
        if (d.type) |t| {
            const mono = try expressionToMonoType(allocator, scope, builtins, t.*);
            try scope.put(name, types.Binding{
                .type = mono,
                .global = true,
                .mutable = false,
                .span = d.name.span,
            });
        } else {
            std.debug.panic("\nInvalid top level int {}", .{d});
        }
    }
    for (ast.foreign_exports) |f| {
        const func = f.value.function;
        const name = func.name.value;
        try order.append(name);
        try untyped.putNoClobber(name, .{ .decorator = f });
        if (f.arguments) |args|
            if (args.positional.len != 1) std.debug.panic("export takes 1 argument", .{});
        const len = func.parameters.len;
        const parameters = try allocator.alloc(monotype.Parameter, len);
        for (func.parameters, parameters) |p, *t| {
            const m = try expressionToMonoType(allocator, scope, builtins, p.type);
            t.* = .{
                .name = p.name.value,
                .type = monotype.withSpan(m, p.span),
                .mutable = p.mutable,
            };
        }
        const return_type = try allocator.create(MonoType);
        return_type.* = try expressionToMonoType(allocator, scope, builtins, func.return_type.*);
        const mono = MonoType{ .function = .{
            .parameters = parameters,
            .return_type = return_type,
            .span = func.span,
        } };
        try scope.put(name, types.Binding{
            .type = mono,
            .global = true,
            .mutable = false,
            .span = func.name.span,
        });
        try foreign_exports.append(name);
    }
    return types.Module{
        .allocator = allocator,
        .constraints = cs,
        .builtins = builtins,
        .order = try order.toOwnedSlice(),
        .untyped = untyped,
        .typed = typed,
        .scope = scope,
        .foreign_exports = try foreign_exports.toOwnedSlice(),
    };
}
