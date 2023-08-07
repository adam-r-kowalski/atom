const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;

const tokenizer = @import("../tokenizer.zig");
const LeftParen = tokenizer.types.LeftParen;
const IfToken = tokenizer.types.If;
const FnToken = tokenizer.types.Fn;
const EnumToken = tokenizer.types.Enum;
const StructToken = tokenizer.types.Struct;
const Token = tokenizer.types.Token;
const types = @import("types.zig");
const pretty_print = @import("pretty_print.zig");
const spanOf = @import("span.zig").expression;
const Builtins = @import("../builtins.zig").Builtins;
const Interned = @import("../interner.zig").Interned;

const Precedence = u32;

const DELTA: Precedence = 10;
const LOWEST: Precedence = 0;
const DEFINE: Precedence = LOWEST + DELTA;
const AND: Precedence = DEFINE + DELTA;
const COMPARE: Precedence = AND + DELTA;
const ADD: Precedence = COMPARE + DELTA;
const MULTIPLY: Precedence = ADD + DELTA;
const EXPONENTIATE: Precedence = MULTIPLY + DELTA;
const PIPELINE: Precedence = EXPONENTIATE + DELTA;
const DOT: Precedence = PIPELINE + DELTA;
const TEMPLATE_LITERAL: Precedence = DOT;
const CALL: Precedence = DOT + DELTA;
const HIGHEST: Precedence = CALL + DELTA;

const Associativity = enum {
    left,
    right,
};

const Context = struct {
    allocator: Allocator,
    tokens: *tokenizer.Iterator,
    precedence: Precedence,
    builtins: Builtins,
};

fn withPrecedence(context: Context, p: Precedence) Context {
    return Context{
        .allocator = context.allocator,
        .tokens = context.tokens,
        .precedence = p,
        .builtins = context.builtins,
    };
}

fn alloc(context: Context, expr: types.Expression) !*const types.Expression {
    const ptr = try context.allocator.create(types.Expression);
    ptr.* = expr;
    return ptr;
}

fn expressionAlloc(context: Context) !*const types.Expression {
    const ptr = try context.allocator.create(types.Expression);
    ptr.* = try expression(context);
    return ptr;
}

fn block(context: Context, begin: tokenizer.types.Pos) !types.Block {
    var exprs = List(types.Expression).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .right_brace => break,
            .new_line => context.tokens.advance(),
            else => try exprs.append(try expression(withPrecedence(context, LOWEST))),
        }
    }
    const end = tokenizer.span.token(context.tokens.consume(.right_brace)).end;
    return types.Block{
        .expressions = try exprs.toOwnedSlice(),
        .span = .{ .begin = begin, .end = end },
    };
}

fn explicitBlock(context: Context, b: tokenizer.types.Block) !types.Block {
    const begin = b.span.begin;
    _ = context.tokens.consume(.left_brace);
    var exprs = List(types.Expression).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .right_brace => break,
            .new_line => context.tokens.advance(),
            else => try exprs.append(try expression(withPrecedence(context, LOWEST))),
        }
    }
    const end = tokenizer.span.token(context.tokens.consume(.right_brace)).end;
    return types.Block{
        .expressions = try exprs.toOwnedSlice(),
        .span = .{ .begin = begin, .end = end },
    };
}

fn structLiteral(context: Context, begin: tokenizer.types.Pos) !types.StructLiteral {
    var fields = types.StructLiteralFields.init(context.allocator);
    var order = List(Interned).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .right_brace => break,
            .new_line => context.tokens.advance(),
            .symbol => |name| {
                context.tokens.advance();
                _ = context.tokens.consume(.colon);
                const value = try expression(withPrecedence(context, DEFINE + 1));
                context.tokens.consumeNewLines();
                context.tokens.maybeConsume(.comma);
                try fields.putNoClobber(name.value, .{
                    .name = name,
                    .value = value,
                    .span = types.Span{ .begin = name.span.begin, .end = spanOf(value).end },
                });
                try order.append(name.value);
            },
            else => |k| std.debug.panic("\nExpected symbol or right brace, found {}", .{k}),
        }
    }
    const end = tokenizer.span.token(context.tokens.consume(.right_brace)).end;
    return types.StructLiteral{
        .fields = fields,
        .order = try order.toOwnedSlice(),
        .span = .{ .begin = begin, .end = end },
    };
}

fn array(context: Context, begin: tokenizer.types.Pos) !types.Array {
    var exprs = List(types.Expression).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .right_bracket => break,
            .new_line => context.tokens.advance(),
            else => {
                try exprs.append(try expression(withPrecedence(context, LOWEST)));
                context.tokens.consumeNewLines();
                context.tokens.maybeConsume(.comma);
            },
        }
    }
    const end = tokenizer.span.token(context.tokens.consume(.right_bracket)).end;
    return types.Array{
        .expressions = try exprs.toOwnedSlice(),
        .span = .{ .begin = begin, .end = end },
    };
}

fn group(context: Context, left_paren: LeftParen) !types.Group {
    const begin = left_paren.span.begin;
    context.tokens.consumeNewLines();
    const expr = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    const end = tokenizer.span.token(context.tokens.consume(.right_paren)).end;
    return .{
        .expression = expr,
        .span = .{ .begin = begin, .end = end },
    };
}

fn branch(context: Context, if_token: IfToken) !types.Branch {
    const begin = if_token.span.begin;
    const lowest = withPrecedence(context, LOWEST);
    var arms = List(types.Arm).init(context.allocator);
    try arms.append(types.Arm{
        .condition = try expression(lowest),
        .then = try block(lowest, tokenizer.span.token(context.tokens.consume(.left_brace)).begin),
    });
    context.tokens.consumeNewLines();
    while (context.tokens.peek()) |t| {
        switch (t) {
            .else_ => {
                context.tokens.advance();
                switch (context.tokens.next().?) {
                    .left_brace => |l| {
                        const else_ = try block(lowest, l.span.begin);
                        const end = else_.span.end;
                        return types.Branch{
                            .arms = try arms.toOwnedSlice(),
                            .else_ = else_,
                            .span = .{ .begin = begin, .end = end },
                        };
                    },
                    .if_ => {
                        try arms.append(types.Arm{
                            .condition = try expression(lowest),
                            .then = try block(lowest, tokenizer.span.token(context.tokens.consume(.left_brace)).begin),
                        });
                        context.tokens.consumeNewLines();
                    },
                    else => |k| std.debug.panic("\nExpected (delimiter '{{') found {}", .{k}),
                }
            },
            else => {
                const pos = arms.items[0].then.span.end;
                const else_ = types.Block{ .expressions = &.{}, .span = .{ .begin = pos, .end = pos } };
                return types.Branch{
                    .arms = try arms.toOwnedSlice(),
                    .else_ = else_,
                    .span = .{ .begin = begin, .end = pos },
                };
            },
        }
    }
    std.debug.panic("\nExpected else token", .{});
}

fn functionParameters(context: Context) ![]types.Parameter {
    _ = context.tokens.consume(.left_paren);
    var parameters = List(types.Parameter).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .new_line => context.tokens.advance(),
            .right_paren => break,
            .symbol => |name| {
                context.tokens.advance();
                _ = context.tokens.consume(.colon);
                const type_ = try expression(withPrecedence(context, DEFINE + 1));
                context.tokens.consumeNewLines();
                context.tokens.maybeConsume(.comma);
                try parameters.append(types.Parameter{
                    .name = name,
                    .type = type_,
                    .mutable = false,
                    .span = .{ .begin = name.span.begin, .end = spanOf(type_).end },
                });
            },
            .mut => |mut| {
                context.tokens.advance();
                const name = context.tokens.consume(.symbol).symbol;
                _ = context.tokens.consume(.colon);
                const type_ = try expression(withPrecedence(context, DEFINE + 1));
                context.tokens.consumeNewLines();
                context.tokens.maybeConsume(.comma);
                try parameters.append(types.Parameter{
                    .name = name,
                    .type = type_,
                    .mutable = true,
                    .span = .{ .begin = mut.span.begin, .end = spanOf(type_).end },
                });
            },
            else => |k| std.debug.panic("\nExpected symbol or right paren, found {}", .{k}),
        }
    }
    _ = context.tokens.consume(.right_paren);
    return try parameters.toOwnedSlice();
}

fn function(context: Context, begin: types.Pos) !types.Expression {
    const name = context.tokens.consume(.symbol).symbol;
    const parameters = try functionParameters(context);
    _ = context.tokens.consume(.right_arrow);
    const return_type = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    if (context.tokens.peek()) |t| {
        if (t == .left_brace) {
            const body = try block(
                withPrecedence(context, LOWEST),
                tokenizer.span.token(context.tokens.consume(.left_brace)).begin,
            );
            const end = body.span.end;
            return types.Expression{
                .function = .{
                    .name = name,
                    .parameters = parameters,
                    .return_type = return_type,
                    .body = body,
                    .span = types.Span{ .begin = begin, .end = end },
                },
            };
        }
    }
    const end = spanOf(return_type.*).end;
    return types.Expression{
        .prototype = .{
            .name = name,
            .parameters = parameters,
            .return_type = return_type,
            .span = types.Span{ .begin = begin, .end = end },
        },
    };
}

fn enumeration(context: Context, enum_: EnumToken) !types.Enumeration {
    const begin = enum_.span.begin;
    _ = context.tokens.consume(.left_brace);
    var variants = List(types.Symbol).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .new_line => context.tokens.advance(),
            .right_brace => break,
            .symbol => |name| {
                context.tokens.advance();
                context.tokens.consumeNewLines();
                context.tokens.maybeConsume(.comma);
                try variants.append(types.Symbol{
                    .value = name.value,
                    .span = name.span,
                });
            },
            else => |k| std.debug.panic("\nExpected symbol or right brace, found {}", .{k}),
        }
    }
    const end = tokenizer.span.token(context.tokens.consume(.right_brace)).end;
    return types.Enumeration{
        .variants = try variants.toOwnedSlice(),
        .span = types.Span{ .begin = begin, .end = end },
    };
}

fn structure(context: Context, struct_: StructToken) !types.Structure {
    const begin = struct_.span.begin;
    _ = context.tokens.consume(.left_brace);
    var fields = types.StructFields.init(context.allocator);
    var order = List(Interned).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .new_line => context.tokens.advance(),
            .right_brace => break,
            .symbol => |name| {
                context.tokens.advance();
                _ = context.tokens.consume(.colon);
                const type_ = try expression(withPrecedence(context, DEFINE + 1));
                context.tokens.consumeNewLines();
                context.tokens.maybeConsume(.comma);
                try fields.putNoClobber(name.value, .{
                    .name = name,
                    .type = type_,
                    .span = types.Span{ .begin = name.span.begin, .end = spanOf(type_).end },
                });
                try order.append(name.value);
            },
            else => |k| std.debug.panic("\nExpected symbol or right brace, found {}", .{k}),
        }
    }
    const end = tokenizer.span.token(context.tokens.consume(.right_brace)).end;
    return types.Structure{
        .fields = fields,
        .order = try order.toOwnedSlice(),
        .span = types.Span{ .begin = begin, .end = end },
    };
}

fn mutable(context: Context, begin: tokenizer.types.Pos) !types.Define {
    const name = context.tokens.next().?.symbol;
    switch (context.tokens.next().?) {
        .colon => {
            const type_ = try expressionAlloc(withPrecedence(context, DEFINE + 1));
            _ = context.tokens.consume(.equal);
            const value = try expressionAlloc(withPrecedence(context, DEFINE + 1));
            return types.Define{
                .name = name,
                .type = type_,
                .value = value,
                .mutable = true,
                .span = types.Span{
                    .begin = begin,
                    .end = spanOf(value.*).end,
                },
            };
        },
        .equal => {
            const value = try expressionAlloc(withPrecedence(context, DEFINE + 1));
            return types.Define{
                .name = name,
                .type = null,
                .value = value,
                .mutable = true,
                .span = types.Span{
                    .begin = begin,
                    .end = spanOf(value.*).end,
                },
            };
        },
        else => |k| std.debug.panic("\nExpected colon or equal, found {}", .{k}),
    }
}

fn prefix(context: Context) !types.Expression {
    switch (context.tokens.next().?) {
        .int => |i| return .{ .int = i },
        .float => |f| return .{ .float = f },
        .symbol => |s| return .{ .symbol = s },
        .string => |s| return .{ .string = s },
        .bool => |b| return .{ .bool = b },
        .left_paren => |l| return .{ .group = try group(context, l) },
        .if_ => |i| return .{ .branch = try branch(context, i) },
        .fn_ => |f| return try function(context, f.span.begin),
        .enum_ => |e| return .{ .enumeration = try enumeration(context, e) },
        .struct_ => |s| return .{ .structure = try structure(context, s) },
        .block => |b| return .{ .block = try explicitBlock(context, b) },
        .left_brace => |l| return .{ .struct_literal = try structLiteral(context, l.span.begin) },
        .left_bracket => |l| return .{ .array = try array(context, l.span.begin) },
        .mut => |m| return .{ .define = try mutable(context, m.span.begin) },
        .undefined => |u| return .{ .undefined = u },
        .template_literal => |t| return .{ .template_literal = try templateLiteral(context, null, t) },
        .template_literal_begin => |t| return .{ .template_literal = try templateLiteralBegin(context, null, t) },
        else => |kind| std.debug.panic("\nNo prefix parser for {}\n", .{kind}),
    }
}

fn define(context: Context, name: types.Symbol) !types.Expression {
    context.tokens.advance();
    const value = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    if (name.value.eql(context.builtins.underscore)) {
        return types.Expression{ .drop = .{
            .type = null,
            .value = value,
            .span = types.Span{
                .begin = name.span.begin,
                .end = spanOf(value.*).end,
            },
        } };
    }
    return types.Expression{ .define = .{
        .name = name,
        .type = null,
        .value = value,
        .mutable = false,
        .span = types.Span{
            .begin = name.span.begin,
            .end = spanOf(value.*).end,
        },
    } };
}

fn plusEqual(context: Context, name: types.Symbol) !types.PlusEqual {
    context.tokens.advance();
    const value = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    return types.PlusEqual{
        .name = name,
        .value = value,
        .span = types.Span{
            .begin = name.span.begin,
            .end = spanOf(value.*).end,
        },
    };
}

fn timesEqual(context: Context, name: types.Symbol) !types.TimesEqual {
    context.tokens.advance();
    const value = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    return types.TimesEqual{
        .name = name,
        .value = value,
        .span = types.Span{
            .begin = name.span.begin,
            .end = spanOf(value.*).end,
        },
    };
}

fn annotate(context: Context, name: types.Symbol) !types.Define {
    context.tokens.advance();
    const type_ = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    _ = context.tokens.consume(.equal);
    const value = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    return types.Define{
        .name = name,
        .type = type_,
        .value = value,
        .mutable = false,
        .span = types.Span{
            .begin = name.span.begin,
            .end = spanOf(value.*).end,
        },
    };
}

fn binaryOp(context: Context, left: types.Expression, kind: types.BinaryOpKind) !types.BinaryOp {
    context.tokens.advance();
    const right = try expressionAlloc(context);
    return types.BinaryOp{
        .kind = kind,
        .left = try alloc(context, left),
        .right = right,
        .span = types.Span{
            .begin = spanOf(left).begin,
            .end = spanOf(right.*).end,
        },
    };
}

fn call(context: Context, left: types.Expression) !types.Call {
    context.tokens.advance();
    var arguments = List(types.Argument).init(context.allocator);
    var named_arguments = Map(Interned, types.Argument).init(context.allocator);
    var named_arguments_order = List(Interned).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .new_line => context.tokens.advance(),
            .right_paren => break,
            .mut => |mut| {
                context.tokens.advance();
                const value = try expression(withPrecedence(context, DEFINE + 1));
                try arguments.append(.{
                    .value = value,
                    .mutable = true,
                    .span = types.Span{
                        .begin = mut.span.begin,
                        .end = spanOf(value).end,
                    },
                });
                context.tokens.consumeNewLines();
                context.tokens.maybeConsume(.comma);
            },
            else => {
                const name_or_value = try expression(withPrecedence(context, DEFINE + 1));
                if (context.tokens.peek()) |t2| {
                    switch (t2) {
                        .equal => {
                            context.tokens.advance();
                            const value = try expression(withPrecedence(context, DEFINE + 1));
                            try named_arguments.putNoClobber(name_or_value.symbol.value, .{
                                .value = value,
                                .mutable = false,
                                .span = types.Span{
                                    .begin = spanOf(name_or_value).begin,
                                    .end = spanOf(value).end,
                                },
                            });
                            try named_arguments_order.append(name_or_value.symbol.value);
                            context.tokens.consumeNewLines();
                            context.tokens.maybeConsume(.comma);
                        },
                        else => {
                            try arguments.append(.{ .value = name_or_value, .mutable = false, .span = spanOf(name_or_value) });
                            context.tokens.consumeNewLines();
                            context.tokens.maybeConsume(.comma);
                        },
                    }
                }
            },
        }
    }
    const end = context.tokens.next().?.right_paren.span.end;
    return types.Call{
        .function = try alloc(context, left),
        .arguments = try arguments.toOwnedSlice(),
        .named_arguments = named_arguments,
        .named_arguments_order = try named_arguments_order.toOwnedSlice(),
        .span = types.Span{ .begin = spanOf(left).begin, .end = end },
    };
}

fn index(context: Context, left: types.Expression) !types.Index {
    context.tokens.advance();
    var indices = List(types.Expression).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .new_line => context.tokens.advance(),
            .right_bracket => break,
            else => {
                const expr = try expression(withPrecedence(context, DEFINE + 1));
                try indices.append(expr);
                context.tokens.consumeNewLines();
                context.tokens.maybeConsume(.comma);
            },
        }
    }
    const end = context.tokens.next().?.right_bracket.span.end;
    return types.Index{
        .expression = try alloc(context, left),
        .indices = try indices.toOwnedSlice(),
        .span = types.Span{ .begin = spanOf(left).begin, .end = end },
    };
}

fn templateLiteral(context: Context, left: ?types.Symbol, t: tokenizer.types.TemplateLiteral) !types.TemplateLiteral {
    if (left) |_| context.tokens.advance();
    const strings = try context.allocator.alloc(types.String, 1);
    strings[0] = .{ .value = t.value, .span = t.span };
    if (left) |f| {
        return types.TemplateLiteral{
            .function = f,
            .strings = strings,
            .arguments = &.{},
            .span = types.Span{ .begin = f.span.begin, .end = t.span.end },
        };
    }
    return types.TemplateLiteral{
        .function = null,
        .strings = strings,
        .arguments = &.{},
        .span = t.span,
    };
}

fn templateLiteralBegin(context: Context, left: ?types.Symbol, b: tokenizer.types.TemplateLiteralBegin) !types.TemplateLiteral {
    if (left) |_| context.tokens.advance();
    var strings = List(types.String).init(context.allocator);
    try strings.append(.{ .value = b.value, .span = b.span });
    var arguments = List(types.Expression).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .template_literal_end => |e| {
                context.tokens.advance();
                try strings.append(.{ .value = e.value, .span = e.span });
                if (left) |f| {
                    return types.TemplateLiteral{
                        .function = f,
                        .strings = try strings.toOwnedSlice(),
                        .arguments = try arguments.toOwnedSlice(),
                        .span = types.Span{ .begin = f.span.begin, .end = e.span.end },
                    };
                }
                return types.TemplateLiteral{
                    .function = null,
                    .strings = try strings.toOwnedSlice(),
                    .arguments = try arguments.toOwnedSlice(),
                    .span = types.Span{ .begin = b.span.begin, .end = e.span.end },
                };
            },
            .template_literal_middle => |e| {
                context.tokens.advance();
                try strings.append(.{ .value = e.value, .span = e.span });
            },
            else => {
                const expr = try expression(withPrecedence(context, DEFINE + 1));
                try arguments.append(expr);
                context.tokens.consumeNewLines();
                context.tokens.maybeConsume(.comma);
            },
        }
    }
    std.debug.panic("\nUnterminated template literal", .{});
}

const Infix = union(enum) {
    define,
    plus_equal,
    times_equal,
    annotate,
    call,
    index,
    binary_op: types.BinaryOpKind,
    template_literal: tokenizer.types.TemplateLiteral,
    template_literal_begin: tokenizer.types.TemplateLiteralBegin,
};

fn precedence(i: Infix) Precedence {
    return switch (i) {
        .define => DEFINE,
        .plus_equal => DEFINE,
        .times_equal => DEFINE,
        .annotate => DEFINE,
        .call => CALL,
        .index => CALL,
        .binary_op => |b| switch (b) {
            .add => ADD,
            .subtract => ADD,
            .multiply => MULTIPLY,
            .divide => MULTIPLY,
            .modulo => MULTIPLY,
            .exponentiate => EXPONENTIATE,
            .equal => COMPARE,
            .greater => COMPARE,
            .less => COMPARE,
            .or_ => AND,
            .dot => DOT,
            .pipeline => DOT,
        },
        .template_literal => TEMPLATE_LITERAL,
        .template_literal_begin => TEMPLATE_LITERAL,
    };
}

fn associativity(i: Infix) Associativity {
    return switch (i) {
        .define => .right,
        .plus_equal => .right,
        .times_equal => .right,
        .annotate => .right,
        .call => .left,
        .index => .left,
        .binary_op => |b| switch (b) {
            .add => .left,
            .subtract => .left,
            .multiply => .left,
            .divide => .left,
            .modulo => .left,
            .exponentiate => .right,
            .equal => .left,
            .greater => .left,
            .less => .left,
            .or_ => .left,
            .dot => .left,
            .pipeline => .left,
        },
        .template_literal => .left,
        .template_literal_begin => .left,
    };
}

fn infix(context: Context, left: types.Expression) ?Infix {
    if (context.tokens.peek()) |t| {
        return switch (t) {
            .equal => .define,
            .plus_equal => .plus_equal,
            .times_equal => .times_equal,
            .colon => .annotate,
            .plus => .{ .binary_op = .add },
            .minus => .{ .binary_op = .subtract },
            .times => .{ .binary_op = .multiply },
            .slash => .{ .binary_op = .divide },
            .percent => .{ .binary_op = .modulo },
            .caret => .{ .binary_op = .exponentiate },
            .equal_equal => .{ .binary_op = .equal },
            .greater => .{ .binary_op = .greater },
            .less => .{ .binary_op = .less },
            .or_ => .{ .binary_op = .or_ },
            .dot => .{ .binary_op = .dot },
            .bar_greater => .{ .binary_op = .pipeline },
            .left_paren => switch (left) {
                .symbol => .call,
                else => null,
            },
            .left_bracket => .index,
            .template_literal => |l| switch (left) {
                .symbol => .{ .template_literal = l },
                else => null,
            },
            .template_literal_begin => |l| switch (left) {
                .symbol => .{ .template_literal_begin = l },
                else => null,
            },
            else => null,
        };
    }
    return null;
}

fn parseInfix(parser: Infix, context: Context, left: types.Expression) !types.Expression {
    return switch (parser) {
        .define => try define(context, left.symbol),
        .plus_equal => .{ .plus_equal = try plusEqual(context, left.symbol) },
        .times_equal => .{ .times_equal = try timesEqual(context, left.symbol) },
        .annotate => .{ .define = try annotate(context, left.symbol) },
        .call => .{ .call = try call(context, left) },
        .index => .{ .index = try index(context, left) },
        .binary_op => |kind| .{ .binary_op = try binaryOp(context, left, kind) },
        .template_literal => |t| .{ .template_literal = try templateLiteral(context, left.symbol, t) },
        .template_literal_begin => |t| .{ .template_literal = try templateLiteralBegin(context, left.symbol, t) },
    };
}

fn expression(context: Context) error{OutOfMemory}!types.Expression {
    var left = try prefix(context);
    while (true) {
        if (infix(context, left)) |parser| {
            var next = precedence(parser);
            if (context.precedence > next) return left;
            if (associativity(parser) == .left) next += 1;
            left = try parseInfix(parser, withPrecedence(context, next), left);
        } else {
            return left;
        }
    }
}

pub fn parse(allocator: Allocator, builtins: Builtins, tokens: []const tokenizer.types.Token) !types.Module {
    var iterator = tokenizer.Iterator.init(tokens);
    const context = Context{
        .allocator = allocator,
        .tokens = &iterator,
        .precedence = LOWEST,
        .builtins = builtins,
    };
    var foreign_imports = List(types.TopLevelForeignImport).init(allocator);
    var structures = List(types.TopLevelStructure).init(allocator);
    var enumerations = List(types.TopLevelEnumeration).init(allocator);
    var defines = List(types.Define).init(allocator);
    var functions = List(types.Function).init(allocator);
    var foreign_exports = List(types.Call).init(allocator);
    var ignored = List(types.Expression).init(allocator);
    while (iterator.peek()) |t| {
        switch (t) {
            .new_line => context.tokens.advance(),
            else => {
                const e = try expression(context);
                switch (e) {
                    .define => |d| {
                        if (d.mutable) std.debug.panic("\nNo top level mutable definitions allowed", .{});
                        switch (d.value.*) {
                            .structure => |s| try structures.append(.{
                                .name = d.name,
                                .type = d.type,
                                .structure = s,
                                .span = d.span,
                            }),
                            .enumeration => |en| try enumerations.append(.{
                                .name = d.name,
                                .type = d.type,
                                .enumeration = en,
                                .span = d.span,
                            }),
                            .call => |c| {
                                switch (c.function.*) {
                                    .symbol => |s| {
                                        if (s.value.eql(builtins.foreign_import)) {
                                            try foreign_imports.append(.{
                                                .name = d.name,
                                                .type = d.type,
                                                .call = c,
                                                .span = d.span,
                                            });
                                        } else {
                                            try defines.append(d);
                                        }
                                    },
                                    else => try defines.append(d),
                                }
                            },
                            else => try defines.append(d),
                        }
                    },
                    .function => |f| try functions.append(f),
                    .call => |c| {
                        switch (c.function.*) {
                            .symbol => |s| {
                                if (s.value.eql(builtins.foreign_export)) {
                                    try foreign_exports.append(c);
                                } else {
                                    try ignored.append(e);
                                }
                            },
                            else => try ignored.append(e),
                        }
                    },
                    else => try ignored.append(e),
                }
            },
        }
    }
    return types.Module{
        .foreign_imports = try foreign_imports.toOwnedSlice(),
        .structures = try structures.toOwnedSlice(),
        .enumerations = try enumerations.toOwnedSlice(),
        .functions = try functions.toOwnedSlice(),
        .defines = try defines.toOwnedSlice(),
        .foreign_exports = try foreign_exports.toOwnedSlice(),
        .ignored = try ignored.toOwnedSlice(),
    };
}
