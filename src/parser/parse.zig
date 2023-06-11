const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const tokenizer = @import("../tokenizer.zig");
const LeftParen = tokenizer.types.LeftParen;
const IfToken = tokenizer.types.If;
const FnToken = tokenizer.types.Fn;
const Token = tokenizer.types.Token;
const types = @import("types.zig");
const pretty_print = @import("pretty_print.zig");
const spanOf = @import("span.zig").expression;

const Precedence = u32;

const DELTA: Precedence = 10;
const LOWEST: Precedence = 0;
const DEFINE: Precedence = LOWEST + DELTA;
const DOT: Precedence = DEFINE + DELTA;
const AND: Precedence = DOT + DELTA;
const COMPARE: Precedence = AND + DELTA;
const ADD: Precedence = COMPARE + DELTA;
const MULTIPLY: Precedence = ADD + DELTA;
const EXPONENTIATE: Precedence = MULTIPLY + DELTA;
const CALL: Precedence = EXPONENTIATE + DELTA;
const ARRAY_OF: Precedence = CALL + DELTA;
const HIGHEST: Precedence = ARRAY_OF + DELTA;

const Associativity = enum {
    left,
    right,
};

const Context = struct {
    allocator: Allocator,
    tokens: *tokenizer.Iterator,
    precedence: Precedence,
};

fn withPrecedence(context: Context, p: Precedence) Context {
    return Context{
        .allocator = context.allocator,
        .tokens = context.tokens,
        .precedence = p,
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

fn array(context: Context, begin: tokenizer.types.Pos) !types.Array {
    var exprs = List(types.Expression).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .right_bracket => break,
            .new_line => context.tokens.advance(),
            else => try exprs.append(try expression(withPrecedence(context, LOWEST))),
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
    const expr = try expressionAlloc(withPrecedence(context, LOWEST));
    const end = tokenizer.span.token(context.tokens.consume(.right_paren)).end;
    return types.Group{
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

fn functionParameters(context: Context) ![]const types.Parameter {
    var parameters = List(types.Parameter).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .right_paren => break,
            .symbol => |name| {
                context.tokens.advance();
                _ = context.tokens.consume(.colon);
                const type_ = try expression(withPrecedence(context, DEFINE + 1));
                context.tokens.maybeConsume(.comma);
                try parameters.append(types.Parameter{ .name = name, .type = type_ });
            },
            else => |k| std.debug.panic("\nExpected symbol or right paren, found {}", .{k}),
        }
    }
    _ = context.tokens.consume(.right_paren);
    return parameters.toOwnedSlice();
}

fn function(context: Context, fn_: FnToken) !types.Expression {
    const begin = fn_.span.begin;
    _ = context.tokens.consume(.left_paren);
    const parameters = try functionParameters(context);
    const return_type = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    if (context.tokens.peek()) |t| {
        if (t == .left_brace) {
            const body = try block(withPrecedence(context, LOWEST), tokenizer.span.token(context.tokens.consume(.left_brace)).begin);
            const end = body.span.end;
            return types.Expression{
                .function = .{
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
            .parameters = parameters,
            .return_type = return_type,
            .span = types.Span{ .begin = begin, .end = end },
        },
    };
}

fn mutable(context: Context, begin: tokenizer.types.Pos) !types.Define {
    const name = context.tokens.next().?.symbol;
    _ = context.tokens.consume(.colon);
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
        .fn_ => |f| return try function(context, f),
        .left_brace => |l| return .{ .block = try block(context, l.span.begin) },
        .left_bracket => |l| return .{ .array = try array(context, l.span.begin) },
        .mut => |m| return .{ .define = try mutable(context, m.span.begin) },
        .undefined => |u| return .{ .undefined = u },
        else => |kind| std.debug.panic("\nNo prefix parser for {}\n", .{kind}),
    }
}

fn define(context: Context, name: types.Symbol) !types.Define {
    context.tokens.advance();
    const value = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    return types.Define{
        .name = name,
        .type = null,
        .value = value,
        .mutable = false,
        .span = types.Span{
            .begin = name.span.begin,
            .end = spanOf(value.*).end,
        },
    };
}

fn addAssign(context: Context, name: types.Symbol) !types.AddAssign {
    context.tokens.advance();
    const value = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    return types.AddAssign{
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
    var arguments = List(types.Expression).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .right_paren => break,
            else => {
                try arguments.append(try expression(withPrecedence(context, DEFINE + 1)));
                context.tokens.maybeConsume(.comma);
            },
        }
    }
    const end = context.tokens.next().?.right_paren.span.end;
    return types.Call{
        .function = try alloc(context, left),
        .arguments = try arguments.toOwnedSlice(),
        .span = types.Span{ .begin = spanOf(left).begin, .end = end },
    };
}

fn arrayOf(context: Context, left: types.Expression) !types.ArrayOf {
    switch (left) {
        .array => |a| {
            const element_type = try expressionAlloc(context);
            const span = types.Span{ .begin = spanOf(left).begin, .end = spanOf(element_type.*).end };
            if (a.expressions.len == 0) {
                return types.ArrayOf{
                    .size = null,
                    .element_type = element_type,
                    .span = span,
                };
            }
            if (a.expressions.len > 1) std.debug.panic("\nExpected array of size 1, found {}", .{a.expressions.len});
            switch (a.expressions[0]) {
                .int => |int| {
                    return types.ArrayOf{
                        .size = int,
                        .element_type = element_type,
                        .span = span,
                    };
                },
                else => std.debug.panic("\nExpected array size to be int, found {}", .{a.expressions[0]}),
            }
        },
        else => std.debug.panic("\nExpected array, found {}", .{left}),
    }
}

const Infix = union(enum) {
    define,
    add_assign,
    annotate,
    call,
    array_of,
    binary_op: types.BinaryOpKind,
};

fn precedence(i: Infix) Precedence {
    return switch (i) {
        .define => DEFINE,
        .add_assign => DEFINE,
        .annotate => DEFINE,
        .call => CALL,
        .array_of => ARRAY_OF,
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
        },
    };
}

fn associativity(i: Infix) Associativity {
    return switch (i) {
        .define => .right,
        .add_assign => .right,
        .annotate => .right,
        .call => .left,
        .array_of => .right,
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
        },
    };
}

fn infix(context: Context, left: types.Expression) ?Infix {
    if (context.tokens.peek()) |t| {
        return switch (t) {
            .equal => .define,
            .plus_equal => .add_assign,
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
            .left_paren => switch (left) {
                .symbol => .call,
                else => null,
            },
            .symbol => |_| switch (left) {
                .array => .array_of,
                else => null,
            },
            else => null,
        };
    }
    return null;
}

fn parseInfix(parser: Infix, context: Context, left: types.Expression) !types.Expression {
    return switch (parser) {
        .define => .{ .define = try define(context, left.symbol) },
        .add_assign => .{ .add_assign = try addAssign(context, left.symbol) },
        .annotate => .{ .define = try annotate(context, left.symbol) },
        .call => .{ .call = try call(context, left) },
        .array_of => .{ .array_of = try arrayOf(context, left) },
        .binary_op => |kind| .{ .binary_op = try binaryOp(context, left, kind) },
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

pub fn parse(allocator: Allocator, tokens: []const tokenizer.types.Token) !types.Module {
    var iterator = tokenizer.Iterator.init(tokens);
    const context = Context{
        .allocator = allocator,
        .tokens = &iterator,
        .precedence = LOWEST,
    };
    var expressions = List(types.Expression).init(allocator);
    while (iterator.peek()) |t| {
        switch (t) {
            .new_line => context.tokens.advance(),
            else => try expressions.append(try expression(context)),
        }
    }
    return types.Module{
        .expressions = try expressions.toOwnedSlice(),
    };
}
