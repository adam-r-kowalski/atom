const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const tokenizer_types = @import("../tokenizer/types.zig");
const Token = tokenizer_types.Token;
const Span = tokenizer_types.Span;
const types = @import("types.zig");
const Expression = types.Expression;
const Kind = types.Kind;
const BinaryOpKind = types.BinaryOpKind;
const Parameter = types.Parameter;
const Module = types.Module;

const Precedence = u32;

const DELTA: Precedence = 10;
const LOWEST: Precedence = 0;
const DEFINE: Precedence = LOWEST + DELTA;
const ANNOTATE: Precedence = DEFINE;
const GREATER: Precedence = ANNOTATE + DELTA;
const LESS: Precedence = GREATER;
const ADD: Precedence = GREATER + DELTA;
const SUBTRACT: Precedence = ADD;
const MULTIPLY: Precedence = ADD + DELTA;
const EXPONENTIATE: Precedence = MULTIPLY + DELTA;
const CALL: Precedence = EXPONENTIATE + DELTA;
const HIGHEST: Precedence = CALL + DELTA;

const Context = struct {
    allocator: Allocator,
    tokens: []const Token,
    token_index: u64,
    precedence: Precedence,
};

fn nextToken(context: *Context) Token {
    const token = context.tokens[context.token_index];
    context.token_index += 1;
    return token;
}

fn peekToken(context: Context) ?Token {
    if (context.token_index >= context.tokens.len)
        return null;
    return context.tokens[context.token_index];
}

fn int(context: *Context) Expression {
    const token = nextToken(context);
    return Expression{
        .kind = .{ .int = token.kind.int },
        .span = token.span,
    };
}

fn float(context: *Context) Expression {
    const token = nextToken(context);
    return Expression{
        .kind = .{ .float = token.kind.float },
        .span = token.span,
    };
}

fn symbol(context: *Context) Expression {
    const token = nextToken(context);
    return Expression{
        .kind = .{ .symbol = token.kind.symbol },
        .span = token.span,
    };
}

fn string(context: *Context) Expression {
    const token = nextToken(context);
    return Expression{
        .kind = .{ .string = token.kind.string },
        .span = token.span,
    };
}

fn boolean(context: *Context) Expression {
    const token = nextToken(context);
    return Expression{
        .kind = .{ .bool = token.kind.bool },
        .span = token.span,
    };
}

fn consume(context: *Context, kind: tokenizer_types.Kind) Token {
    const token = nextToken(context);
    std.debug.assert(std.meta.activeTag(token.kind) == kind);
    return token;
}

fn maybeConsume(context: *Context, kind: tokenizer_types.Kind) void {
    const token = context.tokens[context.token_index];
    if (std.meta.activeTag(token.kind) == kind)
        context.token_index += 1;
}

fn alloc(context: *Context, expr: Expression) !*const Expression {
    const ptr = try context.allocator.create(Expression);
    ptr.* = expr;
    return ptr;
}

fn expressionAlloc(context: *Context) !*const Expression {
    const ptr = try context.allocator.create(Expression);
    ptr.* = try expression(context);
    return ptr;
}

fn block(context: *Context) !Expression {
    const begin = consume(context, .left_brace).span.begin;
    var exprs = List(Expression).init(context.allocator);
    while (peekToken(context.*)) |token| {
        switch (token.kind) {
            .right_brace => break,
            .new_line => context.token_index += 1,
            else => {
                context.precedence = LOWEST;
                try exprs.append(try expression(context));
            },
        }
    }
    const end = consume(context, .right_brace).span.end;
    return Expression{
        .kind = .{ .block = try exprs.toOwnedSlice() },
        .span = .{ .begin = begin, .end = end },
    };
}

fn blockAlloc(context: *Context) !*const Expression {
    const ptr = try context.allocator.create(Expression);
    ptr.* = try block(context);
    return ptr;
}

fn group(context: *Context) !Expression {
    const begin = consume(context, .left_paren).span.begin;
    context.precedence = LOWEST;
    const expr = try expressionAlloc(context);
    const end = consume(context, .right_paren).span.end;
    return Expression{
        .kind = .{ .group = expr },
        .span = .{ .begin = begin, .end = end },
    };
}

fn conditional(context: *Context) !Expression {
    const begin = consume(context, .if_).span.begin;
    context.precedence = LOWEST;
    const condition = try expressionAlloc(context);
    const then = try blockAlloc(context);
    _ = consume(context, .else_);
    const else_ = try blockAlloc(context);
    const end = else_.span.end;
    return Expression{
        .kind = .{ .if_ = .{ .condition = condition, .then = then, .else_ = else_ } },
        .span = .{ .begin = begin, .end = end },
    };
}

fn functionParameters(context: *Context) ![]const Parameter {
    var parameters = List(Parameter).init(context.allocator);
    while (peekToken(context.*)) |token| {
        switch (token.kind) {
            .right_paren => break,
            .symbol => {
                const name = try alloc(context, symbol(context));
                _ = consume(context, .colon);
                context.precedence = DEFINE + 1;
                const type_ = try expressionAlloc(context);
                maybeConsume(context, .comma);
                try parameters.append(Parameter{ .name = name, .type = type_ });
            },
            else => |k| std.debug.panic("\nExpected symbol or right paren, found {}", .{k}),
        }
    }
    _ = consume(context, .right_paren);
    return parameters.toOwnedSlice();
}

fn function(context: *Context) !Expression {
    const begin = consume(context, .fn_).span.begin;
    _ = consume(context, .left_paren);
    const parameters = try functionParameters(context);
    context.precedence = DEFINE + 1;
    const return_type = try expressionAlloc(context);
    if (peekToken(context.*)) |token| {
        if (token.kind == .left_brace) {
            context.precedence = LOWEST;
            const body = try blockAlloc(context);
            const end = body.span.end;
            return Expression{
                .kind = .{
                    .function = .{
                        .parameters = parameters,
                        .return_type = return_type,
                        .body = body,
                    },
                },
                .span = Span{ .begin = begin, .end = end },
            };
        }
    }
    const end = return_type.span.end;
    return Expression{
        .kind = .{
            .prototype = .{
                .parameters = parameters,
                .return_type = return_type,
            },
        },
        .span = Span{ .begin = begin, .end = end },
    };
}

fn prefix(context: *Context) !Expression {
    const token = context.tokens[context.token_index];
    switch (token.kind) {
        .int => return int(context),
        .float => return float(context),
        .symbol => return symbol(context),
        .string => return string(context),
        .bool => return boolean(context),
        .left_paren => return try group(context),
        .if_ => return try conditional(context),
        .fn_ => return try function(context),
        .left_brace => return try block(context),
        else => |kind| std.debug.panic("\nNo prefix parser for {}\n", .{kind}),
    }
}

const Asscociativity = enum {
    left,
    right,
};

fn define(context: *Context, name: Expression) !Expression {
    context.token_index += 1;
    const value = try expressionAlloc(context);
    return Expression{
        .kind = .{
            .define = .{
                .name = try alloc(context, name),
                .type = null,
                .value = value,
            },
        },
        .span = Span{ .begin = name.span.begin, .end = value.span.end },
    };
}

fn annotate(context: *Context, name: Expression) !Expression {
    context.token_index += 1;
    context.precedence = DEFINE + 1;
    const type_ = try expressionAlloc(context);
    _ = consume(context, .equal);
    context.precedence = LOWEST;
    const value = try expressionAlloc(context);
    return Expression{
        .kind = .{
            .define = .{
                .name = try alloc(context, name),
                .type = type_,
                .value = value,
            },
        },
        .span = Span{ .begin = name.span.begin, .end = value.span.end },
    };
}

fn binaryOp(context: *Context, left: Expression, kind: BinaryOpKind) !Expression {
    context.token_index += 1;
    const right = try expressionAlloc(context);
    return Expression{
        .kind = .{
            .binary_op = .{
                .kind = kind,
                .left = try alloc(context, left),
                .right = right,
            },
        },
        .span = Span{ .begin = left.span.begin, .end = right.span.end },
    };
}

fn call(context: *Context, left: Expression) !Expression {
    context.token_index += 1;
    var arguments = List(Expression).init(context.allocator);
    while (peekToken(context.*)) |token| {
        switch (token.kind) {
            .right_paren => break,
            else => {
                context.precedence = DEFINE + 1;
                try arguments.append(try expression(context));
                maybeConsume(context, .comma);
            },
        }
    }
    const end = consume(context, .right_paren).span.end;
    return Expression{
        .kind = .{
            .call = .{
                .function = try alloc(context, left),
                .arguments = try arguments.toOwnedSlice(),
            },
        },
        .span = Span{ .begin = left.span.begin, .end = end },
    };
}

const Infix = struct {
    precedence: Precedence,
    associativity: Asscociativity,
    kind: union(enum) {
        define,
        annotate,
        call,
        binary_op: BinaryOpKind,
    },
};

fn infix(context: *Context, left: Expression) ?Infix {
    if (peekToken(context.*)) |token| {
        switch (token.kind) {
            .equal => return .{ .kind = .define, .precedence = DEFINE, .associativity = .right },
            .colon => return .{ .kind = .annotate, .precedence = ANNOTATE, .associativity = .right },
            .plus => return .{ .kind = .{ .binary_op = .add }, .precedence = ADD, .associativity = .left },
            .minus => return .{ .kind = .{ .binary_op = .subtract }, .precedence = SUBTRACT, .associativity = .left },
            .times => return .{ .kind = .{ .binary_op = .multiply }, .precedence = MULTIPLY, .associativity = .left },
            .caret => return .{ .kind = .{ .binary_op = .exponentiate }, .precedence = EXPONENTIATE, .associativity = .right },
            .greater => return .{ .kind = .{ .binary_op = .greater }, .precedence = GREATER, .associativity = .left },
            .less => return .{ .kind = .{ .binary_op = .less }, .precedence = LESS, .associativity = .left },
            .left_paren => switch (left.kind) {
                .symbol => return .{ .kind = .call, .precedence = CALL, .associativity = .left },
                else => return null,
            },
            else => return null,
        }
    }
    return null;
}

fn parseInfix(parser: Infix, context: *Context, left: Expression) !Expression {
    switch (parser.kind) {
        .define => return try define(context, left),
        .annotate => return try annotate(context, left),
        .call => return try call(context, left),
        .binary_op => |kind| return try binaryOp(context, left, kind),
    }
}

fn expression(context: *Context) error{OutOfMemory}!Expression {
    var left = try prefix(context);
    const previous = context.precedence;
    while (true) {
        if (infix(context, left)) |parser| {
            var next = parser.precedence;
            if (context.precedence > next) return left;
            if (parser.associativity == .left) next += 1;
            context.precedence = next;
            left = try parseInfix(parser, context, left);
            context.precedence = previous;
        } else {
            return left;
        }
    }
}

pub fn parse(allocator: Allocator, tokens: []const Token) !Module {
    var context = Context{
        .allocator = allocator,
        .tokens = tokens,
        .token_index = 0,
        .precedence = LOWEST,
    };
    var expressions = List(Expression).init(allocator);
    while (peekToken(context)) |token| {
        switch (token.kind) {
            .new_line => context.token_index += 1,
            else => try expressions.append(try expression(&context)),
        }
    }
    return Module{ .expressions = try expressions.toOwnedSlice() };
}
