const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const tokenizer_types = @import("../tokenizer/types.zig");
const Token = tokenizer_types.Token;
const Span = tokenizer_types.Span;
const LeftParen = tokenizer_types.LeftParen;
const LeftBrace = tokenizer_types.LeftBrace;
const IfToken = tokenizer_types.If;
const FnToken = tokenizer_types.Fn;
const types = @import("types.zig");
const Expression = types.Expression;
const Group = types.Group;
const If = types.If;
const Block = types.Block;
const BinaryOpKind = types.BinaryOpKind;
const Parameter = types.Parameter;
const Function = types.Function;
const Prototype = types.Prototype;
const Define = types.Define;
const BinaryOp = types.BinaryOp;
const Symbol = types.Symbol;
const Call = types.Call;
const Module = types.Module;
const expressionSpan = @import("span.zig").span;

const Precedence = u32;

const DELTA: Precedence = 10;
const LOWEST: Precedence = 0;
const DEFINE: Precedence = LOWEST + DELTA;
const AND: Precedence = DEFINE + DELTA;
const COMPARE: Precedence = AND + DELTA;
const ADD: Precedence = COMPARE + DELTA;
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

fn maybeConsume(context: *Context, tag: std.meta.Tag(Token)) void {
    const token = context.tokens[context.token_index];
    if (std.meta.activeTag(token) == tag)
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

fn block(context: *Context, left_brace: LeftBrace) !Block {
    const begin = left_brace.span.begin;
    var exprs = List(Expression).init(context.allocator);
    while (peekToken(context.*)) |token| {
        switch (token) {
            .right_brace => break,
            .new_line => context.token_index += 1,
            else => {
                context.precedence = LOWEST;
                try exprs.append(try expression(context));
            },
        }
    }
    const end = nextToken(context).right_brace.span.end;
    return Block{
        .expressions = try exprs.toOwnedSlice(),
        .span = .{ .begin = begin, .end = end },
    };
}

fn group(context: *Context, left_paren: LeftParen) !Group {
    const begin = left_paren.span.begin;
    context.precedence = LOWEST;
    const expr = try expressionAlloc(context);
    const end = nextToken(context).right_paren.span.end;
    return Group{
        .expression = expr,
        .span = .{ .begin = begin, .end = end },
    };
}

fn conditional(context: *Context, if_: IfToken) !If {
    const begin = if_.span.begin;
    context.precedence = LOWEST;
    const condition = try expressionAlloc(context);
    const then = try block(context, nextToken(context).left_brace);
    _ = nextToken(context).else_;
    const else_ = try block(context, nextToken(context).left_brace);
    const end = else_.span.end;
    return If{
        .condition = condition,
        .then = then,
        .else_ = else_,
        .span = .{ .begin = begin, .end = end },
    };
}

fn functionParameters(context: *Context) ![]const Parameter {
    var parameters = List(Parameter).init(context.allocator);
    while (peekToken(context.*)) |token| {
        switch (token) {
            .right_paren => break,
            .symbol => |name| {
                _ = nextToken(context);
                _ = nextToken(context).colon;
                context.precedence = DEFINE + 1;
                const type_ = try expression(context);
                maybeConsume(context, .comma);
                try parameters.append(Parameter{ .name = name, .type = type_ });
            },
            else => |k| std.debug.panic("\nExpected symbol or right paren, found {}", .{k}),
        }
    }
    _ = nextToken(context).right_paren;
    return parameters.toOwnedSlice();
}

fn function(context: *Context, fn_: FnToken) !Expression {
    const begin = fn_.span.begin;
    _ = nextToken(context).left_paren;
    const parameters = try functionParameters(context);
    context.precedence = DEFINE + 1;
    const return_type = try expressionAlloc(context);
    if (peekToken(context.*)) |token| {
        if (token == .left_brace) {
            context.precedence = LOWEST;
            const body = try block(context, nextToken(context).left_brace);
            const end = body.span.end;
            return Expression{
                .function = .{
                    .parameters = parameters,
                    .return_type = return_type,
                    .body = body,
                    .span = Span{ .begin = begin, .end = end },
                },
            };
        }
    }
    const end = expressionSpan(return_type.*).end;
    return Expression{
        .prototype = .{
            .parameters = parameters,
            .return_type = return_type,
            .span = Span{ .begin = begin, .end = end },
        },
    };
}

fn prefix(context: *Context) !Expression {
    const token = nextToken(context);
    switch (token) {
        .int => |i| return .{ .int = i },
        .float => |f| return .{ .float = f },
        .symbol => |s| return .{ .symbol = s },
        .string => |s| return .{ .string = s },
        .bool => |b| return .{ .bool = b },
        .left_paren => |l| return .{ .group = try group(context, l) },
        .if_ => |i| return .{ .if_ = try conditional(context, i) },
        .fn_ => |f| return try function(context, f),
        .left_brace => |l| return .{ .block = try block(context, l) },
        else => |kind| std.debug.panic("\nNo prefix parser for {}\n", .{kind}),
    }
}

const Asscociativity = enum {
    left,
    right,
};

fn define(context: *Context, name: Symbol) !Define {
    context.token_index += 1;
    const value = try expressionAlloc(context);
    return Define{
        .name = name,
        .type = null,
        .value = value,
        .span = Span{
            .begin = name.span.begin,
            .end = expressionSpan(value.*).end,
        },
    };
}

fn annotate(context: *Context, name: Symbol) !Define {
    context.token_index += 1;
    context.precedence = DEFINE + 1;
    const type_ = try expressionAlloc(context);
    _ = nextToken(context).equal;
    context.precedence = LOWEST;
    const value = try expressionAlloc(context);
    return Define{
        .name = name,
        .type = type_,
        .value = value,
        .span = Span{
            .begin = name.span.begin,
            .end = expressionSpan(value.*).end,
        },
    };
}

fn binaryOp(context: *Context, left: Expression, kind: BinaryOpKind) !BinaryOp {
    context.token_index += 1;
    const right = try expressionAlloc(context);
    return BinaryOp{
        .kind = kind,
        .left = try alloc(context, left),
        .right = right,
        .span = Span{
            .begin = expressionSpan(left).begin,
            .end = expressionSpan(right.*).end,
        },
    };
}

fn call(context: *Context, left: Expression) !Call {
    context.token_index += 1;
    var arguments = List(Expression).init(context.allocator);
    while (peekToken(context.*)) |token| {
        switch (token) {
            .right_paren => break,
            else => {
                context.precedence = DEFINE + 1;
                try arguments.append(try expression(context));
                maybeConsume(context, .comma);
            },
        }
    }
    const end = nextToken(context).right_paren.span.end;
    return Call{
        .function = try alloc(context, left),
        .arguments = try arguments.toOwnedSlice(),
        .span = Span{ .begin = expressionSpan(left).begin, .end = end },
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
        switch (token) {
            .equal => return .{ .kind = .define, .precedence = DEFINE, .associativity = .right },
            .colon => return .{ .kind = .annotate, .precedence = DEFINE, .associativity = .right },
            .plus => return .{ .kind = .{ .binary_op = .add }, .precedence = ADD, .associativity = .left },
            .minus => return .{ .kind = .{ .binary_op = .subtract }, .precedence = ADD, .associativity = .left },
            .times => return .{ .kind = .{ .binary_op = .multiply }, .precedence = MULTIPLY, .associativity = .left },
            .percent => return .{ .kind = .{ .binary_op = .modulo }, .precedence = MULTIPLY, .associativity = .left },
            .caret => return .{ .kind = .{ .binary_op = .exponentiate }, .precedence = EXPONENTIATE, .associativity = .right },
            .equal_equal => return .{ .kind = .{ .binary_op = .equal }, .precedence = COMPARE, .associativity = .left },
            .greater => return .{ .kind = .{ .binary_op = .greater }, .precedence = COMPARE, .associativity = .left },
            .less => return .{ .kind = .{ .binary_op = .less }, .precedence = COMPARE, .associativity = .left },
            .or_ => return .{ .kind = .{ .binary_op = .or_ }, .precedence = AND, .associativity = .left },
            .left_paren => switch (left) {
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
        .define => return .{ .define = try define(context, left.symbol) },
        .annotate => return .{ .define = try annotate(context, left.symbol) },
        .call => return .{ .call = try call(context, left) },
        .binary_op => |kind| return .{ .binary_op = try binaryOp(context, left, kind) },
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
        switch (token) {
            .new_line => context.token_index += 1,
            else => try expressions.append(try expression(&context)),
        }
    }
    return Module{ .expressions = try expressions.toOwnedSlice() };
}
