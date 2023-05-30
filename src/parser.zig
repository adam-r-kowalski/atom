const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const Indent = @import("indent.zig").Indent;
const token = @import("token.zig");
const Span = @import("span.zig").Span;
const Pos = @import("span.zig").Pos;
const LeftParen = token.LeftParen;
const IfToken = token.If;
const FnToken = token.Fn;
const Token = token.Token;
const Tokens = token.Tokens;
const ast = @import("ast.zig");
const Expression = ast.Expression;
const Block = ast.Block;
const Group = ast.Group;
const Arm = ast.Arm;
const Branch = ast.Branch;
const Parameter = ast.Parameter;
const Symbol = ast.Symbol;
const BinaryOpKind = ast.BinaryOpKind;
const Define = ast.Define;
const BinaryOp = ast.BinaryOp;
const Call = ast.Call;
const Module = ast.Module;

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
const DOT: Precedence = CALL + DELTA;
const HIGHEST: Precedence = DOT + DELTA;

const Context = struct {
    allocator: Allocator,
    tokens: *Tokens,
    precedence: Precedence,
};

fn withPrecedence(context: Context, precedence: Precedence) Context {
    return Context{
        .allocator = context.allocator,
        .tokens = context.tokens,
        .precedence = precedence,
    };
}

fn consume(tokens: *Tokens, tag: std.meta.Tag(Token)) Token {
    const t = tokens.next().?;
    if (std.meta.activeTag(t) != tag)
        std.debug.panic("\nExpected token {} found {}", .{ tag, t });
    return t;
}

fn maybeConsume(tokens: *Tokens, tag: std.meta.Tag(Token)) void {
    if (tokens.peek()) |t| {
        if (std.meta.activeTag(t) == tag)
            tokens.advance();
    }
}

fn consumeNewLines(context: Context) void {
    while (context.tokens.peek()) |t| {
        switch (t) {
            .new_line => context.tokens.advance(),
            else => return,
        }
    }
}

fn alloc(context: Context, expr: Expression) !*const Expression {
    const ptr = try context.allocator.create(Expression);
    ptr.* = expr;
    return ptr;
}

fn expressionAlloc(context: Context) !*const Expression {
    const ptr = try context.allocator.create(Expression);
    ptr.* = try expression(context);
    return ptr;
}

fn block(context: Context, begin: Pos) !Block {
    var exprs = List(Expression).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .right_brace => break,
            .new_line => context.tokens.advance(),
            else => try exprs.append(try expression(withPrecedence(context, LOWEST))),
        }
    }
    const end = consume(context.tokens, .right_brace).span().end;
    return Block{
        .expressions = try exprs.toOwnedSlice(),
        .span = .{ .begin = begin, .end = end },
    };
}

fn group(context: Context, left_paren: LeftParen) !Group {
    const begin = left_paren.span.begin;
    const expr = try expressionAlloc(withPrecedence(context, LOWEST));
    const end = consume(context.tokens, .right_paren).span().end;
    return Group{
        .expression = expr,
        .span = .{ .begin = begin, .end = end },
    };
}

fn branch(context: Context, if_token: IfToken) !Branch {
    const begin = if_token.span.begin;
    const lowest = withPrecedence(context, LOWEST);
    var arms = List(Arm).init(context.allocator);
    try arms.append(Arm{
        .condition = try expression(lowest),
        .then = try block(lowest, consume(context.tokens, .left_brace).span().begin),
    });
    consumeNewLines(context);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .else_ => {
                context.tokens.advance();
                switch (context.tokens.next().?) {
                    .left_brace => |l| {
                        const else_ = try block(lowest, l.span.begin);
                        const end = else_.span.end;
                        return Branch{
                            .arms = try arms.toOwnedSlice(),
                            .else_ = else_,
                            .span = .{ .begin = begin, .end = end },
                        };
                    },
                    .if_ => {
                        try arms.append(Arm{
                            .condition = try expression(lowest),
                            .then = try block(lowest, consume(context.tokens, .left_brace).span().begin),
                        });
                        consumeNewLines(context);
                    },
                    else => |k| std.debug.panic("\nExpected (delimiter '{{') found {}", .{k}),
                }
            },
            else => {
                const pos = arms.items[0].then.span.end;
                const else_ = Block{ .expressions = &.{}, .span = .{ .begin = pos, .end = pos } };
                return Branch{
                    .arms = try arms.toOwnedSlice(),
                    .else_ = else_,
                    .span = .{ .begin = begin, .end = pos },
                };
            },
        }
    }
    std.debug.panic("\nExpected else token", .{});
}

fn functionParameters(context: Context) ![]const Parameter {
    var parameters = List(Parameter).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .right_paren => break,
            .symbol => |name| {
                context.tokens.advance();
                _ = consume(context.tokens, .colon);
                const type_ = try expression(withPrecedence(context, DEFINE + 1));
                maybeConsume(context.tokens, .comma);
                try parameters.append(Parameter{ .name = name, .type = type_ });
            },
            else => |k| std.debug.panic("\nExpected symbol or right paren, found {}", .{k}),
        }
    }
    _ = consume(context.tokens, .right_paren);
    return parameters.toOwnedSlice();
}

fn function(context: Context, fn_: FnToken) !Expression {
    const begin = fn_.span.begin;
    _ = consume(context.tokens, .left_paren);
    const parameters = try functionParameters(context);
    const return_type = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    if (context.tokens.peek()) |t| {
        if (t == .left_brace) {
            const body = try block(withPrecedence(context, LOWEST), consume(context.tokens, .left_brace).span().begin);
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
    const end = return_type.span().end;
    return Expression{
        .prototype = .{
            .parameters = parameters,
            .return_type = return_type,
            .span = Span{ .begin = begin, .end = end },
        },
    };
}

fn prefix(context: Context) !Expression {
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
        else => |kind| std.debug.panic("\nNo prefix parser for {}\n", .{kind}),
    }
}

const Asscociativity = enum {
    left,
    right,
};

fn define(context: Context, name: Symbol) !Define {
    context.tokens.advance();
    const value = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    return Define{
        .name = name,
        .type = null,
        .value = value,
        .span = Span{
            .begin = name.span.begin,
            .end = value.span().end,
        },
    };
}

fn annotate(context: Context, name: Symbol) !Define {
    context.tokens.advance();
    const type_ = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    _ = consume(context.tokens, .equal);
    const value = try expressionAlloc(withPrecedence(context, DEFINE + 1));
    return Define{
        .name = name,
        .type = type_,
        .value = value,
        .span = Span{
            .begin = name.span.begin,
            .end = value.span().end,
        },
    };
}

fn binaryOp(context: Context, left: Expression, kind: BinaryOpKind) !BinaryOp {
    context.tokens.advance();
    const right = try expressionAlloc(context);
    return BinaryOp{
        .kind = kind,
        .left = try alloc(context, left),
        .right = right,
        .span = Span{
            .begin = left.span().begin,
            .end = right.span().end,
        },
    };
}

fn call(context: Context, left: Expression) !Call {
    context.tokens.advance();
    var arguments = List(Expression).init(context.allocator);
    while (context.tokens.peek()) |t| {
        switch (t) {
            .right_paren => break,
            else => {
                try arguments.append(try expression(withPrecedence(context, DEFINE + 1)));
                maybeConsume(context.tokens, .comma);
            },
        }
    }
    const end = context.tokens.next().?.right_paren.span.end;
    return Call{
        .function = try alloc(context, left),
        .arguments = try arguments.toOwnedSlice(),
        .span = Span{ .begin = left.span().begin, .end = end },
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

fn infix(context: Context, left: Expression) ?Infix {
    if (context.tokens.peek()) |t| {
        switch (t) {
            .equal => return .{ .kind = .define, .precedence = DEFINE, .associativity = .right },
            .colon => return .{ .kind = .annotate, .precedence = DEFINE, .associativity = .right },
            .plus => return .{ .kind = .{ .binary_op = .add }, .precedence = ADD, .associativity = .left },
            .minus => return .{ .kind = .{ .binary_op = .subtract }, .precedence = ADD, .associativity = .left },
            .times => return .{ .kind = .{ .binary_op = .multiply }, .precedence = MULTIPLY, .associativity = .left },
            .slash => return .{ .kind = .{ .binary_op = .divide }, .precedence = MULTIPLY, .associativity = .left },
            .percent => return .{ .kind = .{ .binary_op = .modulo }, .precedence = MULTIPLY, .associativity = .left },
            .caret => return .{ .kind = .{ .binary_op = .exponentiate }, .precedence = EXPONENTIATE, .associativity = .right },
            .equal_equal => return .{ .kind = .{ .binary_op = .equal }, .precedence = COMPARE, .associativity = .left },
            .greater => return .{ .kind = .{ .binary_op = .greater }, .precedence = COMPARE, .associativity = .left },
            .less => return .{ .kind = .{ .binary_op = .less }, .precedence = COMPARE, .associativity = .left },
            .or_ => return .{ .kind = .{ .binary_op = .or_ }, .precedence = AND, .associativity = .left },
            .dot => return .{ .kind = .{ .binary_op = .dot }, .precedence = AND, .associativity = .left },
            .left_paren => switch (left) {
                .symbol => return .{ .kind = .call, .precedence = CALL, .associativity = .left },
                else => return null,
            },
            else => return null,
        }
    }
    return null;
}

fn parseInfix(parser: Infix, context: Context, left: Expression) !Expression {
    switch (parser.kind) {
        .define => return .{ .define = try define(context, left.symbol) },
        .annotate => return .{ .define = try annotate(context, left.symbol) },
        .call => return .{ .call = try call(context, left) },
        .binary_op => |kind| return .{ .binary_op = try binaryOp(context, left, kind) },
    }
}

fn expression(context: Context) error{OutOfMemory}!Expression {
    var left = try prefix(context);
    while (true) {
        if (infix(context, left)) |parser| {
            var next = parser.precedence;
            if (context.precedence > next) return left;
            if (parser.associativity == .left) next += 1;
            left = try parseInfix(parser, withPrecedence(context, next), left);
        } else {
            return left;
        }
    }
}

pub fn parse(allocator: Allocator, tokens: *Tokens) !Module {
    const context = Context{
        .allocator = allocator,
        .tokens = tokens,
        .precedence = LOWEST,
    };
    var expressions = List(Expression).init(allocator);
    while (tokens.peek()) |t| {
        switch (t) {
            .new_line => context.tokens.advance(),
            else => try expressions.append(try expression(context)),
        }
    }
    return Module{
        .expressions = try expressions.toOwnedSlice(),
        .compile_errors = tokens.compile_errors,
    };
}
