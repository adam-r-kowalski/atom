const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const tokenizer_types = @import("../tokenizer/types.zig");
const Token = tokenizer_types.Token;
const Span = tokenizer_types.Span;
const Indent = tokenizer_types.Indent;
const types = @import("types.zig");
const Expression = types.Expression;
const TopLevel = types.TopLevel;
const BinaryOpKind = types.BinaryOpKind;
const Parameter = types.Parameter;
const Int = types.Int;
const Float = types.Float;
const Symbol = types.Symbol;
const Bool = types.Bool;
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
const ARROW: Precedence = EXPONENTIATE + DELTA;
const HIGHEST: Precedence = ARROW + DELTA;

const Context = struct {
    allocator: Allocator,
    tokens: []const Token,
    token_index: u64,
    precedence: Precedence,
    indent: Indent,
};

fn int(context: *Context, i: Int) Expression {
    context.token_index += 1;
    return Expression{ .int = i };
}

fn float(context: *Context, f: Float) Expression {
    context.token_index += 1;
    return Expression{ .float = f };
}

fn symbol(context: *Context, s: Symbol) !Expression {
    context.token_index += 1;
    return Expression{ .symbol = s };
}

fn boolean(context: *Context, b: Bool) !Expression {
    context.token_index += 1;
    return Expression{ .bool = b };
}

fn consume(context: *Context, kind: std.meta.Tag(Token)) void {
    std.debug.assert(std.meta.activeTag(context.tokens[context.token_index]) == kind);
    context.token_index += 1;
}

fn tryConsume(context: *Context, kind: std.meta.Tag(Token)) bool {
    if (context.token_index >= context.tokens.len) return false;
    if (std.meta.activeTag(context.tokens[context.token_index]) != kind) return false;
    context.token_index += 1;
    return true;
}

fn consumeIndent(context: *Context) bool {
    if (context.tokens.len <= context.token_index) return false;
    switch (context.tokens[context.token_index]) {
        .indent => {
            context.token_index += 1;
            return true;
        },
        else => return false,
    }
}

fn consumeSymbol(context: *Context) Symbol {
    const token = context.tokens[context.token_index];
    switch (token) {
        .symbol => |s| {
            context.token_index += 1;
            return s;
        },
        else => |kind| std.debug.panic("\nExpected symbol, got {}\n", .{kind}),
    }
}

fn span(value: anytype) Span {
    const tag_name = @tagName(value);
    inline for (std.meta.fields(@TypeOf(value))) |field|
        if (std.mem.eql(u8, field.name, tag_name))
            return @field(value, field.name).span;
    std.debug.panic("\nNo span for {}\n", .{value});
}

fn group(context: *Context) !Expression {
    const begin = span(context.tokens[context.token_index]).begin;
    context.token_index += 1;
    context.precedence = LOWEST;
    const expr = try expressionAlloc(context);
    std.debug.assert(context.tokens[context.token_index] == .right_paren);
    const end = span(context.tokens[context.token_index]).end;
    context.token_index += 1;
    return Expression{ .group = .{ .expression = expr, .span = .{ .begin = begin, .end = end } } };
}

fn if_(context: *Context) !Expression {
    const begin = span(context.tokens[context.token_index]).begin;
    context.token_index += 1;
    context.precedence = LOWEST;
    const condition = try expressionAlloc(context);
    consume(context, .then);
    const then = try block(context);
    _ = consumeIndent(context);
    consume(context, .else_);
    const else_ = try block(context);
    const end = span(else_[else_.len - 1]).end;
    return Expression{
        .if_ = .{
            .condition = condition,
            .then = then,
            .else_ = else_,
            .span = .{ .begin = begin, .end = end },
        },
    };
}

fn functionParameters(context: *Context) ![]const Parameter {
    var parameters = List(Parameter).init(context.allocator);
    while (context.tokens.len > context.token_index) {
        switch (context.tokens[context.token_index]) {
            .right_paren => {
                context.token_index += 1;
                return parameters.toOwnedSlice();
            },
            else => {
                const name = consumeSymbol(context);
                consume(context, .colon);
                context.precedence = DEFINE + 1;
                const type_ = try expression(context);
                _ = tryConsume(context, .comma);
                try parameters.append(Parameter{ .name = name, .type = type_ });
            },
        }
    }
    std.debug.panic("\nExpected right paren when reading function parameters", .{});
}

fn function(context: *Context) !Expression {
    consume(context, .fn_);
    const name = consumeSymbol(context);
    consume(context, .left_paren);
    const parameters = try functionParameters(context);
    context.precedence = DEFINE + 1;
    const return_type = try expressionAlloc(context);
    consume(context, .equal);
    context.precedence = LOWEST;
    const body = try block(context);
    return Expression{
        .function = .{
            .name = name,
            .parameters = parameters,
            .return_type = return_type,
            .body = body,
            .span = Span{
                .begin = name.span.begin,
                .end = span(body[body.len - 1]).end,
            },
        },
    };
}

fn prefix(context: *Context) !Expression {
    const token = context.tokens[context.token_index];
    switch (token) {
        .int => |i| return int(context, i),
        .float => |f| return float(context, f),
        .symbol => |s| return symbol(context, s),
        .bool => |b| return boolean(context, b),
        .left_paren => return try group(context),
        .if_ => return try if_(context),
        .fn_ => return try function(context),
        else => |kind| std.debug.panic("\nNo prefix parser for {}\n", .{kind}),
    }
}

const Asscociativity = enum {
    left,
    right,
};

fn sameIndent(x: Indent, y: Indent) bool {
    return x.kind == y.kind and x.count == y.count;
}

fn block(context: *Context) ![]const Expression {
    var exprs = List(Expression).init(context.allocator);
    switch (context.tokens[context.token_index]) {
        .indent => |indent| {
            context.token_index += 1;
            context.indent = indent;
            while (true) {
                context.precedence = LOWEST;
                const expr = try expression(context);
                try exprs.append(expr);
                if (context.tokens.len <= context.token_index) break;
                switch (context.tokens[context.token_index]) {
                    .indent => |next_indent| {
                        if (!sameIndent(indent, next_indent)) break;
                        context.token_index += 1;
                    },
                    else => break,
                }
            }
        },
        else => {
            context.precedence = LOWEST;
            const expr = try expression(context);
            try exprs.append(expr);
        },
    }
    return exprs.toOwnedSlice();
}

fn alloc(context: *Context, ast: Expression) !*const Expression {
    const ptr = try context.allocator.create(Expression);
    ptr.* = ast;
    return ptr;
}

fn define(context: *Context, left: Expression) !Expression {
    const name = left.symbol;
    context.token_index += 1;
    const body = try block(context);
    return Expression{ .define = .{
        .name = name,
        .type = null,
        .body = body,
        .span = Span{
            .begin = name.span.begin,
            .end = span(body[body.len - 1]).end,
        },
    } };
}

fn annotate(context: *Context, left: Expression) !Expression {
    const name = left.symbol;
    context.token_index += 1;
    context.precedence = ARROW;
    const type_ = try expressionAlloc(context);
    consume(context, .equal);
    context.precedence = LOWEST;
    const body = try block(context);
    return Expression{ .define = .{
        .name = name,
        .type = type_,
        .body = body,
        .span = Span{
            .begin = name.span.begin,
            .end = span(body[body.len - 1]).end,
        },
    } };
}

fn binaryOp(context: *Context, left: Expression, kind: BinaryOpKind) !Expression {
    context.token_index += 1;
    const right = try expression(context);
    return Expression{
        .binary_op = .{
            .kind = kind,
            .left = try alloc(context, left),
            .right = try alloc(context, right),
            .span = Span{
                .begin = span(left).begin,
                .end = span(right).end,
            },
        },
    };
}

fn call(context: *Context, left: Expression) !Expression {
    context.token_index += 1;
    var arguments = List(Expression).init(context.allocator);
    while (context.tokens.len > context.token_index) {
        switch (context.tokens[context.token_index]) {
            .right_paren => |r| {
                context.token_index += 1;
                return Expression{
                    .call = .{
                        .function = try alloc(context, left),
                        .arguments = arguments.toOwnedSlice(),
                        .span = Span{
                            .begin = span(left).begin,
                            .end = r.span.end,
                        },
                    },
                };
            },
            else => {
                const argument = try expression(context);
                _ = tryConsume(context, .comma);
                try arguments.append(argument);
            },
        }
    }
    std.debug.panic("\nExpected ')' but found EOF\n", .{});
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
    if (context.tokens.len <= context.token_index) return null;
    switch (context.tokens[context.token_index]) {
        .equal => return .{ .kind = .define, .precedence = DEFINE, .associativity = .right },
        .colon => return .{ .kind = .annotate, .precedence = ANNOTATE, .associativity = .right },
        .plus => return .{ .kind = .{ .binary_op = .add }, .precedence = ADD, .associativity = .left },
        .minus => return .{ .kind = .{ .binary_op = .subtract }, .precedence = SUBTRACT, .associativity = .left },
        .times => return .{ .kind = .{ .binary_op = .multiply }, .precedence = MULTIPLY, .associativity = .left },
        .caret => return .{ .kind = .{ .binary_op = .exponentiate }, .precedence = EXPONENTIATE, .associativity = .right },
        .greater => return .{ .kind = .{ .binary_op = .greater }, .precedence = GREATER, .associativity = .left },
        .less => return .{ .kind = .{ .binary_op = .less }, .precedence = LESS, .associativity = .left },
        .left_paren => switch (left) {
            .symbol => return .{ .kind = .call, .precedence = CALL, .associativity = .left },
            else => return null,
        },
        else => return null,
    }
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

fn expressionAlloc(context: *Context) !*const Expression {
    const ast = try context.allocator.create(Expression);
    ast.* = try expression(context);
    return ast;
}

fn import(context: *Context) !TopLevel {
    const begin = span(context.tokens[context.token_index]).begin;
    context.token_index += 1;
    consume(context, .fn_);
    const name = consumeSymbol(context);
    consume(context, .left_paren);
    const parameters = try functionParameters(context);
    const return_type = try expressionAlloc(context);
    return TopLevel{
        .import = .{
            .name = name,
            .parameters = parameters,
            .return_type = return_type,
            .span = Span{
                .begin = begin,
                .end = span(parameters[parameters.len - 1].type).end,
            },
        },
    };
}

fn export_(context: *Context) !TopLevel {
    const begin = span(context.tokens[context.token_index]).begin;
    context.token_index += 1;
    context.precedence = LOWEST;
    switch (try expression(context)) {
        .function => |f| return TopLevel{ .export_ = .{
            .function = f,
            .span = .{
                .begin = begin,
                .end = f.span.end,
            },
        } },
        else => std.debug.panic("\nCan only export function", .{}),
    }
}

fn topLevel(context: *Context) !?TopLevel {
    while (consumeIndent(context)) {}
    if (context.tokens.len <= context.token_index) return null;
    const token = context.tokens[context.token_index];
    switch (token) {
        .import => return try import(context),
        .export_ => return try export_(context),
        else => switch (try expression(context)) {
            .define => |d| return TopLevel{ .define = d },
            .function => |f| return TopLevel{ .function = f },
            else => |e| std.debug.panic("\nInvalid top level expression {}", .{e}),
        },
    }
}

pub fn parse(allocator: Allocator, tokens: []const Token) !Module {
    var context = Context{
        .allocator = allocator,
        .tokens = tokens,
        .token_index = 0,
        .precedence = LOWEST,
        .indent = Indent{
            .kind = .space,
            .count = 0,
            .span = .{
                .begin = .{ .line = 1, .column = 1 },
                .end = .{ .line = 1, .column = 1 },
            },
        },
    };
    var list = List(TopLevel).init(allocator);
    while (try topLevel(&context)) |t| try list.append(t);
    const top_level = list.toOwnedSlice();
    return Module{
        .top_level = top_level,
        .span = .{
            .begin = span(top_level[0]).begin,
            .end = span(top_level[top_level.len - 1]).end,
        },
    };
}
