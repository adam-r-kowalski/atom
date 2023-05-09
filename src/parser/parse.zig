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
const Ast = types.Ast;
const BinaryOpKind = types.BinaryOpKind;
const Parameter = types.Parameter;

const Precedence = u32;

const DELTA = 10;
const LOWEST = 0;
const DEFINE = LOWEST + DELTA;
const ANNOTATE = DEFINE;
const GREATER = ANNOTATE + DELTA;
const LESS = GREATER;
const ADD = GREATER + DELTA;
const SUBTRACT = ADD;
const MULTIPLY = ADD + DELTA;
const EXPONENTIATE = MULTIPLY + DELTA;
const CALL = EXPONENTIATE + DELTA;
const ARROW = EXPONENTIATE + DELTA;
const HIGHEST = ARROW + DELTA;

const Context = struct {
    allocator: Allocator,
    tokens: []const Token,
    token_index: u64,
    precedence: Precedence,
    indent: Indent,
};

fn int(context: *Context, token: Token) Ast {
    context.token_index += 1;
    return Ast{ .kind = .{ .int = token.kind.int }, .span = token.span };
}

fn symbol(context: *Context, token: Token) !Ast {
    context.token_index += 1;
    return Ast{ .kind = .{ .symbol = token.kind.symbol }, .span = token.span };
}

fn boolean(context: *Context, token: Token) !Ast {
    context.token_index += 1;
    return Ast{ .kind = .{ .bool = token.kind.bool }, .span = token.span };
}

fn consume(context: *Context, kind: tokenizer_types.Kind) void {
    std.debug.assert(std.meta.activeTag(context.tokens[context.token_index].kind) == kind);
    context.token_index += 1;
}

fn tryConsume(context: *Context, kind: tokenizer_types.Kind) bool {
    if (context.token_index >= context.tokens.len) return false;
    if (std.meta.activeTag(context.tokens[context.token_index].kind) != kind) return false;
    context.token_index += 1;
    return true;
}

fn consumeIndent(context: *Context) bool {
    switch (context.tokens[context.token_index].kind) {
        .indent => {
            context.token_index += 1;
            return true;
        },
        else => return false,
    }
}

fn consumeSymbol(context: *Context) !Ast {
    const token = context.tokens[context.token_index];
    switch (token.kind) {
        .symbol => return try symbol(context, token),
        else => |kind| std.debug.panic("\nExpected symbol, got {}\n", .{kind}),
    }
}

fn group(context: *Context) !Ast {
    const begin = context.tokens[context.token_index].span.begin;
    context.token_index += 1;
    context.precedence = LOWEST;
    const expr = try expressionAlloc(context);
    std.debug.assert(context.tokens[context.token_index].kind == .right_paren);
    const end = context.tokens[context.token_index].span.end;
    context.token_index += 1;
    return Ast{ .kind = .{ .group = expr }, .span = .{ .begin = begin, .end = end } };
}

fn if_(context: *Context) !Ast {
    const begin = context.tokens[context.token_index].span.begin;
    context.token_index += 1;
    context.precedence = LOWEST;
    const condition = try expressionAlloc(context);
    consume(context, .then);
    const then = try block(context);
    _ = consumeIndent(context);
    consume(context, .else_);
    const else_ = try block(context);
    const end = else_[else_.len - 1].span.end;
    return Ast{
        .kind = .{ .if_ = .{ .condition = condition, .then = then, .else_ = else_ } },
        .span = .{ .begin = begin, .end = end },
    };
}

fn import(context: *Context) !Ast {
    const begin = context.tokens[context.token_index].span.begin;
    context.token_index += 1;
    context.precedence = LOWEST;
    const expr = try expressionAlloc(context);
    const end = expr.span.end;
    return Ast{ .kind = .{ .import = expr }, .span = .{ .begin = begin, .end = end } };
}

fn export_(context: *Context) !Ast {
    const begin = context.tokens[context.token_index].span.begin;
    context.token_index += 1;
    context.precedence = LOWEST;
    const expr = try expressionAlloc(context);
    const end = expr.span.end;
    return Ast{ .kind = .{ .export_ = expr }, .span = .{ .begin = begin, .end = end } };
}

fn prefix(context: *Context) !Ast {
    const token = context.tokens[context.token_index];
    switch (token.kind) {
        .int => return int(context, token),
        .symbol => return symbol(context, token),
        .bool => return boolean(context, token),
        .left_paren => return try group(context),
        .if_ => return try if_(context),
        .import => return try import(context),
        .export_ => return try export_(context),
        else => |kind| std.debug.panic("\nNo prefix parser for {}\n", .{kind}),
    }
}

const Asscociativity = enum {
    left,
    right,
};

fn block(context: *Context) ![]const Ast {
    var exprs = List(Ast).init(context.allocator);
    switch (context.tokens[context.token_index].kind) {
        .indent => |indent| {
            context.token_index += 1;
            context.indent = indent;
            while (true) {
                context.precedence = LOWEST;
                const expr = try expression(context);
                try exprs.append(expr);
                if (context.tokens.len <= context.token_index) break;
                switch (context.tokens[context.token_index].kind) {
                    .indent => |next_indent| {
                        if (!std.meta.eql(indent, next_indent)) break;
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

fn alloc(context: *Context, ast: Ast) !*const Ast {
    const ptr = try context.allocator.create(Ast);
    ptr.* = ast;
    return ptr;
}

fn define(context: *Context, name: Ast) !Ast {
    context.token_index += 1;
    const body = try block(context);
    const span = Span{ .begin = name.span.begin, .end = body[body.len - 1].span.end };
    return Ast{
        .kind = .{ .define = .{
            .name = try alloc(context, name),
            .type = null,
            .body = body,
        } },
        .span = span,
    };
}

fn annotate(context: *Context, name: Ast) !Ast {
    context.token_index += 1;
    context.precedence = ARROW;
    const type_ = try expressionAlloc(context);
    consume(context, .equal);
    context.precedence = LOWEST;
    const body = try block(context);
    const span = Span{ .begin = name.span.begin, .end = body[body.len - 1].span.end };
    return Ast{
        .kind = .{ .define = .{
            .name = try alloc(context, name),
            .type = type_,
            .body = body,
        } },
        .span = span,
    };
}

fn binaryOp(context: *Context, left: Ast, kind: BinaryOpKind) !Ast {
    context.token_index += 1;
    const right = try expression(context);
    const span = Span{ .begin = left.span.begin, .end = right.span.end };
    return Ast{
        .kind = .{ .binary_op = .{
            .kind = kind,
            .left = try alloc(context, left),
            .right = try alloc(context, right),
        } },
        .span = span,
    };
}

const Stage = enum { return_type, body };

fn convertCallToFunction(context: *Context, name: Ast, arguments: []const Ast, stage: Stage) !Ast {
    context.token_index += 1;
    const return_type = blk: {
        if (stage == .return_type) {
            context.precedence = DEFINE + 1;
            const expr = try expressionAlloc(context);
            consume(context, .equal);
            break :blk expr;
        } else break :blk null;
    };
    const body = try block(context);
    const span = Span{
        .begin = name.span.begin,
        .end = body[body.len - 1].span.end,
    };
    const parameters = try context.allocator.alloc(Parameter, arguments.len);
    for (arguments) |argument, i|
        parameters[i] = Parameter{ .name = argument, .type = null };
    return Ast{
        .kind = .{ .function = .{
            .name = try alloc(context, name),
            .parameters = parameters,
            .return_type = return_type,
            .body = body,
        } },
        .span = span,
    };
}

fn function(context: *Context, name: Ast, arguments: []const Ast) !Ast {
    var parameters = try List(Parameter).initCapacity(context.allocator, arguments.len);
    for (arguments) |argument|
        try parameters.append(Parameter{ .name = argument, .type = null });
    context.token_index += 1;
    context.precedence = LOWEST;
    parameters.items[parameters.items.len - 1].type = try expression(context);
    while (context.tokens.len > context.token_index) {
        switch (context.tokens[context.token_index].kind) {
            .right_paren => {
                context.token_index += 1;
                break;
            },
            .comma => context.token_index += 1,
            else => {
                context.precedence = HIGHEST;
                const parameter = try expression(context);
                try parameters.append(Parameter{ .name = parameter, .type = null });
                if (context.tokens[context.token_index].kind == .colon) {
                    context.token_index += 1;
                    context.precedence = LOWEST;
                    parameters.items[parameters.items.len - 1].type = try expression(context);
                }
            },
        }
    }
    const return_type = blk: {
        if (context.tokens[context.token_index].kind == .arrow) {
            context.token_index += 1;
            context.precedence = DEFINE + 1;
            const expr = try expressionAlloc(context);
            break :blk expr;
        } else break :blk null;
    };
    if (!tryConsume(context, .equal)) {
        const span = Span{
            .begin = name.span.begin,
            .end = return_type.?.span.end,
        };
        return Ast{
            .kind = .{ .declaration = .{
                .name = try alloc(context, name),
                .parameters = parameters.toOwnedSlice(),
                .return_type = return_type,
            } },
            .span = span,
        };
    }
    const body = try block(context);
    const span = Span{
        .begin = name.span.begin,
        .end = body[body.len - 1].span.end,
    };
    return Ast{
        .kind = .{ .function = .{
            .name = try alloc(context, name),
            .parameters = parameters.toOwnedSlice(),
            .return_type = return_type,
            .body = body,
        } },
        .span = span,
    };
}

fn callOrFunction(context: *Context, left: Ast) !Ast {
    context.token_index += 1;
    var arguments = List(Ast).init(context.allocator);
    while (context.tokens.len > context.token_index) {
        switch (context.tokens[context.token_index].kind) {
            .right_paren => {
                context.token_index += 1;
                break;
            },
            else => {
                context.precedence = HIGHEST;
                const argument = try expression(context);
                try arguments.append(argument);
                switch (context.tokens[context.token_index].kind) {
                    .comma => context.token_index += 1,
                    .colon => return try function(context, left, arguments.toOwnedSlice()),
                    else => {},
                }
            },
        }
    }
    if (context.tokens.len > context.token_index) {
        switch (context.tokens[context.token_index].kind) {
            .equal => return try convertCallToFunction(context, left, arguments.toOwnedSlice(), .body),
            .arrow => return try convertCallToFunction(context, left, arguments.toOwnedSlice(), .return_type),
            else => {},
        }
    }
    const span = Span{
        .begin = left.span.begin,
        .end = arguments.items[arguments.items.len - 1].span.end,
    };
    return Ast{
        .kind = .{ .call = .{
            .function = try alloc(context, left),
            .arguments = arguments.toOwnedSlice(),
        } },
        .span = span,
    };
}

const Infix = struct {
    precedence: Precedence,
    associativity: Asscociativity,
    kind: union(enum) {
        define,
        annotate,
        call_or_function,
        binary_op: BinaryOpKind,
    },
};

fn infix(context: *Context, left: Ast) ?Infix {
    if (context.tokens.len <= context.token_index) return null;
    switch (context.tokens[context.token_index].kind) {
        .equal => return .{ .kind = .define, .precedence = DEFINE, .associativity = .right },
        .colon => return .{ .kind = .annotate, .precedence = ANNOTATE, .associativity = .right },
        .plus => return .{ .kind = .{ .binary_op = .add }, .precedence = ADD, .associativity = .left },
        .minus => return .{ .kind = .{ .binary_op = .subtract }, .precedence = SUBTRACT, .associativity = .left },
        .times => return .{ .kind = .{ .binary_op = .multiply }, .precedence = MULTIPLY, .associativity = .left },
        .caret => return .{ .kind = .{ .binary_op = .exponentiate }, .precedence = EXPONENTIATE, .associativity = .right },
        .greater => return .{ .kind = .{ .binary_op = .greater }, .precedence = GREATER, .associativity = .left },
        .less => return .{ .kind = .{ .binary_op = .less }, .precedence = LESS, .associativity = .left },
        .left_paren => switch (left.kind) {
            .symbol => return .{ .kind = .call_or_function, .precedence = CALL, .associativity = .left },
            else => return null,
        },
        else => return null,
    }
}

fn parseInfix(parser: Infix, context: *Context, left: Ast) !Ast {
    switch (parser.kind) {
        .define => return try define(context, left),
        .annotate => return try annotate(context, left),
        .binary_op => |kind| return try binaryOp(context, left, kind),
        .call_or_function => return try callOrFunction(context, left),
    }
}

fn expression(context: *Context) error{OutOfMemory}!Ast {
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

fn expressionAlloc(context: *Context) !*const Ast {
    const ast = try context.allocator.create(Ast);
    ast.* = try expression(context);
    return ast;
}

pub fn parse(allocator: Allocator, tokens: []const Token) !Ast {
    var context = Context{
        .allocator = allocator,
        .tokens = tokens,
        .token_index = 0,
        .precedence = LOWEST,
        .indent = Indent{ .space = 0 },
    };
    var top_level = List(Ast).init(allocator);
    while (context.tokens.len > context.token_index) {
        while (consumeIndent(&context)) {}
        if (context.tokens.len <= context.token_index) break;
        const ast = try expression(&context);
        try top_level.append(ast);
    }
    const module = top_level.toOwnedSlice();
    return Ast{
        .kind = .{ .module = module },
        .span = .{
            .begin = module[0].span.begin,
            .end = module[module.len - 1].span.end,
        },
    };
}
