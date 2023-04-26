const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const tokenizer = @import("tokenizer.zig");
const Tokens = tokenizer.Tokens;
const Span = tokenizer.Span;
const Indent = tokenizer.Indent;

const Kind = enum {
    symbol,
    define,
};

const Define = struct {
    name: Expression,
    value: Expression,
};

const Lambda = struct {
    parameters: List(Expression),
    body: Expression,
};

pub const Ast = struct {
    kind: List(Kind),
    span: List(Span),
    index: List(u64),
    symbol: List(Interned),
    define: List(Define),
    lambda: List(Lambda),
    top_level: List(Expression),

    pub fn deinit(self: Ast) void {
        self.kind.deinit();
        self.span.deinit();
        self.index.deinit();
        self.symbol.deinit();
        self.define.deinit();
        self.top_level.deinit();
    }
};

const Precedence = u32;

const LOWEST = 0;
const DEFINE = LOWEST + 10;
const HIGHEST = DEFINE + 10;

const Expression = u64;

const Context = struct {
    allocator: Allocator,
    tokens: Tokens,
    ast: Ast,
    token_index: u64,
    precedence: Precedence,
    indent: Indent,
};

fn symbol(context: *Context, s: Interned) !Expression {
    const self = context.ast.kind.items.len;
    const span = context.tokens.span.items[context.token_index];
    try context.ast.kind.append(.symbol);
    try context.ast.span.append(span);
    try context.ast.index.append(context.ast.symbol.items.len);
    try context.ast.symbol.append(s);
    context.token_index += 1;
    return self;
}

fn lambda(context: *Context) !Expression {
    const begin = context.tokens.span.items[context.token_index].begin;
    context.token_index += 1;
    var params = List(Expression).init(context.allocator);
    context.precedence = HIGHEST;
    while (context.tokens.kind.items[context.token_index] != .dot) {
        const param = try expression(context);
        try params.append(param);
    }
    std.debug.assert(context.tokens.kind.items[context.token_index] == .dot);
    context.token_index += 1;
    const body = try expression(context);
    const self = context.ast.kind.items.len;
    const end = context.ast.span.items[body].end;
    try context.ast.kind.append(.symbol);
    try context.ast.span.append(.{ .begin = begin, .end = end });
    try context.ast.index.append(context.ast.lambda.items.len);
    try context.ast.lambda.append(.{ .parameters = params, .body = body });
    return self;
}

fn prefix(context: *Context) !Expression {
    switch (context.tokens.kind.items[context.token_index]) {
        .symbol => |s| return try symbol(context, s),
        .backslash => return try lambda(context),
        else => |kind| std.debug.panic("\nNo prefix parser for {}\n", .{kind}),
    }
}

const Asscociativity = enum {
    left,
    right,
};

fn define(context: *Context, name: Expression) !Expression {
    context.token_index += 1;
    const value = try expression(context);
    const span = Span{
        .begin = context.ast.span.items[name].begin,
        .end = context.ast.span.items[value].end,
    };
    const self = context.ast.kind.items.len;
    try context.ast.kind.append(.define);
    try context.ast.span.append(span);
    try context.ast.index.append(context.ast.define.items.len);
    try context.ast.define.append(.{ .name = name, .value = value });
    return self;
}

const Infix = struct {
    precedence: Precedence,
    asscociativity: Asscociativity,
    kind: enum {
        define,
    },

    fn parse(self: Infix, context: *Context, left: Expression) !Expression {
        switch (self.kind) {
            .define => return try define(context, left),
        }
    }
};

fn infix(context: *Context) ?Infix {
    switch (context.tokens.kind.items[context.token_index]) {
        .equal => return .{ .kind = .define, .precedence = 1, .asscociativity = .right },
        else => return null,
    }
}

fn expression(context: *Context) error{OutOfMemory}!Expression {
    var left = try prefix(context);
    while (true) {
        if (infix(context)) |parser| {
            var next = parser.precedence;
            if (context.precedence > next) return left;
            if (parser.asscociativity == .left) next += 1;
            context.precedence = next;
            left = try parser.parse(context, left);
        } else return left;
    }
}

pub fn parse(allocator: Allocator, tokens: Tokens) !Ast {
    var ast = Ast{
        .kind = List(Kind).init(allocator),
        .span = List(Span).init(allocator),
        .index = List(u64).init(allocator),
        .symbol = List(Interned).init(allocator),
        .define = List(Define).init(allocator),
        .lambda = List(Lambda).init(allocator),
        .top_level = List(Expression).init(allocator),
    };
    var context = Context{
        .allocator = allocator,
        .tokens = tokens,
        .ast = ast,
        .token_index = 0,
        .precedence = LOWEST,
        .indent = Indent{ .space = 0 },
    };
    const expr = try expression(&context);
    try context.ast.top_level.append(expr);
    return ast;
}

pub fn toString(_: Allocator, _: Intern, _: Ast) ![]u8 {
    return "";
}
