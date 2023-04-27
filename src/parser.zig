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
    lambda,
    binary_op,
};

const Define = struct {
    name: Expression,
    type: ?Expression,
    value: Expression,
};

const Lambda = struct {
    parameters: List(Expression),
    body: Expression,
};

const BinaryOpKind = enum {
    add,
    arrow,
};

const BinaryOp = struct {
    kind: BinaryOpKind,
    left: Expression,
    right: Expression,
};

pub const Ast = struct {
    kind: List(Kind),
    span: List(Span),
    index: List(u64),
    symbol: List(Interned),
    define: List(Define),
    lambda: List(Lambda),
    binary_op: List(BinaryOp),
    top_level: List(Expression),

    pub fn deinit(self: Ast) void {
        self.kind.deinit();
        self.span.deinit();
        self.index.deinit();
        self.symbol.deinit();
        self.define.deinit();
        for (self.lambda.items) |l| l.parameters.deinit();
        self.lambda.deinit();
        self.binary_op.deinit();
        self.top_level.deinit();
    }
};

const Precedence = u32;

const DELTA = 10;
const LOWEST = 0;
const DEFINE = LOWEST + DELTA;
const ANNOTATE = DEFINE;
const ADD = ANNOTATE + DELTA;
const ARROW = ADD + DELTA;
const HIGHEST = ADD + DELTA;

const Expression = u64;

const Context = struct {
    allocator: Allocator,
    tokens: Tokens,
    ast: *Ast,
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
    context.precedence = LOWEST;
    std.debug.assert(context.tokens.kind.items[context.token_index] == .dot);
    context.token_index += 1;
    const body = try expression(context);
    const self = context.ast.kind.items.len;
    const end = context.ast.span.items[body].end;
    try context.ast.kind.append(.lambda);
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
    try context.ast.define.append(.{ .name = name, .type = null, .value = value });
    return self;
}

fn annotate(context: *Context, name: Expression) !Expression {
    context.token_index += 1;
    context.precedence = ARROW;
    const type_ = try expression(context);
    std.debug.assert(context.tokens.kind.items[context.token_index] == .equal);
    context.token_index += 1;
    context.precedence = LOWEST;
    const value = try expression(context);
    const span = Span{
        .begin = context.ast.span.items[name].begin,
        .end = context.ast.span.items[value].end,
    };
    const self = context.ast.kind.items.len;
    try context.ast.kind.append(.define);
    try context.ast.span.append(span);
    try context.ast.index.append(context.ast.define.items.len);
    try context.ast.define.append(.{ .name = name, .type = type_, .value = value });
    return self;
}

fn binaryOp(context: *Context, left: Expression, kind: BinaryOpKind) !Expression {
    context.token_index += 1;
    const right = try expression(context);
    const span = Span{
        .begin = context.ast.span.items[left].begin,
        .end = context.ast.span.items[right].end,
    };
    const self = context.ast.kind.items.len;
    try context.ast.kind.append(.binary_op);
    try context.ast.span.append(span);
    try context.ast.index.append(context.ast.binary_op.items.len);
    try context.ast.binary_op.append(.{ .kind = kind, .left = left, .right = right });
    return self;
}

const Infix = struct {
    precedence: Precedence,
    asscociativity: Asscociativity,
    kind: union(enum) {
        define,
        annotate,
        binary_op: BinaryOpKind,
    },

    fn parse(self: Infix, context: *Context, left: Expression) !Expression {
        switch (self.kind) {
            .define => return try define(context, left),
            .annotate => return try annotate(context, left),
            .binary_op => |kind| return try binaryOp(context, left, kind),
        }
    }
};

fn infix(context: *Context) ?Infix {
    if (context.tokens.kind.items.len <= context.token_index) return null;
    switch (context.tokens.kind.items[context.token_index]) {
        .equal => return .{ .kind = .define, .precedence = DEFINE, .asscociativity = .right },
        .colon => return .{ .kind = .annotate, .precedence = ANNOTATE, .asscociativity = .right },
        .plus => return .{ .kind = .{ .binary_op = .add }, .precedence = ADD, .asscociativity = .left },
        .arrow => return .{ .kind = .{ .binary_op = .arrow }, .precedence = ARROW, .asscociativity = .right },
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
            const last = context.precedence;
            context.precedence = next;
            left = try parser.parse(context, left);
            context.precedence = last;
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
        .binary_op = List(BinaryOp).init(allocator),
        .top_level = List(Expression).init(allocator),
    };
    var context = Context{
        .allocator = allocator,
        .tokens = tokens,
        .ast = &ast,
        .token_index = 0,
        .precedence = LOWEST,
        .indent = Indent{ .space = 0 },
    };
    const expr = try expression(&context);
    try context.ast.top_level.append(expr);
    return ast;
}

fn symbolToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression) !void {
    const s = ast.symbol.items[ast.index.items[expr]];
    try writer.writeAll(intern.lookup(s));
}

fn typeToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression) !void {
    switch (ast.kind.items[expr]) {
        .binary_op => {
            const b = ast.binary_op.items[ast.index.items[expr]];
            std.debug.assert(b.kind == .arrow);
            try writer.writeAll("(-> ");
            try typeToString(writer, intern, ast, b.left);
            try writer.writeAll(" ");
            try typeToString(writer, intern, ast, b.left);
            try writer.writeAll(")");
        },
        .symbol => try symbolToString(writer, intern, ast, expr),
        else => std.debug.panic("\ncannot convert type to string {}\n", .{ast.kind.items[expr]}),
    }
}

fn defineToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression) !void {
    const d = ast.define.items[ast.index.items[expr]];
    try writer.writeAll("(def ");
    if (d.type) |t| {
        try writer.writeAll("(: ");
        try symbolToString(writer, intern, ast, d.name);
        try writer.writeAll(" ");
        try typeToString(writer, intern, ast, t);
        try writer.writeAll(")");
    } else {
        try symbolToString(writer, intern, ast, d.name);
    }
    try writer.writeAll(" ");
    try expressionToString(writer, intern, ast, d.value);
    try writer.writeAll(")");
}

fn lambdaToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression) !void {
    const l = ast.lambda.items[ast.index.items[expr]];
    try writer.writeAll("(fn (");
    for (l.parameters.items) |p, i| {
        try expressionToString(writer, intern, ast, p);
        if (i < l.parameters.items.len - 1) try writer.writeAll(" ");
    }
    try writer.writeAll(") ");
    try expressionToString(writer, intern, ast, l.body);
    try writer.writeAll(")");
}

fn binaryOpToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression) !void {
    const b = ast.binary_op.items[ast.index.items[expr]];
    try writer.writeAll("(");
    switch (b.kind) {
        .add => try writer.writeAll("+"),
        .arrow => try writer.writeAll("->"),
    }
    try writer.writeAll(" ");
    try expressionToString(writer, intern, ast, b.left);
    try writer.writeAll(" ");
    try expressionToString(writer, intern, ast, b.right);
    try writer.writeAll(")");
}

fn expressionToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression) error{OutOfMemory}!void {
    switch (ast.kind.items[expr]) {
        .symbol => try symbolToString(writer, intern, ast, expr),
        .define => try defineToString(writer, intern, ast, expr),
        .lambda => try lambdaToString(writer, intern, ast, expr),
        .binary_op => try binaryOpToString(writer, intern, ast, expr),
    }
}

pub fn toString(allocator: Allocator, intern: Intern, ast: Ast) ![]u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    for (ast.top_level.items) |expr| try expressionToString(writer, intern, ast, expr);
    return list.toOwnedSlice();
}
