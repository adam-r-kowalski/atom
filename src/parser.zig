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
    int,
    symbol,
    define,
    lambda,
    binary_op,
    group,
    if_,
};

const Define = struct {
    name: Expression,
    type: ?Expression,
    body: List(Expression),
};

const Parameter = struct {
    name: Expression,
    type: ?Expression,
};

const Lambda = struct {
    parameters: List(Parameter),
    return_type: ?Expression,
    body: List(Expression),
};

const BinaryOpKind = enum {
    add,
    subtract,
    multiply,
    exponentiate,
    greater,
    less,
    arrow,
};

const BinaryOp = struct {
    kind: BinaryOpKind,
    left: Expression,
    right: Expression,
};

const Group = Expression;

const If = struct {
    condition: Expression,
    then: List(Expression),
    else_: List(Expression),
};

pub const Ast = struct {
    kind: List(Kind),
    span: List(Span),
    index: List(u64),
    int: List(Interned),
    symbol: List(Interned),
    define: List(Define),
    lambda: List(Lambda),
    binary_op: List(BinaryOp),
    group: List(Group),
    if_: List(If),
    top_level: List(Expression),

    pub fn deinit(self: Ast) void {
        self.kind.deinit();
        self.span.deinit();
        self.index.deinit();
        self.int.deinit();
        self.symbol.deinit();
        for (self.define.items) |d| d.body.deinit();
        self.define.deinit();
        for (self.lambda.items) |l| {
            l.parameters.deinit();
            l.body.deinit();
        }
        self.lambda.deinit();
        self.binary_op.deinit();
        self.group.deinit();
        for (self.if_.items) |i| {
            i.then.deinit();
            i.else_.deinit();
        }
        self.if_.deinit();
        self.top_level.deinit();
    }
};

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
const ARROW = EXPONENTIATE + DELTA;
const HIGHEST = ARROW + DELTA;

const Expression = u64;

const Context = struct {
    allocator: Allocator,
    tokens: Tokens,
    ast: *Ast,
    token_index: u64,
    precedence: Precedence,
    indent: Indent,
};

fn int(context: *Context, s: Interned) !Expression {
    const self = context.ast.kind.items.len;
    const span = context.tokens.span.items[context.token_index];
    try context.ast.kind.append(.int);
    try context.ast.span.append(span);
    try context.ast.index.append(context.ast.int.items.len);
    try context.ast.int.append(s);
    context.token_index += 1;
    return self;
}

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

fn consume(context: *Context, kind: tokenizer.Kind) void {
    std.debug.assert(std.meta.activeTag(context.tokens.kind.items[context.token_index]) == kind);
    context.token_index += 1;
}

fn consumeIndent(context: *Context) void {
    switch (context.tokens.kind.items[context.token_index]) {
        .indent => context.token_index += 1,
        else => {},
    }
}

fn consumeSymbol(context: *Context) !Expression {
    switch (context.tokens.kind.items[context.token_index]) {
        .symbol => |s| return try symbol(context, s),
        else => |kind| std.debug.panic("\nExpected symbol, got {}\n", .{kind}),
    }
}

fn lambda(context: *Context) !Expression {
    const begin = context.tokens.span.items[context.token_index].begin;
    context.token_index += 1;
    var parameters = List(Parameter).init(context.allocator);
    var return_type: ?Expression = null;
    while (true) {
        switch (context.tokens.kind.items[context.token_index]) {
            .symbol => |s| {
                const name = try symbol(context, s);
                try parameters.append(.{ .name = name, .type = null });
            },
            .left_paren => {
                context.token_index += 1;
                const name = try consumeSymbol(context);
                consume(context, .colon);
                context.precedence = LOWEST;
                const type_ = try expression(context);
                consume(context, .right_paren);
                try parameters.append(.{ .name = name, .type = type_ });
            },
            .dot => {
                context.token_index += 1;
                break;
            },
            .arrow => {
                context.token_index += 1;
                return_type = try expression(context);
            },
            else => |kind| std.debug.panic("\nExpected symbol, got {}\n", .{kind}),
        }
    }
    const body = try block(context);
    const self = context.ast.kind.items.len;
    const end = context.ast.span.items[body.items[body.items.len - 1]].end;
    try context.ast.kind.append(.lambda);
    try context.ast.span.append(.{ .begin = begin, .end = end });
    try context.ast.index.append(context.ast.lambda.items.len);
    try context.ast.lambda.append(.{
        .parameters = parameters,
        .return_type = return_type,
        .body = body,
    });
    return self;
}

fn group(context: *Context) !Expression {
    const begin = context.tokens.span.items[context.token_index].begin;
    context.token_index += 1;
    context.precedence = LOWEST;
    const expr = try expression(context);
    std.debug.assert(context.tokens.kind.items[context.token_index] == .right_paren);
    const end = context.tokens.span.items[context.token_index].end;
    context.token_index += 1;
    const self = context.ast.kind.items.len;
    try context.ast.kind.append(.group);
    try context.ast.span.append(.{ .begin = begin, .end = end });
    try context.ast.index.append(context.ast.group.items.len);
    try context.ast.group.append(expr);
    return self;
}

fn if_(context: *Context) !Expression {
    const begin = context.tokens.span.items[context.token_index].begin;
    context.token_index += 1;
    context.precedence = LOWEST;
    const condition = try expression(context);
    consume(context, .then);
    const then = try block(context);
    consumeIndent(context);
    consume(context, .else_);
    const else_ = try block(context);
    const end = context.tokens.span.items[else_.items[else_.items.len - 1]].end;
    const self = context.ast.kind.items.len;
    try context.ast.kind.append(.if_);
    try context.ast.span.append(.{ .begin = begin, .end = end });
    try context.ast.index.append(context.ast.if_.items.len);
    try context.ast.if_.append(If{ .condition = condition, .then = then, .else_ = else_ });
    return self;
}

fn prefix(context: *Context) !Expression {
    switch (context.tokens.kind.items[context.token_index]) {
        .int => |s| return try int(context, s),
        .symbol => |s| return try symbol(context, s),
        .backslash => return try lambda(context),
        .left_paren => return try group(context),
        .if_ => return try if_(context),
        else => |kind| std.debug.panic("\nNo prefix parser for {}\n", .{kind}),
    }
}

const Asscociativity = enum {
    left,
    right,
};

fn block(context: *Context) !List(Expression) {
    var exprs = List(Expression).init(context.allocator);
    switch (context.tokens.kind.items[context.token_index]) {
        .indent => |indent| {
            context.token_index += 1;
            context.indent = indent;
            while (true) {
                context.precedence = LOWEST;
                const expr = try expression(context);
                try exprs.append(expr);
                if (context.tokens.kind.items.len <= context.token_index) break;
                switch (context.tokens.kind.items[context.token_index]) {
                    .indent => |next_indent| {
                        if (!std.meta.eql(context.indent, next_indent)) break;
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
    return exprs;
}

fn define(context: *Context, name: Expression) !Expression {
    context.token_index += 1;
    const body = try block(context);
    const span = Span{
        .begin = context.ast.span.items[name].begin,
        .end = context.ast.span.items[body.items[body.items.len - 1]].end,
    };
    const self = context.ast.kind.items.len;
    try context.ast.kind.append(.define);
    try context.ast.span.append(span);
    try context.ast.index.append(context.ast.define.items.len);
    try context.ast.define.append(.{ .name = name, .type = null, .body = body });
    return self;
}

fn annotate(context: *Context, name: Expression) !Expression {
    context.token_index += 1;
    context.precedence = ARROW;
    const type_ = try expression(context);
    consume(context, .equal);
    context.precedence = LOWEST;
    const body = try block(context);
    const span = Span{
        .begin = context.ast.span.items[name].begin,
        .end = context.ast.span.items[body.items[body.items.len - 1]].end,
    };
    const self = context.ast.kind.items.len;
    try context.ast.kind.append(.define);
    try context.ast.span.append(span);
    try context.ast.index.append(context.ast.define.items.len);
    try context.ast.define.append(.{ .name = name, .type = type_, .body = body });
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
        .minus => return .{ .kind = .{ .binary_op = .subtract }, .precedence = SUBTRACT, .asscociativity = .left },
        .times => return .{ .kind = .{ .binary_op = .multiply }, .precedence = MULTIPLY, .asscociativity = .left },
        .caret => return .{ .kind = .{ .binary_op = .exponentiate }, .precedence = EXPONENTIATE, .asscociativity = .right },
        .greater => return .{ .kind = .{ .binary_op = .greater }, .precedence = GREATER, .asscociativity = .left },
        .less => return .{ .kind = .{ .binary_op = .less }, .precedence = LESS, .asscociativity = .left },
        .arrow => return .{ .kind = .{ .binary_op = .arrow }, .precedence = ARROW, .asscociativity = .right },
        else => return null,
    }
}

fn expression(context: *Context) error{OutOfMemory}!Expression {
    var left = try prefix(context);
    const previous = context.precedence;
    while (true) {
        if (infix(context)) |parser| {
            var next = parser.precedence;
            if (context.precedence > next) return left;
            if (parser.asscociativity == .left) next += 1;
            context.precedence = next;
            left = try parser.parse(context, left);
            context.precedence = previous;
        } else return left;
    }
}

pub fn parse(allocator: Allocator, tokens: Tokens) !Ast {
    var ast = Ast{
        .kind = List(Kind).init(allocator),
        .span = List(Span).init(allocator),
        .index = List(u64).init(allocator),
        .int = List(Interned).init(allocator),
        .symbol = List(Interned).init(allocator),
        .define = List(Define).init(allocator),
        .lambda = List(Lambda).init(allocator),
        .binary_op = List(BinaryOp).init(allocator),
        .group = List(Group).init(allocator),
        .if_ = List(If).init(allocator),
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

fn intToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression) !void {
    const s = ast.int.items[ast.index.items[expr]];
    try writer.writeAll(intern.lookup(s));
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
            try typeToString(writer, intern, ast, b.right);
            try writer.writeAll(")");
        },
        .symbol => try symbolToString(writer, intern, ast, expr),
        else => std.debug.panic("\ncannot convert type to string {}\n", .{ast.kind.items[expr]}),
    }
}

fn indentToString(writer: List(u8).Writer, indent: u64) !void {
    var i: u64 = 0;
    while (i < indent) {
        try writer.writeAll("    ");
        i += 1;
    }
}

fn blockToString(writer: List(u8).Writer, intern: Intern, ast: Ast, exprs: List(Expression), indent: u64, new_line: bool) !void {
    if (exprs.items.len == 1) {
        if (new_line) {
            try writer.writeAll("\n");
            try indentToString(writer, indent);
        } else {
            try writer.writeAll(" ");
        }
        try expressionToString(writer, intern, ast, exprs.items[0], indent);
        return;
    }
    try writer.writeAll("\n");
    try indentToString(writer, indent);
    try writer.writeAll("(block");
    for (exprs.items) |expr| {
        try writer.writeAll("\n");
        try indentToString(writer, indent + 1);
        try expressionToString(writer, intern, ast, expr, indent);
    }
    try writer.writeAll(")");
}

fn defineToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression, indent: u64) !void {
    const d = ast.define.items[ast.index.items[expr]];
    try writer.writeAll("(def ");
    try symbolToString(writer, intern, ast, d.name);
    if (d.type) |t| {
        try writer.writeAll(" ");
        try typeToString(writer, intern, ast, t);
    }
    try blockToString(writer, intern, ast, d.body, indent + 1, false);
    try writer.writeAll(")");
}

fn parameterToString(writer: List(u8).Writer, intern: Intern, ast: Ast, p: Parameter) !void {
    if (p.type) |t| {
        try writer.writeAll("(");
        try symbolToString(writer, intern, ast, p.name);
        try writer.writeAll(" ");
        try typeToString(writer, intern, ast, t);
        try writer.writeAll(")");
    } else {
        try symbolToString(writer, intern, ast, p.name);
    }
}

fn lambdaToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression, indent: u64) !void {
    const l = ast.lambda.items[ast.index.items[expr]];
    try writer.writeAll("(fn [");
    for (l.parameters.items) |p, i| {
        try parameterToString(writer, intern, ast, p);
        if (i < l.parameters.items.len - 1) try writer.writeAll(" ");
    }
    try writer.writeAll("]");
    if (l.return_type) |t| {
        try writer.writeAll(" ");
        try typeToString(writer, intern, ast, t);
    }
    try blockToString(writer, intern, ast, l.body, indent, false);
    try writer.writeAll(")");
}

fn binaryOpToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression, indent: u64) !void {
    const b = ast.binary_op.items[ast.index.items[expr]];
    try writer.writeAll("(");
    switch (b.kind) {
        .add => try writer.writeAll("+"),
        .subtract => try writer.writeAll("-"),
        .multiply => try writer.writeAll("*"),
        .exponentiate => try writer.writeAll("^"),
        .greater => try writer.writeAll(">"),
        .less => try writer.writeAll("<"),
        .arrow => try writer.writeAll("->"),
    }
    try writer.writeAll(" ");
    try expressionToString(writer, intern, ast, b.left, indent);
    try writer.writeAll(" ");
    try expressionToString(writer, intern, ast, b.right, indent);
    try writer.writeAll(")");
}

fn groupToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression, indent: u64) !void {
    const g = ast.group.items[ast.index.items[expr]];
    return try expressionToString(writer, intern, ast, g, indent);
}

fn ifToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression, indent: u64) !void {
    const i = ast.if_.items[ast.index.items[expr]];
    try writer.writeAll("(if ");
    try expressionToString(writer, intern, ast, i.condition, indent);
    try blockToString(writer, intern, ast, i.then, indent + 1, false);
    try blockToString(writer, intern, ast, i.else_, indent + 1, i.then.items.len > 1);
    try writer.writeAll(")");
}

fn expressionToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression, indent: u64) error{OutOfMemory}!void {
    switch (ast.kind.items[expr]) {
        .int => try intToString(writer, intern, ast, expr),
        .symbol => try symbolToString(writer, intern, ast, expr),
        .define => try defineToString(writer, intern, ast, expr, indent),
        .lambda => try lambdaToString(writer, intern, ast, expr, indent),
        .binary_op => try binaryOpToString(writer, intern, ast, expr, indent),
        .group => try groupToString(writer, intern, ast, expr, indent),
        .if_ => try ifToString(writer, intern, ast, expr, indent),
    }
}

pub fn toString(allocator: Allocator, intern: Intern, ast: Ast) ![]u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    const indent: u64 = 0;
    for (ast.top_level.items) |expr| try expressionToString(writer, intern, ast, expr, indent);
    return list.toOwnedSlice();
}
