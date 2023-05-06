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
    function,
    binary_op,
    group,
    if_,
    call,
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

const Function = struct {
    name: Expression,
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

const Call = struct {
    function: Expression,
    arguments: List(Expression),
};

pub const Ast = struct {
    kind: List(Kind),
    span: List(Span),
    index: List(u64),
    int: List(Interned),
    symbol: List(Interned),
    define: List(Define),
    function: List(Function),
    binary_op: List(BinaryOp),
    group: List(Group),
    if_: List(If),
    call: List(Call),
    top_level: List(Expression),

    pub fn deinit(self: Ast) void {
        self.kind.deinit();
        self.span.deinit();
        self.index.deinit();
        self.int.deinit();
        self.symbol.deinit();
        for (self.define.items) |d| d.body.deinit();
        self.define.deinit();
        for (self.function.items) |f| {
            f.parameters.deinit();
            f.body.deinit();
        }
        self.function.deinit();
        self.binary_op.deinit();
        self.group.deinit();
        for (self.if_.items) |i| {
            i.then.deinit();
            i.else_.deinit();
        }
        self.if_.deinit();
        for (self.call.items) |c| c.arguments.deinit();
        self.call.deinit();
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
const CALL = EXPONENTIATE + DELTA;
const ARROW = EXPONENTIATE + DELTA;
const HIGHEST = ARROW + DELTA;

pub const Expression = u64;

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
    try context.ast.binary_op.append(BinaryOp{ .kind = kind, .left = left, .right = right });
    return self;
}

const Stage = enum { return_type, body };

fn convertCallToFunction(context: *Context, left: Expression, arguments: *List(Expression), stage: Stage) !Expression {
    context.token_index += 1;
    const return_type = blk: {
        if (stage == .return_type) {
            context.precedence = DEFINE + 1;
            const expr = try expression(context);
            consume(context, .equal);
            break :blk expr;
        } else break :blk null;
    };
    const body = try block(context);
    const span = Span{
        .begin = context.ast.span.items[left].begin,
        .end = context.ast.span.items[body.items[body.items.len - 1]].end,
    };
    const self = context.ast.kind.items.len;
    try context.ast.kind.append(.function);
    try context.ast.span.append(span);
    try context.ast.index.append(context.ast.function.items.len);
    var parameters = try List(Parameter).initCapacity(context.allocator, arguments.items.len);
    for (arguments.items) |argument| {
        try parameters.append(Parameter{ .name = argument, .type = null });
    }
    arguments.deinit();
    try context.ast.function.append(Function{
        .name = left,
        .parameters = parameters,
        .return_type = return_type,
        .body = body,
    });
    return self;
}

fn function(context: *Context, left: Expression, arguments: *List(Expression)) !Expression {
    var parameters = try List(Parameter).initCapacity(context.allocator, arguments.items.len);
    for (arguments.items) |argument| {
        try parameters.append(Parameter{ .name = argument, .type = null });
    }
    arguments.deinit();
    context.token_index += 1;
    context.precedence = LOWEST;
    parameters.items[parameters.items.len - 1].type = try expression(context);
    while (context.tokens.kind.items.len > context.token_index) {
        switch (context.tokens.kind.items[context.token_index]) {
            .right_paren => {
                context.token_index += 1;
                break;
            },
            .comma => context.token_index += 1,
            else => {
                context.precedence = HIGHEST;
                const parameter = try expression(context);
                try parameters.append(Parameter{ .name = parameter, .type = null });
                if (context.tokens.kind.items[context.token_index] == .colon) {
                    context.token_index += 1;
                    context.precedence = LOWEST;
                    parameters.items[parameters.items.len - 1].type = try expression(context);
                }
            },
        }
    }
    const return_type = blk: {
        if (context.tokens.kind.items[context.token_index] == .arrow) {
            context.token_index += 1;
            context.precedence = DEFINE + 1;
            const expr = try expression(context);
            break :blk expr;
        } else break :blk null;
    };
    consume(context, .equal);
    const body = try block(context);
    const span = Span{
        .begin = context.ast.span.items[left].begin,
        .end = context.ast.span.items[body.items[body.items.len - 1]].end,
    };
    const self = context.ast.kind.items.len;
    try context.ast.kind.append(.function);
    try context.ast.span.append(span);
    try context.ast.index.append(context.ast.function.items.len);
    try context.ast.function.append(Function{
        .name = left,
        .parameters = parameters,
        .return_type = return_type,
        .body = body,
    });
    return self;
}

fn callOrFunction(context: *Context, left: Expression) !Expression {
    context.token_index += 1;
    var arguments = List(Expression).init(context.allocator);
    while (context.tokens.kind.items.len > context.token_index) {
        switch (context.tokens.kind.items[context.token_index]) {
            .right_paren => {
                context.token_index += 1;
                break;
            },
            else => {
                context.precedence = HIGHEST;
                const argument = try expression(context);
                try arguments.append(argument);
                switch (context.tokens.kind.items[context.token_index]) {
                    .comma => context.token_index += 1,
                    .colon => return try function(context, left, &arguments),
                    else => {},
                }
            },
        }
    }
    if (context.tokens.kind.items.len > context.token_index) {
        switch (context.tokens.kind.items[context.token_index]) {
            .equal => return try convertCallToFunction(context, left, &arguments, .body),
            .arrow => return try convertCallToFunction(context, left, &arguments, .return_type),
            else => {},
        }
    }
    const span = Span{
        .begin = context.ast.span.items[left].begin,
        .end = context.ast.span.items[arguments.items[arguments.items.len - 1]].end,
    };
    const self = context.ast.kind.items.len;
    try context.ast.kind.append(.call);
    try context.ast.span.append(span);
    try context.ast.index.append(context.ast.call.items.len);
    try context.ast.call.append(Call{ .function = left, .arguments = arguments });
    return self;
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

fn infix(context: *Context, left: Expression) ?Infix {
    if (context.tokens.kind.items.len <= context.token_index) return null;
    switch (context.tokens.kind.items[context.token_index]) {
        .equal => return .{ .kind = .define, .precedence = DEFINE, .associativity = .right },
        .colon => return .{ .kind = .annotate, .precedence = ANNOTATE, .associativity = .right },
        .plus => return .{ .kind = .{ .binary_op = .add }, .precedence = ADD, .associativity = .left },
        .minus => return .{ .kind = .{ .binary_op = .subtract }, .precedence = SUBTRACT, .associativity = .left },
        .times => return .{ .kind = .{ .binary_op = .multiply }, .precedence = MULTIPLY, .associativity = .left },
        .caret => return .{ .kind = .{ .binary_op = .exponentiate }, .precedence = EXPONENTIATE, .associativity = .right },
        .greater => return .{ .kind = .{ .binary_op = .greater }, .precedence = GREATER, .associativity = .left },
        .less => return .{ .kind = .{ .binary_op = .less }, .precedence = LESS, .associativity = .left },
        .left_paren => switch (context.tokens.kind.items[left]) {
            .symbol => return .{ .kind = .call_or_function, .precedence = CALL, .associativity = .left },
            else => return null,
        },
        else => return null,
    }
}

fn parseInfix(parser: Infix, context: *Context, left: Expression) !Expression {
    switch (parser.kind) {
        .define => return try define(context, left),
        .annotate => return try annotate(context, left),
        .binary_op => |kind| return try binaryOp(context, left, kind),
        .call_or_function => return try callOrFunction(context, left),
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
        .function = List(Function).init(allocator),
        .binary_op = List(BinaryOp).init(allocator),
        .group = List(Group).init(allocator),
        .if_ = List(If).init(allocator),
        .call = List(Call).init(allocator),
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
    try writer.writeAll(interner.lookup(intern, s));
}

fn symbolToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression) !void {
    const s = ast.symbol.items[ast.index.items[expr]];
    try writer.writeAll(interner.lookup(intern, s));
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
    try writer.writeAll("\n");
    var i: u64 = 0;
    while (i < indent) {
        try writer.writeAll("  ");
        i += 1;
    }
}

fn blockToString(writer: List(u8).Writer, intern: Intern, ast: Ast, exprs: List(Expression), indent: u64, new_line: bool) !void {
    if (exprs.items.len == 1) {
        if (new_line) {
            try indentToString(writer, indent);
        } else {
            try writer.writeAll(" ");
        }
        try expressionToString(writer, intern, ast, exprs.items[0], indent);
        return;
    }
    try indentToString(writer, indent);
    try writer.writeAll("(block");
    for (exprs.items) |expr| {
        try indentToString(writer, indent + 1);
        try expressionToString(writer, intern, ast, expr, indent + 1);
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

fn functionToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression, indent: u64) !void {
    const f = ast.function.items[ast.index.items[expr]];
    try writer.writeAll("(defn ");
    try symbolToString(writer, intern, ast, f.name);
    try writer.writeAll(" [");
    for (f.parameters.items) |p, i| {
        try parameterToString(writer, intern, ast, p);
        if (i < f.parameters.items.len - 1) try writer.writeAll(" ");
    }
    try writer.writeAll("]");
    if (f.return_type) |t| {
        try writer.writeAll(" ");
        try typeToString(writer, intern, ast, t);
    }
    try blockToString(writer, intern, ast, f.body, indent + 1, false);
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

fn callToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression, indent: u64) !void {
    const c = ast.call.items[ast.index.items[expr]];
    try writer.writeAll("(");
    try expressionToString(writer, intern, ast, c.function, indent);
    for (c.arguments.items) |a| {
        try writer.writeAll(" ");
        try expressionToString(writer, intern, ast, a, indent);
    }
    try writer.writeAll(")");
}

fn expressionToString(writer: List(u8).Writer, intern: Intern, ast: Ast, expr: Expression, indent: u64) error{OutOfMemory}!void {
    switch (ast.kind.items[expr]) {
        .int => try intToString(writer, intern, ast, expr),
        .symbol => try symbolToString(writer, intern, ast, expr),
        .define => try defineToString(writer, intern, ast, expr, indent),
        .function => try functionToString(writer, intern, ast, expr, indent),
        .binary_op => try binaryOpToString(writer, intern, ast, expr, indent),
        .group => try groupToString(writer, intern, ast, expr, indent),
        .if_ => try ifToString(writer, intern, ast, expr, indent),
        .call => try callToString(writer, intern, ast, expr, indent),
    }
}

pub fn toString(allocator: Allocator, intern: Intern, ast: Ast) ![]u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    const indent: u64 = 0;
    for (ast.top_level.items) |expr| try expressionToString(writer, intern, ast, expr, indent);
    return list.toOwnedSlice();
}
