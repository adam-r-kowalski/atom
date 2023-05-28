const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const tokenizer = @import("tokenizer.zig");
const Pos = tokenizer.Pos;
const LeftParen = tokenizer.LeftParen;
const IfToken = tokenizer.If;
const FnToken = tokenizer.Fn;
const Token = tokenizer.Token;
const Tokens = tokenizer.Tokens;
pub const Span = tokenizer.Span;
pub const Int = tokenizer.Int;
pub const Float = tokenizer.Float;
pub const Symbol = tokenizer.Symbol;
pub const String = tokenizer.String;
pub const Bool = tokenizer.Bool;

const Indent = struct {
    value: u64,

    fn increment(self: Indent) Indent {
        return Indent{ .value = self.value + 1 };
    }

    fn toString(self: Indent, writer: anytype) !void {
        try writer.writeAll("\n");
        for (0..self.value) |_| try writer.writeAll("    ");
    }
};

pub const Define = struct {
    name: Symbol,
    type: ?*const Expression,
    value: *const Expression,
    span: Span,

    fn toString(self: Define, writer: anytype, intern: Intern, indent: Indent) !void {
        try writer.writeAll("(def ");
        try writer.writeAll(intern.lookup(self.name.value));
        if (self.type) |t| {
            try writer.writeAll(" ");
            try t.toString(writer, intern, Indent{ .value = 0 });
        }
        try writer.writeAll(" ");
        try self.value.toString(writer, intern, indent.increment());
        try writer.writeAll(")");
    }
};

pub const Parameter = struct {
    name: Symbol,
    type: Expression,

    fn toString(self: Parameter, writer: anytype, intern: Intern) !void {
        try writer.writeAll("(");
        try writer.writeAll(intern.lookup(self.name.value));
        try writer.writeAll(" ");
        try self.type.toString(writer, intern, Indent{ .value = 0 });
        try writer.writeAll(")");
    }
};

pub const Block = struct {
    expressions: []const Expression,
    span: Span,

    fn toString(self: Block, writer: anytype, intern: Intern, indent: Indent) !void {
        try indent.toString(writer);
        if (self.expressions.len == 1) {
            return try self.expressions[0].toString(writer, intern, indent.increment());
        }
        try writer.writeAll("(block");
        for (self.expressions) |expr| {
            try indent.increment().toString(writer);
            try expr.toString(writer, intern, indent.increment());
        }
        try writer.writeAll(")");
    }
};

pub const Function = struct {
    parameters: []const Parameter,
    return_type: *const Expression,
    body: Block,
    span: Span,

    fn toString(self: Function, writer: anytype, intern: Intern, indent: Indent) !void {
        try writer.writeAll("(fn [");
        for (self.parameters, 0..) |p, j| {
            if (j > 0) try writer.writeAll(" ");
            try p.toString(writer, intern);
        }
        try writer.writeAll("] ");
        try self.return_type.toString(writer, intern, indent);
        try self.body.toString(writer, intern, indent);
        try writer.writeAll(")");
    }
};

pub const Prototype = struct {
    parameters: []const Parameter,
    return_type: *const Expression,
    span: Span,

    fn toString(self: Prototype, writer: anytype, intern: Intern) !void {
        try writer.writeAll("(fn [");
        for (self.parameters, 0..) |p, j| {
            if (j > 0) try writer.writeAll(" ");
            try p.toString(writer, intern);
        }
        try writer.writeAll("] ");
        try self.return_type.toString(writer, intern, Indent{ .value = 0 });
        try writer.writeAll(")");
    }
};

pub const BinaryOpKind = enum {
    add,
    subtract,
    multiply,
    divide,
    modulo,
    exponentiate,
    equal,
    greater,
    less,
    or_,
    dot,

    fn toString(self: BinaryOpKind, writer: anytype) !void {
        switch (self) {
            .add => try writer.writeAll("+"),
            .subtract => try writer.writeAll("-"),
            .multiply => try writer.writeAll("*"),
            .divide => try writer.writeAll("/"),
            .modulo => try writer.writeAll("%"),
            .exponentiate => try writer.writeAll("^"),
            .equal => try writer.writeAll("=="),
            .greater => try writer.writeAll(">"),
            .less => try writer.writeAll("<"),
            .or_ => try writer.writeAll("or"),
            .dot => try writer.writeAll("."),
        }
    }
};

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    left: *const Expression,
    right: *const Expression,
    span: Span,

    fn toString(self: BinaryOp, writer: anytype, intern: Intern, indent: Indent) !void {
        try writer.writeAll("(");
        try self.kind.toString(writer);
        try writer.writeAll(" ");
        try self.left.toString(writer, intern, indent);
        try writer.writeAll(" ");
        try self.right.toString(writer, intern, indent);
        try writer.writeAll(")");
    }
};

pub const Group = struct {
    expression: *const Expression,
    span: Span,

    fn toString(self: Group, writer: anytype, intern: Intern, indent: Indent) !void {
        try self.expression.toString(writer, intern, indent);
    }
};

pub const If = struct {
    condition: *const Expression,
    then: Block,
    else_: Block,
    span: Span,

    fn toString(self: If, writer: anytype, intern: Intern, indent: Indent) !void {
        try writer.writeAll("(if ");
        try self.condition.toString(writer, intern, indent);
        try self.then.toString(writer, intern, indent);
        try self.else_.toString(writer, intern, indent);
        try writer.writeAll(")");
    }
};

pub const Cond = struct {
    conditions: []const Expression,
    thens: []const Block,
    else_: Block,
    span: Span,

    fn toString(self: Cond, writer: anytype, intern: Intern, indent: Indent) !void {
        try writer.writeAll("(cond");
        for (self.conditions, self.thens) |b, t| {
            try indent.toString(writer);
            try b.toString(writer, intern, indent);
            try t.toString(writer, intern, indent.increment());
        }
        try indent.toString(writer);
        try writer.writeAll("else");
        try self.else_.toString(writer, intern, indent.increment());
        try writer.writeAll(")");
    }
};

pub const Call = struct {
    function: *const Expression,
    arguments: []const Expression,
    span: Span,

    fn toString(self: Call, writer: anytype, intern: Intern, indent: Indent) !void {
        try writer.writeAll("(");
        try self.function.toString(writer, intern, indent);
        for (self.arguments) |a| {
            try writer.writeAll(" ");
            try a.toString(writer, intern, indent.increment());
        }
        try writer.writeAll(")");
    }
};

pub const Expression = union(enum) {
    int: Int,
    float: Float,
    symbol: Symbol,
    string: String,
    bool: Bool,
    define: Define,
    function: Function,
    prototype: Prototype,
    binary_op: BinaryOp,
    group: Group,
    block: Block,
    if_else: If,
    cond: Cond,
    call: Call,

    pub fn span(self: Expression) Span {
        return switch (self) {
            .int => |e| e.span,
            .float => |e| e.span,
            .symbol => |e| e.span,
            .string => |e| e.span,
            .bool => |e| e.span,
            .define => |e| e.span,
            .function => |e| e.span,
            .prototype => |e| e.span,
            .binary_op => |e| e.span,
            .group => |e| e.span,
            .block => |e| e.span,
            .if_else => |e| e.span,
            .cond => |e| e.span,
            .call => |e| e.span,
        };
    }

    fn toString(self: Expression, writer: anytype, intern: Intern, indent: Indent) error{NoSpaceLeft}!void {
        switch (self) {
            .int => |i| try writer.writeAll(intern.lookup(i.value)),
            .float => |f| try writer.writeAll(intern.lookup(f.value)),
            .symbol => |s| try writer.writeAll(intern.lookup(s.value)),
            .string => |s| try writer.writeAll(intern.lookup(s.value)),
            .bool => |b| try writer.print("{}", .{b.value}),
            .define => |d| try d.toString(writer, intern, indent),
            .function => |f| try f.toString(writer, intern, indent),
            .prototype => |p| try p.toString(writer, intern),
            .binary_op => |b| try b.toString(writer, intern, indent),
            .group => |g| try g.toString(writer, intern, indent),
            .block => |b| try b.toString(writer, intern, indent),
            .if_else => |i| try i.toString(writer, intern, indent),
            .cond => |c| try c.toString(writer, intern, indent),
            .call => |c| try c.toString(writer, intern, indent),
        }
    }
};

pub const Ast = struct {
    expressions: []const Expression,
    intern: Intern,

    pub fn format(
        self: Ast,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        for (self.expressions, 0..) |e, i| {
            if (i > 0) try writer.writeAll("\n\n");
            e.toString(writer, self.intern, Indent{ .value = 0 }) catch unreachable;
        }
    }
};

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
    const token = tokens.next().?;
    if (std.meta.activeTag(token) != tag)
        std.debug.panic("\nExpected token {} found {}", .{ tag, token });
    return token;
}

fn maybeConsume(tokens: *Tokens, tag: std.meta.Tag(Token)) void {
    if (tokens.peek()) |token| {
        if (std.meta.activeTag(token) == tag)
            tokens.advance();
    }
}

fn consumeNewLines(context: Context) void {
    while (context.tokens.peek()) |token| {
        switch (token) {
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
    while (context.tokens.peek()) |token| {
        switch (token) {
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

fn cond(context: Context, if_: IfToken) !Cond {
    const begin = if_.span.begin;
    context.tokens.advance();
    var conditions = List(Expression).init(context.allocator);
    var thens = List(Block).init(context.allocator);
    const lowest = withPrecedence(context, LOWEST);
    while (true) {
        consumeNewLines(context);
        if (context.tokens.peek()) |token| {
            switch (token) {
                .else_ => {
                    context.tokens.advance();
                    break;
                },
                else => {
                    try conditions.append(try expression(lowest));
                    try thens.append(try block(lowest, consume(context.tokens, .left_brace).span().begin));
                },
            }
        } else {
            std.debug.panic("\nExpected else in cond", .{});
        }
    }
    const else_ = try block(lowest, consume(context.tokens, .left_brace).span().begin);
    consumeNewLines(context);
    const end = consume(context.tokens, .right_brace).span().end;
    return Cond{
        .conditions = try conditions.toOwnedSlice(),
        .thens = try thens.toOwnedSlice(),
        .else_ = else_,
        .span = .{ .begin = begin, .end = end },
    };
}

fn ifElse(context: Context, if_: IfToken) !If {
    const begin = if_.span.begin;
    const lowest = withPrecedence(context, LOWEST);
    const condition = try expressionAlloc(lowest);
    const then = try block(lowest, consume(context.tokens, .left_brace).span().begin);
    switch (context.tokens.next().?) {
        .else_ => {
            const else_ = try block(lowest, consume(context.tokens, .left_brace).span().begin);
            const end = else_.span.end;
            return If{
                .condition = condition,
                .then = then,
                .else_ = else_,
                .span = .{ .begin = begin, .end = end },
            };
        },
        else => {
            const else_ = Block{
                .expressions = &.{},
                .span = .{
                    .begin = then.span.begin,
                    .end = then.span.end,
                },
            };
            const end = then.span.end;
            return If{
                .condition = condition,
                .then = then,
                .else_ = else_,
                .span = .{ .begin = begin, .end = end },
            };
        },
    }
}

fn ifElseOrCond(context: Context, if_: IfToken) !Expression {
    if (context.tokens.peek()) |token| {
        return switch (token) {
            .left_brace => .{ .cond = try cond(context, if_) },
            else => .{ .if_else = try ifElse(context, if_) },
        };
    }
    std.debug.panic("\nExpected left brace or expression after if", .{});
}

fn functionParameters(context: Context) ![]const Parameter {
    var parameters = List(Parameter).init(context.allocator);
    while (context.tokens.peek()) |token| {
        switch (token) {
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
    if (context.tokens.peek()) |token| {
        if (token == .left_brace) {
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
        .if_ => |i| return try ifElseOrCond(context, i),
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
    while (context.tokens.peek()) |token| {
        switch (token) {
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
    if (context.tokens.peek()) |token| {
        switch (token) {
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

pub fn parse(allocator: Allocator, tokens: *Tokens) !Ast {
    const context = Context{
        .allocator = allocator,
        .tokens = tokens,
        .precedence = LOWEST,
    };
    var expressions = List(Expression).init(allocator);
    while (tokens.peek()) |token| {
        switch (token) {
            .new_line => context.tokens.advance(),
            else => try expressions.append(try expression(context)),
        }
    }
    return Ast{
        .expressions = try expressions.toOwnedSlice(),
        .intern = tokens.intern,
    };
}
