const std = @import("std");
const List = std.ArrayList;
const Allocator = std.mem.Allocator;

const interner = @import("../interner.zig");
const Interned = interner.Interned;
const Intern = interner.Intern;
const types = @import("types.zig");
const Ast = types.Ast;
const Define = types.Define;
const Function = types.Function;
const Declaration = types.Declaration;
const BinaryOp = types.BinaryOp;
const If = types.If;
const Parameter = types.Parameter;
const Block = types.Block;
const Call = types.Call;
const Module = types.Module;

fn interned(writer: List(u8).Writer, intern: Intern, s: Interned) !void {
    try writer.writeAll(interner.lookup(intern, s));
}

fn type_(writer: List(u8).Writer, intern: Intern, ast: Ast) !void {
    switch (ast.kind) {
        .binary_op => |b| {
            std.debug.assert(b.kind == .arrow);
            try writer.writeAll("(-> ");
            try type_(writer, intern, b.left.*);
            try writer.writeAll(" ");
            try type_(writer, intern, b.right.*);
            try writer.writeAll(")");
        },
        .symbol => |s| try interned(writer, intern, s),
        else => std.debug.panic("\ncannot convert type to string {}\n", .{ast}),
    }
}

fn newlineAndIndent(writer: List(u8).Writer, indent: u64) !void {
    try writer.writeAll("\n");
    var i: u64 = 0;
    while (i < indent) {
        try writer.writeAll("    ");
        i += 1;
    }
}

fn block(writer: List(u8).Writer, intern: Intern, exprs: []const Ast, indent: u64, new_line: bool) !void {
    if (exprs.len == 1) {
        if (new_line) {
            try newlineAndIndent(writer, indent);
        } else {
            try writer.writeAll(" ");
        }
        return try expression(writer, intern, exprs[0], indent);
    }
    try newlineAndIndent(writer, indent);
    try writer.writeAll("(block");
    for (exprs) |ast| {
        try newlineAndIndent(writer, indent + 1);
        try expression(writer, intern, ast, indent + 1);
    }
    try writer.writeAll(")");
}

fn define(writer: List(u8).Writer, intern: Intern, d: Define, indent: u64) !void {
    try writer.writeAll("(def ");
    try interned(writer, intern, d.name.kind.symbol);
    if (d.type) |t| {
        try writer.writeAll(" ");
        try type_(writer, intern, t.*);
    }
    try block(writer, intern, d.body, indent + 1, false);
    try writer.writeAll(")");
}

fn parameter(writer: List(u8).Writer, intern: Intern, p: Parameter) !void {
    if (p.type) |t| {
        try writer.writeAll("(");
        try interned(writer, intern, p.name.kind.symbol);
        try writer.writeAll(" ");
        try type_(writer, intern, t);
        try writer.writeAll(")");
    } else {
        try interned(writer, intern, p.name.kind.symbol);
    }
}

fn function(writer: List(u8).Writer, intern: Intern, f: Function, indent: u64) !void {
    try writer.writeAll("(defn ");
    try interned(writer, intern, f.name.kind.symbol);
    try writer.writeAll(" [");
    for (f.parameters) |p, i| {
        try parameter(writer, intern, p);
        if (i < f.parameters.len - 1) try writer.writeAll(" ");
    }
    try writer.writeAll("]");
    if (f.return_type) |t| {
        try writer.writeAll(" ");
        try type_(writer, intern, t.*);
    }
    try block(writer, intern, f.body, indent + 1, false);
    try writer.writeAll(")");
}

fn declaration(writer: List(u8).Writer, intern: Intern, d: Declaration) !void {
    try writer.writeAll("(declare ");
    try interned(writer, intern, d.name.kind.symbol);
    try writer.writeAll(" [");
    for (d.parameters) |p, i| {
        try parameter(writer, intern, p);
        if (i < d.parameters.len - 1) try writer.writeAll(" ");
    }
    try writer.writeAll("]");
    if (d.return_type) |t| {
        try writer.writeAll(" ");
        try type_(writer, intern, t.*);
    }
    try writer.writeAll(")");
}

fn binaryOp(writer: List(u8).Writer, intern: Intern, b: BinaryOp, indent: u64) !void {
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
    try expression(writer, intern, b.left.*, indent);
    try writer.writeAll(" ");
    try expression(writer, intern, b.right.*, indent);
    try writer.writeAll(")");
}

fn if_(writer: List(u8).Writer, intern: Intern, i: If, indent: u64) !void {
    try writer.writeAll("(if ");
    try expression(writer, intern, i.condition.*, indent);
    try block(writer, intern, i.then, indent + 1, false);
    try block(writer, intern, i.else_, indent + 1, i.then.len > 1);
    try writer.writeAll(")");
}

fn call(writer: List(u8).Writer, intern: Intern, c: Call, indent: u64) !void {
    try writer.writeAll("(");
    try expression(writer, intern, c.function.*, indent);
    for (c.arguments) |a| {
        try writer.writeAll(" ");
        try expression(writer, intern, a, indent);
    }
    try writer.writeAll(")");
}

fn import(writer: List(u8).Writer, intern: Intern, ast: Ast, indent: u64) !void {
    try writer.writeAll("(import ");
    try expression(writer, intern, ast, indent);
    try writer.writeAll(")");
}

fn export_(writer: List(u8).Writer, intern: Intern, ast: Ast, indent: u64) !void {
    try writer.writeAll("(export ");
    try expression(writer, intern, ast, indent);
    try writer.writeAll(")");
}

fn module(writer: List(u8).Writer, intern: Intern, m: Module, indent: u64) !void {
    for (m) |ast, i| {
        if (i > 0) try writer.writeAll("\n\n");
        try expression(writer, intern, ast, indent);
    }
}

fn expression(writer: List(u8).Writer, intern: Intern, ast: Ast, indent: u64) error{OutOfMemory}!void {
    switch (ast.kind) {
        .int, .symbol => |s| try interned(writer, intern, s),
        .bool => |b| try writer.writeAll(if (b) "true" else "false"),
        .define => |d| try define(writer, intern, d, indent),
        .function => |f| try function(writer, intern, f, indent),
        .declaration => |d| try declaration(writer, intern, d),
        .binary_op => |b| try binaryOp(writer, intern, b, indent),
        .group => |g| try expression(writer, intern, g.*, indent),
        .if_ => |i| try if_(writer, intern, i, indent),
        .call => |c| try call(writer, intern, c, indent),
        .import => |i| try import(writer, intern, i.*, indent),
        .export_ => |e| try export_(writer, intern, e.*, indent),
        .module => |m| try module(writer, intern, m, indent),
    }
}

pub fn toString(allocator: Allocator, intern: Intern, ast: Ast) ![]u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    const indent: u64 = 0;
    try expression(writer, intern, ast, indent);
    return list.toOwnedSlice();
}
