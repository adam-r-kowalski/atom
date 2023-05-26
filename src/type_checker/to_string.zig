const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const types = @import("types.zig");
const Module = types.Module;
const TopLevel = types.TopLevel;
const Symbol = types.Symbol;
const Int = types.Int;
const Float = types.Float;
const String = types.String;
const Bool = types.Bool;
const Function = types.Function;
const MonoType = types.MonoType;
const Expression = types.Expression;
const Constraints = types.Constraints;
const Equal = types.Equal;
const Substitution = types.Substitution;
const If = types.If;
const BinaryOp = types.BinaryOp;
const Call = types.Call;
const Intrinsic = types.Intrinsic;
const Define = types.Define;
const Block = types.Block;
const ForeignImport = types.ForeignImport;
const Convert = types.Convert;

const Indent = u64;

pub fn indent(writer: List(u8).Writer, n: Indent) !void {
    if (n > 0) try writer.writeAll("\n");
    for (0..n) |_| try writer.writeAll("    ");
}

fn symbol(writer: List(u8).Writer, intern: Intern, s: Symbol) !void {
    const name = interner.lookup(intern, s.value);
    try writer.print("symbol{{ name = {s}, type = ", .{name});
    try monotype(writer, s.type);
    try writer.writeAll(" }");
}

fn int(writer: List(u8).Writer, intern: Intern, i: Int) !void {
    const value = interner.lookup(intern, i.value);
    try writer.print("int{{ value = {s}, type = ", .{value});
    try monotype(writer, i.type);
    try writer.writeAll(" }");
}

fn float(writer: List(u8).Writer, intern: Intern, f: Float) !void {
    const value = interner.lookup(intern, f.value);
    try writer.print("float{{ value = {s}, type = ", .{value});
    try monotype(writer, f.type);
    try writer.writeAll(" }");
}

fn string(writer: List(u8).Writer, intern: Intern, s: String) !void {
    const value = interner.lookup(intern, s.value);
    try writer.print("string{{ value = {s}, type = ", .{value});
    try monotype(writer, s.type);
    try writer.writeAll(" }");
}

fn boolean(writer: List(u8).Writer, b: Bool) !void {
    try writer.print("bool{{ value = {}, type = ", .{b.value});
    try monotype(writer, b.type);
    try writer.writeAll(" }");
}

pub fn monotype(writer: List(u8).Writer, m: MonoType) !void {
    switch (m) {
        .i32 => try writer.writeAll("i32"),
        .i64 => try writer.writeAll("i64"),
        .f32 => try writer.writeAll("f32"),
        .str => try writer.writeAll("str"),
        .bool => try writer.writeAll("bool"),
        .void => try writer.writeAll("void"),
        .typevar => |t| try writer.print("${}", .{t}),
        .function => |f| {
            try writer.writeAll("fn(");
            for (f, 0..) |a, i| {
                if (i == f.len - 1) {
                    try writer.writeAll(") ");
                } else if (i > 0) {
                    try writer.writeAll(", ");
                }
                try monotype(writer, a);
            }
        },
    }
}

fn conditional(writer: List(u8).Writer, intern: Intern, i: If, in: Indent) !void {
    try indent(writer, in);
    try writer.writeAll("if =");
    try indent(writer, in + 1);
    try writer.writeAll("condition = ");
    try expression(writer, intern, i.condition.*, in + 2);
    try indent(writer, in + 1);
    try writer.writeAll("then = ");
    try block(writer, intern, i.then, in + 2);
    try indent(writer, in + 1);
    try writer.writeAll("else = ");
    try block(writer, intern, i.else_, in + 2);
    try indent(writer, in + 1);
    try writer.print("type = ", .{});
    try monotype(writer, i.type);
}

fn binaryOp(writer: List(u8).Writer, intern: Intern, b: BinaryOp, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("binary_op =");
    try indent(writer, i + 1);
    try writer.writeAll("kind = ");
    switch (b.kind) {
        .add => try writer.writeAll("+"),
        .multiply => try writer.writeAll("*"),
        .divide => try writer.writeAll("/"),
        else => unreachable,
    }
    try indent(writer, i + 1);
    try writer.writeAll("left = ");
    try expression(writer, intern, b.left.*, i + 2);
    try indent(writer, i + 1);
    try writer.writeAll("right = ");
    try expression(writer, intern, b.right.*, i + 2);
    try indent(writer, i + 1);
    try writer.print("type = ", .{});
    try monotype(writer, b.type);
}

fn call(writer: List(u8).Writer, intern: Intern, c: Call, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("call =");
    try indent(writer, i + 1);
    try expression(writer, intern, c.function.*, i + 2);
    try indent(writer, i + 1);
    try writer.writeAll("arguments =");
    for (c.arguments) |a| {
        try indent(writer, i + 2);
        try expression(writer, intern, a, i + 3);
    }
    try indent(writer, i + 1);
    try writer.print("type = ", .{});
    try monotype(writer, c.type);
}

fn intrinsic(writer: List(u8).Writer, intern: Intern, i: Intrinsic, in: Indent) !void {
    try indent(writer, in);
    try writer.writeAll("intrinsic =");
    try indent(writer, in + 1);
    try writer.writeAll(interner.lookup(intern, i.function));
    try indent(writer, in + 1);
    try writer.writeAll("arguments =");
    for (i.arguments) |a| {
        try indent(writer, in + 2);
        try expression(writer, intern, a, in + 3);
    }
    try indent(writer, in + 1);
    try writer.print("type = ", .{});
    try monotype(writer, i.type);
}

fn define(writer: List(u8).Writer, intern: Intern, d: Define, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("define =");
    try indent(writer, i + 1);
    try writer.writeAll("name = ");
    try symbol(writer, intern, d.name);
    try indent(writer, i + 1);
    try writer.writeAll("type = ");
    try monotype(writer, d.type);
    try indent(writer, i + 1);
    try writer.writeAll("value = ");
    try expression(writer, intern, d.value.*, i + 2);
}

fn function(writer: List(u8).Writer, intern: Intern, f: Function, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("function =");
    if (f.parameters.len != 0) {
        try indent(writer, i + 1);
        try writer.print("parameters =", .{});
    }
    for (f.parameters) |p| {
        try indent(writer, i + 2);
        try symbol(writer, intern, p);
    }
    try indent(writer, i + 1);
    try writer.print("return_type = ", .{});
    try monotype(writer, f.return_type);
    try indent(writer, i + 1);
    try writer.print("body = ", .{});
    try block(writer, intern, f.body, i + 2);
}

fn block(writer: List(u8).Writer, intern: Intern, b: Block, i: Indent) !void {
    for (b.expressions) |expr| try expression(writer, intern, expr, i);
}

fn foreignImport(writer: List(u8).Writer, intern: Intern, f: ForeignImport, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("foreign_import =");
    try indent(writer, i + 1);
    const module = interner.lookup(intern, f.module);
    try writer.print("module = {s}", .{module});
    try indent(writer, i + 1);
    const name = interner.lookup(intern, f.name);
    try writer.print("name = {s}", .{name});
    try indent(writer, i + 1);
    try writer.print("type = ", .{});
    try monotype(writer, f.type);
}

fn convert(writer: List(u8).Writer, intern: Intern, c: Convert, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("convert =");
    try indent(writer, i + 1);
    try writer.print("value = ", .{});
    try expression(writer, intern, c.value.*, i + 1);
    try indent(writer, i + 1);
    try writer.print("type = ", .{});
    try monotype(writer, c.type);
}

fn expression(writer: List(u8).Writer, intern: Intern, e: Expression, in: Indent) error{OutOfMemory}!void {
    switch (e) {
        .symbol => |s| try symbol(writer, intern, s),
        .int => |i| try int(writer, intern, i),
        .float => |f| try float(writer, intern, f),
        .string => |s| try string(writer, intern, s),
        .bool => |b| try boolean(writer, b),
        .if_ => |i| try conditional(writer, intern, i, in),
        .binary_op => |b| try binaryOp(writer, intern, b, in),
        .call => |c| try call(writer, intern, c, in),
        .intrinsic => |i| try intrinsic(writer, intern, i, in),
        .define => |d| try define(writer, intern, d, in),
        .function => |f| try function(writer, intern, f, in),
        .block => |b| try block(writer, intern, b, in),
        .foreign_import => |f| try foreignImport(writer, intern, f, in),
        .convert => |c| try convert(writer, intern, c, in),
        else => |k| std.debug.panic("\nUnhandled expression type {}", .{k}),
    }
}

pub fn toString(writer: List(u8).Writer, intern: Intern, m: Module) !void {
    for (m.order, 0..) |name, i| {
        if (m.typed.get(name)) |e| {
            if (i > 0) try writer.writeAll("\n\n");
            try expression(writer, intern, e, 0);
        }
    }
}
