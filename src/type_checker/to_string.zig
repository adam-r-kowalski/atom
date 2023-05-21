const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const types = @import("types.zig");
const Module = types.Module;
const TopLevel = types.TopLevel;
const Function = types.Function;
const MonoType = types.MonoType;
const Expression = types.Expression;
const Constraints = types.Constraints;
const Equal = types.Equal;
const Substitution = types.Substitution;
const If = types.If;
const BinaryOp = types.BinaryOp;
const Call = types.Call;
const Define = types.Define;

const Indent = u64;

pub fn indent(writer: List(u8).Writer, n: Indent) !void {
    if (n > 0) try writer.writeAll("\n");
    var i: u64 = 0;
    while (i < n) : (i += 1) try writer.writeAll("    ");
}

fn symbol(writer: List(u8).Writer, intern: Intern, e: Expression) !void {
    const s = e.kind.symbol;
    const name = interner.lookup(intern, s);
    try writer.print("symbol{{ name = {s}, type = ", .{name});
    try monotype(writer, e.type);
    try writer.writeAll(" }");
}

fn int(writer: List(u8).Writer, intern: Intern, e: Expression) !void {
    const i = e.kind.int;
    const value = interner.lookup(intern, i);
    try writer.print("int{{ value = {s}, type = ", .{value});
    try monotype(writer, e.type);
    try writer.writeAll(" }");
}

fn float(writer: List(u8).Writer, intern: Intern, e: Expression) !void {
    const f = e.kind.float;
    const value = interner.lookup(intern, f);
    try writer.print("float{{ value = {s}, type = ", .{value});
    try monotype(writer, e.type);
    try writer.writeAll(" }");
}

fn boolean(writer: List(u8).Writer, e: Expression) !void {
    const b = e.kind.bool;
    try writer.print("bool{{ value = {}, type = ", .{b});
    try monotype(writer, e.type);
    try writer.writeAll(" }");
}

pub fn monotype(writer: List(u8).Writer, m: MonoType) !void {
    switch (m) {
        .i32 => try writer.writeAll("i32"),
        .f32 => try writer.writeAll("f32"),
        .bool => try writer.writeAll("bool"),
        .void => try writer.writeAll("void"),
        .typevar => |t| try writer.print("${}", .{t}),
        .function => |f| {
            try writer.writeAll("fn(");
            for (f) |a, i| {
                if (i == f.len - 1) {
                    try writer.writeAll(") ");
                } else if (i > 0) {
                    try writer.writeAll(", ");
                }
                try monotype(writer, a);
            }
        },
        else => std.debug.panic("\nUnhandled monotype type {}", .{m}),
    }
}

fn if_(writer: List(u8).Writer, intern: Intern, e: Expression, in: Indent) !void {
    const i = e.kind.if_;
    try indent(writer, in);
    try writer.writeAll("if =");
    try indent(writer, in + 1);
    try writer.writeAll("condition = ");
    try expression(writer, intern, i.condition.*, in + 2);
    try indent(writer, in + 1);
    try writer.writeAll("then = ");
    try expression(writer, intern, i.then.*, in + 2);
    try indent(writer, in + 1);
    try writer.writeAll("else = ");
    try expression(writer, intern, i.else_.*, in + 2);
    try indent(writer, in + 1);
    try writer.print("type = ", .{});
    try monotype(writer, e.type);
}

fn binaryOp(writer: List(u8).Writer, intern: Intern, e: Expression, i: Indent) !void {
    const b = e.kind.binary_op;
    try indent(writer, i);
    try writer.writeAll("binary_op =");
    try indent(writer, i + 1);
    try writer.writeAll("kind = ");
    switch (b.kind) {
        .add => try writer.writeAll("+"),
        .multiply => try writer.writeAll("*"),
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
    try monotype(writer, e.type);
}

fn call(writer: List(u8).Writer, intern: Intern, e: Expression, i: Indent) !void {
    const c = e.kind.call;
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
    try monotype(writer, e.type);
}

fn define(writer: List(u8).Writer, intern: Intern, e: Expression, i: Indent) !void {
    const d = e.kind.define;
    try indent(writer, i);
    try writer.writeAll("define =");
    try indent(writer, i + 1);
    try writer.writeAll("name = ");
    try expression(writer, intern, d.name.*, i + 2);
    try indent(writer, i + 1);
    try writer.writeAll("type = ");
    try monotype(writer, e.type);
    try indent(writer, i + 1);
    try writer.writeAll("value = ");
    try expression(writer, intern, d.value.*, i + 2);
}

fn function(writer: List(u8).Writer, intern: Intern, e: Expression, i: Indent) !void {
    const f = e.kind.function;
    try indent(writer, i);
    try writer.writeAll("function");
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
    try expression(writer, intern, f.body.*, i + 2);
}

fn block(writer: List(u8).Writer, intern: Intern, e: Expression, i: Indent) !void {
    for (e.kind.block) |expr| try expression(writer, intern, expr, i);
}

fn expression(writer: List(u8).Writer, intern: Intern, e: Expression, in: Indent) error{OutOfMemory}!void {
    switch (e.kind) {
        .symbol => try symbol(writer, intern, e),
        .int => try int(writer, intern, e),
        .float => try float(writer, intern, e),
        .bool => try boolean(writer, e),
        .if_ => try if_(writer, intern, e, in),
        .binary_op => try binaryOp(writer, intern, e, in),
        .call => try call(writer, intern, e, in),
        .define => try define(writer, intern, e, in),
        .function => try function(writer, intern, e, in),
        .block => try block(writer, intern, e, in),
        else => |k| std.debug.panic("\nUnhandled expression type {}", .{k}),
    }
}

pub fn toString(writer: List(u8).Writer, intern: Intern, m: Module) !void {
    for (m.order) |name, i| {
        if (m.typed.get(name)) |e| {
            if (i > 0) try writer.writeAll("\n\n");
            try expression(writer, intern, e, 0);
        }
    }
}
