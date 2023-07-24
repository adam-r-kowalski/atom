const std = @import("std");
const List = std.ArrayList;
const Writer = List(u8).Writer;

const types = @import("types.zig");

const Indent = u64;

fn newlineAndIndent(indent: Indent, writer: Writer) !void {
    if (indent > 0) try writer.writeAll("\n");
    for (0..indent) |_| try writer.writeAll("    ");
}

pub fn typeOf(t: types.Type, writer: Writer) !void {
    switch (t) {
        .i32 => try writer.writeAll("i32"),
        .i64 => try writer.writeAll("i64"),
        .f32 => try writer.writeAll("f32"),
        .f64 => try writer.writeAll("f64"),
        .void => try writer.writeAll("void"),
        .function => |f| {
            for (f.parameters, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(" ");
                try writer.writeAll("(param ");
                try typeOf(arg, writer);
                try writer.writeAll(")");
            }
            switch (f.return_type.*) {
                .void => {},
                else => |k| {
                    try writer.writeAll(" (result ");
                    try typeOf(k, writer);
                    try writer.writeAll(")");
                },
            }
        },
    }
}

pub fn localSet(l: types.LocalSet, indent: Indent, writer: Writer) !void {
    try writer.print("(local.set ${s}", .{l.name.string()});
    try newlineAndIndent(indent + 1, writer);
    try expression(l.value.*, indent + 1, writer);
    try writer.writeAll(")");
}

pub fn globalSet(g: types.GlobalSet, indent: Indent, writer: Writer) !void {
    try writer.print("(global.set ${s}", .{g.name.string()});
    try newlineAndIndent(indent + 1, writer);
    try expression(g.value.*, indent + 1, writer);
    try writer.writeAll(")");
}

pub fn literal(l: types.Literal, writer: Writer) !void {
    switch (l) {
        .bool => |v| try writer.print("(i32.const {})", .{@as(i32, if (v) 1 else 0)}),
        .u32 => |v| try writer.print("(i32.const {})", .{v}),
        .u64 => |v| try writer.print("(i64.const {})", .{v}),
        .i32 => |v| try writer.print("(i32.const {})", .{v}),
        .i64 => |v| try writer.print("(i64.const {})", .{v}),
        .f32 => |v| try writer.print("(f32.const {})", .{v}),
        .f64 => |v| try writer.print("(f64.const {})", .{v}),
    }
}

pub fn call(c: types.Call, indent: Indent, writer: Writer) !void {
    try writer.print("(call ${s}", .{c.function.string()});
    for (c.arguments) |arg| {
        try newlineAndIndent(indent + 1, writer);
        try expression(arg, indent + 1, writer);
    }
    try writer.writeAll(")");
}

pub fn callIntrinsic(c: types.CallIntrinsic, indent: Indent, writer: Writer) !void {
    try writer.writeAll("(call $");
    switch (c.intrinsic) {
        .empty => try writer.writeAll("core/empty"),
    }
    for (c.arguments) |arg| {
        try newlineAndIndent(indent + 1, writer);
        try expression(arg, indent + 1, writer);
    }
    try writer.writeAll(")");
}

pub fn conditional(i: types.If, indent: Indent, writer: Writer) !void {
    try writer.writeAll("(if ");
    switch (i.result) {
        .void => {},
        else => |t| {
            try writer.writeAll("(result ");
            try typeOf(t, writer);
            try writer.writeAll(")");
        },
    }
    try newlineAndIndent(indent + 1, writer);
    try expression(i.condition.*, indent + 1, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("(then");
    try newlineAndIndent(indent + 2, writer);
    try expressions(i.then, indent + 2, writer);
    try writer.writeAll(")");
    if (i.else_.expressions.len > 0) {
        try newlineAndIndent(indent + 1, writer);
        try writer.writeAll("(else");
        try newlineAndIndent(indent + 2, writer);
        try expressions(i.else_, indent + 2, writer);
        try writer.writeAll(")");
    }
    try writer.writeAll(")");
}

pub fn unaryOp(u: types.UnaryOp, indent: Indent, writer: Writer) !void {
    try writer.writeAll("(");
    switch (u.kind) {
        .i32_trunc_f32_s => try writer.writeAll("i32.trunc_f32_s"),
        .i64_trunc_f64_s => try writer.writeAll("i64.trunc_f64_s"),
        .f32_sqrt => try writer.writeAll("f32.sqrt"),
        .f32_convert_i32_s => try writer.writeAll("f32.convert_i32_s"),
        .f32_load => try writer.writeAll("f32.load"),
        .f64_sqrt => try writer.writeAll("f64.sqrt"),
        .f64_convert_i64_s => try writer.writeAll("f64.convert_i64_s"),
        .f64_load => try writer.writeAll("f64.load"),
        .i32_load => try writer.writeAll("i32.load"),
        .i32_load8_u => try writer.writeAll("i32.load8_u"),
        .i64_load => try writer.writeAll("i64.load"),
    }
    try newlineAndIndent(indent + 1, writer);
    try expression(u.expression.*, indent + 1, writer);
    try writer.writeAll(")");
}

pub fn binaryOp(b: types.BinaryOp, indent: Indent, writer: Writer) !void {
    try writer.writeAll("(");
    switch (b.kind) {
        .i32_add => try writer.writeAll("i32.add"),
        .i32_sub => try writer.writeAll("i32.sub"),
        .i32_mul => try writer.writeAll("i32.mul"),
        .i32_div_s => try writer.writeAll("i32.div_s"),
        .i32_eq => try writer.writeAll("i32.eq"),
        .i32_rem_s => try writer.writeAll("i32.rem_s"),
        .i32_or => try writer.writeAll("i32.or"),
        .i32_ge_u => try writer.writeAll("i32.ge_u"),
        .i32_gt_s => try writer.writeAll("i32.gt_s"),
        .i32_lt_s => try writer.writeAll("i32.lt_s"),
        .i32_store => try writer.writeAll("i32.store"),
        .i32_store8 => try writer.writeAll("i32.store8"),
        .i64_add => try writer.writeAll("i64.add"),
        .i64_sub => try writer.writeAll("i64.sub"),
        .i64_mul => try writer.writeAll("i64.mul"),
        .i64_div_s => try writer.writeAll("i64.div_s"),
        .i64_eq => try writer.writeAll("i64.eq"),
        .i64_rem_s => try writer.writeAll("i64.rem_s"),
        .i64_gt_s => try writer.writeAll("i64.gt_s"),
        .i64_lt_s => try writer.writeAll("i64.lt_s"),
        .i64_store => try writer.writeAll("i64.store"),
        .f32_add => try writer.writeAll("f32.add"),
        .f32_sub => try writer.writeAll("f32.sub"),
        .f32_mul => try writer.writeAll("f32.mul"),
        .f32_div => try writer.writeAll("f32.div"),
        .f32_eq => try writer.writeAll("f32.eq"),
        .f32_gt => try writer.writeAll("f32.gt"),
        .f32_lt => try writer.writeAll("f32.lt"),
        .f32_store => try writer.writeAll("f32.store"),
        .f64_add => try writer.writeAll("f64.add"),
        .f64_sub => try writer.writeAll("f64.sub"),
        .f64_mul => try writer.writeAll("f64.mul"),
        .f64_div => try writer.writeAll("f64.div"),
        .f64_eq => try writer.writeAll("f64.eq"),
        .f64_gt => try writer.writeAll("f64.gt"),
        .f64_lt => try writer.writeAll("f64.lt"),
        .f64_store => try writer.writeAll("f64.store"),
    }
    try newlineAndIndent(indent + 1, writer);
    try expression(b.left.*, indent + 1, writer);
    try newlineAndIndent(indent + 1, writer);
    try expression(b.right.*, indent + 1, writer);
    try writer.writeAll(")");
}

pub fn expressions(es: types.Expressions, indent: Indent, writer: Writer) !void {
    for (es.expressions, 0..) |e, i| {
        switch (e) {
            .nop => {},
            else => {
                if (i > 0) {
                    try newlineAndIndent(indent, writer);
                }
                try expression(e, indent, writer);
            },
        }
    }
}

pub fn block(b: types.Block, indent: Indent, writer: Writer) !void {
    try writer.writeAll("(block (result ");
    try typeOf(b.result, writer);
    try writer.writeAll(")");
    for (b.expressions) |expr| {
        try newlineAndIndent(indent + 1, writer);
        try expression(expr, indent + 1, writer);
    }
    try writer.writeAll(")");
}

fn drop(d: types.Drop, indent: Indent, writer: Writer) !void {
    try writer.writeAll("(drop");
    try newlineAndIndent(indent + 1, writer);
    try expression(d.expression.*, indent + 1, writer);
    try writer.writeAll(")");
}

fn memoryCopy(m: types.MemoryCopy, indent: Indent, writer: Writer) !void {
    try writer.writeAll("(memory.copy");
    try newlineAndIndent(indent + 1, writer);
    try expression(m.destination.*, indent + 1, writer);
    try newlineAndIndent(indent + 1, writer);
    try expression(m.source.*, indent + 1, writer);
    try newlineAndIndent(indent + 1, writer);
    try expression(m.size.*, indent + 1, writer);
    try writer.writeAll(")");
}

pub fn expression(e: types.Expression, indent: Indent, writer: Writer) error{OutOfMemory}!void {
    switch (e) {
        .local_get => |l| try writer.print("(local.get ${s})", .{l.name.string()}),
        .local_set => |l| try localSet(l, indent, writer),
        .global_get => |g| try writer.print("(global.get ${s})", .{g.name.string()}),
        .global_set => |g| try globalSet(g, indent, writer),
        .literal => |l| try literal(l, writer),
        .call => |c| try call(c, indent, writer),
        .call_intrinsic => |c| try callIntrinsic(c, indent, writer),
        .if_ => |i| try conditional(i, indent, writer),
        .unary_op => |u| try unaryOp(u, indent, writer),
        .binary_op => |b| try binaryOp(b, indent, writer),
        .expressions => |es| try expressions(es, indent, writer),
        .block => |b| try block(b, indent, writer),
        .drop => |d| try drop(d, indent, writer),
        .memory_copy => |m| try memoryCopy(m, indent, writer),
        .nop => try writer.writeAll("(nop)"),
        .unreachable_ => try writer.writeAll("(unreachable)"),
    }
}

pub fn foreignImport(i: types.ForeignImport, writer: Writer) !void {
    try newlineAndIndent(1, writer);
    try writer.print("(import \"{s}\" \"{s}\" (func ${s} ", .{ i.path[0], i.path[1], i.name.string() });
    try typeOf(i.type, writer);
    try writer.writeAll("))");
}

pub fn dataSegment(d: types.DataSegment, writer: Writer) !void {
    if (d.data.items.len == 0) return;
    try writer.writeAll("\n");
    for (d.data.items) |i| {
        try writer.print("\n    (data (i32.const {}) \"", .{i.offset});
        for (i.bytes) |b| {
            switch (b) {
                '\n' => try writer.writeAll("\\n"),
                '"' => try writer.writeAll("\\\""),
                else => try writer.writeByte(b),
            }
        }
        try writer.writeAll("\")");
    }
}

pub fn global(g: types.Global, writer: Writer) !void {
    try newlineAndIndent(1, writer);
    try writer.print("(global ${s} ", .{g.name.string()});
    try typeOf(g.type, writer);
    try writer.writeAll(" ");
    try expression(g.value, 0, writer);
    try writer.writeAll(")");
}

pub fn function(f: types.Function, writer: Writer) !void {
    try writer.writeAll("\n");
    try newlineAndIndent(1, writer);
    try writer.print("(func ${s}", .{f.name.string()});
    for (f.parameters) |p| {
        try writer.print(" (param ${s} ", .{p.name.string()});
        try typeOf(p.type, writer);
        try writer.writeAll(")");
    }
    switch (f.return_type) {
        .void => {},
        else => {
            try writer.writeAll(" (result ");
            try typeOf(f.return_type, writer);
            try writer.writeAll(")");
        },
    }
    for (f.locals) |l| {
        try newlineAndIndent(2, writer);
        try writer.print("(local ${s} ", .{l.name.string()});
        try typeOf(l.type, writer);
        try writer.writeAll(")");
    }
    for (f.pointers) |l| {
        try newlineAndIndent(2, writer);
        try writer.print("(local ${s} i32)", .{l.name.string()});
    }
    for (f.pointers) |l| {
        try writer.print(
            \\
            \\        (local.set ${s}
            \\            (call $core/alloc
            \\                (i32.const {})))
        , .{ l.name.string(), l.size });
    }
    try newlineAndIndent(2, writer);
    try expressions(f.body, 2, writer);
    try writer.writeAll(")");
}

pub fn foreignExport(e: types.ForeignExport, writer: Writer) !void {
    try writer.writeAll("\n");
    try newlineAndIndent(1, writer);
    try writer.print("(export \"{s}\" (func ${s}))", .{ e.alias.string(), e.name.string() });
}

pub fn intrinsics(is: types.Intrinsics, writer: Writer) !void {
    var iterator = is.keyIterator();
    while (iterator.next()) |value| {
        switch (value.*) {
            .empty => {
                try writer.writeAll(
                    \\
                    \\
                    \\    (func $core/empty (param $size i32) (param $len i32) (result i32)
                    \\        (local $ptr i32)
                    \\            (local.set $ptr
                    \\                (call $core/alloc
                    \\                (i32.const 8)))
                    \\        (i32.store
                    \\            (local.get $ptr)
                    \\            (call $core/alloc
                    \\            (i32.mul
                    \\                (local.get $size)
                    \\                (local.get $len))))
                    \\        (i32.store
                    \\            (i32.add
                    \\                (local.get $ptr)
                    \\                (i32.const 4))
                    \\            (local.get $len))
                    \\        (local.get $ptr))
                );
            },
        }
    }
}

pub fn module(m: types.Module, writer: Writer) !void {
    try writer.writeAll("(module");
    if (m.foreign_imports.len > 0) {
        try writer.writeAll("\n");
        for (m.foreign_imports) |i| try foreignImport(i, writer);
    }
    try writer.writeAll(
        \\
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
    );
    try dataSegment(m.data_segment, writer);
    if (m.uses_memory) {
        try writer.print(
            \\
            \\
            \\    (global $core/arena (mut i32) (i32.const {}))
            \\
            \\    (func $core/alloc (param $size i32) (result i32)
            \\        (local $ptr i32)
            \\        (local.tee $ptr
            \\            (global.get $core/arena))
            \\        (global.set $core/arena
            \\            (i32.add
            \\                (local.get $ptr)
            \\                (local.get $size))))
        , .{m.data_segment.offset});
    }
    try intrinsics(m.intrinsics, writer);
    if (m.globals.len > 0) {
        try writer.writeAll("\n");
        for (m.globals) |g| try global(g, writer);
    }
    for (m.functions) |f| try function(f, writer);
    for (m.foreign_exports) |e| try foreignExport(e, writer);
    try writer.writeAll(")");
}
