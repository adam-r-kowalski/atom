const std = @import("std");
const List = std.ArrayList;
const Writer = List(u8).Writer;

const types = @import("types.zig");

const Indent = u64;

fn newlineAndIndent(indent: Indent, writer: Writer) !void {
    if (indent > 0) try writer.writeAll("\n");
    for (0..indent) |_| try writer.writeAll("    ");
}

pub fn monotype(m: types.MonoType, writer: Writer) !void {
    switch (m) {
        .u8 => try writer.writeAll("u8"),
        .u32 => try writer.writeAll("u32"),
        .i32 => try writer.writeAll("i32"),
        .i64 => try writer.writeAll("i64"),
        .f32 => try writer.writeAll("f32"),
        .f64 => try writer.writeAll("f64"),
        .bool => try writer.writeAll("bool"),
        .void => try writer.writeAll("void"),
        .typevar => |t| try writer.print("${}", .{t.value}),
        .function => |f| {
            try writer.writeAll("fn(");
            for (f.parameters, 0..) |p, i| {
                if (i > 0) {
                    try writer.writeAll(", ");
                }
                if (p.mutable) try writer.writeAll("mut ");
                try writer.print("{s}: ", .{p.name.string()});
                try monotype(p.type, writer);
            }
            try writer.writeAll(") -> ");
            try monotype(f.return_type.*, writer);
        },
        .call => |f| {
            try writer.writeAll("fn(");
            for (f.arguments, 0..) |p, i| {
                if (i > 0) {
                    try writer.writeAll(", ");
                }
                if (p.mutable) try writer.writeAll("mut ");
                try monotype(p.type, writer);
            }
            try writer.writeAll(") -> ");
            try monotype(f.return_type.*, writer);
        },
        .array => |a| {
            switch (a.rank) {
                1 => {
                    switch (a.element_type.*) {
                        .u8 => try writer.writeAll("str"),
                        else => {
                            try writer.writeAll("[]");
                            try monotype(a.element_type.*, writer);
                        },
                    }
                },
                else => {
                    try writer.writeAll("arr[");
                    try monotype(a.element_type.*, writer);
                    try writer.print(", {}]", .{a.rank});
                },
            }
        },
        .enumeration => |e| {
            try writer.writeAll("enum{ ");
            for (e.variants, 0..) |v, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.writeAll(v.string());
            }
            try writer.writeAll(" }");
        },
        .enumeration_instance => |e| {
            try writer.writeAll(e.name.string());
        },
        .structure => |s| {
            try writer.writeAll("struct{ ");
            for (s.order, 0..) |o, i| {
                const field = s.fields.get(o).?;
                if (i > 0) try writer.writeAll(", ");
                try writer.print("{s}: ", .{o.string()});
                try monotype(field, writer);
            }
            try writer.writeAll(" }");
        },
        .structure_literal => |s| {
            try writer.writeAll("struct_literal{ ");
            for (s.order, 0..) |o, i| {
                const field = s.fields.get(o).?;
                if (i > 0) try writer.writeAll(", ");
                try writer.print("{s}: ", .{o.string()});
                try monotype(field, writer);
            }
            try writer.writeAll(" } as ");
            try monotype(s.structure.*, writer);
        },
    }
}

pub fn int(i: types.Int, writer: Writer) !void {
    try writer.print("int{{ value = {}, type = ", .{i.value});
    try monotype(i.type, writer);
    try writer.writeAll(" }");
}

pub fn float(f: types.Float, writer: Writer) !void {
    try writer.print("float{{ value = {}, type = ", .{f.value});
    try monotype(f.type, writer);
    try writer.writeAll(" }");
}

pub fn symbol(s: types.Symbol, writer: Writer) !void {
    try writer.print("symbol{{ value = {}, type = ", .{s.value});
    try monotype(s.type, writer);
    try writer.writeAll(" }");
}

pub fn string(s: types.String, writer: Writer) !void {
    try writer.print("string{{ value = \"{}\", type = ", .{s.value});
    try monotype(s.type, writer);
    try writer.writeAll(" }");
}

pub fn boolean(b: types.Bool, writer: Writer) !void {
    try writer.print("bool{{ value = {}, type = ", .{b.value});
    try monotype(b.type, writer);
    try writer.writeAll(" }");
}

pub fn arm(a: types.Arm, indent: Indent, writer: Writer) !void {
    try newlineAndIndent(indent, writer);
    try writer.writeAll("condition =");
    try newlineAndIndent(indent + 1, writer);
    try expression(a.condition, indent + 1, writer);
    try newlineAndIndent(indent, writer);
    try writer.writeAll("then =");
    try newlineAndIndent(indent + 1, writer);
    try block(a.then, indent + 1, writer);
}

pub fn branch(b: types.Branch, indent: Indent, writer: Writer) !void {
    try writer.writeAll("branch =");
    for (b.arms) |a| try arm(a, indent + 1, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("else =");
    try newlineAndIndent(indent + 2, writer);
    try block(b.else_, indent + 2, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(b.type, writer);
}

pub fn binaryOp(b: types.BinaryOp, indent: Indent, writer: Writer) !void {
    try writer.writeAll("binary_op =");
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("kind = ");
    switch (b.kind) {
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
        .pipeline => try writer.writeAll("|>"),
    }
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("left =");
    try newlineAndIndent(indent + 2, writer);
    try expression(b.left.*, indent + 2, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("right =");
    try newlineAndIndent(indent + 2, writer);
    try expression(b.right.*, indent + 2, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(b.type, writer);
}

pub fn call(c: types.Call, indent: Indent, writer: Writer) !void {
    try writer.writeAll("call =");
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("function = ");
    try expression(c.function.*, indent + 2, writer);
    try newlineAndIndent(indent + 1, writer);
    if (c.arguments.len > 0) {
        try writer.writeAll("arguments =");
        for (c.arguments) |a| {
            try newlineAndIndent(indent + 2, writer);
            try writer.writeAll("argument =");
            try newlineAndIndent(indent + 3, writer);
            try writer.print("mutable = {}", .{a.mutable});
            try newlineAndIndent(indent + 3, writer);
            try writer.writeAll("value = ");
            try expression(a.value, indent + 4, writer);
        }
    }
    if (c.named_arguments.count() > 0) {
        try writer.writeAll("named_arguments =");
        for (c.named_arguments_order) |name| {
            const a = c.named_arguments.get(name).?;
            try newlineAndIndent(indent + 2, writer);
            try writer.writeAll("argument =");
            try newlineAndIndent(indent + 3, writer);
            try writer.print("name = {s}", .{name.string()});
            try newlineAndIndent(indent + 3, writer);
            try writer.print("mutable = {}", .{a.mutable});
            try newlineAndIndent(indent + 3, writer);
            try writer.writeAll("value = ");
            try expression(a.value, indent + 4, writer);
        }
    }
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(c.type, writer);
}

pub fn intrinsic(i: types.Intrinsic, indent: Indent, writer: Writer) !void {
    try writer.writeAll("intrinsic =");
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("function = ");
    try writer.writeAll(i.function.string());
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("arguments =");
    for (i.arguments) |a| {
        try newlineAndIndent(indent + 2, writer);
        try writer.writeAll("argument =");
        try newlineAndIndent(indent + 3, writer);
        try writer.print("mutable = {}", .{a.mutable});
        try newlineAndIndent(indent + 3, writer);
        try writer.writeAll("value = ");
        try expression(a.value, indent + 4, writer);
    }
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(i.type, writer);
}

pub fn define(d: types.Define, indent: Indent, writer: Writer) !void {
    try writer.writeAll("define =");
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("name = ");
    try symbol(d.name, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(d.type, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.print("mutable = {}", .{d.mutable});
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("value =");
    try newlineAndIndent(indent + 2, writer);
    try expression(d.value.*, indent + 2, writer);
}

pub fn drop(d: types.Drop, indent: Indent, writer: Writer) !void {
    try writer.writeAll("drop =");
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(d.type, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("value =");
    try newlineAndIndent(indent + 2, writer);
    try expression(d.value.*, indent + 2, writer);
}

pub fn plusEqual(p: types.PlusEqual, indent: Indent, writer: Writer) !void {
    try writer.writeAll("plus_equal =");
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("name = ");
    try symbol(p.name, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(p.type, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("value =");
    try newlineAndIndent(indent + 2, writer);
    try expression(p.value.*, indent + 2, writer);
}

pub fn timesEqual(t: types.TimesEqual, indent: Indent, writer: Writer) !void {
    try writer.writeAll("times_equal =");
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("name = ");
    try symbol(t.name, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(t.type, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("value =");
    try newlineAndIndent(indent + 2, writer);
    try expression(t.value.*, indent + 2, writer);
}

pub fn function(f: types.Function, indent: Indent, writer: Writer) !void {
    try writer.writeAll("function =");
    if (f.parameters.len != 0) {
        try newlineAndIndent(indent + 1, writer);
        try writer.writeAll("parameters =");
    }
    for (f.parameters) |p| {
        try newlineAndIndent(indent + 2, writer);
        if (p.mutable) try writer.writeAll("mut ");
        try symbol(p.name, writer);
    }
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("return_type = ");
    try monotype(f.return_type, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("body =");
    try newlineAndIndent(indent + 2, writer);
    try block(f.body, indent + 2, writer);
}

pub fn block(b: types.Block, indent: Indent, writer: Writer) !void {
    for (b.expressions, 0..) |expr, i| {
        if (i != 0) try newlineAndIndent(indent, writer);
        try expression(expr, indent, writer);
    }
}

pub fn group(g: types.Group, indent: Indent, writer: Writer) !void {
    try newlineAndIndent(indent, writer);
    try writer.writeAll("group =");
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("expressions =");
    for (g.expressions) |expr| {
        try newlineAndIndent(indent + 2, writer);
        try expression(expr, indent + 2, writer);
    }
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(g.type, writer);
}

pub fn foreignImport(f: types.ForeignImport, indent: Indent, writer: Writer) !void {
    try writer.writeAll("foreign_import =");
    try newlineAndIndent(indent + 1, writer);
    try writer.print("module = \"{s}\"", .{f.module.string()});
    try newlineAndIndent(indent + 1, writer);
    try writer.print("name = \"{s}\"", .{f.name.string()});
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(f.type, writer);
}

pub fn foreignExport(f: types.ForeignExport, indent: Indent, writer: Writer) !void {
    try newlineAndIndent(indent, writer);
    try writer.writeAll("foreign_export =");
    try newlineAndIndent(indent + 1, writer);
    try writer.print("name = \"{s}\"", .{f.name.string()});
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("value =");
    try newlineAndIndent(indent + 2, writer);
    try expression(f.value.*, indent + 2, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(f.type, writer);
}

pub fn convert(c: types.Convert, indent: Indent, writer: Writer) !void {
    try writer.writeAll("convert =");
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("value =");
    try newlineAndIndent(indent + 2, writer);
    try expression(c.value.*, indent + 2, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(c.type, writer);
}

pub fn undefinedKeyword(u: types.Undefined, indent: Indent, writer: Writer) !void {
    try newlineAndIndent(indent, writer);
    try writer.writeAll("undefined{ type = ");
    try monotype(u.type, writer);
    try writer.writeAll(" }");
}

pub fn variant(v: types.Variant, indent: Indent, writer: Writer) !void {
    try writer.writeAll("variant =");
    try newlineAndIndent(indent + 1, writer);
    try writer.print("value = {s}", .{v.value.string()});
    try newlineAndIndent(indent + 1, writer);
    try writer.print("index = {}", .{v.index});
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(v.type, writer);
}

pub fn structLiteral(s: types.StructLiteral, indent: Indent, writer: Writer) !void {
    try writer.writeAll("struct_literal =");
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(s.type, writer);
}

pub fn array(a: types.Array, indent: Indent, writer: Writer) !void {
    try writer.writeAll("array =");
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("expressions =");
    for (a.expressions) |e| {
        try newlineAndIndent(indent + 2, writer);
        try expression(e, indent + 2, writer);
    }
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(a.type, writer);
}

pub fn index(i: types.Index, indent: Indent, writer: Writer) !void {
    try writer.writeAll("index =");
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("expression = ");
    try expression(i.expression.*, indent, writer);
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("indices =");
    for (i.indices) |e| {
        try newlineAndIndent(indent + 2, writer);
        try expression(e, indent + 2, writer);
    }
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(i.type, writer);
}

pub fn templateLiteral(t: types.TemplateLiteral, indent: Indent, writer: Writer) !void {
    try writer.writeAll("template_literal =");
    if (t.function) |f| {
        try newlineAndIndent(indent + 1, writer);
        try writer.writeAll("function = ");
        try symbol(f, writer);
    }
    if (t.strings.len > 0) {
        try newlineAndIndent(indent + 1, writer);
        try writer.writeAll("strings =");
        for (t.strings) |s| {
            try newlineAndIndent(indent + 2, writer);
            try string(s, writer);
        }
    }
    if (t.arguments.len > 0) {
        try newlineAndIndent(indent + 1, writer);
        try writer.writeAll("arguments =");
        for (t.arguments) |a| {
            try newlineAndIndent(indent + 2, writer);
            try expression(a, indent + 2, writer);
        }
    }
    try newlineAndIndent(indent + 1, writer);
    try writer.writeAll("type = ");
    try monotype(t.type, writer);
}

pub fn expression(e: types.Expression, indent: Indent, writer: Writer) error{OutOfMemory}!void {
    switch (e) {
        .int => |i| try int(i, writer),
        .float => |f| try float(f, writer),
        .symbol => |s| try symbol(s, writer),
        .string => |s| try string(s, writer),
        .bool => |b| try boolean(b, writer),
        .branch => |b| try branch(b, indent, writer),
        .binary_op => |b| try binaryOp(b, indent, writer),
        .call => |c| try call(c, indent, writer),
        .intrinsic => |i| try intrinsic(i, indent, writer),
        .define => |d| try define(d, indent, writer),
        .drop => |d| try drop(d, indent, writer),
        .plus_equal => |p| try plusEqual(p, indent, writer),
        .times_equal => |t| try timesEqual(t, indent, writer),
        .function => |f| try function(f, indent, writer),
        .block => |b| try block(b, indent, writer),
        .group => |g| try group(g, indent, writer),
        .foreign_import => |f| try foreignImport(f, indent, writer),
        .foreign_export => |f| try foreignExport(f, indent, writer),
        .convert => |c| try convert(c, indent, writer),
        .undefined => |u| try undefinedKeyword(u, indent, writer),
        .variant => |v| try variant(v, indent, writer),
        .struct_literal => |s| try structLiteral(s, indent, writer),
        .array => |a| try array(a, indent, writer),
        .index => |i| try index(i, indent, writer),
        .template_literal => |t| try templateLiteral(t, indent, writer),
    }
}

pub fn module(m: types.Module, writer: Writer) !void {
    for (m.order, 0..) |name, i| {
        if (m.typed.get(name)) |e| {
            if (i > 0) try writer.writeAll("\n\n");
            try expression(e, 0, writer);
        }
    }
}

pub fn constraints(c: types.Constraints, writer: Writer) !void {
    for (c.equal.items) |e| {
        try writer.writeAll("\nconstraint =");
        try newlineAndIndent(1, writer);
        try writer.writeAll("left = ");
        try monotype(e.left, writer);
        try newlineAndIndent(1, writer);
        try writer.writeAll("right = ");
        try monotype(e.right, writer);
    }
}

pub fn substitution(s: types.Substitution, writer: Writer) !void {
    var iterator = s.map.iterator();
    while (iterator.next()) |entry| {
        try writer.writeAll("\nsubstitution =");
        try newlineAndIndent(1, writer);
        try writer.print("${} =>", .{entry.key_ptr.*});
        try monotype(entry.value_ptr.*, writer);
    }
}
