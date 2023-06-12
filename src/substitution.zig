const std = @import("std");
const Allocator = std.mem.Allocator;
const Map = std.AutoHashMap;

const Span = @import("tokenizer.zig").types.Span;

pub const TypeVar = struct { value: u64 };

const Array = struct {
    size: ?u32,
    element_type: *const MonoType,
};

pub const MonoType = union(enum) {
    void,
    u8,
    i32,
    i64,
    f32,
    f64,
    bool,
    typevar: TypeVar,
    function: []MonoType,
    array: Array,

    pub fn apply(self: *MonoType, s: Substitution) void {
        switch (self.*) {
            .function => |f| for (f) |*t| t.apply(s),
            .typevar => |t| {
                if (s.get(t)) |mono| self.* = mono;
            },
            else => return,
        }
    }
};

pub const TypedSpan = struct {
    span: ?Span,
    type: MonoType,

    pub fn format(self: TypedSpan, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{}", .{self.type});
    }
};

pub const Substitution = struct {
    map: Map(TypeVar, MonoType),

    pub fn init(allocator: Allocator) Substitution {
        return Substitution{
            .map = Map(TypeVar, MonoType).init(allocator),
        };
    }

    pub fn set(self: *Substitution, t: TypeVar, m: MonoType) !void {
        const result = try self.map.getOrPut(t);
        if (result.found_existing) {
            if (std.meta.eql(result.value_ptr.*, m)) return;
            switch (m) {
                .typevar => |t1| try self.set(t1, result.value_ptr.*),
                else => switch (result.value_ptr.*) {
                    .typevar => |t1| try self.set(t1, m),
                    else => return error.CompileError,
                },
            }
        }
        result.value_ptr.* = m;
    }

    pub fn get(self: Substitution, t: TypeVar) ?MonoType {
        return self.map.get(t);
    }

    pub fn simplify(self: *Substitution) u64 {
        var count: u64 = 0;
        var iterator = self.map.iterator();
        while (iterator.next()) |entry| {
            switch (entry.value_ptr.*) {
                .typevar => |t| {
                    if (self.map.get(t)) |v| {
                        entry.value_ptr.* = v;
                        count += 1;
                    }
                },
                else => {},
            }
        }
        return count;
    }

    pub fn format(self: Substitution, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll("\n\n=== Substitution ===");
        var iterator = self.map.iterator();
        while (iterator.next()) |t| {
            try writer.print("\n${} = {}", .{ t.key_ptr.*, t.value_ptr.* });
        }
    }
};
