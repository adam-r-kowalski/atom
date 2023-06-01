const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Interned = struct {
    value: u64,
    intern: *const Intern,

    pub fn eql(self: Interned, other: Interned) bool {
        return self.value == other.value;
    }

    pub fn string(self: Interned) []const u8 {
        return self.intern.lookup(self);
    }

    pub fn format(self: Interned, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.writeAll(self.string());
    }
};

const Map = std.StringArrayHashMap(Interned);

pub const Intern = struct {
    map: Map,
    index: usize,

    pub fn init(allocator: Allocator) Intern {
        return Intern{
            .map = Map.init(allocator),
            .index = 0,
        };
    }

    pub fn store(self: *Intern, str: []const u8) !Interned {
        const result = try self.map.getOrPut(str);
        if (result.found_existing) return result.value_ptr.*;
        result.value_ptr.* = Interned{
            .value = self.index,
            .intern = self,
        };
        self.index += 1;
        return result.value_ptr.*;
    }

    pub fn lookup(self: Intern, interned: Interned) []const u8 {
        return self.map.keys()[interned.value];
    }
};
