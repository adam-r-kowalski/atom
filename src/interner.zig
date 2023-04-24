const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Interned = struct {
    index: usize,
};

const Map = std.StringArrayHashMap(Interned);

pub const Intern = struct {
    map: Map,
    index: usize,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .map = Map.init(allocator),
            .index = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.map.deinit();
    }

    pub fn string(self: *Self, str: []const u8) !Interned {
        const result = try self.map.getOrPut(str);
        if (result.found_existing) return result.value_ptr.*;
        result.value_ptr.* = Interned{ .index = self.index };
        self.index += 1;
        return result.value_ptr.*;
    }

    pub fn lookup(self: Self, interned: Interned) []const u8 {
        return self.map.keys()[interned.index];
    }
};
