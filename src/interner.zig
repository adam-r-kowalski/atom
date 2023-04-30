const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Interned = u64;

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

    pub fn deinit(self: *Intern) void {
        self.map.deinit();
    }
};

pub fn string(self: *Intern, str: []const u8) !Interned {
    const result = try self.map.getOrPut(str);
    if (result.found_existing) return result.value_ptr.*;
    result.value_ptr.* = self.index;
    self.index += 1;
    return result.value_ptr.*;
}

pub fn lookup(self: Intern, interned: Interned) []const u8 {
    return self.map.keys()[interned];
}
