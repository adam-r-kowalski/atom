const std = @import("std");
const Allocator = std.mem.Allocator;

const parser_types = @import("../parser/types.zig");
const UntypedAst = parser_types.Ast;
const types = @import("types.zig");
const Ast = types.Ast;

fn infer(allocator: Allocator, ast: UntypedAst) Ast {

}
