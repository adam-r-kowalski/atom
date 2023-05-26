const std = @import("std");
const atom = @import("atom");

test "type infer convert i32 to f32" {
    const allocator = std.testing.allocator;
    const source =
        \\start = fn(x: i32) f32 {
        \\    convert(x, f32)
        \\}
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ name = start, type = fn(i32) f32 }
        \\    type = void
        \\    value = 
        \\        function =
        \\            parameters =
        \\                symbol{ name = x, type = i32 }
        \\            return_type = f32
        \\            body = 
        \\                convert =
        \\                    value = symbol{ name = x, type = i32 }
        \\                    type = f32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen convert i32 to f32" {
    const allocator = std.testing.allocator;
    const source =
        \\start = fn(x: i32) f32 {
        \\    convert(x, f32)
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (func $start (param $x i32) (result f32)
        \\        (f32.convert_i32_s
        \\            (local.get $x)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
