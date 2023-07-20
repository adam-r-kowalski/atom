const std = @import("std");
const zap = @import("zap");

test "type infer sqrt f32" {
    const allocator = std.testing.allocator;
    const source =
        \\start = (x: f32) f32 {
        \\    sqrt(x)
        \\}
    ;
    const actual = try zap.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = start, type = (f32) f32 }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        function =
        \\            parameters =
        \\                symbol{ value = x, type = f32 }
        \\            return_type = f32
        \\            body =
        \\                intrinsic =
        \\                    function = sqrt
        \\                    arguments =
        \\                        argument =
        \\                            mutable = false
        \\                            value = symbol{ value = x, type = f32 }
        \\                    type = f32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen sqrt f32" {
    const allocator = std.testing.allocator;
    const source =
        \\start = (x: f32) f32 {
        \\    sqrt(x)
        \\}
    ;
    const actual = try zap.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (func $start (param $x f32) (result f32)
        \\        (f32.sqrt
        \\            (local.get $x)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
