const std = @import("std");
const atom = @import("atom");

test "tokenize struct" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\fn start() -> Person {
        \\    Person(name="Bob", age=42)
        \\}
    ;
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(keyword struct)
        \\(symbol Person)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol name)
        \\(operator :)
        \\(symbol str)
        \\(delimiter ',')
        \\(new_line)
        \\(symbol age)
        \\(operator :)
        \\(symbol u8)
        \\(delimiter ',')
        \\(new_line)
        \\(delimiter '}')
        \\(new_line)
        \\(keyword fn)
        \\(symbol start)
        \\(delimiter '(')
        \\(delimiter ')')
        \\(operator ->)
        \\(symbol Person)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol Person)
        \\(delimiter '(')
        \\(symbol name)
        \\(operator =)
        \\(string "Bob")
        \\(delimiter ',')
        \\(symbol age)
        \\(operator =)
        \\(int 42)
        \\(delimiter ')')
        \\(new_line)
        \\(delimiter '}')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse struct" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\fn start() -> Person {
        \\    Person(name="Bob", age=42)
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(struct Person
        \\    name str
        \\    age u8)
        \\
        \\(fn start [] Person
        \\    (Person :name "Bob" :age 42))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer struct" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\fn start() -> Person {
        \\    Person(name="Bob", age=42)
        \\}
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = start, type = fn() -> Person }
        \\    return_type = Person
        \\    body =
        \\        call =
        \\            function = symbol{ value = Person, type = Person }
        \\            named_arguments =
        \\                argument =
        \\                    name = name
        \\                    mutable = false
        \\                    value = string{ value = "Bob", type = str }
        \\                argument =
        \\                    name = age
        \\                    mutable = false
        \\                    value = int{ value = 42, type = u8 }
        \\            type = Person
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen struct" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\fn start() -> Person {
        \\    Person(name="Bob", age=42)
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (data (i32.const 0) "Bob")
        \\
        \\    (global $core/arena (mut i32) (i32.const 3))
        \\
        \\    (func $core/alloc (param $size i32) (result i32)
        \\        (local $ptr i32)
        \\        (local.tee $ptr
        \\            (global.get $core/arena))
        \\        (global.set $core/arena
        \\            (i32.add
        \\                (local.get $ptr)
        \\                (local.get $size))))
        \\
        \\    (func $start (result i32)
        \\        (call $Person
        \\            (call $str
        \\                (i32.const 0)
        \\                (i32.const 3))
        \\            (i32.const 42)))
        \\
        \\    (func $Person (param $name i32) (param $age i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 12)))
        \\        (memory.copy
        \\            (local.get $result)
        \\            (local.get $name)
        \\            (i32.const 8))
        \\        (i32.store8
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 8))
        \\            (local.get $age))
        \\        (local.get $result))
        \\
        \\    (func $str (param $ptr i32) (param $len i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (i32.store
        \\            (local.get $result)
        \\            (local.get $ptr))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 4))
        \\            (local.get $len))
        \\        (local.get $result))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse struct field access" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\fn start() -> str {
        \\    person = Person(name="Bob", age=42)
        \\    person.name
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(struct Person
        \\    name str
        \\    age u8)
        \\
        \\(fn start [] str
        \\    (block
        \\        (def person (Person :name "Bob" :age 42))
        \\        (. person name)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer struct field access" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\fn start() -> str {
        \\    person = Person(name="Bob", age=42)
        \\    person.name
        \\}
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = start, type = fn() -> str }
        \\    return_type = str
        \\    body =
        \\        define =
        \\            name = symbol{ value = person, type = Person }
        \\            type = void
        \\            mutable = false
        \\            value =
        \\                call =
        \\                    function = symbol{ value = Person, type = Person }
        \\                    named_arguments =
        \\                        argument =
        \\                            name = name
        \\                            mutable = false
        \\                            value = string{ value = "Bob", type = str }
        \\                        argument =
        \\                            name = age
        \\                            mutable = false
        \\                            value = int{ value = 42, type = u8 }
        \\                    type = Person
        \\        dot =
        \\            left =
        \\                symbol{ value = person, type = Person }
        \\            right = symbol{ value = name, type = str }
        \\            type = str
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen struct field access str" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\fn start() -> str {
        \\    person = Person(name="Bob", age=42)
        \\    person.name
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (data (i32.const 0) "Bob")
        \\
        \\    (global $core/arena (mut i32) (i32.const 3))
        \\
        \\    (func $core/alloc (param $size i32) (result i32)
        \\        (local $ptr i32)
        \\        (local.tee $ptr
        \\            (global.get $core/arena))
        \\        (global.set $core/arena
        \\            (i32.add
        \\                (local.get $ptr)
        \\                (local.get $size))))
        \\
        \\    (func $start (result i32)
        \\        (local $person i32)
        \\        (local.set $person
        \\            (call $Person
        \\                (call $str
        \\                    (i32.const 0)
        \\                    (i32.const 3))
        \\                (i32.const 42)))
        \\        (call $Person/name
        \\            (local.get $person)))
        \\
        \\    (func $Person (param $name i32) (param $age i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 12)))
        \\        (memory.copy
        \\            (local.get $result)
        \\            (local.get $name)
        \\            (i32.const 8))
        \\        (i32.store8
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 8))
        \\            (local.get $age))
        \\        (local.get $result))
        \\
        \\    (func $str (param $ptr i32) (param $len i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (i32.store
        \\            (local.get $result)
        \\            (local.get $ptr))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 4))
        \\            (local.get $len))
        \\        (local.get $result))
        \\
        \\    (func $Person/name (param $p i32) (result i32)
        \\        (local.get $p))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen struct field access i32" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: i32,
        \\}
        \\
        \\fn start() -> i32 {
        \\    person = Person(name="Bob", age=42)
        \\    person.age
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (data (i32.const 0) "Bob")
        \\
        \\    (global $core/arena (mut i32) (i32.const 3))
        \\
        \\    (func $core/alloc (param $size i32) (result i32)
        \\        (local $ptr i32)
        \\        (local.tee $ptr
        \\            (global.get $core/arena))
        \\        (global.set $core/arena
        \\            (i32.add
        \\                (local.get $ptr)
        \\                (local.get $size))))
        \\
        \\    (func $start (result i32)
        \\        (local $person i32)
        \\        (local.set $person
        \\            (call $Person
        \\                (call $str
        \\                    (i32.const 0)
        \\                    (i32.const 3))
        \\                (i32.const 42)))
        \\        (call $Person/age
        \\            (local.get $person)))
        \\
        \\    (func $Person (param $name i32) (param $age i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 12)))
        \\        (memory.copy
        \\            (local.get $result)
        \\            (local.get $name)
        \\            (i32.const 8))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 8))
        \\            (local.get $age))
        \\        (local.get $result))
        \\
        \\    (func $str (param $ptr i32) (param $len i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (i32.store
        \\            (local.get $result)
        \\            (local.get $ptr))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 4))
        \\            (local.get $len))
        \\        (local.get $result))
        \\
        \\    (func $Person/age (param $p i32) (result i32)
        \\        (i32.load
        \\            (i32.add
        \\                (local.get $p)
        \\                (i32.const 8))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen struct field access u8" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\fn start() -> u8 {
        \\    person = Person(name="Bob", age=42)
        \\    person.age
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (data (i32.const 0) "Bob")
        \\
        \\    (global $core/arena (mut i32) (i32.const 3))
        \\
        \\    (func $core/alloc (param $size i32) (result i32)
        \\        (local $ptr i32)
        \\        (local.tee $ptr
        \\            (global.get $core/arena))
        \\        (global.set $core/arena
        \\            (i32.add
        \\                (local.get $ptr)
        \\                (local.get $size))))
        \\
        \\    (func $start (result i32)
        \\        (local $person i32)
        \\        (local.set $person
        \\            (call $Person
        \\                (call $str
        \\                    (i32.const 0)
        \\                    (i32.const 3))
        \\                (i32.const 42)))
        \\        (call $Person/age
        \\            (local.get $person)))
        \\
        \\    (func $Person (param $name i32) (param $age i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 12)))
        \\        (memory.copy
        \\            (local.get $result)
        \\            (local.get $name)
        \\            (i32.const 8))
        \\        (i32.store8
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 8))
        \\            (local.get $age))
        \\        (local.get $result))
        \\
        \\    (func $str (param $ptr i32) (param $len i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (i32.store
        \\            (local.get $result)
        \\            (local.get $ptr))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 4))
        \\            (local.get $len))
        \\        (local.get $result))
        \\
        \\    (func $Person/age (param $p i32) (result i32)
        \\        (i32.load8_u
        \\            (i32.add
        \\                (local.get $p)
        \\                (i32.const 8))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen struct field access i64" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: i64,
        \\}
        \\
        \\fn start() -> i64 {
        \\    person = Person(name="Bob", age=42)
        \\    person.age
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (data (i32.const 0) "Bob")
        \\
        \\    (global $core/arena (mut i32) (i32.const 3))
        \\
        \\    (func $core/alloc (param $size i32) (result i32)
        \\        (local $ptr i32)
        \\        (local.tee $ptr
        \\            (global.get $core/arena))
        \\        (global.set $core/arena
        \\            (i32.add
        \\                (local.get $ptr)
        \\                (local.get $size))))
        \\
        \\    (func $start (result i64)
        \\        (local $person i32)
        \\        (local.set $person
        \\            (call $Person
        \\                (call $str
        \\                    (i32.const 0)
        \\                    (i32.const 3))
        \\                (i64.const 42)))
        \\        (call $Person/age
        \\            (local.get $person)))
        \\
        \\    (func $Person (param $name i32) (param $age i64) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 16)))
        \\        (memory.copy
        \\            (local.get $result)
        \\            (local.get $name)
        \\            (i32.const 8))
        \\        (i64.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 8))
        \\            (local.get $age))
        \\        (local.get $result))
        \\
        \\    (func $str (param $ptr i32) (param $len i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (i32.store
        \\            (local.get $result)
        \\            (local.get $ptr))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 4))
        \\            (local.get $len))
        \\        (local.get $result))
        \\
        \\    (func $Person/age (param $p i32) (result i64)
        \\        (i64.load
        \\            (i32.add
        \\                (local.get $p)
        \\                (i32.const 8))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen struct field access u32" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: u32,
        \\}
        \\
        \\fn start() -> u32 {
        \\    person = Person(name="Bob", age=42)
        \\    person.age
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (data (i32.const 0) "Bob")
        \\
        \\    (global $core/arena (mut i32) (i32.const 3))
        \\
        \\    (func $core/alloc (param $size i32) (result i32)
        \\        (local $ptr i32)
        \\        (local.tee $ptr
        \\            (global.get $core/arena))
        \\        (global.set $core/arena
        \\            (i32.add
        \\                (local.get $ptr)
        \\                (local.get $size))))
        \\
        \\    (func $start (result i32)
        \\        (local $person i32)
        \\        (local.set $person
        \\            (call $Person
        \\                (call $str
        \\                    (i32.const 0)
        \\                    (i32.const 3))
        \\                (i32.const 42)))
        \\        (call $Person/age
        \\            (local.get $person)))
        \\
        \\    (func $Person (param $name i32) (param $age i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 12)))
        \\        (memory.copy
        \\            (local.get $result)
        \\            (local.get $name)
        \\            (i32.const 8))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 8))
        \\            (local.get $age))
        \\        (local.get $result))
        \\
        \\    (func $str (param $ptr i32) (param $len i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (i32.store
        \\            (local.get $result)
        \\            (local.get $ptr))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 4))
        \\            (local.get $len))
        \\        (local.get $result))
        \\
        \\    (func $Person/age (param $p i32) (result i32)
        \\        (i32.load
        \\            (i32.add
        \\                (local.get $p)
        \\                (i32.const 8))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen struct field access u64" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: u64,
        \\}
        \\
        \\fn start() -> u64 {
        \\    person = Person(name="Bob", age=42)
        \\    person.age
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (data (i32.const 0) "Bob")
        \\
        \\    (global $core/arena (mut i32) (i32.const 3))
        \\
        \\    (func $core/alloc (param $size i32) (result i32)
        \\        (local $ptr i32)
        \\        (local.tee $ptr
        \\            (global.get $core/arena))
        \\        (global.set $core/arena
        \\            (i32.add
        \\                (local.get $ptr)
        \\                (local.get $size))))
        \\
        \\    (func $start (result i64)
        \\        (local $person i32)
        \\        (local.set $person
        \\            (call $Person
        \\                (call $str
        \\                    (i32.const 0)
        \\                    (i32.const 3))
        \\                (i64.const 42)))
        \\        (call $Person/age
        \\            (local.get $person)))
        \\
        \\    (func $Person (param $name i32) (param $age i64) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 16)))
        \\        (memory.copy
        \\            (local.get $result)
        \\            (local.get $name)
        \\            (i32.const 8))
        \\        (i64.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 8))
        \\            (local.get $age))
        \\        (local.get $result))
        \\
        \\    (func $str (param $ptr i32) (param $len i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (i32.store
        \\            (local.get $result)
        \\            (local.get $ptr))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 4))
        \\            (local.get $len))
        \\        (local.get $result))
        \\
        \\    (func $Person/age (param $p i32) (result i64)
        \\        (i64.load
        \\            (i32.add
        \\                (local.get $p)
        \\                (i32.const 8))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen struct field access f64" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: f64,
        \\}
        \\
        \\fn start() -> f64 {
        \\    person = Person(name="Bob", age=42)
        \\    person.age
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (data (i32.const 0) "Bob")
        \\
        \\    (global $core/arena (mut i32) (i32.const 3))
        \\
        \\    (func $core/alloc (param $size i32) (result i32)
        \\        (local $ptr i32)
        \\        (local.tee $ptr
        \\            (global.get $core/arena))
        \\        (global.set $core/arena
        \\            (i32.add
        \\                (local.get $ptr)
        \\                (local.get $size))))
        \\
        \\    (func $start (result f64)
        \\        (local $person i32)
        \\        (local.set $person
        \\            (call $Person
        \\                (call $str
        \\                    (i32.const 0)
        \\                    (i32.const 3))
        \\                (f64.const 42)))
        \\        (call $Person/age
        \\            (local.get $person)))
        \\
        \\    (func $Person (param $name i32) (param $age f64) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 16)))
        \\        (memory.copy
        \\            (local.get $result)
        \\            (local.get $name)
        \\            (i32.const 8))
        \\        (f64.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 8))
        \\            (local.get $age))
        \\        (local.get $result))
        \\
        \\    (func $str (param $ptr i32) (param $len i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (i32.store
        \\            (local.get $result)
        \\            (local.get $ptr))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 4))
        \\            (local.get $len))
        \\        (local.get $result))
        \\
        \\    (func $Person/age (param $p i32) (result f64)
        \\        (f64.load
        \\            (i32.add
        \\                (local.get $p)
        \\                (i32.const 8))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen struct field access f32" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Person {
        \\    name: str,
        \\    age: f32,
        \\}
        \\
        \\fn start() -> f32 {
        \\    person = Person(name="Bob", age=42)
        \\    person.age
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (data (i32.const 0) "Bob")
        \\
        \\    (global $core/arena (mut i32) (i32.const 3))
        \\
        \\    (func $core/alloc (param $size i32) (result i32)
        \\        (local $ptr i32)
        \\        (local.tee $ptr
        \\            (global.get $core/arena))
        \\        (global.set $core/arena
        \\            (i32.add
        \\                (local.get $ptr)
        \\                (local.get $size))))
        \\
        \\    (func $start (result f32)
        \\        (local $person i32)
        \\        (local.set $person
        \\            (call $Person
        \\                (call $str
        \\                    (i32.const 0)
        \\                    (i32.const 3))
        \\                (f32.const 42)))
        \\        (call $Person/age
        \\            (local.get $person)))
        \\
        \\    (func $Person (param $name i32) (param $age f32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 12)))
        \\        (memory.copy
        \\            (local.get $result)
        \\            (local.get $name)
        \\            (i32.const 8))
        \\        (f32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 8))
        \\            (local.get $age))
        \\        (local.get $result))
        \\
        \\    (func $str (param $ptr i32) (param $len i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (i32.store
        \\            (local.get $result)
        \\            (local.get $ptr))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 4))
        \\            (local.get $len))
        \\        (local.get $result))
        \\
        \\    (func $Person/age (param $p i32) (result f32)
        \\        (f32.load
        \\            (i32.add
        \\                (local.get $p)
        \\                (i32.const 8))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
