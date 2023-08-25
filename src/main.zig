const std = @import("std");
const wasmer = @cImport(@cInclude("wasmer.h"));
const Allocator = std.mem.Allocator;
const atom = @import("atom");

const language_name = "atom";
const language_icon = "⚛";
const extension_length = language_name.len + 1;

const List = std.ArrayList;

const Flags = struct {
    file_name: []const u8,
    map: std.StringHashMap(void),

    fn init(allocator: Allocator) !Flags {
        if (std.os.argv.len < 2) {
            std.debug.panic(
                \\---- ERROR - No input file specified --------------------
                \\
                \\Correct usage:
                \\
                \\{s} <input file>.{s}
                \\this will compile and run the {s} program using the wasmer runtime
            , .{ language_name, language_name, language_name });
        }
        const file_name = std.mem.span(std.os.argv[1]);
        var map = std.StringHashMap(void).init(allocator);
        for (std.os.argv[2..]) |flag| try map.putNoClobber(std.mem.span(flag), void{});
        return Flags{ .file_name = file_name, .map = map };
    }

    pub fn contains(self: Flags, flag: []const u8) bool {
        return self.map.contains(flag);
    }
};

fn writeWat(allocator: Allocator, flags: Flags, wat_string: []const u8) !void {
    const file_name_no_suffix = flags.file_name[0 .. flags.file_name.len - extension_length];
    const file_name_wat = try std.fmt.allocPrint(allocator, "{s}.wat", .{file_name_no_suffix});
    const file = try std.fs.cwd().createFile(file_name_wat, .{});
    try file.writer().writeAll(wat_string);
}

fn writeWasm(allocator: Allocator, flags: Flags, wasm_bytes: wasmer.wasm_byte_vec_t) !void {
    const file_name_no_suffix = flags.file_name[0 .. flags.file_name.len - extension_length];
    const file_name_wasm = try std.fmt.allocPrint(allocator, "{s}.wasm", .{file_name_no_suffix});
    const file = try std.fs.cwd().createFile(file_name_wasm, .{});
    const bytes = wasm_bytes.data[0..wasm_bytes.size];
    try file.writer().writeAll(bytes);
}

const Value = union(enum) {
    bool: bool,
    u8: u8,
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
    void,

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        switch (self) {
            .bool => |b| try writer.print("{}", .{b}),
            .u8 => |i| try writer.print("'{c}'", .{i}),
            .i32 => |i| try writer.print("{}", .{i}),
            .i64 => |i| try writer.print("{}", .{i}),
            .f32 => |f| try writer.print("{}", .{f}),
            .f64 => |f| try writer.print("{}", .{f}),
            .void => {},
        }
    }
};

fn wat2wasm(wat_string: []const u8) wasmer.wasm_byte_vec_t {
    var wat: wasmer.wasm_byte_vec_t = undefined;
    wasmer.wasm_byte_vec_new(&wat, wat_string.len, wat_string.ptr);
    var wasm_bytes: wasmer.wasm_byte_vec_t = undefined;
    wasmer.wat2wasm(&wat, &wasm_bytes);
    return wasm_bytes;
}

const WasmModule = struct {
    allocator: Allocator,
    ast: atom.type_checker.types.Module,
    engine: *wasmer.wasm_engine_t,
    store: *wasmer.wasm_store_t,
    module: *wasmer.wasm_module_t,
    instance: *wasmer.wasm_instance_t,
    exports: wasmer.wasm_extern_vec_t,

    fn init(allocator: Allocator, ast: atom.type_checker.types.Module, wasm_bytes: wasmer.wasm_byte_vec_t) WasmModule {
        const engine = wasmer.wasm_engine_new();
        const store = wasmer.wasm_store_new(engine);
        const module = wasmer.wasm_module_new(store, &wasm_bytes);
        if (module == null) std.debug.panic("\nError compiling module!\n", .{});
        const config = wasmer.wasi_config_new("example_program");
        const wasi_env = wasmer.wasi_env_new(store, config);
        if (wasi_env == null) std.debug.panic("\nError building WASI env!\n", .{});
        var imports: wasmer.wasm_extern_vec_t = undefined;
        _ = wasmer.wasi_get_imports(store, wasi_env, module, &imports);
        const instance = wasmer.wasm_instance_new(store, module, &imports, null);
        if (instance == null) std.debug.panic("\nError instantiating module!\n", .{});
        if (!wasmer.wasi_env_initialize_instance(wasi_env, store, instance))
            std.debug.panic("\nError initializing WASI instance!\n", .{});
        var wasm_exports: wasmer.wasm_extern_vec_t = undefined;
        wasmer.wasm_instance_exports(instance, &wasm_exports);
        if (wasm_exports.size == 0) std.debug.panic("\nError getting exports!\n", .{});
        return WasmModule{
            .allocator = allocator,
            .ast = ast,
            .engine = engine.?,
            .store = store.?,
            .module = module.?,
            .instance = instance.?,
            .exports = wasm_exports,
        };
    }

    fn run(self: WasmModule, name: atom.interner.Interned) !Value {
        const func = wasmer.wasi_get_start_function(self.instance);
        if (func == null) std.debug.panic("\nError getting start!\n", .{});
        var args_val = [0]wasmer.wasm_val_t{};
        var results_val = List(wasmer.wasm_val_t).init(self.allocator);
        const exported_function = self.ast.typed.get(name).?.function;
        if (exported_function.parameters.len != 0)
            std.debug.panic("\nOnly functions with no parameters supported!\n", .{});
        const return_type = exported_function.return_type;
        if (return_type != .void) {
            const value = wasmer.wasm_val_t{ .kind = wasmer.WASM_ANYREF, .of = .{ .ref = null } };
            try results_val.append(value);
        }
        var args: wasmer.wasm_val_vec_t = undefined;
        var results: wasmer.wasm_val_vec_t = undefined;
        wasmer.wasm_val_vec_new(&args, 0, &args_val);
        wasmer.wasm_val_vec_new(&results, results_val.items.len, results_val.items.ptr);
        if (wasmer.wasm_func_call(func, &args, &results)) |_| {
            std.debug.panic("\nError calling start!\n", .{});
        }
        switch (return_type) {
            .bool => return .{ .bool = results.data[0].of.i32 == 1 },
            .u8 => return .{ .u8 = @truncate(@as(u32, @intCast(results.data[0].of.i32))) },
            .i32 => return .{ .i32 = results.data[0].of.i32 },
            .i64 => return .{ .i64 = results.data[0].of.i64 },
            .f32 => return .{ .f32 = results.data[0].of.f32 },
            .f64 => return .{ .f64 = results.data[0].of.f64 },
            .void => return .void,
            .structure => return .{ .i32 = results.data[0].of.i32 },
            .array => return .{ .i32 = results.data[0].of.i32 },
            else => |k| std.debug.panic("\nUnsupported return type {}!\n", .{k}),
        }
    }
};

fn compileAndRun(allocator: Allocator, intern: *atom.interner.Intern, errors: *atom.error_reporter.types.Errors, flags: Flags, source: []const u8) !void {
    const builtins = try atom.Builtins.init(intern);
    const tokens = try atom.tokenizer.tokenize(allocator, intern, builtins, source);
    const untyped_ast = try atom.parser.parse(allocator, builtins, tokens);
    var constraints = atom.type_checker.types.Constraints{
        .equal = List(atom.type_checker.types.EqualConstraint).init(allocator),
        .field_of = List(atom.type_checker.types.FieldOfConstraint).init(allocator),
        .next_type_var = 0,
    };
    var ast = try atom.type_checker.infer.module(allocator, &constraints, builtins, untyped_ast);
    const export_count = ast.foreign_exports.len;
    const start = try intern.store("start");
    if (export_count == 0) ast.foreign_exports = &.{start};
    for (ast.foreign_exports) |foreign_export| try atom.type_checker.infer.topLevel(&ast, foreign_export, errors);
    const substitution = try atom.type_checker.solve_constraints.constraints(allocator, constraints, errors);
    ast = try atom.type_checker.apply_substitution.module(allocator, substitution, ast);
    var ir = try atom.code_generator.lower.module(allocator, builtins, ast, intern);
    if (export_count == 0) {
        const alias = try intern.store("_start");
        ir.foreign_exports = &.{.{ .name = start, .alias = alias }};
    }
    var result = List(u8).init(allocator);
    try atom.code_generator.pretty_print.module(ir, result.writer());
    const wat_string = try result.toOwnedSlice();
    const wasm_bytes = wat2wasm(wat_string);
    if (flags.contains("--wat")) {
        try writeWat(allocator, flags, wat_string);
        return;
    }
    if (flags.contains("--wasm")) {
        try writeWasm(allocator, flags, wasm_bytes);
        return;
    }
    if (export_count > 0) {
        try writeWat(allocator, flags, wat_string);
        try writeWasm(allocator, flags, wasm_bytes);
        return;
    }
    const wasm_module = WasmModule.init(allocator, ast, wasm_bytes);
    const value = try wasm_module.run(start);
    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    switch (value) {
        .void => {},
        else => try writer.print("{s} {}", .{ language_icon, value }),
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const flags = try Flags.init(allocator);
    const source = try std.fs.cwd().readFileAlloc(allocator, flags.file_name, std.math.maxInt(usize));
    var intern = atom.interner.Intern.init(allocator);
    var errors = atom.error_reporter.types.Errors{
        .allocator = arena.allocator(),
        .undefined_variable = List(atom.error_reporter.types.UndefinedVariable).init(arena.allocator()),
        .type_mismatch = List(atom.error_reporter.types.TypeMismatch).init(arena.allocator()),
        .mutability_mismatch = List(atom.error_reporter.types.MutabilityMismatch).init(arena.allocator()),
        .reassigning_immutable = List(atom.error_reporter.types.ReassigningImmutable).init(arena.allocator()),
        .source = source,
    };
    compileAndRun(allocator, &intern, &errors, flags, source) catch |e| switch (e) {
        error.CompileError => {
            const stdout = std.io.getStdOut();
            const writer = stdout.writer();
            var result = List(u8).init(allocator);
            try atom.error_reporter.pretty_print.errors(errors, result.writer());
            try writer.print("{s}", .{result.items});
        },
        else => return e,
    };
}
