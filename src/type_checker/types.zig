const std = @import("std");
const Allocator = std.mem.Allocator;
const Map = std.AutoHashMap;
const List = std.ArrayList;

const Builtins = @import("../builtins.zig").Builtins;
const interner = @import("../interner.zig");
const Interned = interner.Interned;
const parser = @import("../parser.zig");
pub const Span = parser.types.Span;
const monotype = @import("monotype.zig");
pub const TypeVar = monotype.TypeVar;
pub const MonoType = monotype.MonoType;
pub const Attribute = @import("../tokenizer.zig").types.Attribute;

pub const Substitution = struct {
    map: Map(u64, MonoType),

    pub fn get(self: Substitution, typevar: TypeVar) ?MonoType {
        return self.map.get(typevar.value);
    }

    pub fn getOrPut(self: *Substitution, typevar: TypeVar) !Map(u64, MonoType).GetOrPutResult {
        return try self.map.getOrPut(typevar.value);
    }
};

pub const EqualConstraint = struct {
    left: MonoType,
    right: MonoType,
};

pub const FieldOfConstraint = struct {
    value: MonoType,
    field: MonoType,
    name: Interned,
};

pub const Constraints = struct {
    equal: List(EqualConstraint),
    field_of: List(FieldOfConstraint),
    next_type_var: u64,
};

pub const Binding = struct {
    type: MonoType,
    global: bool,
    mutable: bool,
    span: Span,
};

pub const Scope = Map(Interned, Binding);

pub const Int = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Float = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Symbol = struct {
    value: Interned,
    span: Span,
    type: MonoType,
    binding: Binding,
};

pub const Bool = struct {
    value: bool,
    span: Span,
    type: MonoType,
};

pub const String = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Define = struct {
    name: Symbol,
    value: *const Expression,
    span: Span,
    mutable: bool,
    type: MonoType,
};

pub const Drop = struct {
    value: *const Expression,
    span: Span,
    type: MonoType,
};

pub const PlusEqual = struct {
    name: Symbol,
    value: *const Expression,
    span: Span,
    type: MonoType,
};

pub const TimesEqual = struct {
    name: Symbol,
    value: *const Expression,
    span: Span,
    type: MonoType,
};

pub const Block = struct {
    expressions: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Parameter = struct {
    name: Symbol,
    mutable: bool,
};

pub const Function = struct {
    name: Symbol,
    parameters: []const Parameter,
    return_type: MonoType,
    body: Block,
    span: Span,
    type: MonoType,
};

pub const Prototype = struct {
    name: Symbol,
    parameters: []const Parameter,
    return_type: MonoType,
    span: Span,
    type: MonoType,
};

pub const Dot = struct {
    left: *const Expression,
    right: Symbol,
    span: Span,
    type: MonoType,
};

pub const BinaryOp = struct {
    kind: parser.types.BinaryOpKind,
    left: *const Expression,
    right: *const Expression,
    span: Span,
    type: MonoType,
};

pub const Arm = struct {
    condition: Expression,
    then: Block,
};

pub const Branch = struct {
    arms: []const Arm,
    else_: Block,
    span: Span,
    type: MonoType,
};

pub const Argument = struct {
    value: Expression,
    mutable: bool,
};

pub const Arguments = struct {
    positional: []const Argument,
    named: Map(Interned, Argument),
    named_order: []const Interned,
    span: Span,
};

pub const Call = struct {
    function: *const Expression,
    arguments: Arguments,
    span: Span,
    type: MonoType,
};

pub const Decorator = struct {
    attribute: Attribute,
    arguments: Arguments,
    value: *const Expression,
    span: Span,
    type: MonoType,
};

pub const Intrinsic = struct {
    function: Interned,
    arguments: []const Argument,
    span: Span,
    type: MonoType,
};

pub const Group = struct {
    expressions: []const Expression,
    span: Span,
    type: MonoType,
};

pub const ForeignImport = struct {
    module: Interned,
    name: Interned,
    prototype: Prototype,
    span: Span,
    type: MonoType,
};

pub const ForeignExport = struct {
    name: Interned,
    function: Function,
    span: Span,
    type: MonoType,
};

pub const Convert = struct {
    value: *const Expression,
    span: Span,
    type: MonoType,
};

pub const Undefined = struct {
    span: Span,
    type: MonoType,
};

pub const Field = struct {
    name: Symbol,
    value: Expression,
    span: Span,
};

pub const Fields = Map(Interned, Field);

pub const Array = struct {
    expressions: []const Expression,
    type: MonoType,
    span: Span,
};

pub const Index = struct {
    expression: *const Expression,
    indices: []const Expression,
    type: MonoType,
    span: Span,
};

pub const TemplateLiteral = struct {
    function: ?Symbol,
    strings: []const String,
    arguments: []const Expression,
    type: MonoType,
    span: Span,
};

pub const Expression = union(enum) {
    int: Int,
    float: Float,
    symbol: Symbol,
    bool: Bool,
    string: String,
    define: Define,
    drop: Drop,
    plus_equal: PlusEqual,
    times_equal: TimesEqual,
    function: Function,
    prototype: Prototype,
    binary_op: BinaryOp,
    dot: Dot,
    group: Group,
    block: Block,
    branch: Branch,
    call: Call,
    decorator: Decorator,
    intrinsic: Intrinsic,
    foreign_import: ForeignImport,
    foreign_export: ForeignExport,
    convert: Convert,
    undefined: Undefined,
    array: Array,
    index: Index,
    template_literal: TemplateLiteral,
};

pub const Untyped = Map(Interned, parser.types.Expression);
pub const Typed = Map(Interned, Expression);

pub const Module = struct {
    allocator: Allocator,
    constraints: *Constraints,
    builtins: Builtins,
    order: []const Interned,
    untyped: Untyped,
    typed: Typed,
    scope: Scope,
    foreign_exports: []const Interned,
};
