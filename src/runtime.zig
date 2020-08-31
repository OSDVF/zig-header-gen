const std = @import("std");
const Allocator = std.mem.Allocator;

pub const dd: i32 = 2;

pub const TypeId = @TagType(TypeInfo);

pub const TypeInfo = union(enum) {
    Type: void,
    Void: void,
    Bool: void,
    NoReturn: void,
    Int: Int,
    Float: Float,
    Pointer: Pointer,
    Array: Array,
    Struct: Struct,
    ComptimeFloat: void,
    ComptimeInt: void,
    Undefined: void,
    Null: void,
    Optional: Optional,
    ErrorUnion: ErrorUnion,
    ErrorSet: ErrorSet,
    Enum: Enum,
    Union: Union,
    Fn: Fn,
    BoundFn: Fn,
    Opaque: void,
    Frame: Frame,
    AnyFrame: AnyFrame,
    Vector: Vector,
    EnumLiteral: void,

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Int = struct {
        is_signed: bool,
        bits: i32,

        pub fn init(comptime m: std.builtin.TypeInfo.Int, comptime depth: i32) Int {
            return comptime .{
                .is_signed = m.is_signed,
                .bits = m.bits,
            };
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Float = struct {
        bits: i32,

        pub fn init(comptime m: std.builtin.TypeInfo.Float, comptime depth: i32) Float {
            return comptime .{
                .bits = m.bits,
            };
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Pointer = struct {
        size: Size,
        is_const: bool,
        is_volatile: bool,
        alignment: i32,
        child: *const TypeInfo,
        is_allowzero: bool,
        /// This field is an optional type.
        /// The type of the sentinel is the element type of the pointer, which is
        /// the value of the `child` field in this struct. However there is no way
        /// to refer to that type here, so we use `var`.
        // sentinel: var,
        /// This data structure is used by the Zig language code generation and
        /// therefore must be kept in sync with the compiler implementation.
        pub const Size = enum {
            One,
            Many,
            Slice,
            C,
        };

        pub fn init(comptime m: std.builtin.TypeInfo.Pointer, comptime depth: i32) Pointer {
            return comptime .{
                .size = @intToEnum(TypeInfo.Pointer.Size, @enumToInt(m.size)),
                .is_const = m.is_const,
                .is_volatile = m.is_volatile,
                .alignment = m.alignment,
                .child = &TypeInfo.init(m.child, depth),
                .is_allowzero = m.is_allowzero,
            };
        }

        pub fn deinit(self: *const Pointer, allocator: *Allocator) void {
            self.child.deinit(allocator);

            allocator.destroy(self.child);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Array = struct {
        len: i32,
        child: *const TypeInfo,
        /// This field is an optional type.
        /// The type of the sentinel is the element type of the array, which is
        /// the value of the `child` field in this struct. However there is no way
        /// to refer to that type here, so we use `var`.
        // sentinel: var,
        pub fn init(comptime m: std.builtin.TypeInfo.Array, comptime depth: i32) Array {
            return comptime .{
                .len = m.len,
                .child = &TypeInfo.init(m.child, depth),
            };
        }

        pub fn deinit(self: *const Array, allocator: *Allocator) void {
            self.child.deinit(allocator);

            allocator.destroy(self.child);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const ContainerLayout = enum {
        Auto,
        Extern,
        Packed,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const StructField = struct {
        name: []const u8,
        field_type: *const TypeInfo,
        // default_value: var,

        pub fn init(comptime f: std.builtin.TypeInfo.StructField, comptime depth: i32) StructField {
            return comptime .{
                .name = f.name,
                .field_type = &TypeInfo.init(f.field_type, depth),
            };
        }

        pub fn deinit(self: *const StructField, allocator: *Allocator) void {
            allocator.free(self.name);

            self.field_type.deinit(allocator);

            allocator.destroy(self.field_type);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Struct = struct {
        name: ?[]const u8,
        layout: ContainerLayout,
        fields: []const StructField,
        decls: []const Declaration,

        pub fn init(comptime m: std.builtin.TypeInfo.Struct, comptime name: []const u8, comptime depth: i32) Struct {
            return comptime .{
                .name = name,
                .layout = @intToEnum(TypeInfo.ContainerLayout, @enumToInt(m.layout)),
                .fields = fields: {
                    comptime var arr: [m.fields.len]StructField = undefined;

                    inline for (m.fields) |f, i| {
                        arr[i] = StructField.init(f, depth - 1);
                    }

                    break :fields &arr;
                },
                .decls = decls: {
                    comptime var arr: [m.decls.len]Declaration = undefined;

                    inline for (m.decls) |f, i| {
                        arr[i] = Declaration.init(f, depth - 1);
                    }

                    break :decls &arr;
                },
            };
        }

        pub fn deinit(self: *const Struct, allocator: *Allocator) void {
            for (self.fields) |f| f.deinit(allocator);
            for (self.decls) |f| f.deinit(allocator);

            allocator.free(self.fields);
            allocator.free(self.decls);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Optional = struct {
        child: *const TypeInfo,

        pub fn init(comptime m: std.builtin.TypeInfo.Optional, comptime depth: i32) Optional {
            return comptime .{
                .child = &TypeInfo.init(m.child, depth),
            };
        }

        pub fn deinit(self: *const Optional, allocator: *Allocator) void {
            self.child.deinit(allocator);

            allocator.destroy(self.child);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const ErrorUnion = struct {
        error_set: *const TypeInfo,
        payload: *const TypeInfo,

        pub fn init(comptime m: std.builtin.TypeInfo.ErrorUnion, comptime depth: i32) ErrorUnion {
            return comptime .{
                .error_set = &TypeInfo.init(m.error_set, depth),
                .payload = &TypeInfo.init(m.payload, depth),
            };
        }

        pub fn deinit(self: *const ErrorUnion, allocator: *Allocator) void {
            self.error_set.deinit(allocator);
            allocator.destroy(self.error_set);

            self.payload.deinit(allocator);
            allocator.destroy(self.payload);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Error = struct {
        name: []const u8,

        pub fn deinit(self: *const Error, allocator: *Allocator) void {
            allocator.free(self.name);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const ErrorSet = ?[]const Error;

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const EnumField = struct {
        name: []const u8,
        value: i32,

        pub fn init(comptime f: std.builtin.TypeInfo.EnumField, comptime depth: i32) EnumField {
            return comptime .{
                .name = f.name,
                .value = f.value,
            };
        }

        pub fn deinit(self: *const EnumField, allocator: *Allocator) void {
            allocator.free(self.name);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Enum = struct {
        name: ?[]const u8,
        layout: ContainerLayout,
        tag_type: *const TypeInfo,
        fields: []const EnumField,
        decls: []const Declaration,
        is_exhaustive: bool,

        pub fn init(comptime m: std.builtin.TypeInfo.Enum, comptime name: []const u8, comptime depth: i32) Enum {
            return comptime .{
                .name = name,
                .layout = @intToEnum(TypeInfo.ContainerLayout, @enumToInt(m.layout)),
                .tag_type = &TypeInfo.init(m.tag_type, depth),
                .fields = fields: {
                    comptime var arr: [m.fields.len]EnumField = undefined;

                    inline for (m.fields) |f, i| {
                        arr[i] = EnumField.init(f, depth - 1);
                    }

                    break :fields &arr;
                },
                .decls = decls: {
                    comptime var arr: [m.decls.len]Declaration = undefined;

                    inline for (m.decls) |f, i| {
                        arr[i] = Declaration.init(f, depth - 1);
                    }

                    break :decls &arr;
                },
                .is_exhaustive = m.is_exhaustive,
            };
        }

        pub fn deinit(self: *const Enum, allocator: *Allocator) void {
            for (self.fields) |f| f.deinit(allocator);
            for (self.decls) |f| f.deinit(allocator);

            allocator.free(self.fields);
            allocator.free(self.decls);

            self.tag_type.deinit(allocator);
            allocator.destroy(self.tag_type);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const UnionField = struct {
        name: []const u8,
        enum_field: ?EnumField,
        field_type: *const TypeInfo,

        pub fn init(comptime f: std.builtin.TypeInfo.UnionField, comptime depth: i32) UnionField {
            return comptime .{
                .name = f.name,
                .enum_field = if (f.enum_field) |ef|
                    .{
                        .name = ef.name,
                        .value = ef.value,
                    }
                else
                    null,
                .field_type = &TypeInfo.init(f.field_type, depth),
            };
        }

        pub fn deinit(self: *const UnionField, allocator: *Allocator) void {
            allocator.free(self.name);

            self.field_type.deinit(allocator);

            allocator.destroy(self.field_type);

            if (self.enum_field) |ef| {
                ef.deinit(allocator);
            }
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Union = struct {
        name: ?[]const u8,
        layout: ContainerLayout,
        tag_type: ?*const TypeInfo,
        fields: []const UnionField,
        decls: []const Declaration,

        pub fn init(comptime m: std.builtin.TypeInfo.Union, comptime name: []const u8, comptime depth: i32) Union {
            return comptime .{
                .name = name,
                .layout = @intToEnum(TypeInfo.ContainerLayout, @enumToInt(m.layout)),
                .tag_type = if (m.tag_type) |t| &TypeInfo.init(t, depth) else null,
                .fields = fields: {
                    comptime var arr: [m.fields.len]UnionField = undefined;

                    inline for (m.fields) |f, i| {
                        arr[i] = UnionField.init(f, depth - 1);
                    }

                    break :fields &arr;
                },
                .decls = decls: {
                    comptime var arr: [m.decls.len]Declaration = undefined;

                    inline for (m.decls) |f, i| {
                        arr[i] = Declaration.init(f, depth - 1);
                    }

                    break :decls &arr;
                },
            };
        }

        pub fn deinit(self: *const Union, allocator: *Allocator) void {
            for (self.fields) |f| f.deinit(allocator);
            for (self.decls) |f| f.deinit(allocator);

            allocator.free(self.fields);
            allocator.free(self.decls);

            if (self.tag_type) |tag_type| {
                tag_type.deinit(allocator);

                allocator.destroy(tag_type);
            }
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const FnArg = struct {
        is_generic: bool,
        is_noalias: bool,
        arg_type: ?*const TypeInfo,

        pub fn init(comptime f: std.builtin.TypeInfo.FnArg, comptime depth: i32) FnArg {
            return comptime .{
                .is_generic = f.is_generic,
                .is_noalias = f.is_noalias,
                .arg_type = if (f.arg_type) |t| &TypeInfo.init(t, depth) else null,
            };
        }

        pub fn deinit(self: *const FnArg, allocator: *Allocator) void {
            if (self.arg_type) |t| {
                t.deinit(allocator);

                allocator.destroy(self.arg_type);
            }
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Fn = struct {
        calling_convention: CallingConvention,
        is_generic: bool,
        is_var_args: bool,
        return_type: ?*const TypeInfo,
        args: []const FnArg,

        pub fn init(comptime m: std.builtin.TypeInfo.Fn, comptime depth: i32) Fn {
            return comptime .{
                .calling_convention = @intToEnum(CallingConvention, @enumToInt(m.calling_convention)),
                .is_generic = m.is_generic,
                .is_var_args = m.is_var_args,
                .return_type = if (m.return_type) |t| &TypeInfo.init(t, depth) else null,
                .args = args: {
                    comptime var arr: [m.args.len]FnArg = undefined;

                    inline for (m.args) |f, i| {
                        arr[i] = FnArg.init(f, depth);
                    }

                    break :args &arr;
                },
            };
        }

        pub fn deinit(self: *const Fn, allocator: *Allocator) void {
            if (self.return_type) |r| {
                r.deinit(allocator);

                allocator.destroy(r);
            }

            for (self.args) |arg| arg.deinit(allocator);

            allocator.free(self.args);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Frame = struct {
        // function: var,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const AnyFrame = struct {
        child: ?*const TypeInfo,

        pub fn init(comptime m: std.builtin.TypeInfo.AnyFrame, comptime depth: i32) AnyFrame {
            return comptime .{
                .child = if (m.child) |t| &TypeInfo.init(t, depth) else null,
            };
        }

        pub fn deinit(self: *const AnyFrame, allocator: *Allocator) void {
            if (self.child) |child| {
                child.deinit(allocator);

                allocator.destroy(child);
            }
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Vector = struct {
        len: i32,
        child: *const TypeInfo,

        pub fn init(comptime m: std.builtin.TypeInfo.Vector, comptime depth: i32) Vector {
            return comptime .{
                .len = m.len,
                .child = &TypeInfo.init(m.child, depth),
            };
        }

        pub fn deinit(self: *const Vector, allocator: *Allocator) void {
            self.child.deinit(allocator);

            allocator.destroy(self.child);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Declaration = struct {
        name: []const u8,
        is_pub: bool,
        data: Data,

        pub fn init(comptime f: std.builtin.TypeInfo.Declaration, comptime depth: i32) Declaration {
            return comptime .{
                .name = f.name,
                .is_pub = f.is_pub,
                .data = Data.init(f.data, depth),
            };
        }

        pub fn deinit(self: *const Declaration, allocator: *Allocator) void {
            self.data.deinit(allocator);

            allocator.free(self.name);
        }

        /// This data structure is used by the Zig language code generation and
        /// therefore must be kept in sync with the compiler implementation.
        pub const Data = union(enum) {
            Type: *const TypeInfo,
            Var: *const TypeInfo,
            Fn: FnDecl,

            pub fn init(comptime d: std.builtin.TypeInfo.Declaration.Data, comptime depth: i32) Data {
                return comptime switch (d) {
                    .Type => |t| .{ .Type = &TypeInfo.init(t, depth) },
                    .Var => |t| .{
                        .Var = &TypeInfo.init(t, depth),
                    },
                    .Fn => |t| .{ .Fn = FnDecl.init(t, depth) },
                };
            }

            /// This data structure is used by the Zig language code generation and
            /// therefore must be kept in sync with the compiler implementation.
            pub const FnDecl = struct {
                fn_type: *const TypeInfo,
                inline_type: Inline,
                is_var_args: bool,
                is_extern: bool,
                is_export: bool,
                lib_name: ?[]const u8,
                return_type: *const TypeInfo,
                arg_names: []const []const u8,

                /// This data structure is used by the Zig language code generation and
                /// therefore must be kept in sync with the compiler implementation.
                pub const Inline = enum {
                    Auto,
                    Always,
                    Never,
                };

                pub fn init(comptime t: std.builtin.TypeInfo.Declaration.Data.FnDecl, comptime depth: i32) FnDecl {
                    return comptime .{
                        .fn_type = &TypeInfo.init(t.fn_type, depth),
                        .inline_type = @intToEnum(TypeInfo.Declaration.Data.FnDecl.Inline, @enumToInt(t.inline_type)),
                        .is_var_args = t.is_var_args,
                        .is_extern = t.is_extern,
                        .is_export = t.is_export,
                        .lib_name = t.lib_name,
                        .return_type = &TypeInfo.init(t.return_type, depth),
                        .arg_names = t.arg_names,
                    };
                }

                pub fn deinit(self: *const FnDecl, allocator: *Allocator) void {
                    self.fn_type.deinit(allocator);
                    self.return_type.deinit(allocator);

                    allocator.destroy(self.fn_type);
                    allocator.destroy(self.return_type);

                    for (self.arg_names) |a| allocator.free(a);
                    allocator.free(self.arg_names);

                    if (self.lib_name) |lib_name| {
                        allocator.free(lib_name);
                    }
                }
            };

            pub fn deinit(self: *const Data, allocator: *Allocator) void {
                switch (self.*) {
                    .Type, .Var => |t| {
                        t.deinit(allocator);

                        allocator.destroy(t);
                    },
                    .Fn => |f| f.deinit(allocator),
                }
            }
        };
    };

    pub fn init(comptime builtin: type, comptime depth: i32) TypeInfo {
        if (depth <= 0) return .{ .Opaque = {} };

        comptime const info = @typeInfo(builtin);

        return comptime switch (info) {
            .Type => .{ .Type = {} },
            .Void => .{ .Void = {} },
            .Bool => .{ .Bool = {} },
            .NoReturn => .{ .NoReturn = {} },
            .Int => |m| .{ .Int = Int.init(m, depth) },
            .Float => |m| .{ .Float = Float.init(m, depth) },
            .Pointer => |m| .{ .Pointer = Pointer.init(m, depth) },
            .Array => |m| .{ .Array = Array.init(m, depth) },
            .Struct => |m| .{ .Struct = Struct.init(m, @typeName(builtin), depth) },
            .ComptimeFloat => .{ .ComptimeFloat = {} },
            .ComptimeInt => .{ .ComptimeInt = {} },
            .Undefined => .{ .Undefined = {} },
            .Null => .{ .Null = {} },
            .Optional => |m| .{ .Optional = Optional.init(m, depth) },
            .ErrorUnion => |m| .{ .ErrorUnion = ErrorUnion.init(m, depth) }, // TODO
            .ErrorSet => |m| .{
                .ErrorSet = errorset: {
                    if (m == null) return null;

                    comptime var arr: [m.?.len]Error = undefined;

                    inline for (m.?) |f, i| {
                        arr[i] = .{
                            .name = f.name,
                        };
                    }

                    break :errorset &arr;
                },
            },
            .Enum => |m| .{ .Enum = Enum.init(m, @typeName(builtin), depth) },
            .Union => |m| .{ .Union = Union.init(m, @typeName(builtin), depth) },
            .Fn => |m| .{ .Fn = Fn.init(m, depth) },
            .BoundFn => |m| .{ .BoundedFn = Fn.init(m, depth) },
            .Opaque => .{ .Opaque = {} },
            .Frame => .{ .Frame = {} }, // TODO
            .AnyFrame => |m| .{ .AnyFrame = AnyFrame.init(m, depth) },
            .Vector => |m| .{ .Vector = Vector.init(m, depth) },
            .EnumLiteral => .{ .EnumLiteral = {} },
        };
    }

    pub fn deinit(self: *TypeInfo, allocator: *Allocator) void {
        switch (self.*) {
            .Array => |a| a.deinit(allocator),
            .Pointer => |p| p.deinit(allocator),
            .Struct => |s| s.deinit(allocator),
            .Union => |u| u.deinit(allocator),
            .Enum => |e| e.deinit(allocator),
            .Optional => |o| o.deinit(allocator),
            .Fn => |f| f.deinit(allocator),
            .ErrorUnion => |e| e.deinit(allocator),
            .ErrorSet => |maybe_set| {
                if (maybe_set) |set| {
                    for (set) |err| err.deinit(allocator);

                    allocator.free(set);
                }
            },
            .AnyFrame => |a| a.deinit(allocator),
            .Vector => |v| v.deinit(allocator),
            else => {},
        }
    }
};

pub const CallingConvention = enum {
    Unspecified,
    C,
    Cold,
    Naked,
    Async,
    Interrupt,
    Signal,
    Stdcall,
    Fastcall,
    Vectorcall,
    Thiscall,
    APCS,
    AAPCS,
    AAPCSVFP,
};

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const talloc = std.testing.allocator;

// TODO .Type

test "Runtime TypeInfo.Void" {
    var info_void = TypeInfo.init(void, dd);
    expect(info_void == .Void);
}

test "Runtime TypeInfo.Bool" {
    var info_bool = TypeInfo.init(bool, dd);
    expect(info_bool == .Bool);
}

// TODO .NoReturn

test "Runtime TypeInfo.Int" {
    var info_i32 = TypeInfo.init(i32, dd);
    expect(info_i32 == .Int);
    expectEqual(@as(i32, 32), info_i32.Int.bits);
    expectEqual(true, info_i32.Int.is_signed);
}

test "Runtime TypeInfo.Float" {
    var info_f64 = TypeInfo.init(f64, dd);
    expect(info_f64 == .Float);
    expectEqual(@as(i32, 64), info_f64.Float.bits);
}

test "Runtime TypeInfo.Pointer" {
    var info_pointer_f64 = TypeInfo.init(*f64, dd);
    expect(info_pointer_f64 == .Pointer);
    expectEqual(TypeInfo.Pointer.Size.One, info_pointer_f64.Pointer.size);
    expectEqual(false, info_pointer_f64.Pointer.is_const);
    expectEqual(false, info_pointer_f64.Pointer.is_volatile);
    expectEqual(@as(i32, 8), info_pointer_f64.Pointer.alignment);
    expect(info_pointer_f64.Pointer.child.* == .Float);
    expectEqual(false, info_pointer_f64.Pointer.is_allowzero);

    var info_pointer_many = TypeInfo.init([*]f64, dd);
    expect(info_pointer_many == .Pointer);
    expectEqual(TypeInfo.Pointer.Size.Many, info_pointer_many.Pointer.size);
    expectEqual(false, info_pointer_many.Pointer.is_const);
    expectEqual(false, info_pointer_many.Pointer.is_volatile);
    expectEqual(@as(i32, 8), info_pointer_many.Pointer.alignment);
    expect(info_pointer_many.Pointer.child.* == .Float);
    expectEqual(false, info_pointer_many.Pointer.is_allowzero);
}

test "Runtime TypeInfo.Array" {
    var info_array = TypeInfo.init([2]i32, dd);
    expect(info_array == .Array);
    expectEqual(@as(i32, 2), info_array.Array.len);
    expect(info_array.Array.child.* == .Int);
}

test "Runtime TypeInfo.Struct" {
    const FooStruct = struct {
        int: i32,

        pub fn bar() void {}
    };

    var info_struct = TypeInfo.init(FooStruct, dd);
    expect(info_struct == .Struct);
    expect(info_struct.Struct.layout == .Auto);
    expectEqual(@as(usize, 1), info_struct.Struct.fields.len);
    expectEqualStrings("int", info_struct.Struct.fields[0].name);
    expect(info_struct.Struct.fields[0].field_type.* == .Int);
}

test "Runtime TypeInfo.ComptimeFloat" {
    var info_comptime_float = TypeInfo.init(comptime_float, dd);
    expect(info_comptime_float == .ComptimeFloat);
}

test "Runtime TypeInfo.ComptimeInt" {
    var info_comptime_int = TypeInfo.init(comptime_int, dd);
    expect(info_comptime_int == .ComptimeInt);
}

// // TODO .Undefined
// // TODO .Null

test "Runtime TypeInfo.Optional" {
    var info_optional = TypeInfo.init(?i32, dd);
    expect(info_optional == .Optional);
    expect(info_optional.Optional.child.* == .Int);
}

// // TODO .ErrorUnion
// // TODO .ErrorSet

test "Runtime TypeInfo.Enum" {
    const FooEnum = enum {
        Foo, Bar
    };

    var info_enum = TypeInfo.init(FooEnum, dd);
    expect(info_enum == .Enum);
}

test "Runtime TypeInfo.Union" {
    const FooUnion = union {
        Foo: void, Bar: i32
    };

    var info_union = TypeInfo.init(FooUnion, dd);
    expect(info_union == .Union);
}

test "Runtime TypeInfo.Fn" {
    // .Fn
    var info_fn = TypeInfo.init(fn () void, dd);
    expect(info_fn == .Fn);
}

test "Runtime TypeInfo.Struct declarations" {
    // .Fn
    var info_fn = TypeInfo.init(struct {
        const WackType = packed struct {
            mr_field: *LameType, ola: u8
        };

        const LameType = struct {
            blah: **WackType,
        };

        pub fn thing(one: usize, two: *LameType, three: [*]u16) bool {
            return one == 1;
        }
    }, dd);
    expect(info_fn == .Struct);
}

// TODO .BoundFn
// TODO .Opaque
// TODO .Frame
// TODO .AnyFrame
// TODO .Vector
// TODO .EnumLiteral
