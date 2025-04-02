const std = @import("std");
const builtin = std.builtin;
const TypeInfo = builtin.Type;
const Declaration = TypeInfo.Declaration;
const warn = std.debug.print;

// Provided generators
pub const C_Generator = @import("generators/c.zig").C_Generator;
pub const Python_Generator = @import("generators/python.zig").Python_Generator;
pub const Ordered_Generator = @import("generators/ordered.zig").Ordered_Generator;

const GeneratorInterface = struct {
    fn init() void {}
    fn deinit() void {}
    fn gen_func() void {}
    fn gen_struct() void {}
    fn gen_enum() void {}
    fn gen_union() void {}
};

fn includeSymbol(comptime decl: Declaration) bool {
    if (decl.data == .Type) {
        const T = decl.data.Type;
        const info = @typeInfo(T);

        return switch (info) {
            .@"struct" => |s| s.layout == .Extern or s.layout == .Packed,
            .@"union" => |u| u.layout == .Extern,
            .@"enum" => |e| e.layout == .Extern,
            else => false,
        };
    }

    return false;
}

fn validateGenerator(comptime Generator: type) void {
    comptime {
        const interface = @typeInfo(GeneratorInterface).@"struct".decls;

        for (interface) |decl| {
            if (@hasDecl(Generator, decl.name) == false) {
                @compileError("Generator: '" ++
                    @typeName(Generator) ++
                    "' is missing function: " ++
                    decl.name);
            }
        }
    }
}

pub fn HeaderGen(comptime S: type, comptime libname: []const u8, comptime libdir: []const u8) type {
    const all_decls: []const Declaration = @typeInfo(S).@"struct".decls;

    return struct {
        decls: @TypeOf(all_decls) = all_decls,
        source_file: []const u8 = libname ++ ".zig",

        const Self = @This();

        pub fn init() Self {
            return Self{};
        }

        pub fn exec(comptime self: Self, comptime Generator: type) void {
            validateGenerator(Generator);

            var cwd = std.fs.cwd();
            cwd.makeDir(libdir) catch |e| switch (e) {
                error.PathAlreadyExists => {},
                else => @panic("Failed to init headers folder"),
            };

            var hdr_dir = cwd.openDir(libdir, .{}) catch @panic("Failed to open header dir");
            defer hdr_dir.close();

            var gen = Generator.init(self.source_file, &hdr_dir);
            defer gen.deinit();

            inline for (self.decls) |decl| {
                comptime var info = @typeInfo(@TypeOf(@field(S, decl.name)));
                if (info == .type) {
                    info = @typeInfo(@field(S, decl.name));
                }
                // iterate exported fns
                switch (info) {
                    .@"fn" => {
                        const func = info.@"fn";
                        gen.gen_func(decl.name, func, false);
                        // iterate exported structs
                    },
                    .@"struct" => {
                        const layout = info.@"struct".layout;
                        if (layout == .@"extern" or layout == .@"packed") {
                            gen.gen_struct(decl.name, info.@"struct");
                        }
                    },
                    .@"union" => {
                        const layout = info.@"union".layout;
                        if (layout == .@"extern") {
                            gen.gen_union(decl.name, info.@"union");
                        }
                        // iterate exported enums
                        // do this first in case target lang needs enums defined before use
                    },
                    .@"enum" => {
                        gen.gen_enum(decl.name, info.@"enum");
                    },
                    else => {},
                }
            }
        }
    };
}
