const std = @import("std");
const Dir = std.fs.Dir;
const FnMeta = std.builtin.Type.Fn;
const FnDecl = std.builtin.Type.Declaration;
const StructMeta = std.builtin.Type.Struct;
const EnumMeta = std.builtin.Type.Enum;
const UnionMeta = std.builtin.Type.Union;
const warn = std.debug.print;

const Language = enum {
    C,
    Cpp,
};

pub fn C_Generator(comptime lang: Language) type {
    return struct {
        file: std.fs.File,
        currently_inside_args: bool,

        const Self = @This();

        pub fn init(comptime src_file: []const u8, dst_dir: *Dir) Self {
            const file = dst_dir.createFile(comptime filebase(src_file) ++ if (lang == .C) ".h" else ".hpp", .{}) catch
                @panic("Failed to create header file for source: " ++ src_file);

            var res = Self{ .file = file, .currently_inside_args = false };

            // write the header's header, lol
            res.write("#ifndef _" ++ comptime filebase(src_file) ++ "_H\n\n#define _" ++ filebase(src_file) ++ "_H\n");
            if (lang == .C) {
                res.write("#include <stddef.h>\n#include <stdint.h>\n#include <stdbool.h>\n\n");
            } else {
                res.write("#include <cstddef>\n#include <cstdint>\n#include <cstdbool>\n\n");
            }

            return res;
        }

        fn filebase(src_file: []const u8) []const u8 {
            const filebaseext = std.fs.path.basename(src_file);
            return filebaseext[0 .. filebaseext.len - 4];
        }

        pub fn deinit(self: *Self) void {
            self.write("\n#endif\n");
            self.file.close();
        }

        pub fn gen_func(self: *Self, comptime name: []const u8, comptime meta: FnMeta, pointer: bool) void {
            switch (meta.calling_convention) {
                .naked => self.write("__attribute__((naked)) "),
                .x86_stdcall => self.write("__attribute__((stdcall)) "),
                .x86_fastcall => self.write("__attribute__((fastcall)) "),
                .x86_thiscall => self.write("__attribute__((thiscall)) "),
                else => {},
            }
            if (!pointer) {
                self.write("extern ");
                if (lang == .Cpp) {
                    self.write("\"C\" ");
                }
                self.writeType(meta.return_type.?);
                self.write(" ");
            }
            self.write(name ++ "(");

            self.currently_inside_args = true;
            inline for (meta.params, 0..) |arg, i| {
                self.writeType(arg.type.?);
                //TODO: Figure out how to get arg names; for now just do arg0..argN
                _ = self.file.writer().print(" arg{}", .{i}) catch unreachable;
                if (i != meta.params.len - 1)
                    self.write(", ");
            }
            self.currently_inside_args = false;

            self.write(")");
            if (!pointer) {
                self.write(";\n\n");
            }
        }

        pub fn gen_struct(self: *Self, comptime name: []const u8, comptime meta: StructMeta) void {
            self.write("typedef struct ");

            if (meta.layout == .@"packed")
                self.write("__attribute__((__packed__)) ");

            self.write(name ++ " {\n");

            inline for (meta.fields) |field| {
                self.write("   ");

                const info = @typeInfo(field.type);
                if (info == .@"fn") {
                    continue; //skip struct functions
                }
                comptime var inner = info;
                inline while (inner == .optional or inner == .pointer) {
                    if (inner == .optional) {
                        inner = @typeInfo(inner.optional.child);
                    } else {
                        inner = @typeInfo(inner.pointer.child);
                    }
                }
                if (inner == .@"fn") {
                    self.writeType(inner.@"fn".return_type.?);
                    self.write(" (*" ++ field.name ++ ")");
                    self.writeType(field.type); //writes just arguments
                } else {
                    if (info == .array) {
                        self.writeType(info.array.child);
                    } else {
                        self.writeType(field.type);
                    }

                    self.write(" " ++ field.name);
                }

                if (info == .array) {
                    _ = self.file.writer().print("[{}]", .{info.array.len}) catch unreachable;
                }

                self.write(";\n");
            }
            self.write("} " ++ name ++ "_t;\n\n");
        }

        pub fn gen_enum(self: *Self, comptime name: []const u8, comptime meta: EnumMeta) void {
            self.write("enum " ++ name ++ " {\n");

            comptime var last = 0;
            inline for (meta.fields, 0..) |field, i| {
                self.write("    " ++ field.name);

                // if field value is unexpected/custom, manually define it
                if ((i == 0 and field.value != 0) or (i > 0 and field.value > last + 1)) {
                    _ = self.file.writer().print(" = {}", .{field.value}) catch unreachable;
                }

                self.write(",\n");

                last = field.value;
            }

            self.write("};\n\n");
        }

        pub fn gen_union(self: *Self, comptime name: []const u8, comptime meta: UnionMeta) void {
            self.write("typedef union ");

            self.write(name ++ " {\n");

            inline for (meta.fields) |field| {
                self.write("   ");
                self.writeType(field.type);
                self.write(" " ++ field.name ++ ";\n");
            }
            self.write("} " ++ name ++ "_t;\n\n");
        }

        fn writeType(self: *Self, comptime T: type) void {
            switch (T) {
                anyopaque => self.write("void"),
                void => self.write("void"),
                bool => self.write("bool"),
                usize => self.write("size_t"),
                isize => self.write("int"),
                u8 => self.write("uint8_t"),
                u16 => self.write("uint16_t"),
                u32 => self.write("uint32_t"),
                u64 => self.write("uint64_t"),
                i8 => self.write("int8_t"),
                i16 => self.write("int16_t"),
                i24 => self.write("int24_t"),
                i32 => self.write("int32_t"),
                i64 => self.write("int64_t"),
                [*]bool => self.write("bool*"),
                [*]usize => self.write("size_t*"),
                [*]isize => self.write("int*"),
                [*]u8 => self.write("uint8_t*"),
                [*]u16 => self.write("uint16_t*"),
                [*]u32 => self.write("uint32_t*"),
                [*]u64 => self.write("uint64_t*"),
                [*]i8 => self.write("int8_t*"),
                [*]i16 => self.write("int16_t*"),
                [*]i32 => self.write("int32_t*"),
                [*]i64 => self.write("int64_t*"),
                else => {
                    const meta = @typeInfo(T);
                    switch (meta) {
                        .pointer => {
                            const child = meta.pointer.child;
                            const childmeta = @typeInfo(child);
                            if (childmeta == .@"struct" and childmeta.@"struct".layout != .Extern) {
                                self.write("void");
                                self.write("*");
                            } else {
                                if (childmeta == .@"fn") {
                                    self.writeType(child);
                                } else {
                                    self.writeType(child);
                                    self.write("*");
                                }
                            }
                        },
                        .optional => self.writeType(meta.optional.child),
                        .array => @compileError("Handle goofy looking C Arrays in the calling function"),
                        .@"fn" => gen_func(self, "", meta.@"fn", true),
                        else => {
                            const fully_qualified = @typeName(T);
                            var iterator = std.mem.splitBackwardsScalar(u8, fully_qualified, '.');
                            const name = iterator.next().?;
                            if (meta == .@"struct") {
                                if (self.currently_inside_args) {
                                    self.write("struct ");
                                    self.write(name);
                                } else {
                                    self.write(name);
                                    self.write("_t");
                                }
                            } else {
                                self.write(name);
                            }
                        },
                    }
                },
            }
        }

        fn write(self: *Self, str: []const u8) void {
            _ = self.file.writeAll(str) catch unreachable;
        }
    };
}
