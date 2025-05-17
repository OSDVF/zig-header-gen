const Builder = @import("std").build.Builder;

const std = @import("std");

// This build.zig is only used as an example of using header_gen

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});

    const mode = b.standardOptimizeOption(.{});

    // HEADER GEN BUILD STEP
    const exe = b.addExecutable(.{
        .name = "example",
        .root_source_file = .{
            .path = "src/example/exports.zig",
        },
        .main_mod_path = .{
            .path = "src/",
        },
        .target = target,
        .optimize = mode,
    });
    exe.addModule("header_gen", b.addModule(
        "hedaer_gen",
        .{
            .source_file = .{
                .path = "src/header_gen.zig",
            },
        },
    ));
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("headergen", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
