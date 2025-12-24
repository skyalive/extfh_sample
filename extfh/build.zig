const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // 1. VBISAM Module & Static Library
    const vbisam_mod = b.createModule(.{
        .target = target,
        .optimize = optimize,
    });
    
    // Add C sources to module
    vbisam_mod.addIncludePath(b.path("lib/vbisam-osscons-patch-main"));
    vbisam_mod.addIncludePath(b.path("lib/vbisam-osscons-patch-main/libvbisam"));
    vbisam_mod.link_libc = true;
    
    const c_flags = &[_][]const u8{
        "-D_GNU_SOURCE",
        "-fno-sanitize=alignment",
    };
    const vbisam_sources = &[_][]const u8{
        "lib/vbisam-osscons-patch-main/libvbisam/isaudit.c",
        "lib/vbisam-osscons-patch-main/libvbisam/isbuild.c",
        "lib/vbisam-osscons-patch-main/libvbisam/ischeck.c",
        "lib/vbisam-osscons-patch-main/libvbisam/iscommon.c",
        "lib/vbisam-osscons-patch-main/libvbisam/isdecimal.c",
        "lib/vbisam-osscons-patch-main/libvbisam/isdelete.c",
        "lib/vbisam-osscons-patch-main/libvbisam/ishelper.c",
        "lib/vbisam-osscons-patch-main/libvbisam/isopen.c",
        "lib/vbisam-osscons-patch-main/libvbisam/isread.c",
        "lib/vbisam-osscons-patch-main/libvbisam/isrecover.c",
        "lib/vbisam-osscons-patch-main/libvbisam/isrewrite.c",
        "lib/vbisam-osscons-patch-main/libvbisam/istrans.c",
        "lib/vbisam-osscons-patch-main/libvbisam/iswrite.c",
        "lib/vbisam-osscons-patch-main/libvbisam/vbdataio.c",
        "lib/vbisam-osscons-patch-main/libvbisam/vbindexio.c",
        "lib/vbisam-osscons-patch-main/libvbisam/vbkeysio.c",
        "lib/vbisam-osscons-patch-main/libvbisam/vblocking.c",
        "lib/vbisam-osscons-patch-main/libvbisam/vblowlevel.c",
        "lib/vbisam-osscons-patch-main/libvbisam/vbmemio.c",
        "lib/vbisam-osscons-patch-main/libvbisam/vbnodememio.c",
    };
    vbisam_mod.addCSourceFiles(.{
        .files = vbisam_sources,
        .flags = c_flags,
    });

    const vbisam_lib = b.addLibrary(.{
        .linkage = .static,
        .name = "vbisam",
        .root_module = vbisam_mod,
    });

    // 2. EXTFH Module
    const extfh_mod = b.addModule("extfh", .{
        .root_source_file = b.path("src/extfh.zig"),
        .target = target,
        .optimize = optimize,
    });
    extfh_mod.linkLibrary(vbisam_lib);
    extfh_mod.linkSystemLibrary("sqlite3", .{});
    extfh_mod.addIncludePath(b.path("lib/vbisam-osscons-patch-main"));
    extfh_mod.addIncludePath(b.path("include"));

    // 3. EXTFH Shared Library
    const extfh_lib = b.addLibrary(.{
        .linkage = .dynamic,
        .name = "extfh",
        .root_module = extfh_mod,
    });
    b.installArtifact(extfh_lib);

    // 4. Tests
    const main_tests = b.addTest(.{
        .root_module = extfh_mod,
        .name = "extfh_tests",
    });
    
    const run_main_tests = b.addRunArtifact(main_tests);

    // Tests for vbisam wrapper
    // We create a separate module for testing isam_vbisam directly
    const vbisam_test_mod = b.createModule(.{
        .root_source_file = b.path("src/isam_vbisam.zig"),
        .target = target,
        .optimize = optimize,
    });
    vbisam_test_mod.linkLibrary(vbisam_lib);
    vbisam_test_mod.linkSystemLibrary("sqlite3", .{});
    vbisam_test_mod.addIncludePath(b.path("lib/vbisam-osscons-patch-main"));
    vbisam_test_mod.addIncludePath(b.path("include"));

    const vbisam_tests = b.addTest(.{
        .root_module = vbisam_test_mod,
        .name = "vbisam_tests",
    });

    const run_vbisam_tests = b.addRunArtifact(vbisam_tests);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_main_tests.step);
    test_step.dependOn(&run_vbisam_tests.step);
}
