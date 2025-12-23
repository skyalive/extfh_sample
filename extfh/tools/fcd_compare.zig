const std = @import("std");

const cob = @cImport({
    @cInclude("gnucobol_common.h");
});

const SimpleFCD3 = extern struct {
    call_id: c_int,
    handle: c_int,
    status: c_short,
    filename: [256]u8,
    file_open_mode: c_short,
    record_varying: c_short,
    record_size: c_int,
    record_key_pos: c_int,
    record_key_size: c_int,
    record_ptr: [*c]u8,
    key_ptr: [*c]u8,
    option: c_short,
    key_number: c_short,
};

pub fn main() !void {
    const out = std.fs.File.stdout().deprecatedWriter();

    try out.print("FCD3 (GnuCOBOL common.h) size: {d}\n", .{@sizeOf(cob.FCD3)});
    try out.print("SimpleFCD3 (current Zig) size: {d}\n", .{@sizeOf(SimpleFCD3)});
    try out.print("\n", .{});

    try out.print("Offsets (GnuCOBOL FCD3):\n", .{});
    try out.print("  fileStatus: {d}\n", .{@offsetOf(cob.FCD3, "fileStatus")});
    try out.print("  fcdLen: {d}\n", .{@offsetOf(cob.FCD3, "fcdLen")});
    try out.print("  fcdVer: {d}\n", .{@offsetOf(cob.FCD3, "fcdVer")});
    try out.print("  openMode: {d}\n", .{@offsetOf(cob.FCD3, "openMode")});
    try out.print("  fnameLen: {d}\n", .{@offsetOf(cob.FCD3, "fnameLen")});
    try out.print("  curRecLen: {d}\n", .{@offsetOf(cob.FCD3, "curRecLen")});
    try out.print("  _fileHandle: {d}\n", .{@offsetOf(cob.FCD3, "_fileHandle")});
    try out.print("  _recPtr: {d}\n", .{@offsetOf(cob.FCD3, "_recPtr")});
    try out.print("  _fnamePtr: {d}\n", .{@offsetOf(cob.FCD3, "_fnamePtr")});

    try out.print("\n", .{});
    try out.print("Offsets (SimpleFCD3):\n", .{});
    try out.print("  call_id: {d}\n", .{@offsetOf(SimpleFCD3, "call_id")});
    try out.print("  handle: {d}\n", .{@offsetOf(SimpleFCD3, "handle")});
    try out.print("  status: {d}\n", .{@offsetOf(SimpleFCD3, "status")});
    try out.print("  filename: {d}\n", .{@offsetOf(SimpleFCD3, "filename")});
    try out.print("  record_ptr: {d}\n", .{@offsetOf(SimpleFCD3, "record_ptr")});
    try out.print("  key_ptr: {d}\n", .{@offsetOf(SimpleFCD3, "key_ptr")});
}
