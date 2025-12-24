const std = @import("std");
const isam = @import("isam_interface.zig");

pub const GoVbisamHandle = struct {
    backend: isam.VbisamBackend,
    handle: isam.IsamFileHandle,
};

const ErrCode = enum(c_int) {
    ok = 0,
    duplicate = 1,
    not_found = 2,
    end_of_file = 3,
    locked = 4,
    io_error = 5,
    not_supported = 6,
    invalid_arg = 7,
};

fn errToCode(err: isam.IsamError) c_int {
    return switch (err) {
        error.Duplicate => @intFromEnum(ErrCode.duplicate),
        error.NotFound => @intFromEnum(ErrCode.not_found),
        error.EndOfFile => @intFromEnum(ErrCode.end_of_file),
        error.Locked => @intFromEnum(ErrCode.locked),
        error.IoError => @intFromEnum(ErrCode.io_error),
        error.NotSupported => @intFromEnum(ErrCode.not_supported),
    };
}

fn mapOpenMode(mode: c_int) ?isam.OpenMode {
    return switch (mode) {
        0 => .INPUT,
        1 => .OUTPUT,
        2 => .IO,
        3 => .EXTEND,
        else => null,
    };
}

fn mapReadMode(mode: c_int) ?isam.ReadMode {
    return switch (mode) {
        0 => .FIRST,
        1 => .NEXT,
        2 => .PREVIOUS,
        3 => .LAST,
        4 => .EQUAL,
        5 => .GREATER,
        6 => .GREATER_EQUAL,
        else => null,
    };
}

fn sliceFromC(ptr: [*c]u8, len: usize) []u8 {
    return @as([*]u8, @ptrCast(ptr))[0..len];
}

fn sliceFromCConst(ptr: [*c]const u8, len: usize) []const u8 {
    return @as([*]const u8, @ptrCast(ptr))[0..len];
}

pub export fn gvbisam_open(path: [*c]const u8, mode: c_int, key_offset: usize, key_size: usize, out_handle: ?*?*GoVbisamHandle) c_int {
    if (path == null or out_handle == null) return @intFromEnum(ErrCode.invalid_arg);
    const open_mode = mapOpenMode(mode) orelse return @intFromEnum(ErrCode.invalid_arg);

    const allocator = std.heap.c_allocator;
    const filename = std.mem.span(path);

    var backend = isam.VbisamBackend.init(allocator);
    var handle = backend.open(filename, open_mode) catch |err| {
        return errToCode(err);
    };
    handle.key_offset = key_offset;
    handle.key_size = key_size;

    const ctx = allocator.create(GoVbisamHandle) catch return @intFromEnum(ErrCode.io_error);
    ctx.* = .{ .backend = backend, .handle = handle };
    out_handle.?.* = ctx;
    return @intFromEnum(ErrCode.ok);
}

pub export fn gvbisam_create(path: [*c]const u8, mode: c_int, record_size: usize, key_offset: usize, key_size: usize, out_handle: ?*?*GoVbisamHandle) c_int {
    if (path == null or out_handle == null or record_size == 0 or key_size == 0) {
        return @intFromEnum(ErrCode.invalid_arg);
    }
    const open_mode = mapOpenMode(mode) orelse return @intFromEnum(ErrCode.invalid_arg);

    const allocator = std.heap.c_allocator;
    const filename = std.mem.span(path);

    var backend = isam.VbisamBackend.init(allocator);
    const handle = backend.create(filename, open_mode, record_size, key_offset, key_size) catch |err| {
        return errToCode(err);
    };

    const ctx = allocator.create(GoVbisamHandle) catch return @intFromEnum(ErrCode.io_error);
    ctx.* = .{ .backend = backend, .handle = handle };
    out_handle.?.* = ctx;
    return @intFromEnum(ErrCode.ok);
}

pub export fn gvbisam_close(ctx: ?*GoVbisamHandle) c_int {
    const ctx_ptr = ctx orelse return @intFromEnum(ErrCode.invalid_arg);
    const allocator = std.heap.c_allocator;
    ctx_ptr.backend.close(ctx_ptr.handle) catch |err| return errToCode(err);
    ctx_ptr.backend.deinit();
    allocator.destroy(ctx_ptr);
    return @intFromEnum(ErrCode.ok);
}

pub export fn gvbisam_record_size(ctx: ?*GoVbisamHandle) usize {
    const ctx_ptr = ctx orelse return 0;
    return ctx_ptr.handle.record_size;
}

pub export fn gvbisam_key_offset(ctx: ?*GoVbisamHandle) usize {
    const ctx_ptr = ctx orelse return 0;
    return ctx_ptr.handle.key_offset;
}

pub export fn gvbisam_key_size(ctx: ?*GoVbisamHandle) usize {
    const ctx_ptr = ctx orelse return 0;
    return ctx_ptr.handle.key_size;
}

pub export fn gvbisam_read(ctx: ?*GoVbisamHandle, buffer: [*c]u8, buffer_len: usize, mode: c_int, out_len: ?*usize) c_int {
    if (buffer == null) return @intFromEnum(ErrCode.invalid_arg);
    const ctx_ptr = ctx orelse return @intFromEnum(ErrCode.invalid_arg);
    const read_mode = mapReadMode(mode) orelse return @intFromEnum(ErrCode.invalid_arg);
    if (buffer_len < ctx_ptr.handle.record_size) return @intFromEnum(ErrCode.invalid_arg);

    const slice = sliceFromC(buffer, ctx_ptr.handle.record_size);
    ctx_ptr.backend.read(ctx_ptr.handle, slice, read_mode) catch |err| return errToCode(err);
    if (out_len) |ptr| ptr.* = ctx_ptr.handle.record_size;
    return @intFromEnum(ErrCode.ok);
}

pub export fn gvbisam_read_key(ctx: ?*GoVbisamHandle, key_ptr: [*c]const u8, key_len: usize, buffer: [*c]u8, buffer_len: usize, out_len: ?*usize) c_int {
    if (key_ptr == null or buffer == null) return @intFromEnum(ErrCode.invalid_arg);
    const ctx_ptr = ctx orelse return @intFromEnum(ErrCode.invalid_arg);
    if (ctx_ptr.handle.key_size == 0 or ctx_ptr.handle.key_size != key_len) return @intFromEnum(ErrCode.invalid_arg);
    if (buffer_len < ctx_ptr.handle.record_size) return @intFromEnum(ErrCode.invalid_arg);

    const key = sliceFromCConst(key_ptr, key_len);
    const slice = sliceFromC(buffer, ctx_ptr.handle.record_size);

    ctx_ptr.backend.start(ctx_ptr.handle, key, .EQUAL) catch |err| return errToCode(err);
    ctx_ptr.backend.read(ctx_ptr.handle, slice, .NEXT) catch |err| return errToCode(err);

    if (ctx_ptr.handle.key_offset + ctx_ptr.handle.key_size <= slice.len) {
        const rec_key = slice[ctx_ptr.handle.key_offset .. ctx_ptr.handle.key_offset + ctx_ptr.handle.key_size];
        if (!std.mem.eql(u8, rec_key, key)) return @intFromEnum(ErrCode.not_found);
    }

    if (out_len) |ptr| ptr.* = ctx_ptr.handle.record_size;
    return @intFromEnum(ErrCode.ok);
}

pub export fn gvbisam_write(ctx: ?*GoVbisamHandle, buffer: [*c]const u8, buffer_len: usize) c_int {
    if (buffer == null) return @intFromEnum(ErrCode.invalid_arg);
    const ctx_ptr = ctx orelse return @intFromEnum(ErrCode.invalid_arg);
    if (buffer_len < ctx_ptr.handle.record_size) return @intFromEnum(ErrCode.invalid_arg);
    const slice = sliceFromCConst(buffer, ctx_ptr.handle.record_size);
    ctx_ptr.backend.write(ctx_ptr.handle, slice) catch |err| return errToCode(err);
    return @intFromEnum(ErrCode.ok);
}

pub export fn gvbisam_rewrite(ctx: ?*GoVbisamHandle, buffer: [*c]const u8, buffer_len: usize) c_int {
    if (buffer == null) return @intFromEnum(ErrCode.invalid_arg);
    const ctx_ptr = ctx orelse return @intFromEnum(ErrCode.invalid_arg);
    if (buffer_len < ctx_ptr.handle.record_size) return @intFromEnum(ErrCode.invalid_arg);
    const slice = sliceFromCConst(buffer, ctx_ptr.handle.record_size);
    ctx_ptr.backend.rewrite(ctx_ptr.handle, slice) catch |err| return errToCode(err);
    return @intFromEnum(ErrCode.ok);
}

pub export fn gvbisam_delete(ctx: ?*GoVbisamHandle) c_int {
    const ctx_ptr = ctx orelse return @intFromEnum(ErrCode.invalid_arg);
    ctx_ptr.backend.delete(ctx_ptr.handle) catch |err| return errToCode(err);
    return @intFromEnum(ErrCode.ok);
}
