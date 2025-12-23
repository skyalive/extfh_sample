const std = @import("std");

pub const VbisamError = error{
    Duplicate,
    NoRecord,
    NoCurrent,
    EndFile,
    Locked,
    IoError,
    MemoryError,
    BadKey,
    BadFile,
    Other,
};

pub const OpenMode = enum(c_int) {
    INPUT = 0,
    OUTPUT = 1,
    INOUT = 2,
};

pub const ReadMode = enum(c_int) {
    FIRST = 0,
    LAST = 1,
    NEXT = 2,
    PREV = 3,
    CURR = 4,
    EQUAL = 5,
    GREAT = 6,
    GTEQ = 7,
};

pub const FileLockMode = enum(c_int) {
    NONE = 0,
    AUTO = 0x200,
    MANUAL = 0x400,
    EXCLUSIVE = 0x800,
};

pub const KeyType = enum(c_short) {
    CHAR = 0,
    INT = 1,
    LONG = 2,
    DOUBLE = 3,
    FLOAT = 4,
    QUAD = 5,
};

pub const KeyDescBuilder = struct {
    parts: [8]KeyPart = undefined,
    count: usize = 0,

    const KeyPart = extern struct {
        start: c_short,
        leng: c_short,
        type: c_short,
    };

    pub fn init() KeyDescBuilder {
        return .{};
    }

    pub fn addPart(self: *KeyDescBuilder, start: c_short, leng: c_short, key_type: KeyType) void {
        if (self.count >= 8) return;
        self.parts[self.count] = KeyPart{
            .start = start,
            .leng = leng,
            .type = @intFromEnum(key_type),
        };
        self.count += 1;
    }

    pub fn build(self: *KeyDescBuilder) KeyDesc {
        var kd: KeyDesc = undefined;
        // Default init
        kd = std.mem.zeroes(KeyDesc);
        
        kd.k_nparts = @intCast(self.count);
        var total_len: c_short = 0;
        for (0..self.count) |i| {
            kd.k_part[i] = self.parts[i];
            total_len += self.parts[i].leng;
        }
        kd.k_len = total_len;
        return kd;
    }
};

pub const KeyDesc = extern struct {
    k_flags: c_short,
    k_nparts: c_short,
    k_part: [8]KeyDescBuilder.KeyPart,
    k_len: c_short,
    k_rootnode: c_int, // vbisam_off_t is int or long long based on define. wrapper assumes default (often int or check system)
                       // vbisam.h defines vbisam_off_t as int unless VBISAM_USE_LONG_LONG is defined.
                       // We will assume 'int' for now, but safer to use c_longlong if unsure.
                       // Looking at vbisam.h: #define vbisam_off_t int (unless defined)
                       // We will use c_longlong to be safe or check build flags.
                       // Let's use c_longlong to align with typical 64-bit systems or if forced.
                       // Actually, let's use c_long for generic approach or match the C definition.
                       // In build.zig I won't define VBISAM_USE_LONG_LONG unless necessary.
                       // Let's stick to c_int if that's the default.
                       // Wait, vbisam.h says:
                       // #ifdef VBISAM_USE_LONG_LONG -> long long
                       // #else -> int
                       // If I don't define it, it is int.
};

// C API
extern fn isopen(filename: [*c]const u8, mode: c_int) c_int;
extern fn isclose(handle: c_int) c_int;
extern fn isbuild(filename: [*c]const u8, max_rec_len: c_int, key_desc: *KeyDesc, mode: c_int) c_int;
extern fn isread(handle: c_int, record: [*c]u8, mode: c_int) c_int;
extern fn iswrite(handle: c_int, record: [*c]u8) c_int;
extern fn isrewrite(handle: c_int, record: [*c]u8) c_int;
extern fn isdelete(handle: c_int, record: [*c]u8) c_int;
extern fn isdelcurr(handle: c_int) c_int;
extern fn isstart(handle: c_int, key_desc: *KeyDesc, length: c_int, record: [*c]u8, mode: c_int) c_int;

extern var iserrno: c_int;

// Wrapper Struct
pub const IsamFile = struct {
    handle: c_int,
    record_size: usize,
    allocator: std.mem.Allocator,

    pub fn open(allocator: std.mem.Allocator, filename: []const u8, mode: OpenMode, lock: FileLockMode) VbisamError!IsamFile {
        _ = lock; // TODO: handle lock mode
        
        // Ensure null-termination
        const filename_c = allocator.dupeZ(u8, filename) catch return error.MemoryError;
        defer allocator.free(filename_c);

        // vbisam isopen mode:
        // ISINPUT 0, ISOUTPUT 1, ISINOUT 2
        // plus flags like ISVARLEN (0x10)
        // We might need to handle fixed/var length. For now assume fixed (default).
        
        const c_mode = @intFromEnum(mode);
        const handle = isopen(filename_c, c_mode);
        if (handle < 0) {
            return mapIsErrno();
        }

        // We need record size. 
        // vbisam doesn't easily give it back on open?
        // Actually it might be accessible via other calls or we assume the user knows.
        // The wrapper code in isam_vbisam.zig sets record_size from handle_result if open succeeds.
        // But IsamFile.open needs to return it.
        // For now, we return a partial IsamFile and let caller handle size or 
        // we might need `isindexinfo` or similar to query it?
        // `isreclen` global variable might hold it after open?
        // Let's assume a default or that we can query it.
        // For now, let's just return what we have.
        // Wait, IsamFile needs record_size.
        // In `isopen.c` it fills a structure.
        // Let's use `isdi_recsize(handle)` if available?
        // Declared in vbisam.h: extern int isdi_recsize (const int ihandle);
        
        // I need to add extern for isdi_recsize
        const rec_size = isdi_datlen(handle);

        return IsamFile{
            .handle = handle,
            .record_size = @intCast(rec_size),
            .allocator = allocator,
        };
    }

    pub fn build(allocator: std.mem.Allocator, filename: []const u8, record_size: usize, key_desc: KeyDesc, mode: OpenMode, lock: FileLockMode, var_len: bool) VbisamError!IsamFile {
        _ = lock;
        
        const filename_c = allocator.dupeZ(u8, filename) catch return error.MemoryError;
        defer allocator.free(filename_c);

        var kd = key_desc; // Copy to mutable for pointer
        var c_mode = @intFromEnum(mode);
        if (var_len) {
            c_mode |= 0x10; // ISVARLEN
        }

        if (std.fs.cwd().createFile("debug_extfh.txt", .{ .truncate = false }) catch null) |dbg_file| {
            defer dbg_file.close();
            dbg_file.seekFromEnd(0) catch {};
            var msg_buf: [192]u8 = undefined;
            const msg = std.fmt.bufPrint(
                &msg_buf,
                "VBISAM isbuild rec_size={} k_nparts={} k_len={} kd_addr=0x{x}\n",
                .{ record_size, kd.k_nparts, kd.k_len, @intFromPtr(&kd) },
            ) catch "VBISAM log failed\n";
            dbg_file.writeAll(msg) catch {};
        }

        const handle = isbuild(filename_c, @intCast(record_size), &kd, c_mode);
        if (handle < 0) {
            return mapIsErrno();
        }

        return IsamFile{
            .handle = handle,
            .record_size = record_size,
            .allocator = allocator,
        };
    }

    pub fn close(self: *IsamFile) void {
        _ = isclose(self.handle);
    }

    pub fn read(self: *IsamFile, buffer: []u8, mode: ReadMode, lock: FileLockMode) VbisamError!void {
        _ = lock;
        if (buffer.len < self.record_size) return error.IoError; // Buffer too small

        const ret = isread(self.handle, buffer.ptr, @intFromEnum(mode));
        if (ret < 0) {
            return mapIsErrno();
        }
    }

    pub fn write(self: *IsamFile, buffer: []u8) VbisamError!void {
         const ret = iswrite(self.handle, buffer.ptr);
         if (ret < 0) {
             return mapIsErrno();
         }
    }

    pub fn rewrite(self: *IsamFile, buffer: []u8) VbisamError!void {
         const ret = isrewrite(self.handle, buffer.ptr);
         if (ret < 0) {
             return mapIsErrno();
         }
    }

    pub fn deleteCurrent(self: *IsamFile) VbisamError!void {
        const ret = isdelcurr(self.handle);
        if (ret < 0) {
            return mapIsErrno();
        }
    }

    pub fn start(self: *IsamFile, key_desc: KeyDesc, length: usize, key: []u8, mode: ReadMode) VbisamError!void {
        var kd = key_desc;
        const ret = isstart(self.handle, &kd, @intCast(length), key.ptr, @intFromEnum(mode));
        if (ret < 0) {
            return mapIsErrno();
        }
    }
};

extern fn isdi_datlen(handle: c_int) c_int;

fn mapIsErrno() VbisamError {
    // Mapping based on vbisam.h defines
    // EDUPL 100
    // ENOTOPEN 101
    // ...
    // EENDFILE 110
    // ENOREC 111
    // ELOCKED 107
    
    return switch (iserrno) {
        100 => error.Duplicate,
        110 => error.EndFile,
        111 => error.NoRecord,
        107 => error.Locked,
        112 => error.NoCurrent,
        105 => error.BadFile,
        103 => error.BadKey,
        116 => error.MemoryError,
        else => error.IoError,
    };
}
