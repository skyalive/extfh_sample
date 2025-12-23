/// EXTFH (Extended File Handler) for GnuCOBOL VSAM & Sequential File Support
///
/// This module implements the EXTFH interface for GnuCOBOL, enabling
/// COBOL programs to read/write:
///   - VSAM KSDS (indexed sequential) files via VBISAM
///   - Line Sequential (sequential) files via filesystem I/O
///   - GDG (Generation Data Groups) - Phase 6
///
/// Compilation: cobc -fcallfh=czippfh -c program.cob
/// Link: zig build (automatically links extfh into czippbt)

const std = @import("std");
// Abstraction layer imports (ISAM backend-independent interface)
const isam = @import("isam_interface.zig");
const isam_vbisam = @import("isam_vbisam.zig");

/// File organization types
pub const FileType = enum {
    SEQUENTIAL,  // Line Sequential - traditional sequential file
    INDEXED,     // INDEXED (VSAM KSDS) - keyed sequential file
    RELATIVE,    // RELATIVE (for future use)
    UNKNOWN,
};

/// File Control Descriptor (FCD3) - Interface between COBOL and EXTFH
pub const FCD3 = struct {
    call_id: c_int,              // 1=OPEN, 2=CLOSE, 3=READ, 4=WRITE, 5=REWRITE, 6=DELETE, 7=START
    handle: c_int,               // File handle (input/output)
    status: c_short,             // Status code (output): 0=success, 1=not found, 2=locked, 3=dup key, 4=no rec, 5=I/O err
    filename: [256]u8,           // Filename
    file_open_mode: c_short,     // OPEN mode: 0=INPUT, 1=OUTPUT, 2=I-O
    record_varying: c_short,     // Variable-length records flag
    record_size: c_int,          // Record size in bytes
    record_key_pos: c_int,       // Key position in record
    record_key_size: c_int,      // Key size in bytes
    record_ptr: [*c]u8,          // Pointer to record data buffer
    key_ptr: [*c]u8,             // Pointer to key buffer
    option: c_short,             // Read mode: 0=FIRST, 1=LAST, 2=NEXT, 3=PREV, 4=CURRENT, 5=EQUAL, 6=GTEQ
    key_number: c_short,         // Key index (0=primary)
};

/// File context for tracking open files
pub const ExtfhFileContext = struct {
    file_type: FileType,                // File organization type
    vbisam_handle: c_int,               // VBISAM file handle (indexed files only)
    sequential_file: ?std.fs.File = null, // File handle for sequential files
    filename: []const u8,               // Owned filename
    record_size: usize,                 // Record size
    allocator: std.mem.Allocator,       // Memory allocator for cleanup
    is_open: bool,                      // Currently open?
    current_key: c_int,                 // Current key index
    eof_reached: bool = false,          // EOF flag for sequential reads
};

/// Global handle table (maps EXTFH handles to VBISAM contexts)
var handle_table: std.AutoHashMap(c_int, ExtfhFileContext) = undefined;
var handle_table_mutex = std.Thread.Mutex{};
var next_handle: c_int = 1;
var allocator: ?std.mem.Allocator = null;
var backend: ?isam.IsamBackend = null;
var initialized = false;

/// Initialize EXTFH system
pub fn init(alloc: std.mem.Allocator) !void {
    if (initialized) return;

    allocator = alloc;
    handle_table = std.AutoHashMap(c_int, ExtfhFileContext).init(alloc);

    // Initialize VBISAM backend
    backend = .{
        .VBISAM = isam_vbisam.VbisamBackend.init(alloc),
    };

    initialized = true;
}

/// Cleanup EXTFH system
pub fn deinit() void {
    if (!initialized) return;

    handle_table_mutex.lock();
    defer handle_table_mutex.unlock();

    var iter = handle_table.valueIterator();
    while (iter.next()) |context| {
        if (context.is_open) {
            if (context.file_type == .INDEXED) {
                // Close INDEXED files via backend (CRITICAL: prevent resource leak)
                if (backend) |*bknd| {
                    const handle = isam.IsamFileHandle{
                        .backend_type = .VBISAM,
                        .handle = context.vbisam_handle,
                        .record_size = context.record_size,
                        .key_offset = 0,
                        .key_size = 0,
                    };
                    bknd.close(handle) catch {
                        // Ignore errors during cleanup
                    };
                }
            } else if (context.file_type == .SEQUENTIAL) {
                // Close sequential files
                if (context.sequential_file) |file| {
                    file.close();
                }
            }
        }
        if (allocator) |alloc| {
            alloc.free(context.filename);
        }
    }

    handle_table.deinit();
    backend = null;
    initialized = false;
}

/// Main EXTFH handler - called by GnuCOBOL for all I-O operations
pub export fn czippfh(fcd_ptr: [*c]c_int) callconv(.c) void {
    if (!initialized) {
        // Lazy initialization with default allocator
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        defer _ = gpa.deinit();
        init(gpa.allocator()) catch {
            // Can't initialize - set error status
            return;
        };
    }

    // Cast FCD3 pointer
    const fcd: *FCD3 = @alignCast(@ptrCast(fcd_ptr));

    // Dispatch to appropriate handler based on operation code
    switch (fcd.call_id) {
        1 => handleOpen(fcd),
        2 => handleClose(fcd),
        3 => handleRead(fcd),
        4 => handleWrite(fcd),
        5 => handleRewrite(fcd),
        6 => handleDelete(fcd),
        7 => handleStart(fcd),
        8 => handleAbort(fcd),
        9 => handleCommit(fcd),
        10 => handleUnlock(fcd),
        else => {
            fcd.status = 9; // Generic error
        }
    }
}

/// OPEN operation (call_id = 1)
fn handleOpen(fcd: *FCD3) void {
    const alloc = allocator orelse {
        fcd.status = 5; // I/O error
        return;
    };

    handle_table_mutex.lock();
    defer handle_table_mutex.unlock();

    // Extract filename (null-terminated string from FCD3)
    const filename_str = std.mem.sliceTo(&fcd.filename, 0);
    const filename = alloc.dupe(u8, filename_str) catch {
        fcd.status = 5;
        return;
    };

    // Detect file type based on extension
    const file_type = detectFileType(filename, fcd.file_open_mode == 1);

    // Route to appropriate handler based on file type
    switch (file_type) {
        .INDEXED => handleOpenIndexed(fcd, alloc, filename),
        .SEQUENTIAL => handleOpenSequential(fcd, alloc, filename),
        else => {
            // Default to sequential for unknown types
            handleOpenSequential(fcd, alloc, filename);
        },
    }
}

/// OPEN for INDEXED (VSAM KSDS) files via VBISAM
fn handleOpenIndexed(fcd: *FCD3, alloc: std.mem.Allocator, filename: []const u8) void {
    if (backend == null) {
        alloc.free(filename);
        fcd.status = 5; // I/O error
        return;
    }

    // Determine ISAM open mode
    const isam_mode: isam.OpenMode = switch (fcd.file_open_mode) {
        0 => .INPUT,    // COBOL INPUT
        1 => .OUTPUT,   // COBOL OUTPUT
        3 => .EXTEND,   // COBOL EXTEND
        else => .IO,    // COBOL I-O
    };

    // Try to open or create INDEXED file via backend
    const handle_result = if (fcd.file_open_mode == 0) blk: {
        // INPUT mode: open existing file via backend
        break :blk backend.?.open(filename, isam_mode) catch |err| {
            alloc.free(filename);
            fcd.status = switch (err) {
                isam.IsamError.NotFound => 1,
                isam.IsamError.IoError => 5,
                else => 5,
            };
            return;
        };
    } else blk: {
        // OUTPUT/EXTEND mode: create new file via backend
        break :blk backend.?.create(
            filename,
            isam_mode,
            @intCast(fcd.record_size),
            @intCast(fcd.record_key_pos),
            @intCast(fcd.record_key_size),
        ) catch |err| {
            alloc.free(filename);
            fcd.status = switch (err) {
                isam.IsamError.IoError => 5,
                else => 5,
            };
            return;
        };
    };

    // Store context for INDEXED file
    const handle = next_handle;
    next_handle += 1;

    const context = ExtfhFileContext{
        .file_type = .INDEXED,
        .vbisam_handle = handle_result.handle,
        .filename = filename,
        .record_size = handle_result.record_size,
        .allocator = alloc,
        .is_open = true,
        .current_key = 0,
    };

    handle_table.put(handle, context) catch {
        alloc.free(filename);
        fcd.status = 5;
        return;
    };

    fcd.handle = handle;
    fcd.status = 0;
}

/// OPEN for SEQUENTIAL (Line Sequential) files via native filesystem
fn handleOpenSequential(fcd: *FCD3, alloc: std.mem.Allocator, filename: []const u8) void {
    // Determine file open flags
    const file: ?std.fs.File = switch (fcd.file_open_mode) {
        0 => blk: {
            // INPUT mode: open for reading
            break :blk std.fs.cwd().openFile(filename, .{ .mode = .read_only }) catch {
                alloc.free(filename);
                fcd.status = 1; // File not found
                return;
            };
        },
        1 => blk: {
            // OUTPUT mode: create/truncate for writing
            break :blk std.fs.cwd().createFile(filename, .{ .truncate = true }) catch {
                alloc.free(filename);
                fcd.status = 5; // I/O error
                return;
            };
        },
        3 => blk: {
            // EXTEND mode: open for append
            const f = std.fs.cwd().openFile(filename, .{ .mode = .write_only }) catch {
                // If file doesn't exist, create it
                break :blk std.fs.cwd().createFile(filename, .{}) catch {
                    alloc.free(filename);
                    fcd.status = 5;
                    return;
                };
            };
            f.seekFromEnd(0) catch {};
            break :blk f;
        },
        else => blk: {
            // I-O mode: open for read/write
            break :blk std.fs.cwd().openFile(filename, .{ .mode = .read_write }) catch {
                alloc.free(filename);
                fcd.status = 1;
                return;
            };
        },
    };

    // Store context for SEQUENTIAL file
    const handle = next_handle;
    next_handle += 1;

    const context = ExtfhFileContext{
        .file_type = .SEQUENTIAL,
        .vbisam_handle = -1, // Not used for sequential
        .sequential_file = file,
        .filename = filename,
        .record_size = @intCast(fcd.record_size),
        .allocator = alloc,
        .is_open = true,
        .current_key = 0,
    };

    handle_table.put(handle, context) catch {
        if (file) |f| f.close();
        alloc.free(filename);
        fcd.status = 5;
        return;
    };

    fcd.handle = handle;
    fcd.status = 0;
}

/// CLOSE operation (call_id = 2)
fn handleClose(fcd: *FCD3) void {
    handle_table_mutex.lock();
    defer handle_table_mutex.unlock();

    const ctx = handle_table.fetchRemove(fcd.handle) orelse {
        fcd.status = 5; // I/O error
        return;
    };
    const context = ctx.value;

    if (backend == null) {
        fcd.status = 5; // I/O error
        if (context.file_type == .SEQUENTIAL) {
            if (context.sequential_file) |file| {
                file.close();
            }
        }
        context.allocator.free(context.filename);
        return;
    }

    switch (context.file_type) {
        .INDEXED => {
            // Close INDEXED file via backend
            const handle = isam.IsamFileHandle{
                .backend_type = .VBISAM,
                .handle = context.vbisam_handle,
                .record_size = context.record_size,
                .key_offset = 0,
                .key_size = 0,
            };
            backend.?.close(handle) catch |err| {
                fcd.status = mapIsamErrorToStatus(err);
            };
        },
        .SEQUENTIAL => {
            // Close sequential file
            if (context.sequential_file) |file| {
                file.close();
            }
        },
        else => {},
    }

    // Clean up filename
    context.allocator.free(context.filename);

    fcd.status = 0; // Success
}

/// READ operation (call_id = 3)
fn handleRead(fcd: *FCD3) void {
    handle_table_mutex.lock();
    defer handle_table_mutex.unlock();

    const context_ptr = handle_table.getPtr(fcd.handle) orelse {
        fcd.status = 5; // I/O error
        return;
    };

    switch (context_ptr.file_type) {
        .INDEXED => handleReadIndexed(fcd, context_ptr),
        .SEQUENTIAL => handleReadSequential(fcd, context_ptr),
        else => {
            fcd.status = 5; // Unsupported file type
        },
    }
}

/// READ for INDEXED files via VBISAM
fn handleReadIndexed(fcd: *FCD3, context: *ExtfhFileContext) void {
    if (backend == null) {
        fcd.status = 5; // I/O error
        return;
    }

    // Convert COBOL read mode to unified ISAM read mode
    const isam_mode: isam.ReadMode = switch (fcd.option) {
        0 => .FIRST,
        1 => .LAST,
        2 => .NEXT,
        3 => .PREVIOUS,
        4 => .EQUAL,  // COBOL option 4 (CURRENT) maps to EQUAL
        5 => .EQUAL,
        6 => .GREATER_EQUAL,
        else => .NEXT,
    };

    const handle = isam.IsamFileHandle{
        .backend_type = .VBISAM,
        .handle = context.vbisam_handle,
        .record_size = context.record_size,
        .key_offset = @intCast(fcd.record_key_pos),
        .key_size = @intCast(fcd.record_key_size),
    };

    // Read record into buffer
    const record_buf = fcd.record_ptr[0..context.record_size];
    backend.?.read(handle, record_buf, isam_mode) catch |err| {
        fcd.status = mapIsamErrorToStatus(err);
        return;
    };

    fcd.status = 0; // Success
}

/// READ for SEQUENTIAL (Line Sequential) files
fn handleReadSequential(fcd: *FCD3, context: *ExtfhFileContext) void {
    // Check for EOF
    if (context.eof_reached) {
        fcd.status = 4; // End of file / No record
        return;
    }

    const file = context.sequential_file orelse {
        fcd.status = 5; // I/O error
        return;
    };

    // Read one line using low-level read API (Line Sequential format)
    var line_buf: [4096]u8 = undefined;
    var line_len: usize = 0;

    // Read byte by byte until newline or EOF
    while (line_len < line_buf.len) {
        const bytes_read = file.read(line_buf[line_len .. line_len + 1]) catch {
            fcd.status = 5; // I/O error
            return;
        };
        if (bytes_read == 0) {
            // EOF reached
            if (line_len == 0) {
                context.eof_reached = true;
                fcd.status = 4; // End of file
                return;
            }
            break; // Return partial line
        }
        if (line_buf[line_len] == '\n') {
            break; // Line complete
        }
        line_len += 1;
    }

    // Copy to record buffer, padding with spaces if necessary
    const record_buf = fcd.record_ptr[0..context.record_size];
    const copy_len = @min(line_len, context.record_size);
    @memcpy(record_buf[0..copy_len], line_buf[0..copy_len]);
    // Pad remaining with spaces (COBOL convention)
    if (copy_len < context.record_size) {
        @memset(record_buf[copy_len..], ' ');
    }
    fcd.status = 0; // Success
}

/// WRITE operation (call_id = 4)
fn handleWrite(fcd: *FCD3) void {
    handle_table_mutex.lock();
    defer handle_table_mutex.unlock();

    const context = handle_table.getPtr(fcd.handle) orelse {
        fcd.status = 5; // I/O error
        return;
    };

    switch (context.file_type) {
        .INDEXED => handleWriteIndexed(fcd, context),
        .SEQUENTIAL => handleWriteSequential(fcd, context),
        else => {
            fcd.status = 5; // Unsupported file type
        },
    }
}

/// WRITE for INDEXED files via VBISAM
fn handleWriteIndexed(fcd: *FCD3, context: *ExtfhFileContext) void {
    if (backend == null) {
        fcd.status = 5; // I/O error
        return;
    }

    const handle = isam.IsamFileHandle{
        .backend_type = .VBISAM,
        .handle = context.vbisam_handle,
        .record_size = context.record_size,
        .key_offset = @intCast(fcd.record_key_pos),
        .key_size = @intCast(fcd.record_key_size),
    };

    const record_buf = fcd.record_ptr[0..context.record_size];
    backend.?.write(handle, record_buf) catch |err| {
        fcd.status = mapIsamErrorToStatus(err);
        return;
    };

    fcd.status = 0; // Success
}

/// WRITE for SEQUENTIAL (Line Sequential) files
fn handleWriteSequential(fcd: *FCD3, context: *ExtfhFileContext) void {
    const file = context.sequential_file orelse {
        fcd.status = 5; // I/O error
        return;
    };

    // Get record data (trim trailing spaces for Line Sequential)
    const record_buf = fcd.record_ptr[0..context.record_size];
    var end_pos: usize = context.record_size;
    while (end_pos > 0 and record_buf[end_pos - 1] == ' ') {
        end_pos -= 1;
    }

    // Write line using low-level write API (Line Sequential format)
    _ = file.write(record_buf[0..end_pos]) catch {
        fcd.status = 5; // I/O error
        return;
    };
    // Write newline terminator
    _ = file.write("\n") catch {
        fcd.status = 5; // I/O error
        return;
    };

    fcd.status = 0; // Success
}

/// REWRITE operation (call_id = 5) - INDEXED files only
fn handleRewrite(fcd: *FCD3) void {
    handle_table_mutex.lock();
    defer handle_table_mutex.unlock();

    const context = handle_table.get(fcd.handle) orelse {
        fcd.status = 5; // I/O error
        return;
    };

    // REWRITE is only valid for INDEXED files
    if (context.file_type != .INDEXED) {
        fcd.status = 5; // Operation not supported for this file type
        return;
    }

    if (backend == null) {
        fcd.status = 5; // I/O error
        return;
    }

    const handle = isam.IsamFileHandle{
        .backend_type = .VBISAM,
        .handle = context.vbisam_handle,
        .record_size = context.record_size,
        .key_offset = @intCast(fcd.record_key_pos),
        .key_size = @intCast(fcd.record_key_size),
    };

    const record_buf = fcd.record_ptr[0..context.record_size];
    backend.?.rewrite(handle, record_buf) catch |err| {
        fcd.status = mapIsamErrorToStatus(err);
        return;
    };

    fcd.status = 0; // Success
}

/// DELETE operation (call_id = 6) - INDEXED files only
fn handleDelete(fcd: *FCD3) void {
    handle_table_mutex.lock();
    defer handle_table_mutex.unlock();

    const context = handle_table.get(fcd.handle) orelse {
        fcd.status = 5; // I/O error
        return;
    };

    // DELETE is only valid for INDEXED files
    if (context.file_type != .INDEXED) {
        fcd.status = 5; // Operation not supported for this file type
        return;
    }

    if (backend == null) {
        fcd.status = 5; // I/O error
        return;
    }

    const handle = isam.IsamFileHandle{
        .backend_type = .VBISAM,
        .handle = context.vbisam_handle,
        .record_size = context.record_size,
        .key_offset = @intCast(fcd.record_key_pos),
        .key_size = @intCast(fcd.record_key_size),
    };

    backend.?.delete(handle) catch |err| {
        fcd.status = mapIsamErrorToStatus(err);
        return;
    };

    fcd.status = 0; // Success
}

/// START operation (call_id = 7) - Position at key for sequential reading (INDEXED only)
fn handleStart(fcd: *FCD3) void {
    handle_table_mutex.lock();
    defer handle_table_mutex.unlock();

    const context = handle_table.get(fcd.handle) orelse {
        fcd.status = 5; // I/O error
        return;
    };

    // START is only valid for INDEXED files
    if (context.file_type != .INDEXED) {
        fcd.status = 5; // Operation not supported for this file type
        return;
    }

    if (backend == null) {
        fcd.status = 5; // I/O error
        return;
    }

    if (fcd.key_ptr == null or fcd.record_key_size == 0) {
        fcd.status = 5; // I/O error
        return;
    }

    // Convert COBOL read mode to unified ISAM read mode
    const isam_mode: isam.ReadMode = switch (fcd.option) {
        5 => .EQUAL,
        6 => .GREATER_EQUAL,
        else => .EQUAL,
    };

    const handle = isam.IsamFileHandle{
        .backend_type = .VBISAM,
        .handle = context.vbisam_handle,
        .record_size = context.record_size,
        .key_offset = @intCast(fcd.record_key_pos),
        .key_size = @intCast(fcd.record_key_size),
    };

    const key_buf = fcd.key_ptr[0..@intCast(fcd.record_key_size)];
    backend.?.start(handle, key_buf, isam_mode) catch |err| {
        fcd.status = mapIsamErrorToStatus(err);
        return;
    };

    fcd.status = 0; // Success
}

/// ABORT operation (call_id = 8) - Transaction abort (not implemented)
fn handleAbort(fcd: *FCD3) void {
    // Transaction support not yet implemented
    fcd.status = 0; // Success (no-op)
}

/// COMMIT operation (call_id = 9) - Transaction commit (not implemented)
fn handleCommit(fcd: *FCD3) void {
    // Transaction support not yet implemented
    fcd.status = 0; // Success (no-op)
}

/// UNLOCK operation (call_id = 10) - Release locks (INDEXED only)
fn handleUnlock(fcd: *FCD3) void {
    handle_table_mutex.lock();
    defer handle_table_mutex.unlock();

    const context = handle_table.get(fcd.handle) orelse {
        fcd.status = 5; // I/O error
        return;
    };

    // UNLOCK is only valid for INDEXED files
    if (context.file_type != .INDEXED) {
        // For non-indexed files, just return success (no-op)
        fcd.status = 0;
        return;
    }

    if (backend == null) {
        fcd.status = 5; // I/O error
        return;
    }

    const handle = isam.IsamFileHandle{
        .backend_type = .VBISAM,
        .handle = context.vbisam_handle,
        .record_size = context.record_size,
        .key_offset = @intCast(fcd.record_key_pos),
        .key_size = @intCast(fcd.record_key_size),
    };

    backend.?.unlock(handle) catch |err| {
        fcd.status = mapIsamErrorToStatus(err);
        return;
    };

    fcd.status = 0; // Success
}

/// Detect file organization type based on filename extension or create new file
fn detectFileType(filename: []const u8, create_new: bool) FileType {
    _ = create_new; // Reserved for future use

    // Check filename extension for INDEXED (VSAM KSDS) files
    if (std.mem.endsWith(u8, filename, ".isam") or
        std.mem.endsWith(u8, filename, ".idx") or
        std.mem.endsWith(u8, filename, ".ksds") or
        std.mem.endsWith(u8, filename, ".vsam"))
    {
        return .INDEXED;
    }

    // Check for RELATIVE file extensions (future support)
    if (std.mem.endsWith(u8, filename, ".rel") or
        std.mem.endsWith(u8, filename, ".rrds"))
    {
        return .RELATIVE;
    }

    // Default to SEQUENTIAL for:
    // - .txt, .dat, .log files
    // - GDG format files (e.g., REPORT.G0001V00)
    // - Any other extension
    return .SEQUENTIAL;
}

/// Map ISAM errors to FCD3 status codes
fn mapIsamErrorToStatus(err: isam.IsamError) c_short {
    return switch (err) {
        isam.IsamError.Duplicate => 3,      // Duplicate key
        isam.IsamError.NotFound => 4,       // Record not found
        isam.IsamError.EndOfFile => 4,      // Record not found
        isam.IsamError.Locked => 2,         // File locked
        isam.IsamError.IoError => 5,        // I/O error
        isam.IsamError.NotSupported => 5,   // Operation not supported
    };
}


// ============================================================
// Unit Tests
// ============================================================

test "EXTFH: FileType detection" {
    // INDEXED files
    try std.testing.expectEqual(FileType.INDEXED, detectFileType("data.isam", false));
    try std.testing.expectEqual(FileType.INDEXED, detectFileType("data.idx", false));
    try std.testing.expectEqual(FileType.INDEXED, detectFileType("data.ksds", false));
    try std.testing.expectEqual(FileType.INDEXED, detectFileType("data.vsam", false));

    // RELATIVE files
    try std.testing.expectEqual(FileType.RELATIVE, detectFileType("data.rel", false));
    try std.testing.expectEqual(FileType.RELATIVE, detectFileType("data.rrds", false));

    // SEQUENTIAL files (default)
    try std.testing.expectEqual(FileType.SEQUENTIAL, detectFileType("data.txt", false));
    try std.testing.expectEqual(FileType.SEQUENTIAL, detectFileType("data.dat", false));
    try std.testing.expectEqual(FileType.SEQUENTIAL, detectFileType("data.log", false));
    try std.testing.expectEqual(FileType.SEQUENTIAL, detectFileType("REPORT.G0001V00", false));
    try std.testing.expectEqual(FileType.SEQUENTIAL, detectFileType("combined_log.txt", false));
}

test "EXTFH: Initialize and deinit" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    try init(alloc);
    defer deinit();

    try std.testing.expect(initialized);
}
