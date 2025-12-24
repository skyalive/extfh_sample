const std = @import("std");
const sqlite3 = @import("sqlite3.zig").c;
const isam = @import("isam_interface.zig");
const schema = @import("isam_sqlite_schema.zig");

// =============================================================================
// SQLite Backend for ISAM Abstraction Layer (COBOL4J-compatible schema)
// =============================================================================

pub const SqliteBackend = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    db: ?*sqlite3.sqlite3,
    db_path: []const u8,
    current_key: ?[]u8,
    session_id: []const u8,
    process_id: []const u8,
    open_mode: isam.OpenMode,
    record_size: usize,
    key_offset: usize,
    key_size: usize,

    /// Initialize SQLite backend
    pub fn init(allocator: std.mem.Allocator, db_path: []const u8) !Self {
        const pid = std.c.getpid();
        const session_id = try std.fmt.allocPrint(allocator, "{d}-{d}", .{ pid, std.time.milliTimestamp() });
        const process_id = try std.fmt.allocPrint(allocator, "{d}", .{pid});
        var self = Self{
            .allocator = allocator,
            .db = null,
            .db_path = try allocator.dupe(u8, db_path),
            .current_key = null,
            .session_id = session_id,
            .process_id = process_id,
            .open_mode = .INPUT,
            .record_size = 0,
            .key_offset = 0,
            .key_size = 0,
        };
        try self.openDb(db_path);
        return self;
    }

    /// Deinitialize and cleanup
    pub fn deinit(self: *Self) void {
        self.closeDb();
        if (self.current_key) |key| {
            self.allocator.free(key);
        }
        self.allocator.free(self.db_path);
        self.allocator.free(self.session_id);
        self.allocator.free(self.process_id);
    }

    /// Open a SQLite-based INDEXED file
    pub fn open(self: *Self, filename: []const u8, mode: isam.OpenMode) isam.IsamError!isam.IsamFileHandle {
        self.open_mode = mode;
        self.clearCurrentKey();
        try self.openDb(filename);

        const mgr = schema.SchemaManager.init(self.allocator, self.db.?);
        try mgr.ensureBaseSchema();

        const record_size = try mgr.getRecordSize();
        const key_info = try mgr.getPrimaryKeyInfo();

        self.record_size = record_size;
        self.key_offset = key_info.key_offset;
        self.key_size = key_info.key_size;

        return isam.IsamFileHandle{
            .backend_type = .SQLITE,
            .handle = 1,
            .record_size = record_size,
            .key_offset = key_info.key_offset,
            .key_size = key_info.key_size,
        };
    }

    /// Close SQLite file
    pub fn close(self: *Self, _: isam.IsamFileHandle) isam.IsamError!void {
        self.closeDb();
    }

    /// Read a record
    pub fn read(self: *Self, handle: isam.IsamFileHandle, buffer: []u8, mode: isam.ReadMode) isam.IsamError!void {
        if (self.db == null) return error.IoError;

        const db = self.db.?;
        const key_from_buffer = blk: {
            if (handle.key_offset + handle.key_size > buffer.len) {
                return error.IoError;
            }
            break :blk buffer[handle.key_offset .. handle.key_offset + handle.key_size];
        };

        const sql = switch (mode) {
            .FIRST => "SELECT key, value FROM table0 WHERE deleted = 0 ORDER BY key ASC LIMIT 1",
            .LAST => "SELECT key, value FROM table0 WHERE deleted = 0 ORDER BY key DESC LIMIT 1",
            .NEXT => if (self.current_key == null)
                "SELECT key, value FROM table0 WHERE deleted = 0 ORDER BY key ASC LIMIT 1"
            else
                "SELECT key, value FROM table0 WHERE deleted = 0 AND key > ? ORDER BY key ASC LIMIT 1",
            .PREVIOUS => if (self.current_key == null)
                "SELECT key, value FROM table0 WHERE deleted = 0 ORDER BY key DESC LIMIT 1"
            else
                "SELECT key, value FROM table0 WHERE deleted = 0 AND key < ? ORDER BY key DESC LIMIT 1",
            .EQUAL => "SELECT key, value FROM table0 WHERE deleted = 0 AND key = ?",
            .GREATER_EQUAL => "SELECT key, value FROM table0 WHERE deleted = 0 AND key >= ? ORDER BY key ASC LIMIT 1",
            .GREATER => "SELECT key, value FROM table0 WHERE deleted = 0 AND key > ? ORDER BY key ASC LIMIT 1",
        };

        const bind_key = switch (mode) {
            .NEXT, .PREVIOUS => self.current_key,
            .EQUAL, .GREATER_EQUAL, .GREATER => key_from_buffer,
            else => null,
        };

        const not_found_error = switch (mode) {
            .EQUAL, .GREATER_EQUAL, .GREATER => error.NotFound,
            else => error.EndOfFile,
        };

        var stmt: ?*sqlite3.sqlite3_stmt = null;
        var rc = sqlite3.sqlite3_prepare_v2(db, @ptrCast(sql.ptr), @intCast(sql.len), &stmt, null);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;
        defer _ = sqlite3.sqlite3_finalize(stmt.?);

        if (bind_key) |key| {
            rc = sqlite3.sqlite3_bind_blob(stmt.?, 1, @ptrCast(key.ptr), @intCast(key.len), sqlite3.SQLITE_STATIC);
            if (rc != sqlite3.SQLITE_OK) return error.IoError;
        }

        rc = sqlite3.sqlite3_step(stmt.?);
        if (rc == sqlite3.SQLITE_ROW) {
            const key_blob = sqlite3.sqlite3_column_blob(stmt.?, 0);
            const key_len = sqlite3.sqlite3_column_bytes(stmt.?, 0);
            const value_blob = sqlite3.sqlite3_column_blob(stmt.?, 1);
            const value_len = sqlite3.sqlite3_column_bytes(stmt.?, 1);

            if (key_blob == null or value_blob == null) return error.IoError;
            const key_src: [*]const u8 = @ptrCast(key_blob);
            const value_src: [*]const u8 = @ptrCast(value_blob);

            try self.updateCurrentKey(key_src[0..@intCast(key_len)]);
            const copy_len = @min(buffer.len, @as(usize, @intCast(value_len)));
            @memcpy(buffer[0..copy_len], value_src[0..copy_len]);
            return;
        }

        if (rc == sqlite3.SQLITE_DONE) return not_found_error;
        return error.IoError;
    }

    /// Write a new record
    pub fn write(self: *Self, handle: isam.IsamFileHandle, buffer: []const u8) isam.IsamError!void {
        if (self.db == null) return error.IoError;

        const db = self.db.?;

        if (handle.key_offset + handle.key_size > buffer.len) {
            return error.IoError;
        }

        const key = buffer[handle.key_offset .. handle.key_offset + handle.key_size];
        const sql = "INSERT INTO table0 (key, value, deleted) VALUES (?, ?, 0)";

        var stmt: ?*sqlite3.sqlite3_stmt = null;
        var rc = sqlite3.sqlite3_prepare_v2(db, @ptrCast(sql.ptr), @intCast(sql.len), &stmt, null);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;
        defer _ = sqlite3.sqlite3_finalize(stmt.?);

        rc = sqlite3.sqlite3_bind_blob(stmt.?, 1, @ptrCast(key.ptr), @intCast(key.len), sqlite3.SQLITE_STATIC);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_bind_blob(stmt.?, 2, @ptrCast(buffer.ptr), @intCast(buffer.len), sqlite3.SQLITE_STATIC);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_step(stmt.?);
        if (rc == sqlite3.SQLITE_CONSTRAINT) return error.Duplicate;
        if (rc != sqlite3.SQLITE_DONE) return error.IoError;
    }

    /// Rewrite (update) current record
    pub fn rewrite(self: *Self, _: isam.IsamFileHandle, buffer: []const u8) isam.IsamError!void {
        if (self.db == null or self.current_key == null) return error.IoError;

        const db = self.db.?;
        const sql = "UPDATE table0 SET value = ? WHERE key = ? AND deleted = 0";

        var stmt: ?*sqlite3.sqlite3_stmt = null;
        var rc = sqlite3.sqlite3_prepare_v2(db, @ptrCast(sql.ptr), @intCast(sql.len), &stmt, null);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;
        defer _ = sqlite3.sqlite3_finalize(stmt.?);

        rc = sqlite3.sqlite3_bind_blob(stmt.?, 1, @ptrCast(buffer.ptr), @intCast(buffer.len), sqlite3.SQLITE_STATIC);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_bind_blob(stmt.?, 2, @ptrCast(self.current_key.?.ptr), @intCast(self.current_key.?.len), sqlite3.SQLITE_STATIC);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_step(stmt.?);
        if (rc != sqlite3.SQLITE_DONE) return error.IoError;
        if (sqlite3.sqlite3_changes(db) == 0) return error.NotFound;
    }

    /// Delete current record (soft delete)
    pub fn delete(self: *Self, _: isam.IsamFileHandle) isam.IsamError!void {
        if (self.db == null or self.current_key == null) return error.IoError;

        const db = self.db.?;
        const sql = "UPDATE table0 SET deleted = 1 WHERE key = ?";

        var stmt: ?*sqlite3.sqlite3_stmt = null;
        var rc = sqlite3.sqlite3_prepare_v2(db, @ptrCast(sql.ptr), @intCast(sql.len), &stmt, null);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;
        defer _ = sqlite3.sqlite3_finalize(stmt.?);

        rc = sqlite3.sqlite3_bind_blob(stmt.?, 1, @ptrCast(self.current_key.?.ptr), @intCast(self.current_key.?.len), sqlite3.SQLITE_STATIC);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_step(stmt.?);
        if (rc != sqlite3.SQLITE_DONE) return error.IoError;
        if (sqlite3.sqlite3_changes(db) == 0) return error.NotFound;
    }

    /// Start reading by key
    pub fn start(self: *Self, _: isam.IsamFileHandle, key: []const u8, mode: isam.ReadMode) isam.IsamError!void {
        if (self.db == null) return error.IoError;

        const db = self.db.?;
        const sql = switch (mode) {
            .EQUAL => "SELECT key FROM table0 WHERE deleted = 0 AND key = ? LIMIT 1",
            .GREATER_EQUAL => "SELECT key FROM table0 WHERE deleted = 0 AND key >= ? ORDER BY key ASC LIMIT 1",
            .GREATER => "SELECT key FROM table0 WHERE deleted = 0 AND key > ? ORDER BY key ASC LIMIT 1",
            else => return error.NotSupported,
        };

        var stmt: ?*sqlite3.sqlite3_stmt = null;
        var rc = sqlite3.sqlite3_prepare_v2(db, @ptrCast(sql.ptr), @intCast(sql.len), &stmt, null);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;
        defer _ = sqlite3.sqlite3_finalize(stmt.?);

        rc = sqlite3.sqlite3_bind_blob(stmt.?, 1, @ptrCast(key.ptr), @intCast(key.len), sqlite3.SQLITE_STATIC);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_step(stmt.?);
        if (rc == sqlite3.SQLITE_ROW) {
            const key_blob = sqlite3.sqlite3_column_blob(stmt.?, 0);
            const key_len = sqlite3.sqlite3_column_bytes(stmt.?, 0);
            if (key_blob == null) return error.IoError;
            const key_src: [*]const u8 = @ptrCast(key_blob);
            try self.updateCurrentKey(key_src[0..@intCast(key_len)]);
            return;
        }

        if (rc == sqlite3.SQLITE_DONE) return error.NotFound;
        return error.IoError;
    }

    pub fn lock(self: *Self, _: isam.IsamFileHandle, _: isam.LockMode) isam.IsamError!void {
        if (self.db == null) return error.IoError;

        const db = self.db.?;
        const sql = "INSERT INTO file_lock (file_id, locked_by, process_id, locked_at) VALUES (?, ?, ?, CURRENT_TIMESTAMP)";

        var stmt: ?*sqlite3.sqlite3_stmt = null;
        var rc = sqlite3.sqlite3_prepare_v2(db, @ptrCast(sql.ptr), @intCast(sql.len), &stmt, null);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;
        defer _ = sqlite3.sqlite3_finalize(stmt.?);

        rc = sqlite3.sqlite3_bind_text(stmt.?, 1, @ptrCast(self.db_path.ptr), @intCast(self.db_path.len), sqlite3.SQLITE_STATIC);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_bind_text(stmt.?, 2, @ptrCast(self.session_id.ptr), @intCast(self.session_id.len), sqlite3.SQLITE_STATIC);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_bind_text(stmt.?, 3, @ptrCast(self.process_id.ptr), @intCast(self.process_id.len), sqlite3.SQLITE_STATIC);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_step(stmt.?);
        if (rc == sqlite3.SQLITE_CONSTRAINT or rc == sqlite3.SQLITE_BUSY) return error.Locked;
        if (rc != sqlite3.SQLITE_DONE) return error.IoError;
    }

    /// Unlock a file/record
    pub fn unlock(self: *Self, _: isam.IsamFileHandle) isam.IsamError!void {
        if (self.db == null) return error.IoError;

        const db = self.db.?;
        const sql = "DELETE FROM file_lock WHERE file_id = ? AND locked_by = ?";

        var stmt: ?*sqlite3.sqlite3_stmt = null;
        var rc = sqlite3.sqlite3_prepare_v2(db, @ptrCast(sql.ptr), @intCast(sql.len), &stmt, null);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;
        defer _ = sqlite3.sqlite3_finalize(stmt.?);

        rc = sqlite3.sqlite3_bind_text(stmt.?, 1, @ptrCast(self.db_path.ptr), @intCast(self.db_path.len), sqlite3.SQLITE_STATIC);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_bind_text(stmt.?, 2, @ptrCast(self.session_id.ptr), @intCast(self.session_id.len), sqlite3.SQLITE_STATIC);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_step(stmt.?);
        if (rc != sqlite3.SQLITE_DONE) return error.IoError;
    }

    /// Create a new INDEXED file
    pub fn create(self: *Self, filename: []const u8, mode: isam.OpenMode, record_size: usize, key_offset: usize, key_size: usize) isam.IsamError!isam.IsamFileHandle {
        if (mode == .OUTPUT) {
            std.fs.cwd().deleteFile(filename) catch {};
        }

        self.open_mode = mode;
        self.clearCurrentKey();
        try self.openDb(filename);

        const mgr = schema.SchemaManager.init(self.allocator, self.db.?);
        try mgr.createSchema(record_size, key_offset, key_size, 1);

        self.record_size = record_size;
        self.key_offset = key_offset;
        self.key_size = key_size;

        return isam.IsamFileHandle{
            .backend_type = .SQLITE,
            .handle = 1,
            .record_size = record_size,
            .key_offset = key_offset,
            .key_size = key_size,
        };
    }

    fn openDb(self: *Self, db_path: []const u8) isam.IsamError!void {
        self.closeDb();

        const db_path_z = self.allocator.dupeZ(u8, db_path) catch return error.IoError;
        defer self.allocator.free(db_path_z);

        var db: ?*sqlite3.sqlite3 = null;
        const rc = sqlite3.sqlite3_open_v2(
            @ptrCast(db_path_z.ptr),
            &db,
            sqlite3.SQLITE_OPEN_READWRITE | sqlite3.SQLITE_OPEN_CREATE,
            null,
        );
        if (rc != sqlite3.SQLITE_OK or db == null) return error.IoError;

        _ = sqlite3.sqlite3_exec(db.?, "PRAGMA foreign_keys = ON;", null, null, null);

        self.db = db;
        self.allocator.free(self.db_path);
        self.db_path = self.allocator.dupe(u8, db_path) catch return error.IoError;
    }

    fn closeDb(self: *Self) void {
        if (self.db) |db| {
            _ = sqlite3.sqlite3_close(db);
        }
        self.db = null;
    }

    fn clearCurrentKey(self: *Self) void {
        if (self.current_key) |key| {
            self.allocator.free(key);
        }
        self.current_key = null;
    }

    fn updateCurrentKey(self: *Self, key: []const u8) isam.IsamError!void {
        self.clearCurrentKey();
        self.current_key = self.allocator.dupe(u8, key) catch return error.IoError;
    }
};

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

test "SqliteBackend: initialization" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var backend = try SqliteBackend.init(allocator, "/tmp/test_sqlite.db");
    defer backend.deinit();

    try testing.expect(backend.db != null);
}

test "SqliteBackend: create and write" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var backend = try SqliteBackend.init(allocator, "/tmp/test_create.db");
    defer backend.deinit();

    const handle = try backend.create("/tmp/test_create.db", .OUTPUT, 100, 0, 10);
    try testing.expectEqual(@as(usize, 100), handle.record_size);
    try testing.expectEqual(@as(usize, 10), handle.key_size);
}

test "SqliteBackend: file lock" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var backend_a = try SqliteBackend.init(allocator, "/tmp/test_lock.db");
    defer backend_a.deinit();
    const handle_a = try backend_a.create("/tmp/test_lock.db", .OUTPUT, 64, 0, 8);
    try backend_a.lock(handle_a, .EXCLUSIVE);

    var backend_b = try SqliteBackend.init(allocator, "/tmp/test_lock.db");
    defer backend_b.deinit();
    const handle_b = try backend_b.open("/tmp/test_lock.db", .IO);
    try testing.expectError(error.Locked, backend_b.lock(handle_b, .EXCLUSIVE));

    try backend_a.unlock(handle_a);
}
