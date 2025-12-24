const std = @import("std");
const sqlite3 = @import("sqlite3.zig").c;
const isam = @import("isam_interface.zig");

pub const KeyInfo = struct {
    key_number: usize,
    key_offset: usize,
    key_size: usize,
    allow_duplicates: bool,
};

pub const SchemaManager = struct {
    allocator: std.mem.Allocator,
    db: *sqlite3.sqlite3,

    pub fn init(allocator: std.mem.Allocator, db: *sqlite3.sqlite3) SchemaManager {
        return .{
            .allocator = allocator,
            .db = db,
        };
    }

    pub fn ensureBaseSchema(self: *const SchemaManager) isam.IsamError!void {
        try execSQL(self.db, CREATE_TABLE0);
        try execSQL(self.db, CREATE_METADATA_STRING_INT);
        try execSQL(self.db, CREATE_METADATA_KEY);
        try execSQL(self.db, CREATE_FILE_LOCK);
    }

    pub fn createSchema(self: *const SchemaManager, record_size: usize, key_offset: usize, key_size: usize, num_keys: usize) isam.IsamError!void {
        try self.ensureBaseSchema();

        if (num_keys > 1) {
            var key_index: usize = 1;
            while (key_index < num_keys) : (key_index += 1) {
                const sql = std.fmt.allocPrint(self.allocator, CREATE_ALTKEY_TABLE, .{key_index}) catch return error.IoError;
                defer self.allocator.free(sql);
                try execSQL(self.db, sql);
            }
        }

        try setMetadataInt(self.db, "record_size", @intCast(record_size));
        try setMetadataInt(self.db, "num_keys", @intCast(num_keys));
        try setKeyInfo(self.db, 0, key_offset, key_size, false);
    }

    pub fn getRecordSize(self: *const SchemaManager) isam.IsamError!usize {
        const value = try getMetadataInt(self.db, "record_size");
        return @intCast(value);
    }

    pub fn getNumKeys(self: *const SchemaManager) isam.IsamError!usize {
        const value = try getMetadataInt(self.db, "num_keys");
        return @intCast(value);
    }

    pub fn getPrimaryKeyInfo(self: *const SchemaManager) isam.IsamError!KeyInfo {
        return getKeyInfo(self.db, 0);
    }

    fn execSQL(db: *sqlite3.sqlite3, sql: []const u8) isam.IsamError!void {
        var stmt: ?*sqlite3.sqlite3_stmt = null;
        var rc = sqlite3.sqlite3_prepare_v2(db, @ptrCast(sql.ptr), @intCast(sql.len), &stmt, null);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;
        defer _ = sqlite3.sqlite3_finalize(stmt.?);

        rc = sqlite3.sqlite3_step(stmt.?);
        if (rc != sqlite3.SQLITE_DONE) return error.IoError;
    }

    fn setMetadataInt(db: *sqlite3.sqlite3, key: []const u8, value: i32) isam.IsamError!void {
        const sql = "INSERT OR REPLACE INTO metadata_string_int (key, value) VALUES (?, ?)";
        var stmt: ?*sqlite3.sqlite3_stmt = null;
        var rc = sqlite3.sqlite3_prepare_v2(db, @ptrCast(sql.ptr), @intCast(sql.len), &stmt, null);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;
        defer _ = sqlite3.sqlite3_finalize(stmt.?);

        rc = sqlite3.sqlite3_bind_text(stmt.?, 1, @ptrCast(key.ptr), @intCast(key.len), sqlite3.SQLITE_STATIC);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_bind_int(stmt.?, 2, value);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_step(stmt.?);
        if (rc != sqlite3.SQLITE_DONE) return error.IoError;
    }

    fn getMetadataInt(db: *sqlite3.sqlite3, key: []const u8) isam.IsamError!i32 {
        const sql = "SELECT value FROM metadata_string_int WHERE key = ?";
        var stmt: ?*sqlite3.sqlite3_stmt = null;
        var rc = sqlite3.sqlite3_prepare_v2(db, @ptrCast(sql.ptr), @intCast(sql.len), &stmt, null);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;
        defer _ = sqlite3.sqlite3_finalize(stmt.?);

        rc = sqlite3.sqlite3_bind_text(stmt.?, 1, @ptrCast(key.ptr), @intCast(key.len), sqlite3.SQLITE_STATIC);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_step(stmt.?);
        if (rc == sqlite3.SQLITE_ROW) {
            return sqlite3.sqlite3_column_int(stmt.?, 0);
        }
        return error.NotFound;
    }

    fn setKeyInfo(db: *sqlite3.sqlite3, key_number: usize, key_offset: usize, key_size: usize, allow_duplicates: bool) isam.IsamError!void {
        const sql =
            "INSERT OR REPLACE INTO metadata_key (idx, offset, size, duplicate) VALUES (?, ?, ?, ?)";
        var stmt: ?*sqlite3.sqlite3_stmt = null;
        var rc = sqlite3.sqlite3_prepare_v2(db, @ptrCast(sql.ptr), @intCast(sql.len), &stmt, null);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;
        defer _ = sqlite3.sqlite3_finalize(stmt.?);

        rc = sqlite3.sqlite3_bind_int(stmt.?, 1, @intCast(key_number));
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_bind_int(stmt.?, 2, @intCast(key_offset));
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_bind_int(stmt.?, 3, @intCast(key_size));
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_bind_int(stmt.?, 4, if (allow_duplicates) 1 else 0);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_step(stmt.?);
        if (rc != sqlite3.SQLITE_DONE) return error.IoError;
    }

    fn getKeyInfo(db: *sqlite3.sqlite3, key_number: usize) isam.IsamError!KeyInfo {
        const sql = "SELECT offset, size, duplicate FROM metadata_key WHERE idx = ?";
        var stmt: ?*sqlite3.sqlite3_stmt = null;
        var rc = sqlite3.sqlite3_prepare_v2(db, @ptrCast(sql.ptr), @intCast(sql.len), &stmt, null);
        if (rc != sqlite3.SQLITE_OK) return error.IoError;
        defer _ = sqlite3.sqlite3_finalize(stmt.?);

        rc = sqlite3.sqlite3_bind_int(stmt.?, 1, @intCast(key_number));
        if (rc != sqlite3.SQLITE_OK) return error.IoError;

        rc = sqlite3.sqlite3_step(stmt.?);
        if (rc == sqlite3.SQLITE_ROW) {
            const offset_val = sqlite3.sqlite3_column_int(stmt.?, 0);
            const size_val = sqlite3.sqlite3_column_int(stmt.?, 1);
            const dup_val = sqlite3.sqlite3_column_int(stmt.?, 2);

            return KeyInfo{
                .key_number = key_number,
                .key_offset = @intCast(offset_val),
                .key_size = @intCast(size_val),
                .allow_duplicates = dup_val != 0,
            };
        }
        return error.NotFound;
    }

    const CREATE_TABLE0 =
        \\CREATE TABLE IF NOT EXISTS table0 (
        \\  key BLOB PRIMARY KEY,
        \\  value BLOB NOT NULL,
        \\  locked_by TEXT,
        \\  process_id TEXT,
        \\  locked_at TIMESTAMP
        \\);
    ;

    const CREATE_METADATA_STRING_INT =
        \\CREATE TABLE IF NOT EXISTS metadata_string_int (
        \\  key TEXT PRIMARY KEY,
        \\  value INT
        \\);
    ;

    const CREATE_METADATA_KEY =
        \\CREATE TABLE IF NOT EXISTS metadata_key (
        \\  idx INT PRIMARY KEY,
        \\  offset INT,
        \\  size INT,
        \\  duplicate INT
        \\);
    ;

    const CREATE_FILE_LOCK =
        \\CREATE TABLE IF NOT EXISTS file_lock (
        \\  locked_by TEXT PRIMARY KEY,
        \\  process_id TEXT,
        \\  locked_at TIMESTAMP,
        \\  open_mode TEXT CONSTRAINT check_open_mode CHECK (
        \\    open_mode IN ('INPUT', 'OUTPUT', 'I-O', 'EXTEND')
        \\  )
        \\);
    ;

    const CREATE_ALTKEY_TABLE =
        \\CREATE TABLE IF NOT EXISTS table{} (
        \\  key BLOB,
        \\  value BLOB REFERENCES table0(key) ON DELETE CASCADE,
        \\  dupNo INTEGER,
        \\  PRIMARY KEY (key, dupNo)
        \\);
    ;
};
