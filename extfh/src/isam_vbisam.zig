const std = @import("std");
const vbisam = @import("vbisam.zig");
const isam = @import("isam_interface.zig");

/// VBISAM バックエンド実装
pub const VbisamBackend = struct {
    allocator: std.mem.Allocator,

    /// VbisamBackend を初期化
    pub fn init(allocator: std.mem.Allocator) VbisamBackend {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *VbisamBackend) void {
        _ = self;
    }

    /// VBISAMエラーを汎用IsamErrorに変換
    fn mapError(err: vbisam.VbisamError) isam.IsamError {
        return switch (err) {
            error.Duplicate => error.Duplicate,
            error.NoRecord, error.NoCurrent => error.NotFound,
            error.EndFile => error.EndOfFile,
            error.Locked => error.Locked,
            else => error.IoError,
        };
    }

    /// VBISAM OpenMode を isam.OpenMode に変換
    fn mapOpenMode(mode: isam.OpenMode) vbisam.OpenMode {
        return switch (mode) {
            .INPUT => vbisam.OpenMode.INPUT,
            .OUTPUT => vbisam.OpenMode.OUTPUT,
            .IO => vbisam.OpenMode.INOUT,
            .EXTEND => vbisam.OpenMode.INOUT, // EXTEND maps to INOUT (append mode)
        };
    }

    /// VBISAM ReadMode を isam.ReadMode に変換
    fn mapReadMode(mode: isam.ReadMode) vbisam.ReadMode {
        return switch (mode) {
            .FIRST => vbisam.ReadMode.FIRST,
            .NEXT => vbisam.ReadMode.NEXT,
            .PREVIOUS => vbisam.ReadMode.PREV,
            .LAST => vbisam.ReadMode.LAST,
            .EQUAL => vbisam.ReadMode.EQUAL,
            .GREATER_EQUAL => vbisam.ReadMode.GTEQ,
            .GREATER => vbisam.ReadMode.GREAT,
        };
    }

    /// ファイルを開く
    pub fn open(self: *VbisamBackend, filename: []const u8, mode: isam.OpenMode) isam.IsamError!isam.IsamFileHandle {
        const vb_mode = mapOpenMode(mode);

        const vb_file = vbisam.IsamFile.open(self.allocator, filename, vb_mode, .AUTO) catch |err| {
            return mapError(err);
        };

        return isam.IsamFileHandle{
            .backend_type = .VBISAM,
            .handle = vb_file.handle,
            .record_size = vb_file.record_size,
            .key_offset = 0,
            .key_size = 0,
        };
    }

    /// ファイルを閉じる
    pub fn close(self: *VbisamBackend, handle: isam.IsamFileHandle) isam.IsamError!void {
        _ = self;
        var vb_file = vbisam.IsamFile{
            .handle = handle.handle,
            .record_size = handle.record_size,
            .allocator = undefined,
        };
        vb_file.close();
    }

    /// レコードを読み込み
    pub fn read(self: *VbisamBackend, handle: isam.IsamFileHandle, buffer: []u8, mode: isam.ReadMode) isam.IsamError!void {
        _ = self;
        const vb_mode = mapReadMode(mode);

        var vb_file = vbisam.IsamFile{
            .handle = handle.handle,
            .record_size = handle.record_size,
            .allocator = undefined,
        };

        vb_file.read(buffer, vb_mode, .NONE) catch |err| {
            return mapError(err);
        };
    }

    /// レコードを書き込み
    pub fn write(self: *VbisamBackend, handle: isam.IsamFileHandle, buffer: []const u8) isam.IsamError!void {
        _ = self;
        var vb_file = vbisam.IsamFile{
            .handle = handle.handle,
            .record_size = handle.record_size,
            .allocator = undefined,
        };

        vb_file.write(@constCast(buffer)) catch |err| {
            return mapError(err);
        };
    }

    /// レコードを更新（上書き）
    pub fn rewrite(self: *VbisamBackend, handle: isam.IsamFileHandle, buffer: []const u8) isam.IsamError!void {
        _ = self;
        var vb_file = vbisam.IsamFile{
            .handle = handle.handle,
            .record_size = handle.record_size,
            .allocator = undefined,
        };

        vb_file.rewrite(@constCast(buffer)) catch |err| {
            return mapError(err);
        };
    }

    /// レコードを削除
    pub fn delete(self: *VbisamBackend, handle: isam.IsamFileHandle) isam.IsamError!void {
        _ = self;
        var vb_file = vbisam.IsamFile{
            .handle = handle.handle,
            .record_size = handle.record_size,
            .allocator = undefined,
        };

        vb_file.deleteCurrent() catch |err| {
            return mapError(err);
        };
    }

    /// キー位置に移動
    pub fn start(self: *VbisamBackend, handle: isam.IsamFileHandle, key: []const u8, mode: isam.ReadMode) isam.IsamError!void {
        const vb_mode = mapReadMode(mode);

        var vb_file = vbisam.IsamFile{
            .handle = handle.handle,
            .record_size = handle.record_size,
            .allocator = self.allocator,
        };

        // Build key descriptor for START operation
        var key_builder = vbisam.KeyDescBuilder.init();
        _ = key_builder.addPart(@as(c_short, @intCast(handle.key_offset)), @as(c_short, @intCast(handle.key_size)), vbisam.KeyType.CHAR);

        vb_file.start(key_builder.build(), key.len, @constCast(key), vb_mode) catch |err| {
            return mapError(err);
        };
    }

    /// ファイルをロック
    pub fn lock(self: *VbisamBackend, handle: isam.IsamFileHandle, mode: isam.LockMode) isam.IsamError!void {
        _ = self;
        _ = handle;
        _ = mode;
        // VBISAM lock/unlock操作はプライマリで管理される
        // ここではスキップ（no-op）
        return;
    }

    /// ファイルのロックを解除
    pub fn unlock(self: *VbisamBackend, handle: isam.IsamFileHandle) isam.IsamError!void {
        _ = self;
        _ = handle;
        // VBISAM の releaseAll() に相当する操作（全レコードのロック解除）
        // VBISAMではプライマリにロック操作がない場合がある
        // ここではスキップ（no-op）
        return;
    }

    /// 新規ファイルを作成（OUTPUT/EXTEND モード用）
    pub fn create(self: *VbisamBackend, filename: []const u8, mode: isam.OpenMode, record_size: usize, key_offset: usize, key_size: usize) isam.IsamError!isam.IsamFileHandle {
        const vb_mode = switch (mode) {
            .OUTPUT, .EXTEND => vbisam.OpenMode.INOUT,
            else => mapOpenMode(mode),
        };

        if (mode == .OUTPUT) {
            const data_path = std.fmt.allocPrint(self.allocator, "{s}.dat", .{filename}) catch return isam.IsamError.IoError;
            defer self.allocator.free(data_path);
            const index_path = std.fmt.allocPrint(self.allocator, "{s}.idx", .{filename}) catch return isam.IsamError.IoError;
            defer self.allocator.free(index_path);
            std.fs.cwd().deleteFile(data_path) catch {};
            std.fs.cwd().deleteFile(index_path) catch {};
        }

        // KeyDescBuilder を使用してキー記述子を構築
        var key_builder = vbisam.KeyDescBuilder.init();
        _ = key_builder.addPart(@as(c_short, @intCast(key_offset)), @as(c_short, @intCast(key_size)), vbisam.KeyType.CHAR);
        const key_desc = key_builder.build();

        const vb_file = vbisam.IsamFile.build(
            self.allocator,
            filename,
            record_size,
            key_desc,
            vb_mode,
            vbisam.FileLockMode.EXCLUSIVE,
            false,  // variable length
        ) catch |err| {
            return mapError(err);
        };

        return isam.IsamFileHandle{
            .backend_type = .VBISAM,
            .handle = vb_file.handle,
            .record_size = vb_file.record_size,
            .key_offset = key_offset,
            .key_size = key_size,
        };
    }
};

// ============================================================================
// テスト
// ============================================================================

const testing = std.testing;

test "VbisamBackend: initialization" {
    const backend = VbisamBackend.init(testing.allocator);
    _ = backend;
}

test "VbisamBackend: mapOpenMode conversion" {
    const input_mode = isam.OpenMode.INPUT;
    const mapped = VbisamBackend.mapOpenMode(input_mode);
    try testing.expectEqual(vbisam.OpenMode.INPUT, mapped);

    const io_mode = isam.OpenMode.IO;
    const mapped_io = VbisamBackend.mapOpenMode(io_mode);
    try testing.expectEqual(vbisam.OpenMode.INOUT, mapped_io);
}

test "VbisamBackend: mapReadMode conversion" {
    const first_mode = isam.ReadMode.FIRST;
    const mapped = VbisamBackend.mapReadMode(first_mode);
    try testing.expectEqual(vbisam.ReadMode.FIRST, mapped);

    const next_mode = isam.ReadMode.NEXT;
    const mapped_next = VbisamBackend.mapReadMode(next_mode);
    try testing.expectEqual(vbisam.ReadMode.NEXT, mapped_next);
}

test "VbisamBackend: mapError conversion" {
    const duplicate_err: vbisam.VbisamError = error.Duplicate;
    const mapped = VbisamBackend.mapError(duplicate_err);
    try testing.expect(std.mem.eql(u8, @errorName(mapped), "Duplicate"));

    const norecord_err: vbisam.VbisamError = error.NoRecord;
    const mapped_nf = VbisamBackend.mapError(norecord_err);
    try testing.expect(std.mem.eql(u8, @errorName(mapped_nf), "NotFound"));
}

test "VbisamBackend: create method interface" {
    const backend = VbisamBackend.init(testing.allocator);
    _ = backend;
    // create() メソッドのシグネチャ確認
    // 実際のファイル作成テストは統合テストで実施
}
