const std = @import("std");
const build_options = @import("build_options");

const enable_vbisam = build_options.backend == .vbisam or build_options.backend == .both;
const enable_sqlite = build_options.backend == .sqlite or build_options.backend == .both;

/// ISAM エラー型（バックエンド非依存）
pub const IsamError = error{
    Duplicate,      // 重複キー
    NotFound,       // レコード未検出
    EndOfFile,      // EOF
    Locked,         // ロック中
    IoError,        // 汎用I/Oエラー
    NotSupported,   // 未サポート操作
};

/// ファイルオープンモード
pub const OpenMode = enum {
    INPUT,
    OUTPUT,
    IO,
    EXTEND,
};

/// レコード読み込みモード
pub const ReadMode = enum {
    FIRST,
    NEXT,
    PREVIOUS,
    LAST,
    EQUAL,
    GREATER_EQUAL,
    GREATER,
};

/// ロックモード
pub const LockMode = enum {
    NONE,
    SHARED,
    EXCLUSIVE,
};

/// バックエンド種別
pub const BackendType = enum {
    VBISAM,
    SQLITE,
    BDB,
    CISAM,
};

/// バックエンド非依存のISAMファイルハンドル
pub const IsamFileHandle = struct {
    backend_type: BackendType,
    handle: c_int,
    record_size: usize,
    key_offset: usize,
    key_size: usize,

    /// レコードを読み込み
    pub fn read(self: *IsamFileHandle, buffer: []u8, mode: ReadMode) IsamError!void {
        _ = self;
        _ = buffer;
        _ = mode;
        // このメソッドはバックエンドごとに呼び出される
        // IsamBackend経由でディスパッチ
        return IsamError.NotSupported;
    }

    /// レコードを書き込み
    pub fn write(self: *IsamFileHandle, buffer: []const u8) IsamError!void {
        _ = self;
        _ = buffer;
        // このメソッドはバックエンドごとに呼び出される
        // IsamBackend経由でディスパッチ
        return IsamError.NotSupported;
    }
};

/// VBISAMバックエンド（フォワード宣言）
pub const VbisamBackend = if (enable_vbisam)
    @import("isam_vbisam.zig").VbisamBackend
else
    struct {
        pub fn init(_: std.mem.Allocator) @This() {
            return .{};
        }

        pub fn deinit(_: *@This()) void {}

        pub fn open(_: *@This(), _: []const u8, _: OpenMode) IsamError!IsamFileHandle {
            return error.NotSupported;
        }

        pub fn close(_: *@This(), _: IsamFileHandle) IsamError!void {
            return error.NotSupported;
        }

        pub fn read(_: *@This(), _: IsamFileHandle, _: []u8, _: ReadMode) IsamError!void {
            return error.NotSupported;
        }

        pub fn write(_: *@This(), _: IsamFileHandle, _: []const u8) IsamError!void {
            return error.NotSupported;
        }

        pub fn rewrite(_: *@This(), _: IsamFileHandle, _: []const u8) IsamError!void {
            return error.NotSupported;
        }

        pub fn delete(_: *@This(), _: IsamFileHandle) IsamError!void {
            return error.NotSupported;
        }

        pub fn start(_: *@This(), _: IsamFileHandle, _: []const u8, _: ReadMode) IsamError!void {
            return error.NotSupported;
        }

        pub fn lock(_: *@This(), _: IsamFileHandle, _: LockMode) IsamError!void {
            return error.NotSupported;
        }

        pub fn unlock(_: *@This(), _: IsamFileHandle) IsamError!void {
            return error.NotSupported;
        }

        pub fn create(_: *@This(), _: []const u8, _: OpenMode, _: usize, _: usize, _: usize) IsamError!IsamFileHandle {
            return error.NotSupported;
        }
    };

/// SQLiteバックエンド（フォワード宣言）
pub const SqliteBackend = if (enable_sqlite)
    @import("isam_sqlite.zig").SqliteBackend
else
    struct {
        pub fn init(_: std.mem.Allocator, _: []const u8) !@This() {
            return .{};
        }

        pub fn deinit(_: *@This()) void {}

        pub fn open(_: *@This(), _: []const u8, _: OpenMode) IsamError!IsamFileHandle {
            return error.NotSupported;
        }

        pub fn close(_: *@This(), _: IsamFileHandle) IsamError!void {
            return error.NotSupported;
        }

        pub fn read(_: *@This(), _: IsamFileHandle, _: []u8, _: ReadMode) IsamError!void {
            return error.NotSupported;
        }

        pub fn write(_: *@This(), _: IsamFileHandle, _: []const u8) IsamError!void {
            return error.NotSupported;
        }

        pub fn rewrite(_: *@This(), _: IsamFileHandle, _: []const u8) IsamError!void {
            return error.NotSupported;
        }

        pub fn delete(_: *@This(), _: IsamFileHandle) IsamError!void {
            return error.NotSupported;
        }

        pub fn start(_: *@This(), _: IsamFileHandle, _: []const u8, _: ReadMode) IsamError!void {
            return error.NotSupported;
        }

        pub fn lock(_: *@This(), _: IsamFileHandle, _: LockMode) IsamError!void {
            return error.NotSupported;
        }

        pub fn unlock(_: *@This(), _: IsamFileHandle) IsamError!void {
            return error.NotSupported;
        }

        pub fn create(_: *@This(), _: []const u8, _: OpenMode, _: usize, _: usize, _: usize) IsamError!IsamFileHandle {
            return error.NotSupported;
        }
    };

/// ISAM実装の抽象バックエンド
pub const IsamBackend = union(enum) {
    VBISAM: VbisamBackend,
    SQLITE: SqliteBackend,
    // BDB: BdbBackend,         // 将来の拡張
    // CISAM: CisamBackend,     // 将来の拡張

    /// ファイルを開く
    pub fn open(self: *IsamBackend, filename: []const u8, mode: OpenMode) IsamError!IsamFileHandle {
        return switch (self.*) {
            .VBISAM => |*backend| backend.open(filename, mode),
            .SQLITE => |*backend| backend.open(filename, mode),
        };
    }

    pub fn deinit(self: *IsamBackend) void {
        switch (self.*) {
            .VBISAM => |*backend| backend.deinit(),
            .SQLITE => |*backend| backend.deinit(),
        }
    }

    /// ファイルを閉じる
    pub fn close(self: *IsamBackend, handle: IsamFileHandle) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.close(handle),
            .SQLITE => |*backend| backend.close(handle),
        };
    }

    /// レコードを読み込み
    pub fn read(self: *IsamBackend, handle: IsamFileHandle, buffer: []u8, mode: ReadMode) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.read(handle, buffer, mode),
            .SQLITE => |*backend| backend.read(handle, buffer, mode),
        };
    }

    /// レコードを書き込み
    pub fn write(self: *IsamBackend, handle: IsamFileHandle, buffer: []const u8) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.write(handle, buffer),
            .SQLITE => |*backend| backend.write(handle, buffer),
        };
    }

    /// レコードを更新（上書き）
    pub fn rewrite(self: *IsamBackend, handle: IsamFileHandle, buffer: []const u8) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.rewrite(handle, buffer),
            .SQLITE => |*backend| backend.rewrite(handle, buffer),
        };
    }

    /// レコードを削除
    pub fn delete(self: *IsamBackend, handle: IsamFileHandle) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.delete(handle),
            .SQLITE => |*backend| backend.delete(handle),
        };
    }

    /// キー位置に移動
    pub fn start(self: *IsamBackend, handle: IsamFileHandle, key: []const u8, mode: ReadMode) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.start(handle, key, mode),
            .SQLITE => |*backend| backend.start(handle, key, mode),
        };
    }

    /// ファイルをロック
    pub fn lock(self: *IsamBackend, handle: IsamFileHandle, mode: LockMode) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.lock(handle, mode),
            .SQLITE => |*backend| backend.lock(handle, mode),
        };
    }

    /// ファイルのロックを解除
    pub fn unlock(self: *IsamBackend, handle: IsamFileHandle) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.unlock(handle),
            .SQLITE => |*backend| backend.unlock(handle),
        };
    }

    /// 新規ファイルを作成（OUTPUT/EXTEND モード用）
    pub fn create(self: *IsamBackend, filename: []const u8, mode: OpenMode, record_size: usize, key_offset: usize, key_size: usize) IsamError!IsamFileHandle {
        return switch (self.*) {
            .VBISAM => |*backend| backend.create(filename, mode, record_size, key_offset, key_size),
            .SQLITE => |*backend| backend.create(filename, mode, record_size, key_offset, key_size),
        };
    }
};

// ============================================================================
// テスト
// ============================================================================

const testing = std.testing;

test "IsamBackend: VBISAM initialization" {
    const allocator = testing.allocator;
    var backend = IsamBackend{
        .VBISAM = VbisamBackend.init(allocator),
    };
    _ = &backend;
}

test "IsamFileHandle: basic structure" {
    const handle = IsamFileHandle{
        .backend_type = .VBISAM,
        .handle = 42,
        .record_size = 100,
        .key_offset = 0,
        .key_size = 10,
    };

    try testing.expectEqual(@as(c_int, 42), handle.handle);
    try testing.expectEqual(@as(usize, 100), handle.record_size);
    try testing.expectEqual(BackendType.VBISAM, handle.backend_type);
}

test "IsamError: enum values" {
    const err: IsamError = error.NotFound;
    try testing.expect(std.mem.eql(u8, @errorName(err), "NotFound"));
}

test "OpenMode: enum values" {
    try testing.expectEqual(OpenMode.INPUT, .INPUT);
    try testing.expectEqual(OpenMode.OUTPUT, .OUTPUT);
    try testing.expectEqual(OpenMode.IO, .IO);
    try testing.expectEqual(OpenMode.EXTEND, .EXTEND);
}

test "ReadMode: enum values" {
    try testing.expectEqual(ReadMode.FIRST, .FIRST);
    try testing.expectEqual(ReadMode.NEXT, .NEXT);
    try testing.expectEqual(ReadMode.EQUAL, .EQUAL);
    try testing.expectEqual(ReadMode.GREATER_EQUAL, .GREATER_EQUAL);
}

test "IsamBackend: create method dispatch" {
    const allocator = testing.allocator;
    var backend = IsamBackend{
        .VBISAM = VbisamBackend.init(allocator),
    };
    _ = &backend;
    // create() メソッドのディスパッチ確認
    // 実際のファイル作成テストは統合テストで実施
}
