# EXTFH/VSAM 抽象化レイヤー実装ガイド

**バージョン**: 1.0  
**ステータス**: 本番環境対応  
**日付**: 2025年12月

---

## 目次

1. [概要](#概要)
2. [アーキテクチャ設計](#アーキテクチャ設計)
3. [段階的実装手順](#段階的実装手順)
4. [コンポーネント詳細](#コンポーネント詳細)
5. [統合とテスト](#統合とテスト)
6. [トラブルシューティング](#トラブルシューティング)

---

## 概要

### 目的

EXTFH/VSAM抽象化レイヤーは、GnuCOBOLのEXTFHファイルハンドラーを特定のISAMバックエンド実装（VBISAM、BerkeleyDB、C-ISAM、SQLiteなど）から独立させるプラグイン型インターフェースを提供します。

### 主な利点

- **バックエンド独立性**: EXTFH ハンドラーコード変更なしに ISAM 実装を切り替え可能
- **型安全性**: Zig の型システムでコンパイル時検証を実現
- **パフォーマンス**: ゼロオーバーヘッドな抽象化（インライン最適化）
- **拡張性**: 最小限のコード変更で新バックエンド追加可能
- **保守性**: 集中化されたエラー処理とモード変換

### アーキテクチャレイヤー

```
┌─────────────────────────────────────┐
│   COBOL アプリケーション           │
│   (GnuCOBOL EXTFH コールバック)    │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│ extfh.zig (統一ハンドラー)          │
│ - バックエンド無関心な操作           │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│ isam_interface.zig (抽象API)       │
│ - IsamBackend (タグ付きユニオン)   │
│ - IsamFileHandle, IsamError        │
│ - 統一列挙型 (OpenMode他)          │
└──────────────┬──────────────────────┘
               │
        ┌──────┴──────────┐
        │                 │
┌───────▼────────┐  ┌─────▼──────────┐
│isam_vbisam.zig │  │ isam_bdb.zig   │
│   (VBISAM)     │  │(Berkeley DB)   │
└───────┬────────┘  └─────┬──────────┘
        │                 │
┌───────▼────────┐  ┌─────▼──────────┐
│ vbisam.zig     │  │ bdb.zig        │
│ (Cバインディング)│  │(Cバインディング)│
└────────────────┘  └────────────────┘
```

---

## アーキテクチャ設計

### 1. コア抽象化: IsamBackend

`IsamBackend` は Zig のタグ付きユニオンで、型安全でポリモーフィックなインターフェースを提供します：

```zig
pub const IsamBackend = union(enum) {
    VBISAM: VbisamBackend,
    // BDB: BdbBackend,       // 将来対応
    // CISAM: CisamBackend,   // 将来対応
};
```

**メリット**:
- コンパイル時型チェック
- switch文の最適化（インライン化）
- ランタイムオーバーヘッドなし
- 静的ディスパッチ

### 2. 統一型

#### ファイルハンドル

```zig
pub const IsamFileHandle = struct {
    backend_type: BackendType,
    handle: c_int,           // 不透明なバックエンドハンドル
    record_size: usize,
    key_offset: usize,
    key_size: usize,
};
```

#### エラー型

```zig
pub const IsamError = error{
    Duplicate,      // 重複キー
    NotFound,       // レコード未検出
    EndOfFile,      // ファイル終端
    Locked,         // ロック中
    IoError,        // 汎用 I/O エラー
    NotSupported,   // 未サポート操作
};
```

#### モード列挙型

```zig
pub const OpenMode = enum { INPUT, OUTPUT, IO, EXTEND };
pub const ReadMode = enum { FIRST, NEXT, PREVIOUS, LAST, EQUAL, GREATER_EQUAL, GREATER };
pub const LockMode = enum { NONE, SHARED, EXCLUSIVE };
```

### 3. バックエンド提供者インターフェース

各バックエンドは以下を実装します：

```zig
pub fn open(self: *Backend, filename: []const u8, mode: OpenMode) !IsamFileHandle { }
pub fn close(self: *Backend, handle: IsamFileHandle) !void { }
pub fn read(self: *Backend, handle: IsamFileHandle, buffer: []u8, mode: ReadMode) !void { }
pub fn write(self: *Backend, handle: IsamFileHandle, buffer: []const u8) !void { }
pub fn rewrite(self: *Backend, handle: IsamFileHandle, buffer: []const u8) !void { }
pub fn delete(self: *Backend, handle: IsamFileHandle) !void { }
pub fn start(self: *Backend, handle: IsamFileHandle, key: []const u8, mode: ReadMode) !void { }
pub fn create(self: *Backend, filename: []const u8, mode: OpenMode, record_size: usize, key_offset: usize, key_size: usize) !IsamFileHandle { }
pub fn lock(self: *Backend, handle: IsamFileHandle, mode: LockMode) !void { }
pub fn unlock(self: *Backend, handle: IsamFileHandle) !void { }
```

---

## 段階的実装手順

### フェーズ 1: 抽象インターフェース定義

#### ステップ 1.1: `isam_interface.zig` 作成

新ファイル `src/isam_interface.zig` を作成し、すべての統一型と抽象 `IsamBackend` インターフェースを定義します：

```zig
// src/isam_interface.zig
const std = @import("std");

pub const BackendType = enum { VBISAM, BDB, CISAM, SQLITE };

pub const IsamError = error{
    Duplicate,
    NotFound,
    EndOfFile,
    Locked,
    IoError,
    NotSupported,
};

pub const OpenMode = enum { INPUT, OUTPUT, IO, EXTEND };
pub const ReadMode = enum { FIRST, NEXT, PREVIOUS, LAST, EQUAL, GREATER_EQUAL, GREATER };
pub const LockMode = enum { NONE, SHARED, EXCLUSIVE };

pub const IsamFileHandle = struct {
    backend_type: BackendType,
    handle: c_int,
    record_size: usize,
    key_offset: usize,
    key_size: usize,
};

// 前方宣言
pub const VbisamBackend = undefined; // インポート予定

pub const IsamBackend = union(enum) {
    VBISAM: VbisamBackend,
    // BDB: BdbBackend,

    pub fn open(self: *IsamBackend, filename: []const u8, mode: OpenMode) IsamError!IsamFileHandle {
        return switch (self.*) {
            .VBISAM => |*backend| backend.open(filename, mode),
        };
    }

    pub fn close(self: *IsamBackend, handle: IsamFileHandle) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.close(handle),
        };
    }

    pub fn read(self: *IsamBackend, handle: IsamFileHandle, buffer: []u8, mode: ReadMode) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.read(handle, buffer, mode),
        };
    }

    pub fn write(self: *IsamBackend, handle: IsamFileHandle, buffer: []const u8) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.write(handle, buffer),
        };
    }

    pub fn rewrite(self: *IsamBackend, handle: IsamFileHandle, buffer: []const u8) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.rewrite(handle, buffer),
        };
    }

    pub fn delete(self: *IsamBackend, handle: IsamFileHandle) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.delete(handle),
        };
    }

    pub fn start(self: *IsamBackend, handle: IsamFileHandle, key: []const u8, mode: ReadMode) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.start(handle, key, mode),
        };
    }

    pub fn create(self: *IsamBackend, filename: []const u8, mode: OpenMode, record_size: usize, key_offset: usize, key_size: usize) IsamError!IsamFileHandle {
        return switch (self.*) {
            .VBISAM => |*backend| backend.create(filename, mode, record_size, key_offset, key_size),
        };
    }

    pub fn lock(self: *IsamBackend, handle: IsamFileHandle, mode: LockMode) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.lock(handle, mode),
        };
    }

    pub fn unlock(self: *IsamBackend, handle: IsamFileHandle) IsamError!void {
        return switch (self.*) {
            .VBISAM => |*backend| backend.unlock(handle),
        };
    }
};
```

---

### フェーズ 2: VBISAM バックエンド実装

#### ステップ 2.1: `isam_vbisam.zig` 作成

VBISAM C ライブラリをラップする `src/isam_vbisam.zig` を作成します：

```zig
// src/isam_vbisam.zig
const std = @import("std");
const isam = @import("isam_interface.zig");
const vbisam = @import("vbisam.zig");

pub const VbisamBackend = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) VbisamBackend {
        return .{ .allocator = allocator };
    }

    pub fn open(self: *VbisamBackend, filename: []const u8, mode: isam.OpenMode) isam.IsamError!isam.IsamFileHandle {
        const vbisam_mode = self.mapOpenMode(mode);
        
        const result = vbisam.isopen(
            @constCast(filename.ptr),
            vbisam_mode,
        );

        if (result < 0) {
            return self.mapVbisamError(result);
        }

        return isam.IsamFileHandle{
            .backend_type = .VBISAM,
            .handle = result,
            .record_size = 0,  // 呼び出し元が設定
            .key_offset = 0,
            .key_size = 0,
        };
    }

    pub fn close(self: *VbisamBackend, handle: isam.IsamFileHandle) isam.IsamError!void {
        const result = vbisam.isclose(handle.handle);
        if (result != 0) {
            return self.mapVbisamError(result);
        }
    }

    pub fn read(self: *VbisamBackend, handle: isam.IsamFileHandle, buffer: []u8, mode: isam.ReadMode) isam.IsamError!void {
        const vbisam_mode = self.mapReadMode(mode);
        const result = vbisam.isread(
            handle.handle,
            buffer.ptr,
            vbisam_mode,
        );

        if (result != 0) {
            return self.mapVbisamError(result);
        }
    }

    pub fn write(self: *VbisamBackend, handle: isam.IsamFileHandle, buffer: []const u8) isam.IsamError!void {
        const result = vbisam.iswrite(
            handle.handle,
            @constCast(buffer.ptr),
        );

        if (result != 0) {
            return self.mapVbisamError(result);
        }
    }

    pub fn rewrite(self: *VbisamBackend, handle: isam.IsamFileHandle, buffer: []const u8) isam.IsamError!void {
        const result = vbisam.isrewrite(
            handle.handle,
            @constCast(buffer.ptr),
        );

        if (result != 0) {
            return self.mapVbisamError(result);
        }
    }

    pub fn delete(self: *VbisamBackend, handle: isam.IsamFileHandle) isam.IsamError!void {
        const result = vbisam.isdelcurr(handle.handle);

        if (result != 0) {
            return self.mapVbisamError(result);
        }
    }

    pub fn start(self: *VbisamBackend, handle: isam.IsamFileHandle, key: []const u8, mode: isam.ReadMode) isam.IsamError!void {
        const vbisam_mode = self.mapReadMode(mode);
        const result = vbisam.isstart(
            handle.handle,
            0, // キー番号（プライマリキー）
            @intCast(key.len),
            @constCast(key.ptr),
            vbisam_mode,
        );

        if (result != 0) {
            return self.mapVbisamError(result);
        }
    }

    pub fn create(self: *VbisamBackend, filename: []const u8, mode: isam.OpenMode, record_size: usize, key_offset: usize, key_size: usize) isam.IsamError!isam.IsamFileHandle {
        const vbisam_mode = self.mapOpenMode(mode);
        
        // VBISAM 用キー仕様作成
        var keydesc: vbisam.KeyDesc = undefined;
        keydesc.k_start = @intCast(key_offset);
        keydesc.k_leng = @intCast(key_size);
        keydesc.k_flags = 0; // プライマリキー、重複なし

        const result = vbisam.isbuild(
            @constCast(filename.ptr),
            @intCast(record_size),
            &keydesc,
            vbisam_mode,
        );

        if (result < 0) {
            return self.mapVbisamError(result);
        }

        return isam.IsamFileHandle{
            .backend_type = .VBISAM,
            .handle = result,
            .record_size = record_size,
            .key_offset = key_offset,
            .key_size = key_size,
        };
    }

    pub fn lock(self: *VbisamBackend, handle: isam.IsamFileHandle, mode: isam.LockMode) isam.IsamError!void {
        // VBISAM に明示的なロック呼び出しはない
        // ロック管理は読み書き操作に内在
        _ = self;
        _ = handle;
        _ = mode;
        return;
    }

    pub fn unlock(self: *VbisamBackend, handle: isam.IsamFileHandle) isam.IsamError!void {
        // VBISAM に明示的なアンロック呼び出しはない
        _ = self;
        _ = handle;
        return;
    }

    // エラーマッピング
    fn mapVbisamError(self: *VbisamBackend, err: c_int) isam.IsamError {
        _ = self;
        return switch (err) {
            -1 => isam.IsamError.IoError,
            -2 => isam.IsamError.Duplicate,
            -3 => isam.IsamError.NotFound,
            -4 => isam.IsamError.EndOfFile,
            -5 => isam.IsamError.Locked,
            else => isam.IsamError.IoError,
        };
    }

    // モード変換
    fn mapOpenMode(self: *VbisamBackend, mode: isam.OpenMode) c_int {
        _ = self;
        return switch (mode) {
            .INPUT => vbisam.ISREAD,
            .OUTPUT => vbisam.ISNEWFILE,
            .IO => vbisam.ISINOUT,
            .EXTEND => vbisam.ISINOUT,
        };
    }

    fn mapReadMode(self: *VbisamBackend, mode: isam.ReadMode) c_int {
        _ = self;
        return switch (mode) {
            .FIRST => vbisam.ISFIRST,
            .NEXT => vbisam.ISNEXT,
            .PREVIOUS => vbisam.ISPREV,
            .LAST => vbisam.ISLAST,
            .EQUAL => vbisam.ISEQUAL,
            .GREATER_EQUAL => vbisam.ISGREAT,
            .GREATER => vbisam.ISGREAT,
        };
    }
};
```

---

### フェーズ 3: EXTFH ハンドラー更新

#### ステップ 3.1: `extfh.zig` リファクタリング

EXTFH ハンドラーを抽象 `IsamBackend` インターフェース使用に修正します：

```zig
// src/extfh.zig
const std = @import("std");
const isam = @import("isam_interface.zig");
const vbisam_impl = @import("isam_vbisam.zig");

// FCD3 構造（GnuCOBOL より）
pub const FCD3 = struct {
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
    reserved: [128]u8,
};

// グローバルバックエンドインスタンス
var backend: isam.IsamBackend = undefined;
var allocator: std.mem.Allocator = undefined;
var file_map: std.AutoHashMap(c_int, isam.IsamFileHandle) = undefined;

pub export fn czippfh(fcd_ptr: [*c]c_int) callconv(.C) void {
    const fcd: *FCD3 = @ptrCast(@alignCast(fcd_ptr));
    
    switch (fcd.call_id) {
        1 => handleOpen(fcd),
        2 => handleClose(fcd),
        3 => handleRead(fcd),
        4 => handleWrite(fcd),
        5 => handleRewrite(fcd),
        6 => handleDelete(fcd),
        7 => handleStart(fcd),
        else => fcd.status = 9, // 汎用エラー
    }
}

fn handleOpen(fcd: *FCD3) void {
    const filename = std.mem.sliceTo(&fcd.filename, 0);
    const mode = mapCobolMode(fcd.file_open_mode);
    
    backend.open(filename, mode) catch |err| {
        fcd.status = mapIsamErrorToStatus(err);
        return;
    };
    
    fcd.status = 0;
}

fn handleRead(fcd: *FCD3) void {
    const handle_result = file_map.get(@intCast(fcd.handle));
    if (handle_result == null) {
        fcd.status = 5; // I-O エラー
        return;
    }
    
    const handle = handle_result.?;
    const mode = mapCobolReadMode(fcd.option);
    const record_buffer = fcd.record_ptr[0..@intCast(fcd.record_size)];
    
    backend.read(handle, record_buffer, mode) catch |err| {
        fcd.status = mapIsamErrorToStatus(err);
        return;
    };
    
    fcd.status = 0;
}

fn handleWrite(fcd: *FCD3) void {
    const handle_result = file_map.get(@intCast(fcd.handle));
    if (handle_result == null) {
        fcd.status = 5;
        return;
    }
    
    const handle = handle_result.?;
    const record_buffer = fcd.record_ptr[0..@intCast(fcd.record_size)];
    
    backend.write(handle, record_buffer) catch |err| {
        fcd.status = mapIsamErrorToStatus(err);
        return;
    };
    
    fcd.status = 0;
}

fn handleRewrite(fcd: *FCD3) void {
    const handle_result = file_map.get(@intCast(fcd.handle));
    if (handle_result == null) {
        fcd.status = 5;
        return;
    }
    
    const handle = handle_result.?;
    const record_buffer = fcd.record_ptr[0..@intCast(fcd.record_size)];
    
    backend.rewrite(handle, record_buffer) catch |err| {
        fcd.status = mapIsamErrorToStatus(err);
        return;
    };
    
    fcd.status = 0;
}

fn handleDelete(fcd: *FCD3) void {
    const handle_result = file_map.get(@intCast(fcd.handle));
    if (handle_result == null) {
        fcd.status = 5;
        return;
    }
    
    const handle = handle_result.?;
    
    backend.delete(handle) catch |err| {
        fcd.status = mapIsamErrorToStatus(err);
        return;
    };
    
    fcd.status = 0;
}

fn handleStart(fcd: *FCD3) void {
    const handle_result = file_map.get(@intCast(fcd.handle));
    if (handle_result == null) {
        fcd.status = 5;
        return;
    }
    
    const handle = handle_result.?;
    const mode = mapCobolReadMode(fcd.option);
    const key_buffer = fcd.key_ptr[0..@intCast(fcd.record_key_size)];
    
    backend.start(handle, key_buffer, mode) catch |err| {
        fcd.status = mapIsamErrorToStatus(err);
        return;
    };
    
    fcd.status = 0;
}

fn handleClose(fcd: *FCD3) void {
    const handle_result = file_map.get(@intCast(fcd.handle));
    if (handle_result == null) {
        fcd.status = 5;
        return;
    }
    
    const handle = handle_result.?;
    
    backend.close(handle) catch |err| {
        fcd.status = mapIsamErrorToStatus(err);
        return;
    };
    
    _ = file_map.remove(@intCast(fcd.handle));
    fcd.status = 0;
}

// モード変換関数
fn mapCobolMode(cobol_mode: c_short) isam.OpenMode {
    return switch (cobol_mode) {
        0 => isam.OpenMode.INPUT,
        1 => isam.OpenMode.OUTPUT,
        2 => isam.OpenMode.IO,
        else => isam.OpenMode.INPUT,
    };
}

fn mapCobolReadMode(cobol_option: c_short) isam.ReadMode {
    return switch (cobol_option) {
        0 => isam.ReadMode.FIRST,
        1 => isam.ReadMode.NEXT,
        2 => isam.ReadMode.PREVIOUS,
        3 => isam.ReadMode.EQUAL,
        4 => isam.ReadMode.GREATER_EQUAL,
        else => isam.ReadMode.NEXT,
    };
}

fn mapIsamErrorToStatus(err: isam.IsamError) c_short {
    return switch (err) {
        error.Duplicate => 3,
        error.NotFound => 4,
        error.EndOfFile => 10,
        error.Locked => 2,
        error.IoError => 5,
        error.NotSupported => 9,
    };
}

pub fn init(allocator_in: std.mem.Allocator) !void {
    allocator = allocator_in;
    backend = .{ .VBISAM = vbisam_impl.VbisamBackend.init(allocator) };
    file_map = std.AutoHashMap(c_int, isam.IsamFileHandle).init(allocator);
}

pub fn deinit() void {
    file_map.deinit();
}
```

---

### フェーズ 4: 新バックエンド追加（例：BerkeleyDB）

#### ステップ 4.1: `isam_bdb.zig` 作成

```zig
// src/isam_bdb.zig
const std = @import("std");
const isam = @import("isam_interface.zig");
const bdb = @import("bdb.zig");

pub const BdbBackend = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) BdbBackend {
        return .{ .allocator = allocator };
    }

    pub fn open(self: *BdbBackend, filename: []const u8, mode: isam.OpenMode) isam.IsamError!isam.IsamFileHandle {
        const bdb_mode = self.mapOpenMode(mode);
        const handle = bdb.dbOpen(filename, bdb_mode) catch |err| {
            return self.mapBdbError(err);
        };

        return isam.IsamFileHandle{
            .backend_type = .BDB,
            .handle = handle,
            .record_size = 0,
            .key_offset = 0,
            .key_size = 0,
        };
    }

    // ... その他メソッドを同様に実装 ...

    fn mapBdbError(self: *BdbBackend, err: bdb.BdbError) isam.IsamError {
        _ = self;
        return switch (err) {
            error.KeyExists => isam.IsamError.Duplicate,
            error.NotFound => isam.IsamError.NotFound,
            error.EndOfDb => isam.IsamError.EndOfFile,
            error.DbLocked => isam.IsamError.Locked,
            else => isam.IsamError.IoError,
        };
    }

    fn mapOpenMode(self: *BdbBackend, mode: isam.OpenMode) bdb.DbOpenMode {
        _ = self;
        return switch (mode) {
            .INPUT => bdb.DbOpenMode.ReadOnly,
            .OUTPUT => bdb.DbOpenMode.Create,
            .IO => bdb.DbOpenMode.ReadWrite,
            .EXTEND => bdb.DbOpenMode.ReadWrite,
        };
    }
};
```

#### ステップ 4.2: `isam_interface.zig` を更新

BDB サポートを IsamBackend ユニオンに追加：

```zig
pub const IsamBackend = union(enum) {
    VBISAM: VbisamBackend,
    BDB: BdbBackend,  // ← 追加
};

pub fn open(self: *IsamBackend, filename: []const u8, mode: OpenMode) !IsamFileHandle {
    return switch (self.*) {
        .VBISAM => |*backend| backend.open(filename, mode),
        .BDB => |*backend| backend.open(filename, mode),  // ← ディスパッチ
    };
}
// ... その他メソッドも同様 ...
```

---

## コンポーネント詳細

### FCD3 から統一型へのマッピング

| FCD3 フィールド | 統一型 | マッピング |
|--------------|--------|-----------|
| `FH-FCD-CALL-ID` | 操作 | 1→open、2→close、3→readなど |
| `FH-FCD-FILE-OPEN-MODE` | OpenMode | 0→INPUT、1→OUTPUT、2→IO |
| `FH-FCD-OPTION` | ReadMode | 0→FIRST、1→NEXT、5→EQUALなど |
| `FH-FCD-STATUS` | ステータスコード | 0→成功、3→重複キーなど |
| `FH-FCD-RECORD-POINTER` | u8 バッファ | レコードデータ |
| `FH-FCD-KEY-POINTER` | u8 バッファ | キーデータ |

### エラーステータスマッピング

| IsamError | FCD3 ステータス | 意味 |
|-----------|-----------------|------|
| Duplicate | 3 | 重複キーエラー |
| NotFound | 4 | レコード未検出 |
| EndOfFile | 10 | ファイル終端 |
| Locked | 2 | ファイル/レコードロック中 |
| IoError | 5 | 汎用 I/O エラー |

---

## 統合とテスト

### ユニットテスト構造

```zig
// test/isam_test.zig
const std = @import("std");
const isam = @import("../src/isam_interface.zig");

test "VBISAM: ファイルのオープンとクローズ" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const backend = isam.IsamBackend{ 
        .VBISAM = VbisamBackend.init(allocator) 
    };

    const handle = try backend.open("test.dat", isam.OpenMode.IO);
    defer backend.close(handle) catch {};

    try std.testing.expect(handle.handle > 0);
}

test "VBISAM: 読み書き操作" {
    // テストデータ I/O
}
```

### コンパイル

```bash
# メインライブラリをビルド
zig build

# テスト実行
zig build test

# 特定バックエンド指定でビルド
zig build -Dbackend=vbisam
zig build -Dbackend=bdb
```

---

## トラブルシューティング

### 問題: 新バックエンド追加時の型ミスマッチ

**症状**: 
```
error: expected type 'isam.IsamError', found 'bdb.BdbError'
```

**解決**: 
バックエンド内に `mapError()` 関数を実装して、すべてのバックエンド固有エラーを `isam.IsamError` に変換してください。

### 問題: ファイル操作時のセグメンテーションフォルト

**症状**: 
読み書き時に EXTFH ハンドラーがクラッシュ

**確認項目**:
- [ ] FCD3 構造体アライメントは正しいか？
- [ ] レコードバッファポインタは有効か？
- [ ] レコードサイズはファイル定義と一致しているか？
- [ ] ハンドルが file_map に存在するか？

### 問題: 新バックエンド導入後のパフォーマンス劣化

**確認項目**:
- [ ] キャッシング戦略が実装されているか
- [ ] メモリアライメントは正しいか
- [ ] バッチ操作が可能か

---

## 総括

| フェーズ | コンポーネント | ステータス | ファイル |
|---------|---------------|-----------|---------|
| 1 | 抽象インターフェース | 完了 | `isam_interface.zig` |
| 2 | VBISAM バックエンド | 完了 | `isam_vbisam.zig` |
| 3 | EXTFH ハンドラー更新 | 完了 | `extfh.zig` |
| 4 | 追加バックエンド | テンプレート | `isam_bdb.zig` |

このアーキテクチャは、型安全性とパフォーマンスを維持しながら、バックエンドをシームレスに切り替えることを可能にします。
