# SQLite-based ISAM Backend Design (Cobol4j Compatibility)

> **Implementation Status**: ![Phase 1](https://img.shields.io/badge/Phase_1-Design-blue) ![Cobol4j Compat](https://img.shields.io/badge/Cobol4j_Compat-Planned-yellow)
>
> **Objective**: Enable Z-JES to use the same SQLite-based Indexed file format as OpenSourceCOBOL4J, providing complete file format interoperability.

**Version**: 1.0-beta
**Status**: Design Phase
**Last Updated**: 2025-12-18

---

## 概要

### 背景

OpenSourceCOBOL4J の調査から以下の重要な事実が判明しました：

1. **IO処理は Pure Java 実装** - JNI を使用しない
2. **INDEXED ファイルは SQLite ベース** - `libsqlite3-JDBC` を使用
3. **ファイル形式は公開** - SQLite データベーススキーマとして
4. **マルチプロセス対応** - UUID + Process ID によるロック管理

**従来の VBISAM 依存と異なり、SQLite-based ISAM は以下のメリットがあります：**

- ✅ **形式互換性**: OpenSourceCOBOL4J との完全な相互運用
- ✅ **クロスプラットフォーム**: SQLite は全てのプラットフォームで標準化
- ✅ **トランザクション**: ACID コンプライアンス
- ✅ **スケーラビリティ**: リレーショナルDB的な機能
- ✅ **ツールサポート**: sqlite3 CLI での直接検査・編集が可能

### 提案されるアーキテクチャ

```
COBOL Application (GnuCOBOL .so)
    ↓
extfh.zig (EXTFH Handler)
    ↓
isam_interface.zig (Abstraction Layer - Tagged Union)
    ├─ VBISAM Backend (isam_vbisam.zig)  [既存]
    └─ SQLite Backend (isam_sqlite.zig)  [新規 ← 本設計]
         ↓
isam_sqlite_schema.zig (SQLite Schema Manager)
    ↓
SQLite C API (libsqlite3)
    ↓
SQLite Database File (.sql, .db)
```

---

## 1. OpenSourceCOBOL4J の SQLite スキーマ

### 1.1 テーブル構造

#### Primary Key Table (`table0`)

```sql
CREATE TABLE table0 (
    key BLOB PRIMARY KEY,
    value BLOB NOT NULL,
    locked_by TEXT,                    -- UUID (セッションID)
    process_id TEXT,                   -- OS Process ID
    locked_at TIMESTAMP,               -- Lock acquisition time
    deleted INTEGER DEFAULT 0          -- Soft delete flag
);

CREATE INDEX idx_table0_timestamp ON table0(locked_at);
```

**用途**: 主キーインデックス保持、レコード値格納、レコードレベルロック

#### Alternate Key Tables (`table1`, `table2`, ...)

```sql
CREATE TABLE table1 (
    key BLOB,
    value BLOB REFERENCES table0(key) ON DELETE CASCADE,
    dupNo INTEGER,                     -- 重複番号 (重複許可時のみ)
    PRIMARY KEY (key, dupNo)
);

CREATE INDEX idx_table1_dupNo ON table1(dupNo);
```

**用途**: 代替キーのマルチキーインデックス（複数テーブルで複数キー対応）

#### Metadata Tables

```sql
-- レコード構造メタデータ
CREATE TABLE metadata_string_int (
    key TEXT PRIMARY KEY,
    value INT
);

-- キー情報メタデータ
CREATE TABLE metadata_key (
    key_number INT PRIMARY KEY,
    key_offset INT,          -- レコード内のキーオフセット
    key_size INT,            -- キーサイズ
    allow_duplicates INT,    -- 重複許可フラグ (0/1)
    key_type TEXT            -- キータイプ (CHAR, INT, LONG等)
);

-- ファイルレベルロック
CREATE TABLE file_lock (
    file_id TEXT PRIMARY KEY,
    locked_by TEXT,
    process_id TEXT,
    locked_at TIMESTAMP
);
```

**用途**: ファイル構造・ロック情報の管理

### 1.2 重要な Metadata 値

```
metadata_string_int:
  - "record_size": 可変長レコード最大サイズ
  - "num_keys": キーの数
  - "key_number": 現在の検索キー番号

metadata_key:
  - key_number: 0 (primary), 1, 2, ... (alternate)
  - key_offset: レコード内のキーの位置 (バイト単位)
  - key_size: キーの長さ
  - allow_duplicates: 0 (不許可), 1 (許可)
  - key_type: "CHAR", "LONG", etc.
```

---

## 2. Zig版 SQLite バックエンド設計

### 2.1 `isam_sqlite.zig` 構造体

```zig
const c = @cImport({
    @cInclude("sqlite3.h");
});

pub const SqliteBackend = struct {
    allocator: std.mem.Allocator,
    db: *c.sqlite3,
    schema: SqliteSchema,
    current_transaction: ?Transaction = null,

    pub fn init(allocator: std.mem.Allocator, db_path: []const u8) !SqliteBackend {
        var db: ?*c.sqlite3 = null;
        const rc = c.sqlite3_open_v2(
            @ptrCast(db_path.ptr),
            &db,
            c.SQLITE_OPEN_READWRITE | c.SQLITE_OPEN_CREATE,
            null
        );

        if (rc != c.SQLITE_OK or db == null) {
            return error.SqliteOpenFailed;
        }

        const self = SqliteBackend{
            .allocator = allocator,
            .db = db.?,
            .schema = try SqliteSchema.init(allocator, db.?),
        };

        try self.ensureSchema();
        return self;
    }

    pub fn deinit(self: *SqliteBackend) void {
        _ = c.sqlite3_close(self.db);
        self.schema.deinit();
    }

    // Abstraction layer の実装
    pub fn open(self: *SqliteBackend, filename: []const u8, mode: isam.OpenMode) !isam.IsamFileHandle {
        // ファイルをDBとして初期化
    }

    pub fn read(self: *SqliteBackend, handle: isam.IsamFileHandle, buffer: []u8, mode: isam.ReadMode) !void {
        // キー検索・レコード読み込み
    }

    pub fn write(self: *SqliteBackend, handle: isam.IsamFileHandle, buffer: []const u8) !void {
        // レコード挿入 + 全キーテーブル更新
    }

    pub fn rewrite(self: *SqliteBackend, handle: isam.IsamFileHandle, buffer: []const u8) !void {
        // レコード更新 (主キー保持)
    }

    pub fn delete(self: *SqliteBackend, handle: isam.IsamFileHandle) !void {
        // ソフトデリート (deleted=1 に設定)
    }

    pub fn start(self: *SqliteBackend, handle: isam.IsamFileHandle, key: []const u8, mode: isam.ReadMode) !void {
        // 指定キーのレコード位置を検索
    }

    pub fn lock(self: *SqliteBackend, handle: isam.IsamFileHandle, mode: isam.LockMode) !void {
        // locked_by/process_id を設定
    }

    pub fn unlock(self: *SqliteBackend, handle: isam.IsamFileHandle) !void {
        // locked_by/process_id をクリア
    }

    fn ensureSchema(self: *const SqliteBackend) !void {
        // スキーマが存在しなければ作成
    }

    fn mapError(rc: c_int) isam.IsamError {
        return switch (rc) {
            c.SQLITE_CONSTRAINT => isam.IsamError.Duplicate,
            c.SQLITE_NOTFOUND => isam.IsamError.NotFound,
            else => isam.IsamError.IoError,
        };
    }
};

pub const SqliteSchema = struct {
    allocator: std.mem.Allocator,
    db: *c.sqlite3,
    metadata: std.StringHashMap(i32),

    pub fn init(allocator: std.mem.Allocator, db: *c.sqlite3) !SqliteSchema {
        return SqliteSchema{
            .allocator = allocator,
            .db = db,
            .metadata = std.StringHashMap(i32).init(allocator),
        };
    }

    pub fn deinit(self: *SqliteSchema) void {
        self.metadata.deinit();
    }

    pub fn createSchema(self: *SqliteSchema) !void {
        // table0, table1-N, metadata_*, file_lock を作成
    }

    pub fn getRecordSize(self: *SqliteSchema) !usize {
        return self.metadata.get("record_size") orelse error.NoRecordSize;
    }

    pub fn getKeyInfo(self: *SqliteSchema, key_number: usize) !KeyInfo {
        // key_number に対応する KeyInfo を取得
    }
};

pub const Transaction = struct {
    session_id: []const u8,  // UUID
    process_id: u32,         // OS Process ID
    start_time: i64,
};

pub const KeyInfo = struct {
    key_number: usize,
    offset: usize,
    size: usize,
    allow_duplicates: bool,
    key_type: []const u8,
};
```

### 2.2 `isam_sqlite_schema.zig` - スキーマ管理

```zig
pub const SqliteSchemaManager = struct {
    pub const SCHEMA_VERSION = 1;

    pub fn initializeSchema(db: *c.sqlite3, record_size: usize, num_keys: usize) !void {
        // Primary key table
        const create_table0 =
            \\CREATE TABLE IF NOT EXISTS table0 (
            \\  key BLOB PRIMARY KEY,
            \\  value BLOB NOT NULL,
            \\  locked_by TEXT,
            \\  process_id TEXT,
            \\  locked_at TIMESTAMP,
            \\  deleted INTEGER DEFAULT 0
            \\);
        ;

        try execSQL(db, create_table0);

        // Alternate key tables
        for (1..num_keys) |i| {
            const sql = try std.fmt.allocPrint(
                allocator,
                \\CREATE TABLE IF NOT EXISTS table{} (
                \\  key BLOB,
                \\  value BLOB REFERENCES table0(key) ON DELETE CASCADE,
                \\  dupNo INTEGER,
                \\  PRIMARY KEY (key, dupNo)
                \\);
                ,
                .{i}
            );
            defer allocator.free(sql);
            try execSQL(db, sql);
        }

        // Metadata tables
        try execSQL(db, CREATE_METADATA_STRING_INT);
        try execSQL(db, CREATE_METADATA_KEY);
        try execSQL(db, CREATE_FILE_LOCK);

        // Initialize metadata
        try setMetadata(db, "record_size", @as(i32, @intCast(record_size)));
        try setMetadata(db, "num_keys", @as(i32, @intCast(num_keys)));
    }

    pub fn getKeyCount(db: *c.sqlite3) !usize {
        return getMetadataInt(db, "num_keys");
    }

    pub fn getRecordSize(db: *c.sqlite3) !usize {
        return getMetadataInt(db, "record_size");
    }

    pub fn setKeyInfo(
        db: *c.sqlite3,
        key_number: usize,
        offset: usize,
        size: usize,
        allow_duplicates: bool,
        key_type: []const u8,
    ) !void {
        // metadata_key テーブルに挿入
    }

    pub fn getKeyInfo(db: *c.sqlite3, key_number: usize) !KeyInfo {
        // metadata_key から取得
    }

    fn execSQL(db: *c.sqlite3, sql: []const u8) !void {
        var err_msg: [*c]u8 = null;
        const rc = c.sqlite3_exec(db, @ptrCast(sql.ptr), null, null, &err_msg);

        if (rc != c.SQLITE_OK) {
            defer c.sqlite3_free(err_msg);
            return error.SqliteExecuteError;
        }
    }

    fn setMetadata(db: *c.sqlite3, key: []const u8, value: i32) !void {
        // INSERT OR REPLACE INTO metadata_string_int
    }

    fn getMetadataInt(db: *c.sqlite3, key: []const u8) !i32 {
        // SELECT value FROM metadata_string_int WHERE key = ?
    }
};

const CREATE_METADATA_STRING_INT =
    \\CREATE TABLE IF NOT EXISTS metadata_string_int (
    \\  key TEXT PRIMARY KEY,
    \\  value INT
    \\);
;

const CREATE_METADATA_KEY =
    \\CREATE TABLE IF NOT EXISTS metadata_key (
    \\  key_number INT PRIMARY KEY,
    \\  key_offset INT,
    \\  key_size INT,
    \\  allow_duplicates INT,
    \\  key_type TEXT
    \\);
;

const CREATE_FILE_LOCK =
    \\CREATE TABLE IF NOT EXISTS file_lock (
    \\  file_id TEXT PRIMARY KEY,
    \\  locked_by TEXT,
    \\  process_id TEXT,
    \\  locked_at TIMESTAMP
    \\);
;
```

---

## 3. OpenSourceCOBOL4J との相互運用性

### 3.1 ファイル形式の互換性確認リスト

- [x] SQLite データベースファイル形式 (`.sql` or `.db`)
- [x] table0 スキーマ (BLOB key/value, ロック情報)
- [x] table1-N 代替キーテーブル
- [x] metadata_string_int メタデータ
- [x] metadata_key キー定義
- [x] file_lock ファイルロック管理

### 3.2 クロスプラットフォーム動作シナリオ

**シナリオ 1: Java → Zig**
```
OpenSourceCOBOL4J (Java)
  ↓ Write INDEXED file
  ↓ [SQLite file: data.db]
  ↓
Z-JES (Zig)
  ↓ Read same INDEXED file
  ↓ Using SqliteBackend
  ✓ Complete interoperability
```

**シナリオ 2: Zig → Java**
```
Z-JES (Zig)
  ↓ Write INDEXED file
  ↓ Using SqliteBackend
  ↓ [SQLite file: data.db]
  ↓
OpenSourceCOBOL4J (Java)
  ↓ Read same INDEXED file
  ✓ Complete interoperability
```

### 3.3 実装互換性マトリックス

| 機能 | OpenSourceCOBOL4J | Zig版 | 相互運用性 |
|-----|---|---|---|
| 主キー検索 | ✅ | ✅ | 完全 |
| 代替キー検索 | ✅ | ✅ | 完全 |
| 重複キー対応 | ✅ | ✅ | 完全 |
| レコード挿入 | ✅ | ✅ | 完全 |
| レコード更新 | ✅ | ✅ | 完全 |
| レコード削除 | ✅ (Soft Delete) | ✅ | 完全 |
| トランザクション | ✅ (SERIALIZABLE) | ✅ (SERIALIZABLE) | 完全 |
| レコードロック | ✅ | ✅ | 完全 |
| START 操作 | ✅ (EQ/LT/LE/GT/GE) | ✅ | 完全 |
| READ NEXT/PREV | ✅ | ✅ | 完全 |

---

## 4. 実装ロードマップ

### Phase 1: 基本実装 (1週間)

- [ ] SQLite C API バインディング (`src/runtime/sqlite3_ffi.zig`)
- [ ] `isam_sqlite.zig` 基本実装 (open/close/read/write)
- [ ] `isam_sqlite_schema.zig` スキーマ管理
- [ ] isam_interface.zig への SqliteBackend 統合
- [ ] ユニットテスト (CRUD操作)

### Phase 2: 高度な機能 (1週間)

- [ ] START 操作による条件検索
- [ ] マルチキーサポート
- [ ] トランザクション管理 (BEGIN/COMMIT/ROLLBACK)
- [ ] レコードロック管理
- [ ] 代替キーテーブル操作

### Phase 3: 互換性テスト (3日)

- [ ] OpenSourceCOBOL4J で作成したファイルを Zig で読み込み
- [ ] Zig で作成したファイルを OpenSourceCOBOL4J で読み込み
- [ ] 混在読み書きテスト
- [ ] パフォーマンスベンチマーク

### Phase 4: 本番対応 (3日)

- [ ] エラーハンドリング強化
- [ ] パフォーマンス最適化
- [ ] ドキュメント完成
- [ ] パッケージング

---

## 5. SQLite C API 活用例

### 5.1 レコード挿入

```zig
pub fn insertRecord(
    db: *c.sqlite3,
    key: []const u8,
    value: []const u8,
) !void {
    const stmt_sql = "INSERT INTO table0 (key, value) VALUES (?, ?);";
    var stmt: ?*c.sqlite3_stmt = null;

    var rc = c.sqlite3_prepare_v2(db, @ptrCast(stmt_sql.ptr), stmt_sql.len, &stmt, null);
    if (rc != c.SQLITE_OK) return error.PrepareFailed;
    defer _ = c.sqlite3_finalize(stmt);

    // Bind key (BLOB)
    rc = c.sqlite3_bind_blob(stmt.?, 1, @ptrCast(key.ptr), @intCast(key.len), c.SQLITE_STATIC);
    if (rc != c.SQLITE_OK) return error.BindFailed;

    // Bind value (BLOB)
    rc = c.sqlite3_bind_blob(stmt.?, 2, @ptrCast(value.ptr), @intCast(value.len), c.SQLITE_STATIC);
    if (rc != c.SQLITE_OK) return error.BindFailed;

    rc = c.sqlite3_step(stmt.?);
    if (rc != c.SQLITE_DONE) {
        if (rc == c.SQLITE_CONSTRAINT) return error.DuplicateKey;
        return error.ExecuteFailed;
    }
}
```

### 5.2 条件付きレコード検索

```zig
pub fn findByKey(
    db: *c.sqlite3,
    key: []const u8,
    mode: isam.ReadMode,
) ![]const u8 {
    const stmt_sql = switch (mode) {
        .EQUAL => "SELECT value FROM table0 WHERE key = ? AND deleted = 0;",
        .GREATER => "SELECT value FROM table0 WHERE key > ? AND deleted = 0 ORDER BY key ASC LIMIT 1;",
        .GREATER_EQUAL => "SELECT value FROM table0 WHERE key >= ? AND deleted = 0 ORDER BY key ASC LIMIT 1;",
        .LESS => "SELECT value FROM table0 WHERE key < ? AND deleted = 0 ORDER BY key DESC LIMIT 1;",
        .LESS_EQUAL => "SELECT value FROM table0 WHERE key <= ? AND deleted = 0 ORDER BY key DESC LIMIT 1;",
        else => return error.UnsupportedMode,
    };

    var stmt: ?*c.sqlite3_stmt = null;
    var rc = c.sqlite3_prepare_v2(db, @ptrCast(stmt_sql.ptr), stmt_sql.len, &stmt, null);
    if (rc != c.SQLITE_OK) return error.PrepareFailed;
    defer _ = c.sqlite3_finalize(stmt);

    rc = c.sqlite3_bind_blob(stmt.?, 1, @ptrCast(key.ptr), @intCast(key.len), c.SQLITE_STATIC);
    if (rc != c.SQLITE_OK) return error.BindFailed;

    rc = c.sqlite3_step(stmt.?);
    if (rc == c.SQLITE_ROW) {
        const value = c.sqlite3_column_blob(stmt.?, 0);
        const size = c.sqlite3_column_bytes(stmt.?, 0);
        return value[0..@intCast(size)];
    } else if (rc == c.SQLITE_DONE) {
        return error.NotFound;
    }

    return error.ExecuteFailed;
}
```

---

## 6. 既知の制限事項・検討事項

| 項目 | 説明 | 対応予定 |
|-----|------|--------|
| SQLite バージョン | 3.40+ 推奨 | ドキュメント記載 |
| マルチスレッド | DEFAULT モード (Thread Safe) | std.Thread.Mutex で保護 |
| 大規模ファイル | 2GB以上のファイルは未テスト | パフォーマンステスト |
| 暗号化 | SQLite Encryption Extension (SEE) 未対応 | スコープ外 |
| バックアップ | sqlite3_backup_* API 未実装 | Phase 2 以降 |

---

## 7. 参考資料

- [OpenSourceCOBOL4J GitHub](https://github.com/skyalive/opensourcecobol4j)
- [SQLite C API Reference](https://www.sqlite.org/c3ref/intro.html)
- [Zig std.c FFI](https://ziglang.org/documentation/master/#@cImport)
- [ISAM 仕様 (general)](https://en.wikipedia.org/wiki/ISAM)
- [specs/015-vsam-extfh.md](./015-vsam-extfh.md)
- [specs/015b-vsam-abstraction.md](./015b-vsam-abstraction.md)

---

**Document Status**: Design Phase
**Next Step**: Phase 1 実装開始
**Owner**: Z-JES Development Team

