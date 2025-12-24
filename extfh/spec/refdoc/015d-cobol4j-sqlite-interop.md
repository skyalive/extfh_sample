# COBOL4J SQLite インターオペラビリティ仕様書

**Status**: 仕様策定中
**Date**: 2025-12-18
**Version**: 0.1 (Draft)
**Purpose**: GnuCOBOL と OpenCOBOL4J (COBOL4J) 間のデータ共有を実現する SQLite スキーマ標準化

---

## 1. エグゼクティブサマリー

### 1.1 現状の課題

GnuCOBOL と COBOL4J は、INDEXED ファイル処理に異なるバックエンドを使用：

| 項目 | GnuCOBOL | COBOL4J |
|------|----------|---------|
| **実行環境** | In-Process (Zig EXTFH) | Out-of-Process (Java/JVM) |
| **INDEXED実装** | VBISAM (C ライブラリ) | Java `RandomAccessFile` + メモリ内 `TreeMap` |
| **ファイルフォーマット** | VBISAM 専有形式（`.isam`） | Java 独自形式 |
| **相互運用性** | ❌ ファイル互換性なし | ❌ ファイル互換性なし |

### 1.2 解決策

**共通フォーマット: SQLite ベースの標準化スキーマ**

- ✅ GnuCOBOL: `isam_sqlite.zig` で SQLite 対応
- ✅ COBOL4J: SQLite ドライバ標準サポート（JDBC）
- ✅ データ層で統一: 両言語とも同じ SQLite ファイルを読み書き可能

### 1.3 目標

**Phase 5.5: COBOL4J SQLite インターオペラビリティ実装**

GnuCOBOL（Zig 側）が作成した SQLite ファイルを、COBOL4J（Java 側）で読み書き可能にする仕様・実装を完成させる。

---

## 2. COBOL4J の INDEXED ファイル実装（現行）

### 2.1 Java の IndexedFileHandler アーキテクチャ

OpenCOBOL4J は標準 Java I/O を使用：

```java
public class IndexedFileHandler extends AbstractFileHandler {
    private RandomAccessFile raf;              // 物理ファイルアクセス
    private TreeMap<String, Long> keyIndex;   // キー → ファイルオフセット
    private String currentKey;                 // 現在の読み込み位置
    private long currentOffset;                // 現在のオフセット

    public void open(String fileName, OpenMode mode) throws IOException {
        raf = new RandomAccessFile(fileName, "rw");
        loadKeyIndex();  // ファイルをスキャンしてインデックス構築
    }

    public void read(byte[] buffer) throws IOException {
        raf.readFully(buffer);  // 固定長レコード読み込み
    }

    public void write(byte[] buffer) throws IOException {
        String key = extractKey(buffer);  // レコードからキーを抽出
        keyIndex.put(key, raf.length()); // インデックスに追加
        raf.write(buffer);               // ファイルに書き込み
    }
}
```

### 2.2 現在のファイルフォーマット（Java）

**ファイル構造:**
```
[Header (Java ObjectStream)]
[Metadata (TreeMap シリアライゼーション)]
[Record 1: 固定長バイト列]
[Record 2: 固定長バイト列]
...
[Record N: 固定長バイト列]
```

**問題点:**
- Java ObjectStream フォーマットに依存（相互運用困難）
- インデックスがメモリ内のみ（ファイルに永続化されない）
- 大規模ファイルでメモリ枯渇リスク

---

## 3. 提案: SQLite ベースの標準スキーマ

### 3.1 スキーマ設計原則

1. **両言語互換性**: GnuCOBOL（C/Zig）と COBOL4J（Java）で同じ形式を読み書き
2. **永続性**: インデックスをファイルに永続化
3. **スケーラビリティ**: メモリ内キーキャッシュと SQLite インデックスの組み合わせ
4. **後方互換性**: VBISAM ファイルからの変換機構を提供

### 3.2 SQLite スキーマ定義

#### 3.2.1 テーブル: `records`（データ保存）

```sql
CREATE TABLE records (
    -- 主キー（内部用）
    record_id INTEGER PRIMARY KEY AUTOINCREMENT,

    -- ユーザー定義キー（複合キー対応）
    key BLOB NOT NULL UNIQUE,

    -- レコード本体（固定長）
    data BLOB NOT NULL,

    -- メタデータ
    record_size INTEGER NOT NULL,  -- レコード長（バイト）
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    -- 論理削除フラグ（INDEXED ファイル仕様）
    deleted INTEGER DEFAULT 0
);

-- キー検索用インデックス
CREATE UNIQUE INDEX idx_key ON records(key);

-- 削除済みレコード除外用インデックス
CREATE INDEX idx_active ON records(deleted, key);
```

#### 3.2.2 テーブル: `alternate_keys`（複合キー対応）

```sql
CREATE TABLE alternate_keys (
    key_number INTEGER PRIMARY KEY,     -- キー番号（0=主キー, 1=副キー1, ...）
    key_offset INTEGER NOT NULL,        -- レコード内でのキーオフセット
    key_size INTEGER NOT NULL,          -- キーサイズ（バイト）
    allow_duplicates INTEGER DEFAULT 0, -- 重複キー許可（1=許可, 0=禁止）
    key_type TEXT DEFAULT 'BINARY'      -- キー型（BINARY, ASCII, ...）
);
```

#### 3.2.3 テーブル: `file_metadata`（ファイル管理情報）

```sql
CREATE TABLE file_metadata (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL
);

-- 登録される情報例:
-- ('record_size', '500')          -- 固定レコード長
-- ('key_offset', '0')             -- キーフィールドのオフセット
-- ('key_size', '10')              -- キーサイズ
-- ('schema_version', '1.0')       -- スキーマバージョン
-- ('created_by', 'GnuCOBOL')      -- 作成者（GnuCOBOL/COBOL4J）
-- ('created_date', '2025-12-18')  -- 作成日時
-- ('record_count', '1000')        -- レコード数（参考値）
```

#### 3.2.4 テーブル: `file_locks`（ロック管理）

```sql
CREATE TABLE file_locks (
    lock_id INTEGER PRIMARY KEY,
    record_id INTEGER NOT NULL,
    lock_type TEXT NOT NULL,           -- 'SHARED', 'EXCLUSIVE'
    locked_by TEXT NOT NULL,           -- プロセス ID / スレッド ID
    locked_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (record_id) REFERENCES records(record_id)
);
```

### 3.3 レコード格納形式

#### 3.3.1 キー抽出ロジック

```zig
// src/runtime/isam_sqlite_schema.zig

/// INDEXED ファイルレコードからキーを抽出
pub fn extractKey(
    record: []const u8,
    key_offset: usize,
    key_size: usize,
) ![]u8 {
    if (key_offset + key_size > record.len) {
        return error.InvalidKeyOffset;
    }
    return record[key_offset .. key_offset + key_size];
}

/// キーをバイナリ BLOB として SQLite に保存
pub fn storeKeyBlob(
    db: *sqlite3.sqlite3,
    record: []const u8,
    key_offset: usize,
    key_size: usize,
) ![]u8 {
    const key = try extractKey(record, key_offset, key_size);
    return key;  // BLOB として直接保存
}
```

#### 3.3.2 データ保存形式

```
レコード構造（COBOL PICTURE 仕様）:
┌─────────────────────────────────────────────┐
│ キーフィールド  │ その他フィールド         │
├─────────────────────────────────────────────┤
│ offset 0        │ offset key_size          │
│ size: 10        │ size: (record_size - 10) │
└─────────────────────────────────────────────┘

例：
  レコード長: 500 bytes
  キーオフセット: 0
  キーサイズ: 10

  → SQLite records テーブルに保存:
     key: record[0:10]     (BLOB)
     data: record[0:500]   (BLOB - 固定長)
```

#### 3.3.3 エンコーディング処理

**GnuCOBOL → SQLite:**
```zig
// COBOL PICTURE 文字列 (EBCDIC/ASCII) → UTF-8 (SQLite)
// ただし、BLOB として保存するため、エンコーディング変換は不要
// （キーのテキスト検索が必要な場合のみ変換）
```

**COBOL4J → SQLite:**
```java
// Java String (UTF-16) → BLOB (SQLite)
// キーのみ ASCII/EBCDIC で保存
```

---

## 4. 相互運用性の実現

### 4.1 GnuCOBOL 側の実装（Zig）

#### 4.1.1 `isam_sqlite_schema.zig` - SQLite スキーマ管理

```zig
pub const SqliteSchemaManager = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    db: *sqlite3.sqlite3,

    /// SQLite スキーマを初期化（新規作成）
    pub fn initializeSchema(
        self: *Self,
        record_size: usize,
        key_offset: usize,
        key_size: usize,
    ) !void {
        // 1. records テーブル作成
        // 2. alternate_keys テーブル作成
        // 3. file_metadata テーブルに初期値を挿入
        // 4. インデックス作成
    }

    /// ファイルメタデータを取得
    pub fn getFileMetadata(self: *Self) !FileMetadata {
        var result: FileMetadata = undefined;
        // SQL クエリで file_metadata から読み込み
        return result;
    }

    /// レコードをスキーマ仕様に準拠して挿入
    pub fn insertRecord(
        self: *Self,
        record: []const u8,
    ) !void {
        const key = try extractKey(
            record,
            self.metadata.key_offset,
            self.metadata.key_size,
        );

        // INSERT INTO records (key, data, record_size)
        // VALUES (?, ?, ?)
    }
};
```

#### 4.1.2 `isam_sqlite.zig` の更新

SQLite バックエンドを スキーマ管理と統合：

```zig
pub const SqliteBackend = struct {
    // ...既存コード...
    schema_manager: SqliteSchemaManager,

    /// open メソッドをスキーママネージャーと統合
    pub fn open(
        self: *Self,
        filename: []const u8,
        mode: isam.OpenMode,
    ) !isam.IsamFileHandle {
        // 1. SQLite DB をオープン
        // 2. スキーマを検証
        // 3. ファイルメタデータから record_size 等を取得
        // 4. IsamFileHandle を返却
    }
};
```

### 4.2 COBOL4J 側の実装（Java）

#### 4.2.1 SQLite JDBC ドライバの活用

COBOL4J の `IndexedFileHandler` を SQLite 対応に拡張：

```java
// org.opensourcecobol.IndexedFileHandler (推奨拡張)

public class SqliteIndexedFileHandler extends AbstractFileHandler {
    private Connection sqliteConnection;  // JDBC 接続
    private PreparedStatement insertStmt;
    private PreparedStatement selectStmt;

    @Override
    public void open(String fileName, OpenMode mode) throws IOException {
        // SQLite JDBC: "jdbc:sqlite:" + fileName
        sqliteConnection = DriverManager.getConnection(
            "jdbc:sqlite:" + fileName
        );

        // スキーマ存在確認
        validateSchema();

        // メタデータから record_size 等を読み込み
        loadFileMetadata();
    }

    @Override
    public void write(byte[] buffer) throws IOException {
        // キー抽出
        byte[] key = extractKey(buffer);

        // INSERT INTO records (key, data, record_size)
        insertStmt.setBytes(1, key);
        insertStmt.setBytes(2, buffer);
        insertStmt.setInt(3, buffer.length);
        insertStmt.executeUpdate();
    }
}
```

---

## 5. データ互換性検証

### 5.1 テスト仕様

#### テスト 1: GnuCOBOL → SQLite → COBOL4J（ラウンドトリップ）

```zig
test "Interop: GnuCOBOL write, COBOL4J read" {
    // 1. GnuCOBOL (Zig) が INDEXED ファイルに書き込み
    //    レコード例: "KEY001    .... (500 bytes)"

    // 2. SQLite DB ファイルを COBOL4J に渡す

    // 3. COBOL4J (Java) がレコードを読み込み
    //    → 同じデータが取得できることを確認
}
```

#### テスト 2: レコードフォーマット検証

```zig
test "SQLite schema: record format validation" {
    // 1. SQLite に複数レコードを挿入
    // 2. records テーブルをクエリ
    // 3. 各フィールドが正しく保存されていることを確認:
    //    - key (BLOB)
    //    - data (BLOB)
    //    - record_size
    //    - deleted フラグ
}
```

#### テスト 3: キー検索互換性

```zig
test "SQLite schema: key search compatibility" {
    // 1. GnuCOBOL が キー "ABC123" を含むレコード挿入
    // 2. COBOL4J が同じキーで検索
    //    → レコード発見可能なことを確認
}
```

---

## 6. VBISAM 互換性（将来拡張）

### 6.1 VBISAM → SQLite 変換

既存 VBISAM ファイル（`.isam`）から SQLite への変換ツール：

```zig
/// src/utils/vbisam_to_sqlite_converter.zig
pub fn convertVbisamToSqlite(
    vbisam_path: []const u8,
    sqlite_path: []const u8,
) !void {
    // 1. VBISAM ファイルをオープン
    var vbisam_file = try vbisam.IsamFile.open(vbisam_path, .INPUT);
    defer vbisam_file.close() catch {};

    // 2. SQLite DB を作成
    var sqlite_backend = try SqliteBackend.init(allocator, sqlite_path);
    defer sqlite_backend.deinit();

    // 3. VBISAM レコード → SQLite への変換
    while (true) {
        var buffer: [4096]u8 = undefined;
        vbisam_file.read(&buffer) catch |err| {
            if (err == error.EndOfFile) break;
            return err;
        };

        try sqlite_backend.write(undefined, &buffer);
    }
}
```

---

## 7. 実装ロードマップ

### Phase 5.5: SQLite スキーマ管理実装

**Task 1: SQLite スキーマ管理モジュール作成（2日）**
- `src/runtime/isam_sqlite_schema.zig` 作成
- テーブル定義の実装
- メタデータ管理機構

**Task 2: COBOL4J JDBC 統合仕様（1日）**
- COBOL4J への SQLite サポート推奨実装
- ドキュメント作成

**Task 3: ラウンドトリップテスト実装（1日）**
- GnuCOBOL ↔ COBOL4J データ共有テスト
- 互換性検証

**Task 4: 変換ツール実装（1日）**
- VBISAM → SQLite コンバーター
- 後方互換性確保

---

## 8. 参考資料

- [OpenCOBOL4J GitHub](https://github.com/opensourcecobol/opensourcecobol4j)
- [SQLite JDBC ドライバ](https://github.com/xerial/sqlite-jdbc)
- [COBOL ファイル I/O 標準](https://www.ibm.com/docs/ssw_ibm_i_73/rzase/cbldbfindx.htm)

---

**Document Status**: Draft
**Last Updated**: 2025-12-18
**Next Review**: Phase 5.5 実装開始時
