# COBOL4J SQLite インターオペラビリティ仕様書

**Status**: 実装反映済み（更新中）
**Date**: 2025-12-24
**Version**: 0.2
**Purpose**: COBOL4J の SQLite 形式に GnuCOBOL 側を合わせ、同一ファイルを相互に読み書き可能にする

---

## 1. エグゼクティブサマリー

### 1.1 現状の課題

GnuCOBOL と COBOL4J は、INDEXED ファイルの実装が異なるため相互運用ができない：

| 項目 | GnuCOBOL | COBOL4J |
|------|----------|---------|
| **実行環境** | In-Process (Zig EXTFH) | Out-of-Process (Java/JVM) |
| **INDEXED実装** | VBISAM (C ライブラリ) | SQLite JDBC |
| **ファイルフォーマット** | VBISAM 専有形式（`.isam`） | SQLite DB |
| **相互運用性** | ❌ 互換性なし | ❌ 互換性なし |

### 1.2 解決策

**共通フォーマット: COBOL4J の SQLite スキーマ準拠**

- ✅ COBOL4J: 既存 SQLite JDBC 実装（`CobolIndexedFile`）
- ✅ GnuCOBOL: `isam_sqlite.zig` を COBOL4J スキーマに合わせる
- ✅ 同一 SQLite ファイルの相互読み書き

### 1.3 目標

**Phase 5.5: COBOL4J SQLite インターオペラビリティ実装**

COBOL4J が作成した SQLite ファイルを GnuCOBOL 側で読み書き可能にし、GnuCOBOL が作成した SQLite ファイルを COBOL4J 側で読み書き可能にする。

---

## 2. COBOL4J の INDEXED ファイル実装（現行）

### 2.1 COBOL4J の SQLite 実装

COBOL4J は `CobolIndexedFile` で SQLite JDBC を使用し、INDEXED ファイルを SQLite DB として管理する。

### 2.2 現在のファイルフォーマット（SQLite）

COBOL4J の SQLite スキーマが実際の標準フォーマットである。GnuCOBOL 側はこの構造に合わせる。

---

## 3. COBOL4J 準拠の SQLite スキーマ

### 3.1 スキーマ設計原則

1. **COBOL4J 優先**: 既存 COBOL4J の SQLite 形式を正とする  
2. **相互運用性**: GnuCOBOL が COBOL4J の DB をそのまま読める  
3. **後方互換は別途**: VBISAM 変換は将来対応

### 3.2 SQLite スキーマ定義（COBOL4J 準拠）

#### 3.2.1 テーブル: `table0`（主キー・本体）

```sql
CREATE TABLE table0 (
    key BLOB NOT NULL PRIMARY KEY,
    value BLOB NOT NULL,
    locked_by TEXT,
    process_id TEXT,
    locked_at TIMESTAMP
);
CREATE INDEX IF NOT EXISTS table0_key_idx ON table0(key);
```

#### 3.2.2 テーブル: `metadata_string_int`（メタ情報）

```sql
CREATE TABLE metadata_string_int (
    key TEXT NOT NULL PRIMARY KEY,
    value INTEGER NOT NULL
);
-- 例: ('record_size', 100)
```

#### 3.2.3 テーブル: `metadata_key`（キー定義）

```sql
CREATE TABLE metadata_key (
    idx INTEGER NOT NULL PRIMARY KEY,
    offset INTEGER NOT NULL,
    size INTEGER NOT NULL,
    duplicate BOOLEAN
);
```

#### 3.2.4 テーブル: `file_lock`（ファイルロック）

```sql
CREATE TABLE file_lock (
    locked_by TEXT PRIMARY KEY,
    process_id TEXT,
    locked_at TIMESTAMP,
    open_mode TEXT CONSTRAINT check_open_mode CHECK (
        open_mode IN ('INPUT', 'OUTPUT', 'I-O', 'EXTEND')
    )
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

  → SQLite table0 に保存:
     key: record[0:10]     (BLOB)
     value: record[0:500]  (BLOB - 固定長)
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

#### 4.1.1 `isam_sqlite_schema.zig` - COBOL4J スキーマ準拠

GnuCOBOL 側は COBOL4J のスキーマ（`table0`, `metadata_string_int`, `metadata_key`, `file_lock`）を生成・参照する。

#### 4.1.2 `isam_sqlite.zig` の更新

SQLite バックエンドは COBOL4J の SQL 仕様に合わせて実装する。

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

COBOL4J は既存の SQLite JDBC 実装（`CobolIndexedFile`）をそのまま使用する。  
追加の Java ドライバは不要。

---

## 5. データ互換性検証

### 5.1 テスト仕様

#### テスト 1: COBOL4J → GnuCOBOL 読み取り

```
./extfh/demo/run_demo.sh
```

- COBOL4J の `cobj-idx` で DB を作成/参照
- GnuCOBOL 側（Zig）が SQLite を読み書き

#### テスト 2: GnuCOBOL → COBOL4J 読み取り

```
./extfh/demo/run_demo.sh
```

- GnuCOBOL 側（Zig）が DB 作成・書き込み
- COBOL4J の `cobj-idx unload` でレコードを読出し

---

## 6. VBISAM 互換性（将来拡張）

### 6.1 VBISAM との共存

- VBISAM はビルド時に選択可能（`zig build -Dbackend=vbisam`）
- SQLite へ切り替えも可能（`zig build -Dbackend=sqlite`）
- 将来的な VBISAM → SQLite 変換は別タスクで実施

---

## 7. 実装ロードマップ

### Phase 5.5: COBOL4J 互換化

- [x] SQLite スキーマを COBOL4J に合わせる
- [x] COBOL4J → GnuCOBOL 読み取り検証
- [x] GnuCOBOL → COBOL4J 読み取り検証
- [ ] 追加バックエンド（BDB/C-ISAM）拡張
- [ ] VBISAM → SQLite 変換ツール

---

## 8. 参考資料

- [OpenCOBOL4J GitHub](https://github.com/opensourcecobol/opensourcecobol4j)
- [SQLite JDBC ドライバ](https://github.com/xerial/sqlite-jdbc)
- [COBOL ファイル I/O 標準](https://www.ibm.com/docs/ssw_ibm_i_73/rzase/cbldbfindx.htm)

---

**Document Status**: Updated
**Last Updated**: 2025-12-24
**Next Review**: COBOL4J 仕様変更時
