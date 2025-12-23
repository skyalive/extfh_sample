# EXTFH バックエンド実装仕様テンプレート

**目的**: AI/開発者が任意のデータベース向けカスタム ISAM バックエンドを実装するために必要な基礎情報を提供  
**バージョン**: 1.0  
**ステータス**: テンプレート - 実装前に完成させること

---

## エグゼクティブサマリー

本ドキュメントでは、任意のデータベースシステムに対応した EXTFH バックエンドを実装するための **最小限の要件** を概説します。選択するデータベースと VSAM/ISAM 操作へのマッピング方法を指定した上で、AI に提供してください。

---

## パート 1: バックエンド選択と要件

### 1.1 対象データベースシステム

**名前**: _______________________  
**種類**: `[リレーショナル | キー・バリュー | ISAM ネイティブ | オブジェクト | その他]`  
**言語/バインディング**: _______________________  
**バージョン**: _______________________  

**主な特性**:
- [ ] インデックス付きシーケンシャルアクセスをサポート（必須）
- [ ] 複数キーフォーマットをサポート（推奨）
- [ ] トランザクションをサポート（オプション）
- [ ] スレッド安全な操作（マルチユーザー環境で必須）
- [ ] メモリ内またはファイルベース（指定してください）

**例: BerkeleyDB**
```
名前: Berkeley DB (libdb)
種類: キー・バリュー / 組み込み ISAM
言語: Zig バインディング付き C ライブラリ
バージョン: 4.8+
主な特性:
  - B-Tree インデックス (VSAM 相当: KSDS)
  - カーソルを使った範囲クエリ
  - フル ACID トランザクション
  - 明示的ロック機構でスレッド安全
  - ファイルベース、mmap サポート
```

### 1.2 コア機能マトリックス

**以下の表を埋めてください** (✓/✗ または NA をマーク):

| EXTFH 操作 | 要件 | DB 対応 | 備考 |
|-----------|------|---------|------|
| OPEN | 必須 | ✓/✗/NA | ファイル作成/オープン |
| CLOSE | 必須 | ✓/✗/NA | ハンドルのクローズ |
| CREATE | 必須 | ✓/✗/NA | キー仕様付きで新規作成 |
| READ (順序) | 必須 | ✓/✗/NA | キー順でイテレート |
| READ (キー指定) | 必須 | ✓/✗/NA | キー値による直接ルックアップ |
| WRITE | 必須 | ✓/✗/NA | 新規レコード挿入 |
| REWRITE | 必須 | ✓/✗/NA | 現在レコードをインプレース更新 |
| DELETE | 必須 | ✓/✗/NA | 現在レコード削除 |
| START (位置決定) | 必須 | ✓/✗/NA | キーまたは範囲に移動 |
| LOCK | オプション | ✓/✗/NA | 明示的ロック |
| UNLOCK | オプション | ✓/✗/NA | ロック解放 |
| COMMIT | オプション | ✓/✗/NA | トランザクション境界 |
| ABORT | オプション | ✓/✗/NA | ロールバック |

**例: Berkeley DB**
```
OPEN: ✓ (db_open / DB->open)
CLOSE: ✓ (db_close)
CREATE: ✓ (isbuild 相当 → DB->put 空 db)
READ (順序): ✓ (カーソルイテレート DBT→get_next)
READ (キー指定): ✓ (DB->get キー DBT 付き)
WRITE: ✓ (DB->put)
REWRITE: ✓ (DB->put 上書きあり)
DELETE: ✓ (DB->del)
START: ✓ (カーソル → キー範囲に設定)
LOCK: ✓ (明示的 DB_LOCK_* フラグ)
UNLOCK: ✓ (lock_put)
COMMIT: ✓ (txn->commit)
ABORT: ✓ (txn->abort)
```

---

## パート 2: データモデルマッピング

### 2.1 VSAM 概念 vs. お使いのデータベース

**VSAM KSDS (キー・シーケンシャル・データセット)** は EXTFH が対応する主なファイル編成です。

| VSAM 概念 | VSAM 動作 | DB 相当 | 実装上の注記 |
|---------|---------|--------|-----------|
| **ファイル** | シーケンシャルファイル（インデックス付きキー） | データベース / テーブル / B-Tree | DB がファイルを表現する方法 |
| **レコード** | 固定/可変長データ | 行 / ドキュメント / エントリ | DB が個別レコードを保存する方法 |
| **プライマリキー** | 一意、null なし、自動順序付け | プライマリインデックス / B-Tree キー | DB が一意性と順序を保証する方法 |
| **キー位置** | レコード内のバイトオフセット | フィールドセレクタ / キー抽出 | レコード内のキーを特定する方法 |
| **レコードサイズ** | 固定または可変長 | 行スキーマ / バッファサイズ | 固定（通常）または可変？ |
| **重複キー** | 許可/禁止フラグ | UNIQUE 制約 | 挿入時に強制 |
| **シーケンシャルアクセス** | キーで順序付け | B-Tree カーソルイテレート | イテレーション順序の保証 |
| **現在レコード** | 暗黙のポジションマーカー | アクティブカーソル | REWRITE/DELETE の対象 |

**例: BerkeleyDB (B-Tree)**
```
ファイル      → Berkeley DB データベース (libdb_btree)
レコード     → キー・バリュー ペア (DBT 構造)
プライマリキー → B-Tree 内部ノードキー
キー位置    → 抽出ロジック: record[start:start+size]
レコードサイズ → 固定: sizeof(DBT) または DBT.data の可変
重複キー     → DB_DUPSORT フラグで強制（許可しない）
シーケンシャル → カーソルイテレート: db->cursor(), dbc->get(DB_NEXT)
現在レコード   → アクティブカーソルポジション（暗黙）
```

### 2.2 レコード保存形式

**レコードが DB にどのように保存されるかを指定します：**

```
レコード構造:
┌─────────────────┬──────────────────────┬─────────────┐
│  キーフィールド │  ペイロードデータ     │  メタデータ │
│  (key_size)     │  (record_size - key) │  (オプション) │
└─────────────────┴──────────────────────┴─────────────┘

キー抽出:
  オフセット: key_offset (例: COBOL PIC 9(8) の開始位置は 0)
  長さ: key_size   (例: 32 ビット数値キーは 4)
  形式: [BINARY | ASCII | BCD | CUSTOM]

例: COBOL SALES-FILE
  RECORD KEY IS SALES-KEY (8 バイト、位置 0)
  RECORD SIZE: 50 バイト
  
  保存: [8バイトキー][42バイトペイロード] = 50 バイト合計
  DB: DB キー = record[0:8], DB バリュー = record[0:50]
```

### 2.3 キー順序の保証

**質問**: DB はシーケンシャル読み込みでキー順序を保証しますか？

```
[ ] はい - キーをソート済み状態で返す (B-Tree)
    例: BerkeleyDB のカーソル cursor.get(DB_NEXT)
    
[ ] いいえ - 順序保証なし (ハッシュテーブル等)
    例: ハッシュテーブル、無順序マップ
    → 解決策: 外部ソート済みインデックスを保持または読み込み後ソート

[ ] 部分的 - 特定のキー型のみ順序付け
    例: 一部 DB は数値キーのみソート
    → 対応キー型を明記してください
```

**お使いの DB の宣言**:
```
BerkeleyDB: [✓] はい - B-Tree キーソート順序を保持
SQLite: [✓] はい - B-Tree キーソート順序を保持  
DynamoDB: [✗] いいえ - ハッシュベース、GSI を使用
```

---

## パート 3: API マッピング

### 3.1 データベース native API から EXTFH 操作への対応

**各 EXTFH 操作を DB の実際の API 呼び出しにマップします：**

#### 各操作用テンプレート

```zig
// EXTFH 操作: [操作名]
// EXTFH コード: [call_id 値]
// Zig シグネチャ: [isam_interface.zig からのシグネチャ]

pub fn [operation_name](self: *Backend, ...) IsamError![ReturnType] {
    // ステップ 1: 入力を検証
    // ステップ 2: ネイティブ DB API を呼び出し
    // ステップ 3: DB エラーを IsamError にマップ
    // ステップ 4: 結果またはエラーを返す
}

ネイティブ DB API:
  関数: [db_api_call]
  パラメータ: [param1, param2, ...]
  戻り値型: [return_type]
  エラー値: [err1 → IsamError.X, err2 → IsamError.Y]

呼び出し例:
  db.operation(config) → { handle: int, error: code }
```

#### マッピング例

**BerkeleyDB の例**:

```
操作: OPEN (call_id = 1)
───────────────────────────────────────
Zig シグネチャ:
  pub fn open(self: *Backend, filename: []const u8, mode: OpenMode) !IsamFileHandle

ネイティブ DB API:
  DB* db;
  int ret = db_create(&db, NULL, 0);  // DB ハンドル作成
  if (ret == 0) {
    ret = db->open(db, NULL, filename, NULL, DB_BTREE, flags, 0644);
  }

エラーマッピング:
  ENOMEM → IsamError.IoError
  ENOENT → IsamError.NotFound (INPUT モードで非存在ファイル)
  EACCES → IsamError.IoError
  0      → 成功

ファイルハンドル保存:
  struct BdbHandle {
    db: *c.DB,
    cursor: *c.DBC,
    filename: []const u8,
    mode: BdbOpenMode,
  }

───────────────────────────────────────

操作: READ (call_id = 3)
───────────────────────────────────────
Zig シグネチャ:
  pub fn read(self: *Backend, handle: IsamFileHandle, buffer: []u8, mode: ReadMode) !void

ネイティブ DB API:
  DBT key = { 0 }, data = { 0 };
  data.data = buffer.ptr;
  data.ulen = buffer.len;
  data.flags = DB_DBT_USERMEM;
  
  int ret;
  if (mode == ReadMode.FIRST)
    ret = cursor->get(cursor, &key, &data, DB_FIRST);
  else if (mode == ReadMode.NEXT)
    ret = cursor->get(cursor, &key, &data, DB_NEXT);
  // ... など

エラーマッピング:
  DB_NOTFOUND → IsamError.NotFound
  EINVAL      → IsamError.IoError
  0           → 成功

───────────────────────────────────────

操作: WRITE (call_id = 4)
───────────────────────────────────────
Zig シグネチャ:
  pub fn write(self: *Backend, handle: IsamFileHandle, buffer: []const u8) !void

ネイティブ DB API:
  DBT key = { 0 }, data = { 0 };
  key.data = buffer.ptr;           // バッファからキーを抽出
  key.size = handle.key_size;
  data.data = buffer.ptr;
  data.size = buffer.len;
  
  int ret = db->put(db, txn, &key, &data, DB_NOOVERWRITE);
  // DB_NOOVERWRITE で重複キーエラーを保証

エラーマッピング:
  DB_KEYEXIST → IsamError.Duplicate
  ENOMEM      → IsamError.IoError
  0           → 成功

───────────────────────────────────────

操作: REWRITE (call_id = 5)
───────────────────────────────────────
Zig シグネチャ:
  pub fn rewrite(self: *Backend, handle: IsamFileHandle, buffer: []const u8) !void

ネイティブ DB API:
  DBT key = { 0 }, data = { 0 };
  key.data = buffer.ptr;
  key.size = handle.key_size;
  data.data = buffer.ptr;
  data.size = buffer.len;
  
  int ret = db->put(db, txn, &key, &data, 0);
  // フラグなし = 既存キーの上書きを許可

───────────────────────────────────────

操作: DELETE (call_id = 6)
───────────────────────────────────────
Zig シグネチャ:
  pub fn delete(self: *Backend, handle: IsamFileHandle) !void

ネイティブ DB API:
  // カーソルポジションで削除
  int ret = cursor->del(cursor, 0);

または

  // キーで削除
  DBT key = { 0 };
  key.data = current_key.ptr;
  key.size = current_key.len;
  int ret = db->del(db, txn, &key, 0);

───────────────────────────────────────

操作: START (call_id = 7)
───────────────────────────────────────
Zig シグネチャ:
  pub fn start(self: *Backend, handle: IsamFileHandle, key: []const u8, mode: ReadMode) !void

ネイティブ DB API:
  DBT search_key = { 0 };
  search_key.data = key.ptr;
  search_key.size = key.len;
  
  int ret;
  if (mode == ReadMode.EQUAL)
    ret = cursor->get(cursor, &search_key, &data, DB_SET);
  else if (mode == ReadMode.GREATER_EQUAL)
    ret = cursor->get(cursor, &search_key, &data, DB_SET_RANGE);
  // ... など

エラーマッピング:
  DB_NOTFOUND → IsamError.NotFound
  EINVAL      → IsamError.IoError
  0           → 成功（カーソル配置済み）
```

### 3.2 エラーコード翻訳テーブル

**包括的なエラーマッピングを作成します：**

```
お使いの DB エラー    条件                      → IsamError 相当
──────────────────────────────────────────────────────────────────────
DB_NOTFOUND           レコード/キー未検出      → NotFound
DB_KEYEXIST           重複キーで挿入           → Duplicate
DB_RUNRECOVERY        DB 破損                  → IoError
DB_LOCK_NOTGRANTED    ロック取得失敗           → Locked
ENOMEM                メモリ不足               → IoError
EINVAL                無効なパラメータ         → IoError
EIO                   ハードウェア I/O エラー  → IoError
EACCES                アクセス権限拒否         → IoError
ENOENT                ファイルなし             → NotFound (INPUT モード)
[DB 固有]             ...                      → IoError (デフォルト)
```

---

## パート 4: 型変換と定数

### 4.1 モード列挙型

**COBOL/EXTFH モードを DB 相当にマップします：**

```zig
// COBOL OPEN モード (FCD3.file_open_mode)
pub const CobolOpenMode = enum {
    INPUT = 0,    // 読み取り専用
    OUTPUT = 1,   // 書き込み専用（新規作成）
    IO = 2,       // 読み書き（既存ファイル）
    EXTEND = 3,   // 追加モード
};

// お使いの DB 相当
pub const BdbOpenMode = enum {
    READONLY = DB_RDONLY,
    READWRITE = 0,  // デフォルト
    CREATE = DB_CREATE,
    TRUNCATE = DB_TRUNCATE,
};

pub fn mapOpenMode(cobol_mode: CobolOpenMode) BdbOpenMode {
    return switch (cobol_mode) {
        .INPUT => .READONLY,
        .OUTPUT => .CREATE | .TRUNCATE,
        .IO => .READWRITE,
        .EXTEND => .READWRITE,
    };
}
```

```zig
// COBOL READ モード (FCD3.option)
pub const CobolReadMode = enum {
    FIRST = 0,          // スタートポジション
    NEXT = 1,           // 次に移動
    PREVIOUS = 2,       // 前に移動
    LAST = 3,           // エンドポジション
    EQUAL = 4,          // 特定キーを読み込み
    GREATER_EQUAL = 5,  // >= キー値
    GREATER = 6,        // > キー値
};

// お使いの DB 相当
pub const BdbReadMode = enum {
    DB_FIRST = DB_FIRST,
    DB_NEXT = DB_NEXT,
    DB_PREV = DB_PREV,
    DB_LAST = DB_LAST,
    DB_SET = DB_SET,           // 完全一致
    DB_SET_RANGE = DB_SET_RANGE, // >= 一致
};

pub fn mapReadMode(cobol_mode: CobolReadMode) BdbReadMode {
    return switch (cobol_mode) {
        .FIRST => .DB_FIRST,
        .NEXT => .DB_NEXT,
        .PREVIOUS => .DB_PREV,
        .LAST => .DB_LAST,
        .EQUAL => .DB_SET,
        .GREATER_EQUAL => .DB_SET_RANGE,
        .GREATER => .DB_SET_RANGE, // その後イテレート >
    };
}
```

### 4.2 ロックモード

```zig
pub const CobolLockMode = enum {
    NONE = 0,      // ロックなし
    SHARED = 1,    // 読み取りロック（同時読み込み許可）
    EXCLUSIVE = 2, // 書き込みロック（排他アクセス）
};

// お使いの DB 相当
pub fn mapLockMode(cobol_mode: CobolLockMode) BdbLockMode {
    return switch (cobol_mode) {
        .NONE => .DB_LOCK_NG,
        .SHARED => .DB_LOCK_READ,
        .EXCLUSIVE => .DB_LOCK_WRITE,
    };
}
```

---

## パート 5: 並行処理とトランザクション

### 5.1 トランザクションサポート

**お使いの DB がトランザクションをサポートするか宣言します：**

```
[ ] フル ACID トランザクション → DB はアトミックにコミット/ロールバック
[ ] 部分的トランザクション    → DB は一部のトランザクション機能をサポート
[ ] トランザクションなし      → 単一操作のアトミック性のみ
[ ] アプリケーションレベル   → バックエンドラッパーに実装

お使いの DB: [✓] フル ACID トランザクション (BerkeleyDB + DB_INIT_TXN)
```

### 5.2 ロック機構

**お使いの DB が並行アクセスをどのように処理するかを説明します：**

```
並行処理モデル:
  [ ] 楽観的 (ロックなし、コミット時に競合検出)
  [ ] 悲観的 (操作前に明示的ロック)
  [ ] 混合 (両方対応)
  [ ] シングルスレッドのみ

お使いの DB: [✓] 悲観的 - 明示的 DB_LOCK_* フラグが必須

対応ロック型:
  [ ] 共有読み取りロック (複数の読み込み者)
  [ ] 排他書き込みロック (単一ライター)
  [ ] 行レベルロック
  [ ] ページレベルロック
  [ ] テーブルレベルロック
  [ ] 自動ロック昇格

お使いの DB: [✓] 共有、排他、行レベル (B-Tree ノードロック経由)
```

### 5.3 現在レコードポジション

**REWRITE/DELETE のための「現在レコード」がどのように保持されるかを説明します：**

```
質問: READ 後、現在レコードポジションは暗黙的または明示的？

[ ] 暗黙的カーソルポジション (DB によって保持)
    → DB は最後の読み取りポジションを自動記憶
    → REWRITE/DELETE はそのポジションを使用可能
    例: ISAM、BerkeleyDB カーソル

[ ] 明示的カーソル戻り値
    → READ 後はカーソルハンドルを保存する必要がある
    → REWRITE/DELETE に同じカーソルを使用
    
[ ] アプリケーションがキーを追跡
    → READ はキー値を返す
    → REWRITE/DELETE はキーを再指定する必要がある
    
お使いの DB: [✓] 暗黙的カーソルポジション (BerkeleyDB カーソル FCD3 で保持)
```

---

## パート 6: 実装の制約とハマりどころ

### 6.1 メモリ管理

```
質問: お使いの DB はレコードバッファをどのように割り当て/解放しますか？

[ ] 内部バッファへのポインタを返す
    → 呼び出し元は次の操作前にデータをコピーする必要がある
    → 次のカーソル移動でバッファが無効化される
    
[ ] 呼び出し元割り当て、DB が入力 (USERMEM パターン)
    → 呼び出し元がバッファライフタイム管理
    → DB はバッファ再利用まで データ有効性を保証
    
[ ] 動的割り当て (malloc)
    → DB が割り当てメモリを返す
    → 呼び出し元が解放する（メモリリークリスク）

お使いの DB: [✓] USERMEM パターン (呼び出し元が DBT バッファ割り当て)
             ✗ 非対応: DB が DBT バッファ割り当て
          
結果:
  EXTFH は操作シーケンスを通じてバッファを保持する必要がある
  問題: REWRITE 前にバッファが解放される → データロス
  解決: ファイルハンドルコンテキストにレコードコピーを保持
```

### 6.2 Null/ゼロ値の扱い

```
質問: お使いの DB は特殊値をどのように扱いますか？

[ ] キー内のヌルバイト
    → キーは null 終端文字列: Key\0
    → バイナリキーは 0x00 バイト を含む可能性
    
[ ] ゼロ長キー
    → 一部 DB は空キーを拒否
    
[ ] バイナリキー vs. ASCII キー
    → 比較順序が異なる
    
[ ] キー変更安全性
    → START キー K で移動後、K は無効化される？

お使いの DB: [✓] バイナリキー (任意バイト、null 終端なし)
             ✓ ゼロ長キー許可 (ただし珍しい)
             ✗ 非対応: ASCII のみキー
             ✓ キー比較はバイト単位 (0x00 有効)
```

### 6.3 ファイル命名とパス

```
質問: お使いの DB はファイル名をどのように解釈しますか？

[ ] データベースあたり単一ファイル
    → ファイル名 = 単一物理ファイル
    → すべてのレコードが 1 ファイルに
    
[ ] 複数ファイル (インデックス + データ)
    → ファイル名 = ベース名
    → .idx、.dat などを内部作成
    
[ ] ディレクトリベース
    → ファイル名 = DB ファイルを含むディレクトリ
    
[ ] ネットワークパスサポート
    → ファイル名が "user@host:database" 可能？

お使いの DB: [✓] 単一ファイル (BerkeleyDB + DB_BTREE)
             ✓ オプション: 二次インデックスを別ファイルに
             ✗ 非対応: ディレクトリベース
             ✗ 非対応: ネットワークパス（ローカルのみ）

EXTFH への影響:
  FCD3.filename は db_create() で有効である必要がある
  必要に応じてディレクトリを拡張する
  相対パス vs. 絶対パスを処理する
```

### 6.4 レコードサイズ制限

```
質問: レコードサイズの制約は？

お使いの DB:
  最小レコードサイズ: [1 バイト]
  最大レコードサイズ: [BerkeleyDB: DBT.size 経由 4GB]
  典型的最適サイズ: [8 バイト - 64 KB]
  
VSAM KSDS 典型値: 1 - 4000 バイト

結果:
  [ ] 変更不要 - VSAM は DB 制限内に収まる
  [✓] はい - VSAM レコードは快適に収まる
  [ ] 可能性 - 大規模レコードで制限に達する
      → エラー処理で最大レコードサイズをドキュメント化
```

---

## パート 7: 例: SQLite バックエンド仕様

### 完全な例

```
──────────────────────────────────────────────────────────────
バックエンド: SQLite3
──────────────────────────────────────────────────────────────

1. データベース選択
   名前: SQLite 3
   種類: リレーショナル DBMS
   言語: Zig バインディング付き C ライブラリ (libsqlite3)
   バージョン: 3.40+
   
   機能: [✓ ACID] [✓ インデックス] [✓ トランザクション] [✗ マルチDB]

2. VSAM マッピング
   ファイル      → SQLite テーブル: vsam_records
   レコード     → テーブル行: (id INTEGER PRIMARY KEY, data BLOB)
   プライマリキー → テーブル PRIMARY KEY 列
   シーケンシャル → ORDER BY (id) を カーソルで
   
   スキーマ: CREATE TABLE vsam_records (
               id BLOB PRIMARY KEY,
               data BLOB NOT NULL
             )

3. API マッピング
   OPEN     → sqlite3_open()
   READ     → SELECT * FROM vsam_records WHERE id = ? ORDER BY id
   WRITE    → INSERT INTO vsam_records (id, data) VALUES (?, ?)
   REWRITE  → UPDATE vsam_records SET data = ? WHERE id = ?
   DELETE   → DELETE FROM vsam_records WHERE id = ?
   START    → WHERE id >= ? で SELECT を準備; sqlite3_step()
   
4. エラーマッピング
   SQLITE_CONSTRAINT_UNIQUE → IsamError.Duplicate
   SQLITE_NOTFOUND          → IsamError.NotFound (クエリ結果から暗黙)
   SQLITE_CANTOPEN          → IsamError.IoError
   SQLITE_READONLY          → IsamError.Locked
   SQLITE_IOERR             → IsamError.IoError

5. 備考
   - SQLite は準備ステートメントで暗黙のカーソル使用
   - BLOB 型経由でバイナリキーサポート
   - BEGIN/COMMIT/ROLLBACK でトランザクション
   - 単一ファイル DB（WAL モードでスレッド安全）
```

---

## パート 8: AI 実装チェックリスト

バックエンド実装を AI に依頼する前に、提供したことを確認します：

### ドキュメント完成度

- [ ] **バックエンド選択**: 名前、種類、バージョンを指定
- [ ] **機能マトリックス**: すべての EXTFH 操作を ✓/✗ でマーク
- [ ] **データモデル**: VSAM 概念を DB 概念にマップ
- [ ] **API マッピング**: 各 EXTFH 操作をネイティブ DB API にマップ（例示付き）
- [ ] **エラーコード**: 完全なエラー翻訳テーブル
- [ ] **モード変換**: OpenMode、ReadMode、LockMode をコード例付きでマップ
- [ ] **並行処理**: トランザクション対応とロック機構をドキュメント化
- [ ] **エッジケース**: メモリ管理、null 値処理、パス処理、サイズ制限
- [ ] **コードテンプレート**: 少なくとも 1 つの完全な操作実装を示す

### BerkeleyDB 仕様例

```zig
// 完全: 各操作に以下を含む:
// 1. EXTFH シグネチャ ✓
// 2. ネイティブ API 呼び出し ✓
// 3. エラーマッピング ✓
// 4. 動作コード例 ✓

pub fn write(self: *BdbBackend, handle: IsamFileHandle, buffer: []const u8) IsamError!void {
    const bdb_handle: *c.DB = @ptrFromInt(@intCast(handle.handle));
    
    var key = std.mem.zeroes(c.DBT);
    var data = std.mem.zeroes(c.DBT);
    
    key.data = @constCast(buffer.ptr);
    key.size = @intCast(handle.key_size);
    data.data = @constCast(buffer.ptr);
    data.size = @intCast(buffer.len);
    
    const ret = bdb_handle.put(bdb_handle, null, &key, &data, c.DB_NOOVERWRITE);
    if (ret != 0) {
        return self.mapBdbError(ret);
    }
}
```

### 最小限実行可能な仕様

時間が限られている場合、**最小限**以下を提供します：

1. ✅ データベース名とバージョン
2. ✅ 機能マトリックス（各 EXTFH 操作に ✓/✗）
3. ✅ データモデルマッピング表（VSAM → DB）
4. ✅ ネイティブ API 関数名: OPEN、READ、WRITE、DELETE
5. ✅ エラーコードマッピング（最低 5 個の一般的エラー）
6. ✅ 1 つの完全に動作するコード例（例: WRITE 操作）

```
最小 BerkeleyDB 仕様（1 ページ):
  
データベース: Berkeley DB 4.8+
機能: [✓OPEN][✓READ][✓WRITE][✓DELETE][✓START][✓LOCK]

VSAM ファイル → BerkeleyDB (B-Tree)
VSAM レコード → DBT 構造の (key, value) ペア
プライマリキー → record[0:key_size]

操作:
  OPEN    → db_create() + db->open()
  READ    → DB_NEXT での cursor->get()
  WRITE   → DB_NOOVERWRITE フラグ付き db->put()
  DELETE  → cursor->del()
  START   → DB_SET_RANGE での cursor->get()

エラーマッピング:
  DB_NOTFOUND  → NotFound
  DB_KEYEXIST  → Duplicate
  -1           → IoError (汎用)

例: WRITE
  db->put(db, NULL, &key_dbt, &data_dbt, DB_NOOVERWRITE);
  if (ret == DB_KEYEXIST) return IsamError.Duplicate;
```

---

## パート 9: AI プロンプト用テンプレート

このテキストを使用して AI に実装を依頼します：

---

### AI 実装リクエストテンプレート

```
[データベース名] の EXTFH バックエンドを実装してください。

以下の仕様に従い、isam_[name].zig ファイルを作成してください。

仕様:
──────────────
データベース: [名前、バージョン]
種類: [キー・バリュー / リレーショナル / その他]

VSAM マッピング:
  ファイル   → [お使いの DB 表現]
  レコード   → [レコード保存方法]
  キー       → [レコードからキー抽出]
  順序       → [シーケンシャル順序は保証？]

機能 (✓ または ✗ でマーク):
  [ ] OPEN      → ネイティブ API: [関数名] | エラー: [IsamError にマップ]
  [ ] READ      → ネイティブ API: [関数名] | エラー: [IsamError にマップ]
  [ ] WRITE     → ネイティブ API: [関数名] | エラー: [IsamError にマップ]
  [ ] DELETE    → ネイティブ API: [関数名] | エラー: [IsamError にマップ]
  [ ] START     → ネイティブ API: [関数名] | エラー: [IsamError にマップ]
  [ ] REWRITE   → ネイティブ API: [関数名] | エラー: [IsamError にマップ]

エラーコードマッピング:
  [DB エラー]   → [IsamError 相当]
  ...

モード変換:
  OpenMode.INPUT    → [お使いの DB モード]
  ReadMode.FIRST    → [お使いの DB 定数]
  ...

並行処理:
  [ ] トランザクション対応？ [はい/いいえ]
  [ ] ロック機構: [種類]

関数スケルトンを完成させてください:

pub const [DbName]Backend = struct {
    allocator: std.mem.Allocator,
    
    pub fn open(self: *Backend, filename: []const u8, mode: OpenMode) !IsamFileHandle {
        // テンプレート提供:
        // [この仕様からのコード例]
    }
    
    // ... (その他の操作)
};

isam_interface.zig テンプレートをすべての関数シグネチャに使用してください。
```

---

## 総括

| コンポーネント | 目的 | 提供タイミング |
|--------------|------|---------------|
| **パート 1** | バックエンド選択と機能 | 実装前 |
| **パート 2** | VSAM から DB へのマッピング | 実装前 |
| **パート 3** | API マッピング（例示付き） | 必須 - 最重要 |
| **パート 4** | 型変換 | 実装前 |
| **パート 5** | 並行処理モデル | トランザクション必要時 |
| **パート 6** | エッジケースとハマりどころ | 実装中 |
| **パート 7** | 完全な例 | 参考のみ |
| **パート 8** | AI 実装チェックリスト | コード依頼時 |

**重要原則**: パート 1～4 の仕様が詳細なほど、AI は反復なしで正しいバックエンドコードを生成できます。
