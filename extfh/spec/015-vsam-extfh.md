# VSAM & EXTFH Integration Specification (Phase 5.3)

> **Implementation Status**: ![Phase 1](https://img.shields.io/badge/Phase_1-Implemented-brightgreen) ![Phase 2](https://img.shields.io/badge/Phase_2-Implemented-brightgreen) ![Abstraction Layer](https://img.shields.io/badge/Abstraction_Layer-Phase_1_Complete-brightgreen)
>
> | Component | Status | Files |
> |-----------|--------|-------|
> | EXTFH Interface | Implemented | `extfh/src/extfh.zig` |
> | VBISAM Wrapper | Implemented | `extfh/src/vbisam.zig` |
> | ISAM Abstraction Layer | Phase 1 Complete | `extfh/src/isam_interface.zig`, `extfh/src/isam_vbisam.zig` |
> | KSDS Support | Implemented | `extfh/src/vbisam.zig` |
> | RRDS Support | Planning | - |
> | ESDS Support | Planning | - |

## Overview

Phase 5.3 implements COBOL VSAM file support through GnuCOBOL's Extended File Handler (EXTFH) interface. This enables GnuCOBOL programs to read/write VSAM KSDS (indexed sequential) files using VBISAM as the underlying file management system.

**Key Architecture:**
```
GnuCOBOL Program (COBOL)
  │
  ├─ FD VSAM-FILE (INDEXED)
  │   │
  │   └─ I-O statements (OPEN/READ/WRITE/DELETE/REWRITE)
  │
  └─ EXTFH Handler (Zig) ← External File Handler callback
      │
      ├─ Parse FCD3 (File Control Descriptor)
      ├─ Route to VBISAM operations
      └─ VBISAM Library (C)
          │
          └─ ISAM File I/O (indexed access)
```

---

## FCD3 Design Guide - サマリー戦略の詳細

このセクションでは、GnuCOBOL の標準 FCD3 から本質的なフィールドだけを抽出した **CZIPPBT の簡潔な FCD3 設計** について解説します。

### 問題背景：GnuCOBOL 標準 FCD3 の複雑性

GnuCOBOL の標準 FCD3（`xfhfcd3.cpy`）は **70+ のフィールド** を持ち、以下の理由で実装を複雑化させます：

1. **多くのレガシーフィールド** - 古い COBOL システムとの互換性のための予約領域
2. **デバッグ用フィールド** - システム内部の状態追跡用（外部ハンドラ不要）
3. **方言固有の拡張** - 特定のコンパイラでのみ使用
4. **LLM 生成時の認知負荷** - 「どのフィールドを使うべきか」の判断が難しい

### CZIPPBT の解決策：必須フィールドのみを抽出

**基本原則：**
- ✅ **動作に必須なフィールド** だけを struct に含める
- ❌ **予約領域や拡張フィールド** は削除
- ✅ **将来の拡張** は `reserved` 領域に確保

### フィールド選定基準

```
GnuCOBOL 標準 FCD3（70+ フィールド）
        ↓
カテゴリ分類（制御/ファイル/レコード/キー/読み込み）
        ↓
各カテゴリから「必須フィールド」を抽出
        ↓
CZIPPBT FCD3（14 フィールド）← 20% に削減
```

**選定結果の詳細：**

| カテゴリ | 必須フィールド | 理由 | VSAM | Sequential |
|---------|---------------|------|------|-----------|
| 制御 | `call_id` | 操作コード (OPEN/CLOSE/READ等) | ✅ | ✅ |
| 制御 | `handle` | ファイルハンドル（CZIPPBT割当） | ✅ | ✅ |
| 制御 | `status` | 戻り値ステータスコード | ✅ | ✅ |
| ファイル | `filename` | ファイル名（null終端） | ✅ | ✅ |
| ファイル | `file_open_mode` | OPEN モード (0=INPUT, 1=OUTPUT, 2=I-O) | ✅ | ✅ |
| レコード | `record_varying` | 可変長フラグ | ❌ | ✅ |
| レコード | `record_size` | レコードサイズ（バイト） | ✅ | ✅ |
| レコード | `record_ptr` | レコードバッファポインタ | ✅ | ✅ |
| キー | `record_key_pos` | キーのオフセット位置 | ✅ | ❌ |
| キー | `record_key_size` | キーのサイズ | ✅ | ❌ |
| キー | `key_ptr` | キーバッファポインタ | ✅ | ❌ |
| キー | `key_number` | キーインデックス (0=主キー) | ✅ | ❌ |
| 読み込み | `option` | 読み込みモード (FIRST/NEXT/EQUAL/GTEQ等) | ✅ | ❌ |
| 予約 | `reserved[128]` | 将来の拡張用 | ✅ | ✅ |

### 削除したフィールド（と理由）

| 削除フィールド | 理由 |
|--------------|------|
| `fnx_*` (多数) | デバッグ用の内部カウンタ → EXTFH で不要 |
| `dfh_*` (多数) | COBOL ランタイム内部用 → 外部ハンドラで管理 |
| `fcd_attributes` | COBOL4J 固有→別途対応で可 |
| `checkpoint_*` | トランザクション用 → 未実装 |
| `lock_mode` | ロック戦略 → 自動管理で可 |
| `file_status_4byte` | 拡張ステータス → FCD3.status で十分 |

### メリット

```
削減前（GnuCOBOL 標準）
├─ メモリ: 1-2KB
├─ フィールド数: 70+
├─ LLM 認知負荷: ⭐⭐⭐⭐⭐ (高)
└─ 仕様ドキュメント: 数十ページ

        ↓ CZIPPBT 簡潔化

削減後（CZIPPBT）
├─ メモリ: ~400-500 バイト
├─ フィールド数: 14
├─ LLM 認知負荷: ⭐⭐ (低)
└─ 仕様ドキュメント: このセクション（A4 数ページ）
```

---

## GnuCOBOL EXTFH Overview

### What is EXTFH?

EXTFH (Extended File Handler) is a callable interface that allows external programs to intercept and handle COBOL I-O operations. Instead of using GnuCOBOL's built-in file handler, EXTFH routes I-O to a custom handler function.

### Compilation

To use EXTFH, compile with the `-fcallfh` option:

```bash
cobc -fcallfh=czippfh -c program.cob
```

This tells the compiler to route all I-O operations to the function `czippfh()`.

### File Control Descriptor (FCD3)

The FCD3 is the communication structure between COBOL and EXTFH. It's defined in `xfhfcd3.cpy` and contains:

```cobol
      * FCD3 (File Control Descriptor) structure
      * Used to pass I-O operation details to EXTFH handler
       01 FH-FCD.
           05 FH-FCD-CALL-ID           USAGE BINARY-LONG.      * Operation code
           05 FH-FCD-HANDLE            USAGE BINARY-LONG.      * File handle
           05 FH-FCD-STATUS            USAGE BINARY-SHORT.     * Status code (output)
           05 FH-FCD-FILENAME          PIC X(256).             * File name
           05 FH-FCD-FILE-OPEN-MODE    USAGE BINARY-SHORT.     * OPEN mode
           05 FH-FCD-RECORD-VARYING    USAGE BINARY-SHORT.     * Variable length?
           05 FH-FCD-RECORD-SIZE       USAGE BINARY-LONG.      * Record size
           05 FH-FCD-RECORD-KEY        USAGE BINARY-LONG.      * Key position
           05 FH-FCD-RECORD-KEY-SIZE   USAGE BINARY-LONG.      * Key size
           05 FH-FCD-RECORD-POINTER    USAGE POINTER.          * Record data pointer
           05 FH-FCD-KEY-POINTER       USAGE POINTER.          * Key buffer pointer
           05 FH-FCD-OPTION            USAGE BINARY-SHORT.     * Read mode (FIRST/NEXT/EQ/etc)
           05 FH-FCD-KEY-NUMBER        USAGE BINARY-SHORT.     * Key index (0=primary)
           05 FH-FCD-RESERVED          PIC X(128).             * Reserved for extensions
```

#### FCD3 Simplification Strategy (CZIPPBT Design)

GnuCOBOL の標準 FCD3 には 70+ のフィールドがありますが、本プロジェクトでは **必須フィールドのみを抽出** してシンプル化しています。

**サマリーの原則：**

| カテゴリ | 抽出フィールド | 削除フィールド | 理由 |
|---------|---------------|---------------|------|
| **制御用** | `call_id`, `handle`, `status` | ローカル領域、デバッグ情報 | 操作コードと結果の返却のみ必要 |
| **ファイル情報** | `filename`, `file_open_mode` | ファイル属性、作成日時 | VSAM/Sequential の区別が一時的 |
| **レコード情報** | `record_size`, `record_ptr` | フォーマット記述子、圧縮フラグ | バイナリコピーで十分 |
| **キー情報** | `record_key_pos`, `record_key_size`, `key_ptr`, `key_number` | 複合キー定義、ソート順 | INDEXED のみで使用、単純化可能 |
| **読み込みモード** | `option` | 検索パス、スキップ設定 | FIRST/NEXT/EQUAL/GTEQ で十分 |

**Zig 実装での構造体定義：**

```zig
pub const FCD3 = struct {
    // 制御フィールド（3個）
    call_id: c_int,              // 1=OPEN, 2=CLOSE, 3=READ, ...
    handle: c_int,               // ファイルハンドル（CZIPPBT割当）
    status: c_short,             // 返却ステータス（0=成功, 1-9=エラー）

    // ファイル情報（2個）
    filename: [256]u8,           // null終端のファイル名
    file_open_mode: c_short,     // 0=INPUT, 1=OUTPUT, 2=I-O, 3=EXTEND

    // レコード情報（3個）
    record_varying: c_short,     // 可変長フラグ（SEQUENTIAL用）
    record_size: c_int,          // レコードサイズ（バイト）
    record_ptr: [*c]u8,          // レコードバッファへのポインタ

    // キー情報（4個）
    record_key_pos: c_int,       // キー位置（オフセット）
    record_key_size: c_int,      // キーサイズ
    key_ptr: [*c]u8,             // キーバッファへのポインタ
    key_number: c_short,         // キーインデックス（0=主キー）

    // 読み込みモード（1個）
    option: c_short,             // 0=FIRST, 2=NEXT, 5=EQUAL, 6=GTEQ, ...

    // 予約域
    reserved: [128]u8,           // 将来の拡張用
};

// サマリー結果：14フィールド
// → GnuCOBOL標準の約20% に削減
// → 必須フィールドのみで仕様を満たす
```

**メリット：**
- ✅ メモリフットプリント削減（256→400バイト → 200バイト程度）
- ✅ ポインタ操作がシンプルで安全（Zigのスライス化しやすい）
- ✅ LLMが実装するときの認知負荷が低い（14フィールド→3カテゴリ）
- ✅ 仕様変更の影響範囲が限定される

### FCD3 Operations (FH-FCD-CALL-ID Values)

**操作コードの分類と実装フロー：**

| Value | Operation | 対応ファイル型 | 実装ステータス | 説明 |
|-------|-----------|---------------|-----------------|------|
| 1 | **OPEN** | SEQUENTIAL / INDEXED | ✅ 完了 | ファイルを開く（INPUT/OUTPUT/I-O モード） |
| 2 | **CLOSE** | SEQUENTIAL / INDEXED | ✅ 完了 | ファイルを閉じる |
| 3 | **READ** | SEQUENTIAL / INDEXED | ✅ 完了 | レコード読み込み（FIRST/NEXT/EQUAL/GTEQ） |
| 4 | **WRITE** | SEQUENTIAL / INDEXED | ✅ 完了 | レコード書き込み |
| 5 | **REWRITE** | INDEXED のみ | ✅ 完了 | 現在のレコードを上書き |
| 6 | **DELETE** | INDEXED のみ | ✅ 完了 | 現在のレコードを削除 |
| 7 | **START** | INDEXED のみ | ✅ 完了 | キー位置を設定してから READ NEXT 用 |
| 8 | **ABORT** | - | 🔲 未実装 | トランザクション破棄（現在未使用） |
| 9 | **COMMIT** | - | 🔲 未実装 | トランザクションコミット（現在未使用） |
| 10 | **UNLOCK** | INDEXED のみ | ✅ 完了 | レコードロック解放 |

**Call-ID ごとの実装パターン：**

```zig
// src/runtime/extfh.zig の実装ロジック

fn handleOperation(fcd: *FCD3) void {
    switch (fcd.call_id) {
        // === ファイルライフサイクル管理 ===
        1 => handleOpen(fcd),       // 要: 型判定（VSAM vs Sequential）
        2 => handleClose(fcd),      // 要: リソース解放、ハンドル削除

        // === 汎用レコード操作 ===
        3 => handleRead(fcd),       // INDEXED/SEQUENTIAL で動作分岐
        4 => handleWrite(fcd),      // INDEXED/SEQUENTIAL で動作分岐

        // === INDEXED (VSAM) 専用操作 ===
        5 => handleRewrite(fcd),    // INDEXED のみ許可
        6 => handleDelete(fcd),     // INDEXED のみ許可
        7 => handleStart(fcd),      // キーポジション設定（INDEXED のみ）
        10 => handleUnlock(fcd),    // ロック管理（INDEXED のみ）

        // === トランザクション操作（将来用） ===
        8 => handleAbort(fcd),      // no-op（未実装）
        9 => handleCommit(fcd),     // no-op（未実装）

        else => fcd.status = 9,     // Generic error
    }
}
```

**実装上の分岐フロー：**

```
FCD3 受け取り
  ↓
Call-ID 判定
  ├─ 1-2: ファイルライフサイクル
  │        └─ detectFileType() で SEQUENTIAL/INDEXED 判定
  │
  ├─ 3-4: レコード操作
  │        ├─ INDEXED → handleReadIndexed() → VBISAM 経由
  │        └─ SEQUENTIAL → handleReadSequential() → std.fs.File 経由
  │
  ├─ 5-7, 10: INDEXED 専用操作
  │        ├─ INDEXED かつハンドル有効 → VBISAM 呼び出し
  │        └─ 不正な型 → status = 5 (I/O error)
  │
  └─ 8-9: トランザクション（no-op）
          └─ status = 0 (success, but no-op)

結果
  ↓
fcd.status に結果コード設定
```

### Return Status (ステータスコード)

EXTFH ハンドラは `FH-FCD-STATUS` にステータスコードを設定して、操作の成功/失敗を COBOL に返します。

**ステータスコード一覧：**

| 値 | 意味 | 対応する COBOL FILE STATUS | 発生シーン |
|-----|------|------------------------|----------|
| **0** | ✅ 成功 | 00 (Success) | すべての操作が正常に完了 |
| **1** | ❌ ファイルが見つからない | 35 (File not found) | OPEN INPUT で存在しないファイル |
| **2** | ❌ ファイルロック中 | 92 (File locked) | 他プロセスが排他的にロック |
| **3** | ❌ 重複キー | 22 (Duplicate record) | WRITE で既存キーを重複入力 |
| **4** | ❌ レコード未検出 | 23 (Record not found) / 10 (EOF) | READ で対象がない、または EOF |
| **5** | ❌ I/O エラー | 30 (Permanent I/O error) | VBISAM/ファイルシステム エラー |
| **9** | ❌ 汎用エラー | 30 (Permanent error) | 不定義の Call-ID など |
| **30** | ❌ 永続エラー | 30 (Permanent error) | リソース枯渇、致命的エラー |

**実装上の使い分け：**

```zig
// src/runtime/extfh.zig のステータス設定パターン

// === 成功ケース ===
fcd.status = 0;  // すべて成功

// === ファイル操作エラー ===
fcd.status = 1;  // 探索失敗（INPUT で存在確認時）
fcd.status = 5;  // OPEN 失敗、ファイルシステムエラー

// === キー操作エラー（INDEXED のみ） ===
fcd.status = 3;  // 重複キー（WRITE で key_ptr がすでに存在）
fcd.status = 4;  // キー未検出（READ で target key にマッチなし）

// === VBISAM エラーマッピング ===
fn mapIsamErrorToStatus(err: isam.IsamError) c_short {
    return switch (err) {
        isam.IsamError.Duplicate => 3,      // 重複キー
        isam.IsamError.NotFound => 4,       // レコード未検出
        isam.IsamError.EndOfFile => 4,      // EOF（READ NEXT で EOD）
        isam.IsamError.Locked => 2,         // ファイルロック
        isam.IsamError.IoError => 5,        // I/O エラー
        isam.IsamError.NotSupported => 5,   // 非対応操作
    };
}

// === デバッグ用エラー ===
fcd.status = 9;  // 不定義の call_id など
```

**COBOL 側でのエラーハンドリング例：**

```cobol
OPEN INPUT SALES-FILE.
IF SALES-STATUS NOT = 0
    EVALUATE SALES-STATUS
        WHEN 35
            DISPLAY "ファイルが見つかりません"
        WHEN 22
            DISPLAY "重複キーです"
        WHEN 23
            DISPLAY "レコードが見つかりません"
        WHEN 30
            DISPLAY "I/Oエラーが発生しました"
        WHEN OTHER
            DISPLAY "未知のエラー: " SALES-STATUS
    END-EVALUATE
    STOP RUN
END-IF.
```

---

---

## EXTFH Handler Implementation

### Safe Pointer Access Pattern (Zig ポインタ操作の安全化)

GnuCOBOL 側から受け取る FCD3 はポインタであり、直接操作は危険です。CZIPPBT では **Zig の型安全機構を最大限活用** して、安全にアクセスします。

**パターン 1: ファイル名の安全な抽出（null終端化対応）**

```zig
// ❌ 危険なアクセス
const filename = fcd.filename;  // [256]u8 (FCD3内の固定配列)

// ✅ 安全なアクセス（Zig style）
const filename: []const u8 = blk: {
    const len = std.mem.indexOfScalar(u8, &fcd.filename, 0) orelse fcd.filename.len;
    break :blk fcd.filename[0..len];
};
```

**パターン 2: レコード data へのポインタアクセス（バウンダリチェック）**

```zig
// ❌ 危険なアクセス
@memcpy(destination, fcd.record_ptr, fcd.record_size);  // 長さ確認なし

// ✅ 安全なアクセス（スライス化して Zig の型チェック）
const record_buf: [*c]u8 = fcd.record_ptr;
const safe_slice: []u8 = record_buf[0..@intCast(fcd.record_size)];
@memcpy(destination[0..safe_slice.len], safe_slice);
```

**パターン 3: キーバッファの安全なアクセス**

```zig
// ❌ 危険なアクセス
const key_data: []const u8 = undefined;  // null を参照

// ✅ 安全なアクセス
if (fcd.key_ptr == null) {
    fcd.status = 5;  // I/O error
    return;
}
const key_buf: [*c]u8 = fcd.key_ptr;
const key_slice: []const u8 = key_buf[0..@intCast(fcd.record_key_size)];
```

**実装上の推奨パターン：**

```zig
// FCD3 のヘルパーメソッド（オプション）
pub const FCD3 = struct {
    // ... フィールド定義 ...

    /// 安全なファイル名取得
    pub fn getFilename(self: *FCD3) []const u8 {
        const len = std.mem.indexOfScalar(u8, &self.filename, 0) orelse self.filename.len;
        return self.filename[0..len];
    }

    /// 安全なレコードバッファ取得
    pub fn getRecordSlice(self: *FCD3) []u8 {
        return self.record_ptr[0..@intCast(self.record_size)];
    }

    /// 安全なキーバッファ取得
    pub fn getKeySlice(self: *FCD3) ?[]const u8 {
        if (self.key_ptr == null or self.record_key_size == 0) {
            return null;
        }
        return self.key_ptr[0..@intCast(self.record_key_size)];
    }
};
```

---

### Z_EXTFH Function Signature

```zig
/// Extended File Handler for GnuCOBOL
/// Called for all I-O operations when compiled with -fcallfh=czippfh
export fn czippfh(fcd_ptr: [*c]c_int) callconv(.C) void {
    // fcd_ptr points to FCD3 structure in COBOL memory
    const fcd: *FCD3 = @alignCast(@ptrCast(fcd_ptr));

    // Parse FCD3 fields and dispatch to appropriate VBISAM operation
    // ✅ Use safe access patterns (getFilename, getRecordSlice, etc.)
}
```

### FCD3 Structure in Zig

```zig
pub const FCD3 = struct {
    call_id: c_int,              // CALL-ID (operation code)
    handle: c_int,               // File handle
    status: c_short,             // Status code (return value)
    filename: [256]u8,           // Filename
    file_open_mode: c_short,     // OPEN mode (INPUT/OUTPUT/I-O)
    record_varying: c_short,     // Variable-length records?
    record_size: c_int,          // Record size in bytes
    record_key_pos: c_int,       // Key position in record
    record_key_size: c_int,      // Key size
    record_ptr: [*c]u8,          // Pointer to record data
    key_ptr: [*c]u8,             // Pointer to key buffer
    option: c_short,             // Read mode (FIRST/NEXT/EQ/GTEQ/etc)
    key_number: c_short,         // Key index (0=primary key)
    // Additional fields (reserved/extended)
    reserved: [128]u8,           // Reserved for future use
};
```

### File Handle Management

```zig
/// Global file handle table (maps COBOL handles to VBISAM handles)
var handle_table: std.AutoHashMap(c_int, ExtfhFileContext) = undefined;

pub const ExtfhFileContext = struct {
    vbisam_handle: c_int,           // VBISAM file handle
    filename: []const u8,           // Owned filename
    record_size: usize,             // Record size
    record_varying: bool,           // Variable-length records?
    is_open: bool,                  // Currently open?
    vbisam_mode: vbisam.OpenMode,   // VBISAM open mode
    current_key: c_int,             // Current key index
};
```

---

## Operation Handlers

### OPEN (call_id = 1)

```zig
fn handleOpen(fcd: *FCD3) void {
    // 1. Extract filename (null-terminated in FCD3.filename)
    // 2. Determine VBISAM open mode from FCD3.file_open_mode:
    //    - COBOL INPUT (0) → VBISAM.OpenMode.INPUT
    //    - COBOL OUTPUT (1) → VBISAM.OpenMode.OUTPUT
    //    - COBOL I-O (2) → VBISAM.OpenMode.INOUT
    // 3. Call vbisam.open() or vbisam.build()
    // 4. Store in handle_table
    // 5. Return handle in FCD3.handle
    // 6. Set FCD3.status = 0 on success
}
```

**COBOL Example:**
```cobol
FD SALES-FILE
   ORGANIZATION IS INDEXED
   RECORD KEY IS SALES-KEY
   FILE STATUS IS WS-STATUS.
01 SALES-REC.
   05 SALES-KEY      PIC 9(8).
   05 SALES-AMOUNT   PIC 9(10)V99.

OPEN INPUT SALES-FILE.
* Triggers: czippfh() with CALL-ID=1, mode=INPUT
```

### READ (call_id = 3)

Supports multiple read modes:

```zig
fn handleRead(fcd: *FCD3) void {
    // 1. Get file handle from handle_table
    // 2. Map FCD3.option to VBISAM.ReadMode:
    //    - 0 (FIRST) → VBISAM.ReadMode.FIRST
    //    - 1 (LAST) → VBISAM.ReadMode.LAST
    //    - 2 (NEXT) → VBISAM.ReadMode.NEXT
    //    - 3 (PREV) → VBISAM.ReadMode.PREV
    //    - 4 (CURRENT) → VBISAM.ReadMode.CURR
    //    - 5 (EQUAL) → VBISAM.ReadMode.EQUAL
    //    - 6 (GREATER) → VBISAM.ReadMode.GREAT
    //    - 7 (GREATER-EQUAL) → VBISAM.ReadMode.GTEQ
    // 3. If FCD3.key_ptr is set, use isstart() first
    // 4. Call isread() with record_ptr
    // 5. Copy data to FCD3.record_ptr
    // 6. Set FCD3.status based on result
}
```

**COBOL Example:**
```cobol
PROCEDURE DIVISION.
    OPEN INPUT SALES-FILE.

    READ SALES-FILE
        KEY IS SALES-KEY
        AT END MOVE 1 TO WS-EOF
        NOT AT END DISPLAY SALES-REC
    END-READ.

    CLOSE SALES-FILE.
```

### WRITE (call_id = 4)

```zig
fn handleWrite(fcd: *FCD3) void {
    // 1. Get file handle
    // 2. Copy data from FCD3.record_ptr
    // 3. Call iswrite()
    // 4. On duplicate key: set FCD3.status = 3
    // 5. On success: set FCD3.status = 0
}
```

### REWRITE (call_id = 5)

```zig
fn handleRewrite(fcd: *FCD3) void {
    // 1. Get file handle
    // 2. Copy data from FCD3.record_ptr
    // 3. Call isrewrite() (updates current record)
    // 4. Set status
}
```

### DELETE (call_id = 6)

```zig
fn handleDelete(fcd: *FCD3) void {
    // 1. Get file handle
    // 2. Call isdelcurr() (deletes current record)
    // OR if key provided, call isdelete()
    // 3. Set status
}
```

### CLOSE (call_id = 2)

```zig
fn handleClose(fcd: *FCD3) void {
    // 1. Get file handle
    // 2. Call isclose()
    // 3. Remove from handle_table
    // 4. Set FCD3.status = 0
}
```

### START (call_id = 7)

Position at a key value for sequential reading:

```zig
fn handleStart(fcd: *FCD3) void {
    // 1. Get file handle
    // 2. Use FCD3.key_ptr and FCD3.record_key_size
    // 3. Call isstart() with appropriate ReadMode
    // 4. Sets position for next READ NEXT
    // 5. Set status
}
```

**COBOL Example:**
```cobol
MOVE "SEARCH-KEY" TO SALES-KEY.
READ SALES-FILE
    KEY IS SALES-KEY
    INVALID KEY DISPLAY "NOT FOUND"
    VALID KEY DISPLAY SALES-REC
END-READ.
```

---

## Integration with Resolver

The Resolver must detect VSAM files and provide path resolution:

```zig
/// Resolver detects VSAM file type and returns VBISAM-compatible path
pub const FileInfo = struct {
    dsn: []const u8,          // Logical file name (e.g., "SALES-FILE")
    path: []const u8,         // Physical path (e.g., "/data/sales.isam")
    file_type: FileType,      // VSAM, SEQUENTIAL, VBISAM, etc.
    record_size: usize,       // Expected record size
    indexed: bool,            // Is indexed (VSAM)?
    primary_key_pos: usize,   // Primary key position
    primary_key_size: usize,  // Primary key size
};

pub const FileType = enum {
    SEQUENTIAL,               // LINE SEQUENTIAL
    RELATIVE,                 // RELATIVE
    VSAM,                     // INDEXED (VSAM KSDS via VBISAM)
    VBISAM,                   // Direct VBISAM file
    DUMMY,                    // DUMMY device
};
```

### File Catalog Entry

```json
{
  "DSN": "SALES-FILE",
  "TYPE": "VSAM",
  "PATH": "/data/sales.isam",
  "RECORD_SIZE": 100,
  "PRIMARY_KEY": {
    "POSITION": 0,
    "SIZE": 8,
    "TYPE": "BINARY"
  }
}
```

---

## EXTFH Compilation Integration

### build.zig Configuration

```zig
// Compile COBOL program with EXTFH support
const cobol_step = b.addSystemCommand(&[_][]const u8{
    "cobc",
    "-fcallfh=czippfh",  // Enable EXTFH with our handler
    "-fixed",             // Fixed format COBOL
    "-Wall",
    "-fno-separate-compilation",
    "-c",
    "cobol_src/vsam_test.cob",
    "-o", "obj/vsam_test.o",
});

// Link against our EXTFH handler (extfh.zig → extfh.o)
const extfh_lib = b.addStaticLibrary(.{
    .name = "extfh",
    .root_source_file = b.path("src/runtime/extfh.zig"),
    .target = target,
    .optimize = optimize,
});

// Final executable links both
exe.linkLibrary(extfh_lib);
exe.linkLibC();
exe.linkLibrary(vbisam_lib);
```

---

## Example: COBOL VSAM Program

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSAM-TEST.
       AUTHOR. CZIPPBT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-FILE
               ASSIGN TO "sales.isam"
               ORGANIZATION IS INDEXED
               RECORD KEY IS SALES-KEY
               ALTERNATE RECORD KEY IS SALES-DATE
                   WITH DUPLICATES
               FILE STATUS IS SALES-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD SALES-FILE.
       01 SALES-REC.
           05 SALES-KEY        PIC 9(8).
           05 SALES-DATE       PIC 9(8).
           05 SALES-AMOUNT     PIC 9(10)V99.
           05 SALES-DESC       PIC X(50).

       WORKING-STORAGE SECTION.
       01 SALES-STATUS         PIC 9(2).
       01 WS-EOF               PIC 9 VALUE 0.

       PROCEDURE DIVISION.
           PERFORM OPEN-SALES-FILE.
           PERFORM READ-SALES-RECORDS.
           PERFORM CLOSE-SALES-FILE.
           STOP RUN.

       OPEN-SALES-FILE.
           OPEN INPUT SALES-FILE.
           IF SALES-STATUS NOT = 0
               DISPLAY "ERROR OPENING FILE: " SALES-STATUS
               STOP RUN
           END-IF.

       READ-SALES-RECORDS.
           PERFORM UNTIL WS-EOF = 1
               READ SALES-FILE
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       DISPLAY SALES-REC
               END-READ
           END-PERFORM.

       CLOSE-SALES-FILE.
           CLOSE SALES-FILE.
```

**Compilation:**
```bash
cobc -fcallfh=czippfh -fixed -c cobol_src/vsam_test.cob
zig build
```

---

## Testing Strategy

### Unit Tests

1. **Handle Management**
   - [ ] Open file → allocates handle
   - [ ] Close file → deallocates handle
   - [ ] Multiple concurrent opens
   - [ ] Handle reuse after close

2. **Read Operations**
   - [ ] READ FIRST record
   - [ ] READ NEXT sequential
   - [ ] READ with key (EQUAL)
   - [ ] READ GREATER-EQUAL
   - [ ] EOF handling
   - [ ] Record not found

3. **Write Operations**
   - [ ] WRITE new record
   - [ ] Duplicate key detection
   - [ ] REWRITE existing record
   - [ ] DELETE record

4. **Key Handling**
   - [ ] Primary key access
   - [ ] Alternate key access
   - [ ] Composite keys
   - [ ] Key ranges (GTEQ)

### Integration Tests

```bash
# Create test file
zig build test-vsam-create

# Run COBOL program with EXTFH
zig build test-vsam-read

# Verify output matches expected
diff expected.txt actual.txt
```

### COBOL Test Checklist

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-EXTFH.

       * Test 1: OPEN/CLOSE
       OPEN INPUT SALES-FILE.
       CLOSE SALES-FILE.

       * Test 2: Sequential READ
       OPEN INPUT SALES-FILE.
       READ SALES-FILE.
       READ SALES-FILE.
       CLOSE SALES-FILE.

       * Test 3: KEY read (START + READ)
       OPEN INPUT SALES-FILE.
       MOVE SEARCH-KEY TO SALES-KEY.
       READ SALES-FILE
           KEY IS SALES-KEY.
       CLOSE SALES-FILE.

       * Test 4: WRITE
       OPEN OUTPUT SALES-FILE.
       WRITE SALES-REC.
       CLOSE SALES-FILE.

       * Test 5: Multi-key access
       OPEN INPUT SALES-FILE.
       READ SALES-FILE
           KEY ALTERNATE
           RECORD KEY IS SALES-DATE.
       CLOSE SALES-FILE.
```

---

## Error Handling

### VBISAM Error → FCD3 Status Mapping

| VBISAM Error | FCD3 Status | COBOL FILE STATUS |
|---|---|---|
| `NoRecord` | 4 | 23 (Record not found) |
| `Duplicate` | 3 | 22 (Duplicate record) |
| `Locked` | 2 | 92 (File locked) |
| `BadFile` | 5 | 30 (I-O error) |
| `NotOpen` | 5 | 37 (File not open) |
| `EndFile` | 10 | 10 (EOF) |

### COBOL Program Error Handling

```cobol
       OPEN INPUT SALES-FILE.
       IF SALES-STATUS NOT = 0
           EVALUATE SALES-STATUS
               WHEN 23
                   DISPLAY "Record not found"
               WHEN 22
                   DISPLAY "Duplicate key"
               WHEN 92
                   DISPLAY "File locked"
               WHEN OTHER
                   DISPLAY "Unexpected error: " SALES-STATUS
           END-EVALUATE
           STOP RUN
       END-IF.
```

---

## Thread Safety

### Considerations

1. **Handle Table**: Protected by mutex for concurrent file access
2. **VBISAM Handles**: VBISAM is thread-safe with per-handle locking
3. **Memory**: FCD3 structure is in COBOL's memory space (per-job isolation)

```zig
var handle_table_mutex = std.Thread.Mutex{};

fn handleOpen(fcd: *FCD3) void {
    handle_table_mutex.lock();
    defer handle_table_mutex.unlock();

    // Allocate handle
    // Add to table
}
```

---

## Configuration

### config/vsam.json

```json
{
  "vsam": {
    "enabled": true,
    "vbisam_lib_path": "/usr/lib/libvbisam.so",
    "catalog_path": "/var/batch/vsam_catalog.json",
    "default_record_size": 256,
    "max_concurrent_files": 50,
    "lock_mode": "AUTO"
  }
}
```

---

## Limitations

1. **No Transaction Support** (COMMIT/ABORT) - Pending GnuCOBOL enhancement
2. **Fixed EXTFH Signature** - Handler function name is fixed (czippfh)
3. **Key Compression** - Not yet supported (VBISAM capable)
4. **Alternate Key Modes** - Limited duplicate handling
5. **Record-Level Locking** - VBISAM-limited
6. **No VSAM + GDG Combination** - VSAM (INDEXED) files do not support GDG (Generation Data Group) management. GDG is only available for SEQUENTIAL files. This is a design limitation for the current phase.

---

## Future Enhancements

1. **Transaction Support**: Implement ABORT/COMMIT operations
2. **Dynamic Key Creation**: ALTER file to add/remove keys
3. **Audit Logging**: Log all EXTFH operations
4. **Performance Optimization**: Cache key descriptors
5. **COBOL4J Support**: Extend EXTFH for COBOL4J INDEXED files
6. **VSAM GDG Support**: Consider enabling GDG for VSAM files (e.g., `MASTER.G0001V00.isam`) - requires extending GDG resolver to recognize INDEXED file patterns

---

## References

- **GnuCOBOL Manual**: https://gnucobol.sourceforge.io/doc/gnucobol.html
- **GnuCOBOL xfhfcd3.cpy**: https://fossies.org/linux/gnucobol/copy/xfhfcd3.cpy
- **VBISAM Documentation**: https://github.com/skyalive/vbisam-osscons-patch
- **Related Specs**:
  - `specs/001-architecture.md` - System architecture
  - `specs/005-vbisam-integration.md` - VBISAM setup (Phase 5.1-5.2)
  - `docs/cobol-build-integration.md` - GnuCOBOL build integration

---

## Abstraction Layer Architecture

See [specs/015b-vsam-abstraction.md](./015b-vsam-abstraction.md) for detailed architecture design.

### Key Points

**Problem:** EXTFH is tightly coupled to VBISAM C library, making it difficult to support alternative ISAM implementations.

**Solution:** Introduce a **Tagged Union-based abstraction layer** (Zig idiom) that:
- Decouples EXTFH from VBISAM specifics
- Enables multi-backend support (VBISAM, BerkeleyDB, C-ISAM)
- Maintains compile-time type safety and performance
- Allows gradual migration of existing code

### Architecture
```
extfh.zig (EXTFH Handler)
    ↓
isam_interface.zig (Abstraction Layer - Tagged Union)
    ↓
[VbisamBackend, BdbBackend, CisamBackend, ...]
    ↓
Low-level backends (vbisam.zig, bdb.zig, etc.)
```

### Implementation Status

#### Completed (Phase 1-7)

| Phase | Component | Status | Date |
|-------|-----------|--------|------|
| 1 | Core abstraction layer (isam_interface.zig, isam_vbisam.zig) | ✅ | 2025-12-18 |
| 2 | Simple operations replacement (write, rewrite, delete, unlock) | ✅ | 2025-12-18 |
| 3 | ReadMode mapping (handleReadIndexed) | ✅ | 2025-12-18 |
| 4 | KeyDescBuilder abstraction (handleStart) | ✅ | 2025-12-18 |
| 5 | Resource leak fixes (deinit, close) | ✅ | 2025-12-18 |
| 6 | File operations replacement (handleOpenIndexed) | ✅ | 2025-12-18 |
| 7 | Test execution & verification (34/34 tests passed) | ✅ | 2025-12-18 |

**Outcome**: extfh.zig has ZERO dependencies on vbisam.zig → Full plugin architecture achieved

#### Upcoming (Phase 8+)

- **Phase 8**: Final integration and backend independence verification
- **Phase 9+**: Alternative backend implementations (BerkeleyDB, C-ISAM, SQLite)

For implementation details, API documentation, and developer guide, refer to [specs/015b-vsam-abstraction-layer.md](./015b-vsam-abstraction-layer.md).

---

## Version History

| Version | Date | Status | Notes |
|---------|------|--------|-------|
| 1.1 | 2025-12-18 | Updated | Added Abstraction Layer Phase 1 |
| 1.0 | 2025-12-11 | Draft | Initial EXTFH specification |
