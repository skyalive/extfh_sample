# バックエンド差し替えの実装例 - 抽象化の真の価値

**コア原理**: 抽象化があると、**バックエンド初期化だけ** が変わり、その他は全く変わらない。

---

## 魔法: extfh.zig は 100% 変わらない

### シナリオ 1: VBISAM バックエンド

```zig
// main.zig - 本番環境（VBISAM）
const extfh = @import("extfh.zig");
const isam_vbisam = @import("isam_vbisam.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // 唯一の変更: VBISAM バックエンド初期化
    const backend = isam.IsamBackend{
        .VBISAM = isam_vbisam.VbisamBackend.init(allocator),
    };
    
    extfh.init(allocator, backend);
    defer extfh.deinit();
    
    // EXTFH ハンドラーは GnuCOBOL によって呼び出される
    // ... COBOL プログラム実行 ...
}
```

### シナリオ 2: SQLite バックエンド（同じ extfh.zig！）

```zig
// main.zig - 開発環境（SQLite）
const extfh = @import("extfh.zig");
const isam_sqlite = @import("isam_sqlite.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // 唯一の変更: SQLite バックエンド初期化
    const backend = isam.IsamBackend{
        .SQLITE = isam_sqlite.SqliteBackend.init(allocator),
    };
    
    extfh.init(allocator, backend);
    defer extfh.deinit();
    
    // EXTFH ハンドラーは 完全に同じ
    // ... COBOL プログラム実行 ...
}
```

### シナリオ 3: モックバックエンド（テスト用）

```zig
// test.zig - テスト環境（モック）
const extfh = @import("extfh.zig");
const isam_mock = @import("isam_mock.zig");

test "EXTFH read/write with mock" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // 唯一の変更: モックバックエンド初期化
    const backend = isam.IsamBackend{
        .MOCK = isam_mock.MockBackend.init(allocator),
    };
    
    extfh.init(allocator, backend);
    defer extfh.deinit();
    
    // EXTFH ハンドラーは 完全に同じ
    // テストコードはインメモリモックに対して実行
    var fcd: FCD3 = undefined;
    fcd.call_id = 1; // OPEN
    @memcpy(fcd.filename[0..8], "test.dat");
    fcd.filename[8] = 0;
    
    extfh.czippfh(@ptrCast(&fcd.call_id));
    
    try std.testing.expect(fcd.status == 0);
}
```

---

## 証拠: extfh.zig はすべてのシナリオで同じ

```zig
// extfh.zig - VBISAM、SQLite、モックで 完全に同じ
const std = @import("std");
const isam = @import("isam_interface.zig");

var allocator: std.mem.Allocator = undefined;
var backend: isam.IsamBackend = undefined;
var file_map: std.AutoHashMap(c_int, isam.IsamFileHandle) = undefined;

pub fn init(allocator_in: std.mem.Allocator, backend_in: isam.IsamBackend) void {
    allocator = allocator_in;
    backend = backend_in;  // ← どのバックエンドか保存するだけ
    file_map = std.AutoHashMap(c_int, isam.IsamFileHandle).init(allocator);
}

pub export fn czippfh(fcd_ptr: [*c]c_int) callconv(.C) void {
    const fcd: *FCD3 = @ptrCast(@alignCast(fcd_ptr));
    
    switch (fcd.call_id) {
        1 => handleOpen(fcd),
        2 => handleClose(fcd),
        3 => handleRead(fcd),
        4 => handleWrite(fcd),
        // ... など
        else => fcd.status = 9,
    }
}

// ハンドラーは 'backend' を使う - どのバックエンドでも同じ！
fn handleOpen(fcd: *FCD3) void {
    const filename = std.mem.sliceTo(&fcd.filename, 0);
    const mode = mapCobolMode(fcd.file_open_mode);
    
    // この呼び出しは VBISAM、SQLite、モックで機能！
    const handle = backend.open(filename, mode) catch |err| {
        fcd.status = mapErrorToStatus(err);
        return;
    };
    
    file_map.put(fcd.handle, handle) catch { /* ... */ };
    fcd.status = 0;
}

fn handleRead(fcd: *FCD3) void {
    const handle = file_map.get(fcd.handle) orelse {
        fcd.status = 5;
        return;
    };
    
    const mode = mapCobolReadMode(fcd.option);
    const buffer = fcd.record_ptr[0..@intCast(fcd.record_size)];
    
    // この呼び出しは VBISAM、SQLite、モックで機能！
    backend.read(handle, buffer, mode) catch |err| {
        fcd.status = mapErrorToStatus(err);
        return;
    };
    
    fcd.status = 0;
}

fn handleWrite(fcd: *FCD3) void {
    const handle = file_map.get(fcd.handle) orelse {
        fcd.status = 5;
        return;
    };
    
    const buffer = fcd.record_ptr[0..@intCast(fcd.record_size)];
    
    // この呼び出しは VBISAM、SQLite、モックで機能！
    backend.write(handle, buffer) catch |err| {
        fcd.status = mapErrorToStatus(err);
        return;
    };
    
    fcd.status = 0;
}

// ... その他のハンドラーも完全に同じ ...
```

---

## 実例: build.zig でのバックエンド選択

### build.zig - コンパイル時バックエンド選択

```zig
// build.zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    
    // バックエンド選択用ビルドオプション
    const backend_option = b.option(
        []const u8,
        "backend",
        "ISAM バックエンド: vbisam, sqlite, mock (デフォルト: vbisam)"
    ) orelse "vbisam";
    
    const exe = b.addExecutable(.{
        .name = "cobol_app",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    
    // バックエンドに応じて異なる main.zig
    const main_source = switch (std.mem.eql(u8, backend_option, "sqlite")) {
        true => "src/main_sqlite.zig",      // SQLite バージョン
        false => if (std.mem.eql(u8, backend_option, "mock"))
            "src/main_mock.zig"             // モックバージョン
        else
            "src/main_vbisam.zig",          // VBISAM バージョン（デフォルト）
    };
    
    // バックエンドに応じて異なるライブラリをリンク
    switch (std.mem.eql(u8, backend_option, "sqlite")) {
        true => exe.linkSystemLibrary("sqlite3"),
        false => exe.linkSystemLibrary("bdb"),  // VBISAM
    }
    
    b.installArtifact(exe);
}
```

### ビルドコマンド使用方法

```bash
# 本番環境: VBISAM バックエンド
zig build -Dbackend=vbisam

# 開発環境: SQLite バックエンド
zig build -Dbackend=sqlite

# テスト環境: モックバックエンド
zig build -Dbackend=mock
```

---

## 実践例: 環境別初期化

### ランタイムバックエンド選択

```zig
// main.zig - スマートな初期化
const std = @import("std");
const extfh = @import("extfh.zig");
const isam = @import("isam_interface.zig");
const isam_vbisam = @import("isam_vbisam.zig");
const isam_sqlite = @import("isam_sqlite.zig");
const isam_mock = @import("isam_mock.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // 環境変数でバックエンド選択
    const backend_name = std.os.getenv("ISAM_BACKEND") orelse "vbisam";
    
    // 適切なバックエンド初期化
    const backend: isam.IsamBackend = 
        if (std.mem.eql(u8, backend_name, "sqlite"))
            .{ .SQLITE = isam_sqlite.SqliteBackend.init(allocator) }
        else if (std.mem.eql(u8, backend_name, "mock"))
            .{ .MOCK = isam_mock.MockBackend.init(allocator) }
        else
            .{ .VBISAM = isam_vbisam.VbisamBackend.init(allocator) };
    
    extfh.init(allocator, backend);
    defer extfh.deinit();
    
    std.debug.print("使用バックエンド: {s}\n", .{backend_name});
    
    // EXTFH ハンドラーはどのバックエンドでも機能！
    // ... プログラムの残り ...
}
```

### ランタイムバックエンド切り替え例

```bash
# 本番環境
export ISAM_BACKEND=vbisam
./cobol_app

# 開発環境  
export ISAM_BACKEND=sqlite
./cobol_app

# テスト環境
export ISAM_BACKEND=mock
./cobol_app
```

---

## 比較: 直接依存 vs. 抽象化

### 抽象化なし（直接 VBISAM）

```
バックエンド切り替えに必要な変更:
┌────────────────────────────────────────┐
│ extfh.zig (400 行)                     │
│                                        │
│ すべての VBISAM 呼び出しがハードコード:│
│ ├─ vbisam.isopen()                     │
│ ├─ vbisam.isread()                     │
│ ├─ vbisam.iswrite()                    │
│ └─ ...                                 │
│                                        │
│ SQLite に切り替えるには:                │
│ ➜ extfh.zig 全体を書き直し（400 行）  │
│ ➜ すべての呼び出しを sqlite3_*() に変更│
│ ➜ すべてをテストし直す                │
│ ➜ 微妙なバグの危険                     │
└────────────────────────────────────────┘
```

### 抽象化あり（プラグイン型バックエンド）

```
初期化だけが変わる:
┌────────────────────────────────────────┐
│ main.zig (25 行)                       │
│                                        │
│ const backend = isam.IsamBackend{      │
│   .VBISAM = ...  // この行を差し替える  │
│ };               // .SQLITE = ... で    │
│                                        │
│ その他の変更は不要！                  │
│                                        │
│ extfh.zig (400 行)                     │
│ [変更なし - 抽象バックエンド使用]      │
└────────────────────────────────────────┘
```

---

## 実世界シナリオ: マルチ環境デプロイ

### シナリオ: 銀行の COBOL システム

```
要件:
  - 本番環境（東京）: VBISAM（既存 VSAM 互換）
  - ステージング環境（シンガポール）: SQLite（軽量）
  - 開発環境（ローカル）: モック（インスタント、ディスク I/O なし）
  - テスト環境（CI/CD）: モック（並列テスト実行）
```

### 抽象化あり - 同じバイナリをデプロイ

```
build.zig は 1 つの実行可能ファイルを生成: cobol_app

環境ごとの設定でデプロイ:

┌─ 本番サーバー（東京）
│  $ ISAM_BACKEND=vbisam ./cobol_app
│  → VBISAM を使用、レガシー VSAM 互換 ✓
│
├─ ステージングサーバー（シンガポール）
│  $ ISAM_BACKEND=sqlite ./cobol_app
│  → SQLite を使用、軽量 ✓
│
├─ 開発者ノート PC
│  $ ISAM_BACKEND=mock ./cobol_app
│  → モックを使用、インスタント起動 ✓
│
└─ CI/CD パイプライン
   $ ISAM_BACKEND=mock zig build test
   → モック使用、テスト 100 倍高速化 ✓
```

**同じバイナリ。同じ extfh.zig。環境別に異なるバックエンド。**

### 抽象化なし - 環境別バイナリを構築

```
環境ごとに異なるバイナリをコンパイル:

cobol_app_production  (VBISAM バックエンド)
cobol_app_staging     (SQLite バックエンド)
cobol_app_dev         (モックバックエンド)
cobol_app_test        (モックバックエンド)

問題:
  ✗ バイナリ分散（環境ごとに微妙なバグ）
  ✗ デプロイ複雑化（どのバイナリをどこに？）
  ✗ テストギャップ（本番は VBISAM、テストは別）
  ✗ メンテナンス悪夢（extfh.zig の 4+ バージョン）
```

---

## コード行数比較

### 切り替えコスト: 直接 vs. 抽象化

#### 抽象化なし

```
バックエンド切り替え:
  
  1. extfh.zig を extfh_sqlite.zig にコピー
  2. すべての VBISAM 呼び出しを置き換え:
     - vbisam.isopen() → sqlite3_open()
     - vbisam.isread() → sqlite3_prepare() + sqlite3_step()
     - vbisam.iswrite() → sqlite3_exec(INSERT...)
     - ... 20+ 回の置き替え
  3. SQLite 固有エラーハンドリング
  4. ワークフロー全体をテスト
  5. 両バージョンをソース管理に保持
  6. どのバージョンをどこで使うかをドキュメント化
  7. 問題: 発散したバグ修正（1 つで修正、他の方を忘れる）
  
  労力: バックエンド毎に 1-2 週間
  ファイル: extfh.zig の 5+ バージョン（本番、ステージング、開発、テスト）
  リスク: 高
```

#### 抽象化あり

```
バックエンド切り替え:

  1. isam_sqlite.zig を作成（標準インターフェース実装）
  2. main.zig を修正（3 行変更）:
     .{ .VBISAM = ... }  →  .{ .SQLITE = ... }
  3. 新バックエンドでテスト
  4. 完了！
  
  労力: 1 日（バックエンド実装だけ、extfh.zig は再利用）
  ファイル: extfh.zig 1 つ（すべてのバックエンドで再利用）
  リスク: 低（extfh.zig は 1 回テスト、どこでも再利用）
```

---

## 図解: 抽象化でのデータフロー

### VBISAM バックエンド

```
COBOL プログラム
    ↓ (FCD3 構造体: OPEN、filename="sales.dat")
czippfh()
    ↓
handleOpen(fcd)
    ↓
backend.open("sales.dat", INPUT)
    ↓ (VBISAM 実装にディスパッチ)
VbisamBackend.open()
    ↓
vbisam.isopen("sales.dat", ISREAD)
    ↓
VBISAM C ライブラリ
    ↓
ディスクから "sales.dat" を読み込み
```

### SQLite バックエンド（同じ czippfh!）

```
COBOL プログラム
    ↓ (FCD3 構造体: OPEN、filename="sales.dat")
czippfh()
    ↓
handleOpen(fcd)  [同じコード]
    ↓
backend.open("sales.dat", INPUT)
    ↓ (SQLite 実装にディスパッチ)
SqliteBackend.open()
    ↓
sqlite3_open(":memory:")
    ↓
SQLite ライブラリ
    ↓
インメモリデータベースを開く
```

### モックバックエンド（同じ czippfh!）

```
COBOL プログラム
    ↓ (FCD3 構造体: OPEN、filename="sales.dat")
czippfh()
    ↓
handleOpen(fcd)  [同じコード]
    ↓
backend.open("sales.dat", INPUT)
    ↓ (モック実装にディスパッチ)
MockBackend.open()
    ↓
HashMap.put("sales.dat", {})
    ↓
インメモリ HashMap
    ↓
インスタント操作（ディスク I/O なし）
```

---

## 真の利点: テスト

### 抽象化なし

```cobol
* COBOL テストコード
OPEN INPUT SALES-FILE.
READ SALES-FILE.
IF FILE-STATUS != "00"
    DISPLAY "ERROR"
    STOP RUN
END-IF.
DISPLAY "Record: " SALES-RECORD.
CLOSE SALES-FILE.

* 問題:
*   - ディスク上に実際の VSAM/VBISAM ファイルが必要
*   - テスト実行に数秒かかる（ディスク I/O）
*   - エラー条件のテストが難しい
*   - 順序付きテスト（1 つずつ）
*   - 1000 個のテストバリエーションが困難
```

### 抽象化あり

```zig
test "存在しないレコードを読み込み" {
    // モックバックエンド初期化
    const backend = isam.IsamBackend{
        .MOCK = MockBackend.init(allocator),
    };
    extfh.init(allocator, backend);
    
    // 存在しないキーで COBOL READ をシミュレート
    var fcd: FCD3 = undefined;
    fcd.call_id = 3;  // READ
    fcd.handle = 1;
    fcd.option = 4;   // EQUAL
    
    extfh.czippfh(@ptrCast(&fcd.call_id));
    
    // エラーステータスをアサート
    try std.testing.expectEqual(@as(c_short, 4), fcd.status);
    // ✓ ディスク I/O なし、インスタント
    // ✓ エラー条件を簡単にシミュレート
    // ✓ 並列実行可能
    // ✓ 1000+ バリエーションが数秒で完了
}
```

---

## 総括: 何が差し替わるのか？

### これだけ変わる

```zig
// main_vbisam.zig
const backend = isam.IsamBackend{
    .VBISAM = isam_vbisam.VbisamBackend.init(allocator),
};

// main_sqlite.zig (唯一の変更)
const backend = isam.IsamBackend{
    .SQLITE = isam_sqlite.SqliteBackend.init(allocator),
};

// main_mock.zig (唯一の変更)
const backend = isam.IsamBackend{
    .MOCK = isam_mock.MockBackend.init(allocator),
};
```

### その他はすべて同じ

```zig
// extfh.zig - すべてのバックエンドで 100% 同じ
pub export fn czippfh(fcd_ptr: [*c]c_int) callconv(.C) void {
    // VBISAM、SQLite、モックで変わらない
}

fn handleOpen(fcd: *FCD3) void {
    // VBISAM、SQLite、モックで変わらない
}

fn handleRead(fcd: *FCD3) void {
    // VBISAM、SQLite、モックで変わらない
}

// ... その他のハンドラーも同じ ...
```

---

## あなたの質問への回答

> **抽象化レイヤーがあると、BACKEND の差し替えだけで仕組みが入れ替えられる？**

**はい、完全にそうです。**

| 項目 | 変更必須か？ |
|------|-----------|
| **extfh.zig** | ✗ 変更不要 |
| **操作ハンドラー** | ✗ 変更不要 |
| **モードマッピング** | ✗ 変更不要 |
| **エラーハンドリング** | ✗ 変更不要 |
| **main.zig バックエンド初期化** | ✓ 1 行変更 |
| **リビルド** | ✓ 必須 |
| **回帰テスト** | ✗ 最小限（extfh 変更なし） |

**結果**: 1 行変更 → バックエンド全体が差し替わる → 同じ COBOL プログラムが VBISAM、SQLite、モックで動作。

これが抽象化レイヤーの本当の価値です。
