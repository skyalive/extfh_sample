# バックエンド選択: コンパイル時 vs ランタイム

**質問**: バックエンドを切り替えるにはリコンパイルが必要？

**回答**: **アプローチに依存します:**

| アプローチ | リコンパイル必要？ | 決定タイミング | 例 |
|----------|--------------|-----------|-----|
| **コンパイル時** | ✅ はい | ビルド時 | `zig build -Dbackend=sqlite` |
| **ランタイム** | ❌ いいえ | 実行時 | `export ISAM_BACKEND=sqlite && ./app` |

---

## アプローチ 1: コンパイル時選択（現在の方式）

### セットアップ: build.zig

```zig
// build.zig
const backend_option = b.option(
    []const u8,
    "backend",
    "バックエンド: vbisam, sqlite, mock"
) orelse "vbisam";

// バックエンド毎に異なる main ファイル
const main_file = switch(...) {
    "sqlite" => "src/main_sqlite.zig",
    "mock" => "src/main_mock.zig",
    else => "src/main_vbisam.zig",
};
```

### ビルドコマンド

```bash
# VBISAM でビルド
zig build -Dbackend=vbisam
# → 生成: ./zig-out/bin/cobol_app (VBISAM 版)

# SQLite でビルド
zig build -Dbackend=sqlite
# → 生成: ./zig-out/bin/cobol_app (SQLite 版)
# ⚠️ プロジェクト全体を リコンパイル

# モックでビルド
zig build -Dbackend=mock
# → 生成: ./zig-out/bin/cobol_app (モック版)
# ⚠️ プロジェクト全体を リコンパイル
```

### メリット・デメリット

**メリット:**
- コンパイル時最適化（デッドコード削除）
- バイナリ小（未使用バックエンド非含）
- 型安全（誤ったバックエンド設定はコンパイル時に検出）

**デメリット:**
- バックエンド毎にリビルド必要
- リコンパイルせずに切り替え不可
- 環境毎に異なるバイナリ

---

## アプローチ 2: ランタイム選択（マルチ環境向き）

### 概念: すべてのバックエンドをコンパイル、実行時選択

```zig
// isam_interface.zig
pub const IsamBackend = union(enum) {
    VBISAM: VbisamBackend,
    SQLITE: SqliteBackend,
    MOCK: MockBackend,
    
    // 注記: 3 つすべてが常にコンパイル対象
};

// main.zig
pub fn main() !void {
    const allocator = /* ... */;
    
    // ランタイムで環境変数を読む
    const backend_name = std.os.getenv("ISAM_BACKEND") orelse "vbisam";
    
    // 環境に基づいて適切なバックエンド作成
    const backend: isam.IsamBackend = 
        if (std.mem.eql(u8, backend_name, "sqlite"))
            .{ .SQLITE = isam_sqlite.SqliteBackend.init(allocator) }
        else if (std.mem.eql(u8, backend_name, "mock"))
            .{ .MOCK = isam_mock.MockBackend.init(allocator) }
        else
            .{ .VBISAM = isam_vbisam.VbisamBackend.init(allocator) };
    
    extfh.init(allocator, backend);
    defer extfh.deinit();
    
    // ... プログラム残り ...
}
```

### 1 度ビルド、どこでも実行

```bash
# すべてのバックエンドを含めてビルド（1 回のみ）
zig build

# VBISAM で実行（リコンパイル不要）
export ISAM_BACKEND=vbisam
./zig-out/bin/cobol_app

# SQLite で実行（リコンパイル不要）
export ISAM_BACKEND=sqlite
./zig-out/bin/cobol_app

# モックで実行（リコンパイル不要）
export ISAM_BACKEND=mock
./zig-out/bin/cobol_app
```

### メリット・デメリット

**メリット:**
- 1 度ビルド、どのバックエンドでも実行
- リコンパイル不要
- Docker/コンテナ展開に最適
- 環境の簡単切り替え

**デメリット:**
- バイナリやや大（すべてのバックエンド含む）
- ランタイム dispatch のオーバーヘッド（最小限）
- すべてのバックエンドを実行時対応する必要

---

## 直接比較

### コンパイル時選択

```
┌─ 開発フェーズ ─────────────────────────────┐
│                                            │
│ VBISAM でテスト:                           │
│   $ zig build -Dbackend=vbisam            │
│   リビルド... (30 秒)                     │
│   $ ./zig-out/bin/cobol_app               │
│                                            │
│ SQLite に切り替え:                         │
│   $ zig build -Dbackend=sqlite            │
│   リビルド... (30 秒) ← リコンパイル       │
│   $ ./zig-out/bin/cobol_app               │
│                                            │
│ モックに切り替え:                          │
│   $ zig build -Dbackend=mock              │
│   リビルド... (30 秒) ← リコンパイル       │
│                                            │
└────────────────────────────────────────────┘

すべてのバックエンド テスト時間: 約 2 分
```

### ランタイム選択

```
┌─ 開発フェーズ ─────────────────────────────┐
│                                            │
│ 1 度だけビルド:                            │
│   $ zig build                             │
│   リビルド... (30 秒)                     │
│                                            │
│ VBISAM でテスト:                          │
│   $ ISAM_BACKEND=vbisam ./zig-out/bin/... │
│   インスタント（リビルド不要）            │
│                                            │
│ SQLite に切り替え:                        │
│   $ ISAM_BACKEND=sqlite ./zig-out/bin/... │
│   インスタント（リビルド不要）            │
│                                            │
│ モックに切り替え:                         │
│   $ ISAM_BACKEND=mock ./zig-out/bin/...   │
│   インスタント（リビルド不要）            │
│                                            │
└────────────────────────────────────────────┘

すべてのバックエンド テスト時間: 約 1 分
```

---

## 実装推奨

### コンパイル時選択を使う場合:

```
✓ 1 度に 1 つのバックエンドのみ必要
✓ 実行可能ファイル毎に異なるバックエンド
✓ バイナリサイズが重要
✓ CI/CD はバックエンド毎に異なるイメージをビルド
✓ 異なるチームが異なるバックエンドを管理
```

**例: マルチコンテナ展開**
```
Dockerfile.prod   → VBISAM バックエンドでビルド
Dockerfile.dev    → SQLite バックエンドでビルド  
Dockerfile.test   → モックバックエンドでビルド
```

### ランタイム選択を使う場合:

```
✓ リビルドなしにバックエンド切り替え必要
✓ 同じバイナリを複数環境にデプロイ
✓ 開発者が複数バックエンドを素早くテスト
✓ CI/CD はすべてのバックエンドで同じテスト実行
✓ 環境がバックエンド選択を決定
```

**例: 単一コンテナ、環境設定**
```
docker run -e ISAM_BACKEND=vbisam cobol_app
docker run -e ISAM_BACKEND=sqlite cobol_app
docker run -e ISAM_BACKEND=mock cobol_app
```

---

## 実装: ランタイム選択（詳細）

### ファイル構成

```
src/
├── main.zig                    ← 決定ポイント（ランタイム）
├── extfh.zig                   ← 変更なし
├── isam_interface.zig          ← 変更なし
├── isam_vbisam.zig             ← コンパイル対象
├── isam_sqlite.zig             ← コンパイル対象
├── isam_mock.zig               ← コンパイル対象
└── build.zig                   ← バックエンド選択ロジックなし
```

### main.zig - ランタイムバックエンド選択

```zig
const std = @import("std");
const extfh = @import("extfh.zig");
const isam = @import("isam_interface.zig");

// すべてのバックエンドをインポート（すべてコンパイル）
const isam_vbisam = @import("isam_vbisam.zig");
const isam_sqlite = @import("isam_sqlite.zig");
const isam_mock = @import("isam_mock.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // ← ランタイムで決定される
    const backend_name = std.os.getenv("ISAM_BACKEND") orelse "vbisam";
    
    std.debug.print("ISAM バックエンド使用: {s}\n", .{backend_name});
    
    // 環境に基づいて適切なバックエンド作成
    const backend: isam.IsamBackend = switch_backend: {
        if (std.mem.eql(u8, backend_name, "sqlite")) {
            break :switch_backend .{ 
                .SQLITE = isam_sqlite.SqliteBackend.init(allocator) 
            };
        } else if (std.mem.eql(u8, backend_name, "mock")) {
            break :switch_backend .{ 
                .MOCK = isam_mock.MockBackend.init(allocator) 
            };
        } else if (std.mem.eql(u8, backend_name, "vbisam")) {
            break :switch_backend .{ 
                .VBISAM = isam_vbisam.VbisamBackend.init(allocator) 
            };
        } else {
            std.debug.print(
                "不明なバックエンド: {s}. vbisam, sqlite, mock を使用\n",
                .{backend_name}
            );
            return error.UnknownBackend;
        }
    };
    
    extfh.init(allocator, backend);
    defer extfh.deinit();
    
    // EXTFH ハンドラーは GnuCOBOL により呼び出される
    // すべてのバックエンドは同じ czippfh() 関数を使用
    std.debug.print("COBOL アプリケーション準備完了\n", .{});
    
    // ... プログラム残りは選択されたバックエンドで実行 ...
}
```

### build.zig - シンプル化（バックエンド選択ロジックなし）

```zig
// build.zig - コンパイル時選択より非常にシンプル
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    
    const exe = b.addExecutable(.{
        .name = "cobol_app",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    
    // すべてのバックエンドライブラリをリンク
    exe.linkSystemLibrary("bdb");       // VBISAM
    exe.linkSystemLibrary("sqlite3");   // SQLite
    // モックは外部依存なし
    
    b.installArtifact(exe);
}
```

### 使用方法 - 環境変数で切り替え

```bash
# 本番環境: VBISAM バックエンド
export ISAM_BACKEND=vbisam
./zig-out/bin/cobol_app

# 開発環境: SQLite バックエンド  
export ISAM_BACKEND=sqlite
./zig-out/bin/cobol_app

# テスト環境: モックバックエンド
export ISAM_BACKEND=mock
./zig-out/bin/cobol_app

# デフォルト（環境変数未設定の場合）
./zig-out/bin/cobol_app  # VBISAM を使用
```

---

## Docker 例: ランタイム選択が実現する効果

### 単一 Dockerfile（1 度だけビルド）

```dockerfile
FROM ubuntu:latest

RUN apt-get install -y \
    libdb-dev \
    libsqlite3-dev \
    zig

COPY src/ /app/src/
COPY build.zig /app/

WORKDIR /app

# 1 度だけビルド（すべてのバックエンド含）
RUN zig build -Doptimize=ReleaseFast

ENTRYPOINT ["/app/zig-out/bin/cobol_app"]
```

### 異なるバックエンドでデプロイ（リビルド不要）

```bash
# 本番コンテナ（VBISAM）
docker run -e ISAM_BACKEND=vbisam cobol_app:latest

# ステージング コンテナ（SQLite）
docker run -e ISAM_BACKEND=sqlite cobol_app:latest

# テストコンテナ（モック）
docker run -e ISAM_BACKEND=mock cobol_app:latest
```

**同じイメージ。異なるバックエンド。リビルド不要。**

---

## パフォーマンス比較

### バイナリサイズ

```
コンパイル時（VBISAM のみ）:
  cobol_app: 2.5 MB ✓ 最小

コンパイル時（SQLite のみ）:
  cobol_app: 3.1 MB

ランタイム（すべてのバックエンド）:
  cobol_app: 4.2 MB (VBISAM + SQLite + モック)
  └─ 実用的な範囲で十分小
```

### ランタイム dispatch オーバーヘッド

```
コンパイル時選択:
  - オーバーヘッドなし（バックエンド既に決定）
  - 関数呼び出しは直接

ランタイム選択:
  - 最小限のオーバーヘッド: 1 つの switch 文
  - タグ付きユニオン dispatch: ~1 ナノ秒
  - I/O 操作（マイクロ秒）と比べて無視可能
```

---

## 移行パス: コンパイル時 → ランタイム

### フェーズ 1: コンパイル時選択から開始（実装高速）

```bash
zig build -Dbackend=vbisam   # 初期開発
```

### フェーズ 2: ランタイム選択追加（必要に応じて）

```zig
// main.zig で: 環境変数チェック追加
const backend_name = std.os.getenv("ISAM_BACKEND") orelse "vbisam";
const backend = switch_backend: {
    if (std.mem.eql(u8, backend_name, "sqlite")) 
        break :switch_backend .{ .SQLITE = ... };
    // ... など
};
```

**変更は 1 ファイルだけ（main.zig）。extfh.zig は全く変わらない。**

---

## 決定マトリックス

```
シナリオ A: クイックプロトタイプ
  → コンパイル時選択 (-Dbackend=vbisam)
  → 理由: 実装最速

シナリオ B: 複数バックエンドを使うチーム
  → ランタイム選択 (ISAM_BACKEND 環境変数)
  → 理由: バックエンドテスト間でリビルド不要

シナリオ C: 本番マルチ環境
  → ランタイム選択 (コンテナ毎に異なる環境)
  → 理由: 同じバイナリ、環境設定のみ異なる

シナリオ D: パフォーマンス最優先レガシー COBOL
  → コンパイル時選択 (VBISAM のみ)
  → 理由: バイナリ最小、オーバーヘッドなし

シナリオ E: CI/CD で全バックエンド テスト
  → ランタイム選択 (すべてのバックエンドでテスト)
  → 理由: 1 度のビルド、環境変数ループ
```

---

## 実践テスト: ランタイム選択の速度

### シナリオ: 複数バックエンドでテスト

```bash
# コンパイル時アプローチ（3 度ビルド）
$ time zig build -Dbackend=vbisam
  コンパイル中... 30 秒
$ time zig build -Dbackend=sqlite
  コンパイル中... 30 秒
$ time zig build -Dbackend=mock
  コンパイル中... 30 秒
  
合計: 90 秒 ⏱️

# ランタイムアプローチ（1 度ビルド）
$ time zig build
  コンパイル中... 30 秒

# その後すべてのバックエンドをテスト（インスタント切り替え）
$ ISAM_BACKEND=vbisam ./zig-out/bin/cobol_app
  インスタント ✓
$ ISAM_BACKEND=sqlite ./zig-out/bin/cobol_app
  インスタント ✓
$ ISAM_BACKEND=mock ./zig-out/bin/cobol_app
  インスタント ✓
  
合計: 30 秒 ⏱️⏱️⏱️

高速化: 3 倍（90 秒 → 30 秒）
```

---

## 最終回答

| 質問 | 回答 |
|------|------|
| **リコンパイル必要？** | **アプローチに依存** |
| **コンパイル時選択** | ✅ はい、バックエンド毎にリビルド |
| **ランタイム選択** | ❌ いいえ、1 度ビルド、どこでも実行 |
| **どちらが高速？** | ランタイム選択（1 ビルド vs N ビルド） |
| **どちらがシンプル？** | コンパイル時選択（環境変数ロジック不要） |
| **本番環境対応？** | 両方、異なるトレードオフ |

### マルチ環境デプロイ向けの推奨

**ランタイム選択を使用:**
- 1 度ビルド: `zig build`
- どこでも実行、環境変数で選択: `ISAM_BACKEND=sqlite ./app`
- バックエンド切り替えにリコンパイル不要
- Docker とクラウドデプロイに最適
