# EXTFH 仕様検討ログ（GnuCOBOL FCD）

## 目的
GnuCOBOL の EXTFH 仕様（FCD3 と opcode）を確認し、Zig 側実装との整合性を検証する。

## 実施概要（結論）
- 公式仕様では、**opcode は FCD 内ではなく別引数**として渡される。
- GnuCOBOL の `FCD3` は巨大な構造体で、**簡略 FCD（call_id/filename[256] など）とはレイアウトが一致しない**。
- Zig 側は `@cImport` を使い、**FCD3 の最小必要フィールドのみ参照**する方向に変更。

## 参照元
- GnuCOBOL 3.2 公式ソース: `/tmp/gnucobol-src/gnucobol-3.2/libcob/common.h`
  - `FCD3` 定義
  - `EXTFH` シグネチャ
  - opcode 定義（`OP_OPEN_INPUT` など）
- GnuCOBOL copybook: `/usr/share/gnucobol/copy/xfhfcd3.cpy`

## 仕様の要点（common.h）
- EXTFH 関数シグネチャ:
  - `int EXTFH(unsigned char *opcode, FCD3 *fcd);`
- opcode 定義（抜粋）:
  - `OP_OPEN_INPUT  0xFA00`
  - `OP_OPEN_OUTPUT 0xFA01`
  - `OP_OPEN_IO     0xFA02`
  - `OP_OPEN_EXTEND 0xFA03`
  - `OP_CLOSE       0xFA80`
  - `OP_READ_SEQ    0xFAF5`
  - `OP_READ_RAN    0xFAF6`
  - `OP_WRITE       0xFAF3`
  - `OP_REWRITE     0xFAF4`
  - `OP_DELETE      0xFAF2`
  - `OP_START_EQ    0xFAE8`
  - `OP_UNLOCK_REC  0x000F`
- FCD3 はポインタ主体:
  - `fnamePtr` / `recPtr` / `fileHandle` などは **ポインタ**
  - `fnameLen` / `curRecLen` / `maxRecLen` などは **COMP-X（ビッグエンディアン）**

## 実施した作業
### 1) Podman で Linux テスト環境を構築
- `extfh/demo/container/Containerfile` を追加
- `extfh/demo/container/podman-build.sh` と `extfh/demo/run_demo.sh` を追加
- Linux での検証は `extfh/demo/run_demo.sh` に統合

### 2) GnuCOBOL 仕様調査
- Podman 内の copybook を確認
  - `/usr/share/gnucobol/copy/xfhfcd3.cpy`
- 公式ソースを取得して仕様確認
  - `curl -L https://ftp.gnu.org/gnu/gnucobol/gnucobol-3.2.tar.xz`
  - `tar -xf /tmp/gnucobol-3.2.tar.xz -C /tmp/gnucobol-src`
  - `common.h` から `FCD3` / `EXTFH` / opcode を確認

### 3) Zig 実装の比較と調整
- `extfh/include/gnucobol_common.h` を追加（common.h 由来）
- `extfh/tools/fcd_compare.zig` を追加
  - 公式 FCD3 と簡略 FCD のサイズ/オフセット比較
  - 実行例:
    - `repo/zig-aarch64-macos-0.15.2/zig run extfh/tools/fcd_compare.zig -I extfh/include`
- `extfh/src/extfh.zig` を更新
  - `czippfh(opcode, fcd_ptr)` へ変更
  - opcode → `call_id`/`option` へマッピング
  - `fnamePtr`/`fnameLen` から filename を組み立て
  - `recPtr`/`maxRecLen` から record 情報を取得
  - `kdbPtr` から **主キーの先頭コンポーネント**だけを取得
  - `fileStatus` を数値ステータスから更新
- `extfh/build.zig` に `include` パス追加

## 再実行手順（簡易）
1) Podman イメージ作成
   - `./extfh/demo/container/podman-build.sh`
2) Linux テスト
   - `./extfh/demo/run_demo.sh`
4) FCD 比較ツール
   - `repo/zig-aarch64-macos-0.15.2/zig run extfh/tools/fcd_compare.zig -I extfh/include`

## 現状の注意点
- **簡略 FCD（call_id/filename[256] 方式）は GnuCOBOL の FCD3 と非整合**。
- 現在の Zig 実装は **最小フィールド参照**の段階。複合キー等は未対応。
