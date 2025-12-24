#!/usr/bin/env sh
set -eu

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
DEMO_DIR="$ROOT_DIR/extfh/demo"
WORK_DIR="$DEMO_DIR/work"
IMAGE_NAME=${IMAGE_NAME:-ziglibs-ubuntu:25.10}

mkdir -p "$WORK_DIR"

"$DEMO_DIR/container/podman-build.sh"

podman run --rm -t \
  -v "$ROOT_DIR":/work \
  -w /work \
  "$IMAGE_NAME" \
  bash -lc '\
    set -eu; \
    DEMO_DIR=/work/extfh/demo; \
    WORK_DIR=/work/extfh/demo/work; \
    export WORK_DIR; \
    export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:-}"; \
    mkdir -p "$WORK_DIR"/{vbisam,sqlite}; \
    rm -f "$WORK_DIR"/vbisam/indexed.idx* "$WORK_DIR"/vbisam/output.txt; \
    rm -f "$WORK_DIR"/sqlite/indexed.db "$WORK_DIR"/sqlite/from_cobol4j.txt "$WORK_DIR"/sqlite/from_cobol4j_lines.txt "$WORK_DIR"/sqlite/input.txt; \
    \
    printf "Generating input data...\n"; \
    : > "$WORK_DIR/vbisam/input.txt"; \
    i=1; \
    while [ "$i" -le 20 ]; do \
      key=$(printf "%010d" "$i"); \
      printf "%-10s%-90s\n" "$key" "DATA-$i" >> "$WORK_DIR/vbisam/input.txt"; \
      i=$((i+1)); \
    done; \
    \
    printf "Building EXTFH (both backends)...\n"; \
    cd /work/extfh; \
    zig build -Dbackend=both; \
    cp zig-out/lib/libextfh.so "$WORK_DIR/vbisam/"; \
    cp zig-out/lib/libextfh.so "$WORK_DIR/sqlite/"; \
    \
    printf "Compiling COBOL programs...\n"; \
    cd /work/extfh/demo; \
    cobc -x -fcallfh=czippfh cobol/ls_to_idx.cob -L"$WORK_DIR/vbisam" -lextfh -o "$WORK_DIR/ls_to_idx"; \
    cobc -x -fcallfh=czippfh cobol/idx_to_ls.cob -L"$WORK_DIR/vbisam" -lextfh -o "$WORK_DIR/idx_to_ls"; \
    \
    printf "Step 1: line sequential -> VBISAM indexed\n"; \
    export LD_LIBRARY_PATH="$WORK_DIR/vbisam:$LD_LIBRARY_PATH"; \
    export DD_IDXFILE="$WORK_DIR/vbisam/indexed.idx"; \
    export IDXFILE="$WORK_DIR/vbisam/indexed.idx"; \
    cd "$WORK_DIR/vbisam"; \
    "$WORK_DIR/ls_to_idx"; \
    \
    printf "Step 2: VBISAM indexed -> line sequential\n"; \
    export DD_IDXFILE="$WORK_DIR/vbisam/indexed.idx"; \
    export IDXFILE="$WORK_DIR/vbisam/indexed.idx"; \
    cd "$WORK_DIR/vbisam"; \
    "$WORK_DIR/idx_to_ls"; \
    cp "$WORK_DIR/vbisam/output.txt" "$WORK_DIR/sqlite/input.txt"; \
    \
    printf "Step 3: line sequential -> SQLite indexed\n"; \
    export LD_LIBRARY_PATH="$WORK_DIR/sqlite:$LD_LIBRARY_PATH"; \
    export DD_IDXFILE="$WORK_DIR/sqlite/indexed.db"; \
    export IDXFILE="$WORK_DIR/sqlite/indexed.db"; \
    cd "$WORK_DIR/sqlite"; \
    "$WORK_DIR/ls_to_idx"; \
    \
    printf "Preparing COBOL4J tools...\n"; \
    COBOL4J_DIR="$WORK_DIR/cobol4j"; \
    COBOL4J_BIN="$COBOL4J_DIR/libcobj/bin/cobj-idx-test"; \
    COBOL4J_JAR="$COBOL4J_DIR/libcobj/app/build/libs/libcobj.jar"; \
    if [ ! -x "$COBOL4J_BIN" ] || [ ! -f "$COBOL4J_JAR" ]; then \
      rm -rf "$COBOL4J_DIR"; \
      git clone --depth 1 https://github.com/opensourcecobol/opensourcecobol4j "$COBOL4J_DIR"; \
      cd "$COBOL4J_DIR/libcobj"; \
      ./gradlew -q shadowJar; \
    fi; \
    \
    printf "Step 4: COBOL4J unload -> line sequential\n"; \
    "$COBOL4J_BIN" unload "$WORK_DIR/sqlite/indexed.db" "$WORK_DIR/sqlite/from_cobol4j.txt"; \
    \
    printf "Step 5: line sequential -> VBISAM indexed (Go)\n"; \
    export LD_LIBRARY_PATH="/work/extfh/zig-out/lib:$LD_LIBRARY_PATH"; \
    GO_BASE="$WORK_DIR/go_vbisam/poc_vbisam"; \
    GO_IN="$WORK_DIR/go_vbisam/input.txt"; \
    GO_OUT="$WORK_DIR/go_vbisam/output.txt"; \
    mkdir -p "$WORK_DIR/go_vbisam"; \
    cp "$WORK_DIR/vbisam/input.txt" "$GO_IN"; \
    cd /work/extfh/go-vbisam; \
    go run . write-text "$GO_IN" "$GO_BASE"; \
    go run . read-text "$GO_BASE" "$GO_OUT"; \
    \
    printf "Verifying Go output...\n"; \
    if cmp -s "$GO_IN" "$GO_OUT"; then \
      go_match=true; \
      go_diff=""; \
    else \
      go_match=false; \
      go_diff=$(diff -u "$GO_IN" "$GO_OUT" | sed -n "1,12p" || true); \
    fi; \
    printf "go_match=%s\n" "$go_match"; \
    if [ "$go_match" != "true" ] && [ -n "$go_diff" ]; then \
      printf "go_diff_head:\\n%s\\n" "$go_diff"; \
    fi; \
    \
    printf "Verifying output...\n"; \
    in_file="$WORK_DIR/vbisam/input.txt"; \
    out_raw="$WORK_DIR/sqlite/from_cobol4j.txt"; \
    out_file="$WORK_DIR/sqlite/from_cobol4j_lines.txt"; \
    fold -w 100 "$out_raw" > "$out_file"; \
    in_count=$(wc -l < "$in_file"); \
    out_count=$(wc -l < "$out_file"); \
    if cmp -s "$in_file" "$out_file"; then \
      match=true; \
      diff_hint=""; \
    else \
      match=false; \
      diff_hint=$(diff -u "$in_file" "$out_file" | sed -n "1,12p" || true); \
    fi; \
    printf "records_in=%s records_out=%s match=%s\n" "$in_count" "$out_count" "$match"; \
    if [ "$match" != "true" ] && [ -n "$diff_hint" ]; then \
      printf "diff_head:\\n%s\\n" "$diff_hint"; \
    fi
  '
