#!/usr/bin/env sh
set -eu

IMAGE_NAME=${IMAGE_NAME:-ziglibs-ubuntu:25.10}
ZIG_OUT_DIR=${ZIG_OUT_DIR:-.podman/zig-out-linux}
ZIG_CACHE_DIR=${ZIG_CACHE_DIR:-.podman/zig-cache-linux}

mkdir -p "${ZIG_OUT_DIR}" "${ZIG_CACHE_DIR}"

podman run --rm \
  -v "$(pwd)":/work \
  -v "$(pwd)/${ZIG_OUT_DIR}":/work/extfh/zig-out \
  -v "$(pwd)/${ZIG_CACHE_DIR}":/work/extfh/zig-cache \
  -w /work/extfh \
  "${IMAGE_NAME}" \
  bash -lc "zig build && cd test/cobol && ./run_sample.sh"
