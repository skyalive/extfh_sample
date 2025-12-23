#!/usr/bin/env sh
set -eu

IMAGE_NAME=${IMAGE_NAME:-ziglibs-ubuntu:25.10}
ZIG_DIR=${ZIG_DIR:-repo/zig-aarch64-linux-0.15.2}

podman build \
  --build-arg ZIG_DIR="${ZIG_DIR}" \
  -f container/Containerfile \
  -t "${IMAGE_NAME}" \
  .
