#!/usr/bin/env sh
set -eu

IMAGE_NAME=${IMAGE_NAME:-ziglibs-ubuntu:25.10}

podman run --rm -it \
  -v "$(pwd)":/work \
  -w /work/extfh \
  "${IMAGE_NAME}" \
  bash
