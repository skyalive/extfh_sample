# Go ↔ VBISAM PoC (EXTFH Wrapper)

**Status**: PoC (working)
**Date**: 2025-12-24
**Purpose**: Verify that Go can read/write VBISAM via a thin C ABI wrapper around EXTFH.

## Scope
- Backend: VBISAM only
- File format: COBOL INDEXED (`.idx` + `.dat`)
- Operations: open/create, read (seq/key), write, rewrite, delete, close
- Key type: fixed-length ASCII (`PIC X(10)`), offset 0
- Record length: 100 bytes

## Architecture
1) `extfh/src/go_vbisam_api.zig` exports a small C ABI for VBISAM.
2) Go uses `cgo` to call the ABI (`extfh/go-vbisam/vbisam.h`).
3) VBISAM is linked through EXTFH with `-Dbackend=vbisam`.

## Public C ABI (PoC)
- `gvbisam_open(path, mode, key_offset, key_size, &handle)`
- `gvbisam_create(path, mode, record_size, key_offset, key_size, &handle)`
- `gvbisam_close(handle)`
- `gvbisam_read(handle, buf, len, mode, &out_len)`
- `gvbisam_read_key(handle, key, key_len, buf, len, &out_len)`
- `gvbisam_write(handle, buf, len)`
- `gvbisam_rewrite(handle, buf, len)`
- `gvbisam_delete(handle)`

Error codes are mapped to a small enum (`GV_ERR_*`).

## Go PoC
Location: `extfh/go-vbisam/`

- `go run .` writes and reads 5 records (basic smoke test)
- `go run . write-text <input.txt> <base_path>`
- `go run . read-text <base_path> <output.txt>`

The text helper reads/writes fixed 100-byte records (newline-separated lines).

## Build/Run
macOS:
```
cd /Users/uemonmac/dev/__claude/__ziglibs/extfh
zig build -Dbackend=vbisam
cd /Users/uemonmac/dev/__claude/__ziglibs/extfh/go-vbisam
export DYLD_LIBRARY_PATH=../zig-out/lib
go run .
```

Linux (Podman):
```
cd /Users/uemonmac/dev/__claude/__ziglibs
podman run --rm -t -v "$PWD":/work -w /work ziglibs-ubuntu:25.10 bash -lc '
  set -eu
  apt-get update
  apt-get install -y --no-install-recommends golang-go
  cd /work/extfh
  zig build -Dbackend=vbisam
  cd /work/extfh/go-vbisam
  export LD_LIBRARY_PATH=../zig-out/lib
  go run .
'
```

## Demo Integration
`extfh/demo/run_demo.sh` includes:
- `text -> Go -> VBISAM -> Go -> text`
- Output match check (`go_match=true`)

## Known Constraints
- Fixed record size (100 bytes) and key size (10 bytes) in PoC.
- Key comparison is byte-based; no numeric conversion.
- Linux build links `libcob` to resolve `cob_set_exception`.
