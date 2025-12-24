# Go ↔ VBISAM PoC (EXTFH wrapper)

This PoC uses the EXTFH VBISAM backend via a small C ABI wrapper exported from `libextfh`.

## Build steps

1) Build EXTFH with VBISAM backend:
```
cd /Users/uemonmac/dev/__claude/__ziglibs/extfh
zig build -Dbackend=vbisam
```

2) Run the Go sample:
```
cd /Users/uemonmac/dev/__claude/__ziglibs/extfh/go-vbisam
export DYLD_LIBRARY_PATH=../zig-out/lib
go run .
```

3) Text -> VBISAM -> Text:
```
cd /Users/uemonmac/dev/__claude/__ziglibs/extfh/go-vbisam
export DYLD_LIBRARY_PATH=../zig-out/lib
go run . write-text ./work/input.txt ./work/poc_vbisam
go run . read-text ./work/poc_vbisam ./work/output.txt
```

## Notes
- Records are fixed length (100 bytes).
- Key is 10-byte ASCII at offset 0 (`PIC X(10)`).
- VBISAM files are created under `extfh/go-vbisam/work/` as `poc_vbisam.idx` and `poc_vbisam.dat`.
- On Linux, set `LD_LIBRARY_PATH` instead of `DYLD_LIBRARY_PATH`.
