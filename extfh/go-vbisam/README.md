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
export LD_LIBRARY_PATH=../zig-out/lib
go run .
```

## Notes
- Records are fixed length (100 bytes).
- Key is 10-byte ASCII at offset 0 (`PIC X(10)`).
- VBISAM files are created under `extfh/go-vbisam/work/` as `poc_vbisam.idx` and `poc_vbisam.dat`.
