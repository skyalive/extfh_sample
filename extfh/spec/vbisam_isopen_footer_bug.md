# VBISAM Issue Report: keydesc footer check fails on unsigned char platforms

## Summary
`isopen.c` compares the keydesc footer byte against `-1`. On platforms where `char` is unsigned, this comparison fails because `0xff` is not equal to `-1`. As a result, `isopen()` returns `EBADFILE` and INDEXED OPEN fails right after `isbuild()`.

## Environment
- VBISAM source: `extfh/lib/vbisam-osscons-patch-main`
- Affected function: `libvbisam/isopen.c`

## Steps to Reproduce
1. Call `isbuild()` to create a new INDEXED file.
2. `isbuild()` calls `isopen()` internally.
3. `isopen()` fails with `EBADFILE` on unsigned-char platforms.

## Expected Behavior
`isopen()` should succeed and allow subsequent INDEXED I/O.

## Actual Behavior
`isopen()` returns `EBADFILE` due to a keydesc footer mismatch check.

## Root Cause
The footer validation compares a byte against `-1`, which is not portable when `char` is unsigned:

```c
if (*(cvbnodetmp + inodesize - 3) != -1
    || *(cvbnodetmp + inodesize - 2) != 0x7e) {
    /* EBADFILE */
}
```

## Proposed Fix
Compare as unsigned bytes or against `0xff` explicitly:

```c
if ((unsigned char)*(cvbnodetmp + inodesize - 3) != 0xff
    || (unsigned char)*(cvbnodetmp + inodesize - 2) != 0x7e) {
    /* EBADFILE */
}
```

## Impact
INDEXED file OPEN fails on platforms where `char` is unsigned (common on aarch64). A compiler flag workaround exists (`-fsigned-char`), but source-level fix is more portable.

## Evidence (local repro)
- Failing log (before fix): `VBISAM isopen failed: keydesc node footer mismatch errno=105`
- Successful log (after fix): `VBISAM isopen 'testfile' cvalidation=[0x56,0x42] ...` followed by successful READ/WRITE in COBOL sample.

## Patch Location
- File: `extfh/lib/vbisam-osscons-patch-main/libvbisam/isopen.c`
- Change: footer compare using `unsigned char` and `0xff` literal.

## Patch Files
- `extfh/spec/vbisam_isopen_footer_fix.patch` (local tree)
- `extfh/spec/vbisam_isopen_footer_fix_vbisam.patch` (vbisam repo path)
