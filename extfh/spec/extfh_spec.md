# EXTFH (Extended File Handler) Specification

## Overview

EXTFH is a Zig-based implementation of the Extended File Handler interface for GnuCOBOL. It allows COBOL programs to interact with various file backends through a unified interface.

The current implementation supports:
- **VSAM KSDS (Keyed Sequenced Data Set)**: Implemented via the VBISAM library.
- **SQLite-backed INDEXED files**: Implemented via `isam_sqlite.zig` (COBOL4J SQLite format).
- **Line Sequential**: Implemented via standard filesystem I/O.

## Architecture

### Build-time backend selection

`zig build -Dbackend=<name>` selects the INDEXED backend at build time.

Available options:
- `vbisam` (default)
- `sqlite`
- `both` (build both backends and select at runtime by extension)
- `none` (sequential only)

The system consists of the following components:

1.  **EXTFH Entry Point (`extfh.zig`)**: Exports the `czippfh` function, which matches the GnuCOBOL `EXTFH` signature. It handles the `FCD3` (File Control Descriptor) structure.
2.  **ISAM Interface (`isam_interface.zig`)**: A backend-agnostic abstraction layer for ISAM (Indexed Sequential Access Method) operations.
3.  **Backends**:
    -   **VBISAM (`isam_vbisam.zig`, `vbisam.zig`)**: Wraps the `libvbisam` C library.
    -   **SQLite (`isam_sqlite.zig`)**: COBOL4J 互換の SQLite backend。
4.  **Sequential Handler**: A built-in handler in `extfh.zig` for non-indexed files.

### Runtime backend selection (when built with `both`)

When built with `-Dbackend=both`, EXTFH selects the indexed backend based on the resolved filename.

Filename resolution order:
1) `DD_<name>` environment variable
2) `dd_<name>` environment variable
3) `<name>` environment variable
4) the original ASSIGN name

Backend selection priority (after resolution):
- **SQLite**: `.db`, `.sqlite`, `.sqlite3`
- **VBISAM**: `.isam`, `.idx`, `.ksds`, `.vsam`
- If only one backend is built, it is used as a fallback.
- If both backends are built and the extension does not match either group, OPEN fails with status `5`.

## Interface

### `czippfh`

The main entry point follows the GnuCOBOL EXTFH signature:

```c
int czippfh(unsigned char *opcode, FCD3 *fcd);
```

`opcode` is a 2-byte operation code passed by the COBOL runtime. The handler dispatches based on
`opcode` and uses `FCD3` for parameters and status.

### FCD3 Structure

The File Control Descriptor (FCD3) is used to pass parameters between GnuCOBOL and EXTFH.

This project targets the official GnuCOBOL FCD3 layout (see `extfh/include/gnucobol_common.h`).
Only the subset required for current operations is read.

| Field | Type | Description |
|---|---|---|
| `fileStatus` | `u8[2]` | COBOL file status digits |
| `openMode` | `u8` | 0=INPUT, 1=OUTPUT, 2=I-O, 3=EXTEND |
| `fnameLen` | `u8[2]` | Filename length (COMP-X) |
| `_fnamePtr` | pointer | Filename buffer |
| `curRecLen` | `u8[4]` | Current record length (COMP-X) |
| `maxRecLen` | `u8[4]` | Max record length (COMP-X) |
| `_recPtr` | pointer | Record buffer |
| `_kdbPtr` | pointer | Key definition block (KDB) |
| `_fileHandle` | pointer | Back-end handle storage |

## Supported Operations

Operations are selected by the 16-bit `opcode` provided by GnuCOBOL.

### Opcode Reference (official)

Defined in both current (`repo/gnucobol-osscons-patch/libcob/common.h:2473`) and next
(`repo/gnucobol-3.3-dev/gnucobol-3.3-dev/libcob/common.h:2514`) releases.

**Subset used by this project (official values):**
- `0xFA00` OP_OPEN_INPUT
- `0xFA01` OP_OPEN_OUTPUT
- `0xFA02` OP_OPEN_IO
- `0xFA03` OP_OPEN_EXTEND
- `0xFA80` OP_CLOSE
- `0xFAF5` OP_READ_SEQ
- `0xFAF9` OP_READ_PREV
- `0xFAF6` OP_READ_RAN
- `0xFAC9` OP_READ_DIR
- `0xFAF1` OP_READ_POSITION
- `0xFAF3` OP_WRITE
- `0xFAF4` OP_REWRITE
- `0xFAF7` OP_DELETE
- `0xFAE8` OP_START_EQ
- `0xFAEB` OP_START_GE
- `0xFAEA` OP_START_GT
- `0xFA0E` OP_UNLOCK
- `0x000F` OP_UNLOCK_REC

### Unsupported Opcode Handling

- If `opcode` is not recognized, EXTFH returns status `9` (Unknown Operation) and leaves the file state unchanged.
- If `opcode` is recognized but the operation is not supported for the current file type (e.g., REWRITE on SEQUENTIAL),
  EXTFH returns status `5` (I/O error / unsupported).

### OPEN (`OP_OPEN_*`)
Opens a file. Detects file type based on extension or creates new one.
- **INDEXED**: `.isam`, `.idx`, `.ksds`, `.vsam`, `.db`, `.sqlite`, `.sqlite3`
- **SEQUENTIAL**: All others (default)
- **INDEXED filename normalization**: if the requested name ends in `.isam`, EXTFH strips the extension
  and uses the base name for VBISAM (`{base}.dat` / `{base}.idx`).
- **OUTPUT/EXTEND (INDEXED)**: the existing `{base}.dat` / `{base}.idx` files are removed before create.

### CLOSE (`OP_CLOSE`)
Closes the file and releases resources.

### READ (`OP_READ_*`)
Reads a record.
- **INDEXED**: Supports RANDOM (by key), SEQUENTIAL (NEXT/PREV/etc).
- **SEQUENTIAL**: Supports sequential read only.

### WRITE (`OP_WRITE`)
Writes a new record.

### REWRITE (`OP_REWRITE`)
Updates an existing record. Supported for INDEXED only.

### DELETE (`OP_DELETE`)
Deletes a record. Supported for INDEXED only.

### START (`OP_START_*`)
Positions the cursor for sequential reading. Supported for INDEXED only.

### UNLOCK (`OP_UNLOCK`/`OP_UNLOCK_REC`)
Releases locks. Both file unlock (`OP_UNLOCK`) and record unlock (`OP_UNLOCK_REC`) are mapped to unlock.

## Error Handling

Errors are returned via the `status` field in FCD3.

- `0`: Success
- `1`: Not Found
- `2`: Locked
- `3`: Duplicate Key
- `4`: End of File / No Record
- `5`: I/O Error / General Error
- `9`: Unknown Operation

Note (GnuCOBOL EOF handling):
- When EOF occurs (status `10`), GnuCOBOL retains an exception code internally.
- EXTFH clears the exception on successful CLOSE to prevent the runtime from
  invoking the default error handler after EOF.

## Related Specs

- `extfh/spec/gnucobol_extfh_opcode_diff.md` (official opcode list and delta vs local implementation)
- `extfh/spec/vbisam_isopen_footer_bug.md` (VBISAM unsigned-char footer issue report)

## Compilation & Usage

To use with GnuCOBOL:

```bash
cobc -fcallfh=czippfh -c program.cob
# Link with libextfh
```
