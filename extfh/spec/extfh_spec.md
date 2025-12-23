# EXTFH (Extended File Handler) Specification

## Overview

EXTFH is a Zig-based implementation of the Extended File Handler interface for GnuCOBOL. It allows COBOL programs to interact with various file backends through a unified interface.

The current implementation supports:
- **VSAM KSDS (Keyed Sequenced Data Set)**: Implemented via the VBISAM library.
- **Line Sequential**: Implemented via standard filesystem I/O.

## Architecture

The system consists of the following components:

1.  **EXTFH Entry Point (`extfh.zig`)**: Exports the `czippfh` function, which matches the GnuCOBOL `EXTFH` signature. It handles the `FCD3` (File Control Descriptor) structure.
2.  **ISAM Interface (`isam_interface.zig`)**: A backend-agnostic abstraction layer for ISAM (Indexed Sequential Access Method) operations.
3.  **Backends**:
    -   **VBISAM (`isam_vbisam.zig`, `vbisam.zig`)**: Wraps the `libvbisam` C library.
    -   **SQLite (`isam_sqlite.zig`)**: (Planned/Stub) SQLite backend.
4.  **Sequential Handler**: A built-in handler in `extfh.zig` for non-indexed files.

## Interface

### `czippfh`

The main entry point is:

```c
void czippfh(unsigned char *fcd_ptr);
```

This function interprets the operation code in the FCD3 structure and dispatches it to the appropriate handler.

### FCD3 Structure

The File Control Descriptor (FCD3) is used to pass parameters between GnuCOBOL and EXTFH.

| Field | Type | Description |
|---|---|---|
| `call_id` | `c_int` | Operation Code (1=OPEN, 2=CLOSE, 3=READ, 4=WRITE, ...) |
| `handle` | `c_int` | File Handle (assigned by EXTFH on OPEN) |
| `status` | `c_short` | Status Code (0=Success, 1=Not Found, etc.) |
| `filename` | `[256]u8` | Filename string |
| `file_open_mode`| `c_short` | 0=INPUT, 1=OUTPUT, 2=I-O, 3=EXTEND |
| `record_varying`| `c_short` | Variable length flag |
| `record_size` | `c_int` | Record size in bytes |
| `record_key_pos`| `c_int` | Key position (for INDEXED) |
| `record_key_size`| `c_int` | Key size (for INDEXED) |
| `record_ptr` | `*c_void` | Pointer to data buffer |
| `key_ptr` | `*c_void` | Pointer to key buffer |
| `option` | `c_short` | Read mode (NEXT, PREV, etc.) |
| `key_number` | `c_short` | Key index (0=Primary) |

## Supported Operations

### OPEN (`call_id = 1`)
Opens a file. Detects file type based on extension or creates new one.
- **INDEXED**: `.isam`, `.idx`, `.ksds`, `.vsam`
- **SEQUENTIAL**: All others (default)

### CLOSE (`call_id = 2`)
Closes the file and releases resources.

### READ (`call_id = 3`)
Reads a record.
- **INDEXED**: Supports RANDOM (by key), SEQUENTIAL (NEXT/PREV/etc).
- **SEQUENTIAL**: Supports sequential read only.

### WRITE (`call_id = 4`)
Writes a new record.

### REWRITE (`call_id = 5`)
Updates an existing record. Supported for INDEXED only.

### DELETE (`call_id = 6`)
Deletes a record. Supported for INDEXED only.

### START (`call_id = 7`)
Positions the cursor for sequential reading. Supported for INDEXED only.

### UNLOCK (`call_id = 10`)
Releases locks.

## Error Handling

Errors are returned via the `status` field in FCD3.

- `0`: Success
- `1`: Not Found
- `2`: Locked
- `3`: Duplicate Key
- `4`: End of File / No Record
- `5`: I/O Error / General Error
- `9`: Unknown Operation

## Compilation & Usage

To use with GnuCOBOL:

```bash
cobc -fcallfh=czippfh -c program.cob
# Link with libextfh
```
