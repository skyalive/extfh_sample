
## GnuCOBOL EXTFH Overview



### What is EXTFH?

EXTFH (Extended File Handler) is a callable interface that allows external programs to intercept and handle COBOL I-O operations. Instead of using GnuCOBOL's built-in file handler, EXTFH routes I-O to a custom handler function.

### Compilation 

To use EXTFH, compile with the `-fcallfh` option:

```bash
cobc -fcallfh=czippfh -c program.cob
```

This tells the compiler to route all I-O operations to the function `czippfh()`.

### File Control Descriptor (FCD3)

The FCD3 is the communication structure between COBOL and EXTFH. It's defined in `xfhfcd3.cpy` and contains:

```cobol
      * FCD3 (File Control Descriptor) structure
      * Used to pass I-O operation details to EXTFH handler
       01 FH-FCD.
           05 FH-FCD-CALL-ID           USAGE BINARY-LONG.      * Operation code
           05 FH-FCD-HANDLE            USAGE BINARY-LONG.      * File handle
           05 FH-FCD-STATUS            USAGE BINARY-SHORT.     * Status code (output)
           05 FH-FCD-FILENAME          PIC X(256).             * File name
           05 FH-FCD-FILE-OPEN-MODE    USAGE BINARY-SHORT.     * OPEN mode
           05 FH-FCD-RECORD-VARYING    USAGE BINARY-SHORT.     * Variable length?
           05 FH-FCD-RECORD-SIZE       USAGE BINARY-LONG.      * Record size
           05 FH-FCD-RECORD-KEY        USAGE BINARY-LONG.      * Key position
           05 FH-FCD-RECORD-KEY-SIZE   USAGE BINARY-LONG.      * Key size
           05 FH-FCD-RECORD-POINTER    USAGE POINTER.          * Record data pointer
           05 FH-FCD-KEY-POINTER       USAGE POINTER.          * Key buffer pointer
           05 FH-FCD-OPTION            USAGE BINARY-SHORT.     * Read mode (FIRST/NEXT/EQ/etc)
           05 FH-FCD-KEY-NUMBER        USAGE BINARY-SHORT.     * Key index (0=primary)
```

### FCD3 Operations (FH-FCD-CALL-ID Values)

| Value | Operation | Description |
|-------|-----------|-------------|
| 1 | OPEN | Open file for I-O |
| 2 | CLOSE | Close file |
| 3 | READ | Read record |
| 4 | WRITE | Write record |
| 5 | REWRITE | Update current record |
| 6 | DELETE | Delete record |
| 7 | START | Position at key |
| 8 | ABORT | Abort transaction |
| 9 | COMMIT | Commit transaction |
| 10 | UNLOCK | Release locks |

### Return Status

The handler sets `FH-FCD-STATUS` to indicate success/failure:

| Value | Meaning |
|-------|---------|
| 0 | Success |
| 1 | File not found |
| 2 | File locked |
| 3 | Duplicate key |
| 4 | Record not found |
| 5 | I-O error |
| 9 | Generic error |
| 30 | Permanent error |

---

## EXTFH Handler Implementation for Zig 0.15.2 (sample code)

### Function Signature 

```zig
/// Extended File Handler for GnuCOBOL
/// Called for all I-O operations when compiled with -fcallfh=czippfh
export fn czippfh(fcd_ptr: [*c]c_int) callconv(.C) void {
    // fcd_ptr points to FCD3 structure in COBOL memory
    // Parse FCD3 fields and dispatch to appropriate VBISAM operation
}
```

### FCD3 Structure in Zig

```zig
pub const FCD3 = struct {
    call_id: c_int,              // CALL-ID (operation code)
    handle: c_int,               // File handle
    status: c_short,             // Status code (return value)
    filename: [256]u8,           // Filename
    file_open_mode: c_short,     // OPEN mode (INPUT/OUTPUT/I-O)
    record_varying: c_short,     // Variable-length records?
    record_size: c_int,          // Record size in bytes
    record_key_pos: c_int,       // Key position in record
    record_key_size: c_int,      // Key size
    record_ptr: [*c]u8,          // Pointer to record data
    key_ptr: [*c]u8,             // Pointer to key buffer
    option: c_short,             // Read mode (FIRST/NEXT/EQ/GTEQ/etc)
    key_number: c_short,         // Key index (0=primary key)
    // Additional fields (reserved/extended)
    reserved: [128]u8,           // Reserved for future use
};
```

### File Handle Management

```zig
/// Global file handle table (maps COBOL handles to VBISAM handles)
var handle_table: std.AutoHashMap(c_int, ExtfhFileContext) = undefined;

pub const ExtfhFileContext = struct {
    vbisam_handle: c_int,           // VBISAM file handle
    filename: []const u8,           // Owned filename
    record_size: usize,             // Record size
    record_varying: bool,           // Variable-length records?
    is_open: bool,                  // Currently open?
    vbisam_mode: vbisam.OpenMode,   // VBISAM open mode
    current_key: c_int,             // Current key index
};
```

---

## Operation Handlers

### OPEN (call_id = 1)

```zig
fn handleOpen(fcd: *FCD3) void {
    // 1. Extract filename (null-terminated in FCD3.filename)
    // 2. Determine VBISAM open mode from FCD3.file_open_mode:
    //    - COBOL INPUT (0) → VBISAM.OpenMode.INPUT
    //    - COBOL OUTPUT (1) → VBISAM.OpenMode.OUTPUT
    //    - COBOL I-O (2) → VBISAM.OpenMode.INOUT
    // 3. Call vbisam.open() or vbisam.build()
    // 4. Store in handle_table
    // 5. Return handle in FCD3.handle
    // 6. Set FCD3.status = 0 on success
}
```

**COBOL Example:**
```cobol
FD SALES-FILE
   ORGANIZATION IS INDEXED
   RECORD KEY IS SALES-KEY
   FILE STATUS IS WS-STATUS.
01 SALES-REC.
   05 SALES-KEY      PIC 9(8).
   05 SALES-AMOUNT   PIC 9(10)V99.

OPEN INPUT SALES-FILE.
* Triggers: czippfh() with CALL-ID=1, mode=INPUT
```

### READ (call_id = 3)

Supports multiple read modes:

```zig
fn handleRead(fcd: *FCD3) void {
    // 1. Get file handle from handle_table
    // 2. Map FCD3.option to VBISAM.ReadMode:
    //    - 0 (FIRST) → VBISAM.ReadMode.FIRST
    //    - 1 (LAST) → VBISAM.ReadMode.LAST
    //    - 2 (NEXT) → VBISAM.ReadMode.NEXT
    //    - 3 (PREV) → VBISAM.ReadMode.PREV
    //    - 4 (CURRENT) → VBISAM.ReadMode.CURR
    //    - 5 (EQUAL) → VBISAM.ReadMode.EQUAL
    //    - 6 (GREATER) → VBISAM.ReadMode.GREAT
    //    - 7 (GREATER-EQUAL) → VBISAM.ReadMode.GTEQ
    // 3. If FCD3.key_ptr is set, use isstart() first
    // 4. Call isread() with record_ptr
    // 5. Copy data to FCD3.record_ptr
    // 6. Set FCD3.status based on result
}
```

**COBOL Example:**
```cobol
PROCEDURE DIVISION.
    OPEN INPUT SALES-FILE.

    READ SALES-FILE
        KEY IS SALES-KEY
        AT END MOVE 1 TO WS-EOF
        NOT AT END DISPLAY SALES-REC
    END-READ.

    CLOSE SALES-FILE.
```

### WRITE (call_id = 4)

```zig
fn handleWrite(fcd: *FCD3) void {
    // 1. Get file handle
    // 2. Copy data from FCD3.record_ptr
    // 3. Call iswrite()
    // 4. On duplicate key: set FCD3.status = 3
    // 5. On success: set FCD3.status = 0
}
```

### REWRITE (call_id = 5)

```zig
fn handleRewrite(fcd: *FCD3) void {
    // 1. Get file handle
    // 2. Copy data from FCD3.record_ptr
    // 3. Call isrewrite() (updates current record)
    // 4. Set status
}
```

### DELETE (call_id = 6)

```zig
fn handleDelete(fcd: *FCD3) void {
    // 1. Get file handle
    // 2. Call isdelcurr() (deletes current record)
    // OR if key provided, call isdelete()
    // 3. Set status
}
```

### CLOSE (call_id = 2)

```zig
fn handleClose(fcd: *FCD3) void {
    // 1. Get file handle
    // 2. Call isclose()
    // 3. Remove from handle_table
    // 4. Set FCD3.status = 0
}
```

### START (call_id = 7)

Position at a key value for sequential reading:

```zig
fn handleStart(fcd: *FCD3) void {
    // 1. Get file handle
    // 2. Use FCD3.key_ptr and FCD3.record_key_size
    // 3. Call isstart() with appropriate ReadMode
    // 4. Sets position for next READ NEXT
    // 5. Set status
}
```

**COBOL Example:**
```cobol
MOVE "SEARCH-KEY" TO SALES-KEY.
READ SALES-FILE
    KEY IS SALES-KEY
    INVALID KEY DISPLAY "NOT FOUND"
    VALID KEY DISPLAY SALES-REC
END-READ.
```

---
