# EXTFH Backend Implementation Specification Template

**Purpose**: Provide AI/developers with essential requirements to implement a custom ISAM backend  
**Version**: 1.0  
**Status**: Template - Complete before implementation  

---

## Executive Summary

This document outlines the **minimum requirements** for implementing a new EXTFH backend for any database system. Use this template to specify your chosen database and how it maps to VSAM/ISAM operations.

---

## Part 1: Backend Selection & Requirements

### 1.1 Target Database System

**Name**: _______________________  
**Type**: `[Relational | Key-Value | ISAM-Native | Object | Other]`  
**Language/Bindings**: _______________________  
**Version**: _______________________  

**Key Characteristics**:
- [ ] Supports indexed sequential access (primary requirement)
- [ ] Supports multiple key formats (secondary requirement)
- [ ] Supports transactions (optional but recommended)
- [ ] Thread-safe operations (required for multi-user)
- [ ] In-memory or file-based storage (specify)

**Example: BerkeleyDB**
```
Name: Berkeley DB (libdb)
Type: Key-Value / Embedded ISAM
Language: C library with Zig bindings
Version: 4.8+
Key Characteristics:
  - B-Tree indexing (VSAM equivalent: KSDS)
  - Range queries with cursor iteration
  - Full ACID transactions
  - Thread-safe with explicit locking
  - File-based, mmap support
```

### 1.2 Core Capabilities Matrix

**Complete the following table** (mark ✓/✗ or NA):

| EXTFH Operation | Requirement | Your DB Support | Notes |
|-----------------|-------------|-----------------|-------|
| OPEN | Required | ✓/✗/NA | Open/create database |
| CLOSE | Required | ✓/✗/NA | Close handle gracefully |
| CREATE | Required | ✓/✗/NA | Create new file with key specs |
| READ (sequential) | Required | ✓/✗/NA | Iterate via key order |
| READ (by key) | Required | ✓/✗/NA | Direct lookup by key value |
| WRITE | Required | ✓/✗/NA | Insert new record |
| REWRITE | Required | ✓/✗/NA | Update current record in-place |
| DELETE | Required | ✓/✗/NA | Delete current record |
| START (position) | Required | ✓/✗/NA | Seek to key or range |
| LOCK | Optional | ✓/✗/NA | Explicit locking |
| UNLOCK | Optional | ✓/✗/NA | Release locks |
| COMMIT | Optional | ✓/✗/NA | Transaction boundary |
| ABORT | Optional | ✓/✗/NA | Rollback transaction |

**Example: Berkeley DB**
```
OPEN: ✓ (db_open / DB->open)
CLOSE: ✓ (db_close)
CREATE: ✓ (isbuild equivalent → DB->put with empty db)
READ (sequential): ✓ (cursor iteration DBT→get_next)
READ (by key): ✓ (DB->get with key DBT)
WRITE: ✓ (DB->put)
REWRITE: ✓ (DB->put with overwrite)
DELETE: ✓ (DB->del)
START: ✓ (cursor → set to key range)
LOCK: ✓ (explicit DB_LOCK_*  flags)
UNLOCK: ✓ (lock_put)
COMMIT: ✓ (txn->commit)
ABORT: ✓ (txn->abort)
```

---

## Part 2: Data Model Mapping

### 2.1 VSAM Concepts vs. Your Database

**VSAM KSDS (Key-Sequenced Data Set)** is the primary COBOL file organization supported by EXTFH.

| VSAM Concept | VSAM Behavior | Your DB Equivalent | Implementation Notes |
|--------------|---------------|--------------------|----------------------|
| **File** | Sequential file with indexed keys | Database / Table / B-Tree | How your DB represents a file |
| **Record** | Fixed/variable-length data | Row / Document / Entry | How your DB stores individual records |
| **Primary Key** | Unique, non-null, auto-ordered | Primary Index / B-Tree Key | How your DB ensures uniqueness and ordering |
| **Key Position** | Byte offset in record | Field selector / Key extraction | How to identify the key within record data |
| **Record Size** | Fixed or variable length | Row schema / Buffer size | Fixed (most common) or variable? |
| **Duplicate Keys** | Allowed/Disallowed flag | UNIQUE constraint | Enforced at insert time |
| **Sequential Access** | Ordered by key | Cursor iteration on B-Tree | Iteration order guarantees |
| **Current Record** | Implicit position marker | Active cursor | Implicit for REWRITE/DELETE |

**Example: BerkeleyDB (B-Tree)**
```
File           → Berkeley DB database (libdb_btree)
Record         → Key-Value pair (DBT structures)
Primary Key    → B-Tree internal node keys
Key Position   → Extraction logic: record[start:start+size]
Record Size    → Fixed: sizeof(DBT) or variable in DBT.data
Duplicate Keys → Enforced via DB_DUPSORT flag = not allowed
Sequential Access → Cursor iteration: db->cursor(), dbc->get(DB_NEXT)
Current Record → Active cursor position (implicit)
```

### 2.2 Record Storage Format

**Specify how records are stored in your DB:**

```
Record Structure:
┌─────────────────┬──────────────────────┬─────────────┐
│  Key Field      │  Payload Data        │  Metadata   │
│  (key_size)     │  (record_size - key) │  (optional) │
└─────────────────┴──────────────────────┴─────────────┘

Key Extraction:
  offset: key_offset (e.g., 0 for COBOL PIC 9(8) at field start)
  length: key_size   (e.g., 4 for 32-bit numeric key)
  format: [BINARY | ASCII | BCD | CUSTOM]

Example: COBOL SALES-FILE
  RECORD KEY IS SALES-KEY (8 bytes, position 0)
  RECORD SIZE: 50 bytes
  
  Storage: [8-byte-key][42-byte-payload] = 50 bytes total
  Your DB: DB key = record[0:8], DB value = record[0:50]
```

### 2.3 Key Ordering Guarantee

**Question**: Does your database guarantee key ordering in sequential reads?

```
[ ] Yes - Database returns records in key-sorted order (B-Tree)
    Example: BerkeleyDB with cursor.get(DB_NEXT)
    
[ ] No - Database has no guaranteed order
    Example: Hash table, unordered map
    → SOLUTION: Maintain external sorted index or load-and-sort

[ ] Partial - Ordering only for specific key types
    Example: Some DBs only sort numeric keys
    → SPECIFY: Which key types are supported
```

**Declaration for your DB**:
```
BerkeleyDB: [✓] Yes - B-Tree maintains key sort order
SQLite: [✓] Yes - B-Tree maintains key sort order  
DynamoDB: [✗] No - Hash-based, use GSI (Global Secondary Index)
```

---

## Part 3: API Mapping

### 3.1 Database Native API to EXTFH Operations

**Map each EXTFH operation to your DB's actual API calls:**

#### Template for Each Operation

```zig
// EXTFH Operation: [OPERATION NAME]
// EXTFH Code: [call_id value]
// Zig Signature: [fn signature from isam_interface.zig]

pub fn [operation_name](self: *Backend, ...) IsamError![ReturnType] {
    // Step 1: Validate inputs
    // Step 2: Call native DB API
    // Step 3: Map DB errors to IsamError
    // Step 4: Return result or error
}

Native DB API:
  Function: [db_api_call]
  Parameters: [param1, param2, ...]
  Return Type: [return_type]
  Error Values: [err1 → IsamError.X, err2 → IsamError.Y]

Example Call:
  db.operation(config) → { handle: int, error: code }
```

#### Example Mappings

**BerkeleyDB Example**:

```
OPERATION: OPEN (call_id = 1)
───────────────────────────────────────
Zig Signature:
  pub fn open(self: *Backend, filename: []const u8, mode: OpenMode) !IsamFileHandle

Native DB API:
  DB* db;
  int ret = db_create(&db, NULL, 0);  // Create DB handle
  if (ret == 0) {
    ret = db->open(db, NULL, filename, NULL, DB_BTREE, flags, 0644);
  }

Error Mapping:
  ENOMEM → IsamError.IoError
  ENOENT → IsamError.NotFound (if opening non-existent file in INPUT mode)
  EACCES → IsamError.IoError
  0      → Success

File Handle Storage:
  struct BdbHandle {
    db: *c.DB,
    cursor: *c.DBC,
    filename: []const u8,
    mode: BdbOpenMode,
  }

───────────────────────────────────────

OPERATION: READ (call_id = 3)
───────────────────────────────────────
Zig Signature:
  pub fn read(self: *Backend, handle: IsamFileHandle, buffer: []u8, mode: ReadMode) !void

Native DB API:
  DBT key = { 0 }, data = { 0 };
  data.data = buffer.ptr;
  data.ulen = buffer.len;
  data.flags = DB_DBT_USERMEM;
  
  int ret;
  if (mode == ReadMode.FIRST)
    ret = cursor->get(cursor, &key, &data, DB_FIRST);
  else if (mode == ReadMode.NEXT)
    ret = cursor->get(cursor, &key, &data, DB_NEXT);
  // ... etc

Error Mapping:
  DB_NOTFOUND → IsamError.NotFound
  EINVAL      → IsamError.IoError
  0           → Success

───────────────────────────────────────

OPERATION: WRITE (call_id = 4)
───────────────────────────────────────
Zig Signature:
  pub fn write(self: *Backend, handle: IsamFileHandle, buffer: []const u8) !void

Native DB API:
  DBT key = { 0 }, data = { 0 };
  key.data = buffer.ptr;           // Extract key from buffer
  key.size = handle.key_size;
  data.data = buffer.ptr;
  data.size = buffer.len;
  
  int ret = db->put(db, txn, &key, &data, DB_NOOVERWRITE);
  // DB_NOOVERWRITE ensures duplicate key error

Error Mapping:
  DB_KEYEXIST → IsamError.Duplicate
  ENOMEM      → IsamError.IoError
  0           → Success

───────────────────────────────────────

OPERATION: REWRITE (call_id = 5)
───────────────────────────────────────
Zig Signature:
  pub fn rewrite(self: *Backend, handle: IsamFileHandle, buffer: []const u8) !void

Native DB API:
  DBT key = { 0 }, data = { 0 };
  key.data = buffer.ptr;
  key.size = handle.key_size;
  data.data = buffer.ptr;
  data.size = buffer.len;
  
  int ret = db->put(db, txn, &key, &data, 0);
  // No flags = allow overwrite of existing key

───────────────────────────────────────

OPERATION: DELETE (call_id = 6)
───────────────────────────────────────
Zig Signature:
  pub fn delete(self: *Backend, handle: IsamFileHandle) !void

Native DB API:
  // Delete at cursor position
  int ret = cursor->del(cursor, 0);

OR

  // Delete by key
  DBT key = { 0 };
  key.data = current_key.ptr;
  key.size = current_key.len;
  int ret = db->del(db, txn, &key, 0);

───────────────────────────────────────

OPERATION: START (call_id = 7)
───────────────────────────────────────
Zig Signature:
  pub fn start(self: *Backend, handle: IsamFileHandle, key: []const u8, mode: ReadMode) !void

Native DB API:
  DBT search_key = { 0 };
  search_key.data = key.ptr;
  search_key.size = key.len;
  
  int ret;
  if (mode == ReadMode.EQUAL)
    ret = cursor->get(cursor, &search_key, &data, DB_SET);
  else if (mode == ReadMode.GREATER_EQUAL)
    ret = cursor->get(cursor, &search_key, &data, DB_SET_RANGE);
  // ... etc

Error Mapping:
  DB_NOTFOUND → IsamError.NotFound
  EINVAL      → IsamError.IoError
  0           → Success (cursor now positioned)
```

### 3.2 Error Code Translation Table

**Create a comprehensive error mapping:**

```
Your DB Error          Condition                  → IsamError Equivalent
──────────────────────────────────────────────────────────────────────
DB_NOTFOUND            Record/key not found       → NotFound
DB_KEYEXIST            Insert with duplicate key  → Duplicate
DB_RUNRECOVERY         Database corruption        → IoError
DB_LOCK_NOTGRANTED     Lock acquisition failed    → Locked
ENOMEM                 Out of memory              → IoError
EINVAL                 Invalid parameters        → IoError
EIO                    Hardware I/O error         → IoError
EACCES                 Permission denied          → IoError
ENOENT                 File not found             → NotFound (if INPUT mode)
[Your DB specific]     ...                        → IoError (default fallback)
```

---

## Part 4: Type Conversions & Constants

### 4.1 Mode Enumerations

**Map COBOL/EXTFH modes to your DB equivalents:**

```zig
// COBOL OPEN modes (FCD3.file_open_mode)
pub const CobolOpenMode = enum {
    INPUT = 0,    // Read-only
    OUTPUT = 1,   // Write-only (create new)
    IO = 2,       // Read-write (existing file)
    EXTEND = 3,   // Append mode
};

// Your Database equivalents
pub const BdbOpenMode = enum {
    READONLY = DB_RDONLY,
    READWRITE = 0,  // Default
    CREATE = DB_CREATE,
    TRUNCATE = DB_TRUNCATE,
};

pub fn mapOpenMode(cobol_mode: CobolOpenMode) BdbOpenMode {
    return switch (cobol_mode) {
        .INPUT => .READONLY,
        .OUTPUT => .CREATE | .TRUNCATE,
        .IO => .READWRITE,
        .EXTEND => .READWRITE,
    };
}
```

```zig
// COBOL READ modes (FCD3.option)
pub const CobolReadMode = enum {
    FIRST = 0,          // Position at start
    NEXT = 1,           // Move to next
    PREVIOUS = 2,       // Move to previous
    LAST = 3,           // Position at end
    EQUAL = 4,          // Read specific key
    GREATER_EQUAL = 5,  // >= key value
    GREATER = 6,        // > key value
};

// Your DB equivalents
pub const BdbReadMode = enum {
    DB_FIRST = DB_FIRST,
    DB_NEXT = DB_NEXT,
    DB_PREV = DB_PREV,
    DB_LAST = DB_LAST,
    DB_SET = DB_SET,           // Exact match
    DB_SET_RANGE = DB_SET_RANGE, // >= match
};

pub fn mapReadMode(cobol_mode: CobolReadMode) BdbReadMode {
    return switch (cobol_mode) {
        .FIRST => .DB_FIRST,
        .NEXT => .DB_NEXT,
        .PREVIOUS => .DB_PREV,
        .LAST => .DB_LAST,
        .EQUAL => .DB_SET,
        .GREATER_EQUAL => .DB_SET_RANGE,
        .GREATER => .DB_SET_RANGE, // Then iterate to >
    };
}
```

### 4.2 Lock Modes

```zig
pub const CobolLockMode = enum {
    NONE = 0,      // No lock
    SHARED = 1,    // Read lock (allow concurrent reads)
    EXCLUSIVE = 2, // Write lock (exclusive access)
};

// Your DB equivalent
pub fn mapLockMode(cobol_mode: CobolLockMode) BdbLockMode {
    return switch (cobol_mode) {
        .NONE => .DB_LOCK_NG,
        .SHARED => .DB_LOCK_READ,
        .EXCLUSIVE => .DB_LOCK_WRITE,
    };
}
```

---

## Part 5: Concurrency & Transactions

### 5.1 Transaction Support

**Declare if your DB supports transactions:**

```
[ ] Full ACID transactions     → DB can commit/abort atomically
[ ] Partial transactions       → DB supports some transaction features
[ ] No transactions            → Single operation atomicity only
[ ] Application-level only     → Must implement in backend wrapper

Your DB: [✓] Full ACID transactions (BerkeleyDB with DB_INIT_TXN)
```

### 5.2 Locking Mechanism

**Describe how your DB handles concurrent access:**

```
Concurrency Model:
  [ ] Optimistic (no locks, detect conflicts on commit)
  [ ] Pessimistic (explicit locks before operations)
  [ ] Mixed (both supported)
  [ ] Single-threaded only

Your DB: [✓] Pessimistic - explicit DB_LOCK_* flags required

Lock Types Supported:
  [ ] Shared read locks (multiple readers)
  [ ] Exclusive write locks (single writer)
  [ ] Row-level locks
  [ ] Page-level locks
  [ ] Table-level locks
  [ ] Automatic lock escalation

Your DB: [✓] Shared, Exclusive, Row-level (via B-Tree node locks)
```

### 5.3 Current Record Position

**Explain how "current record" is maintained for REWRITE/DELETE:**

```
Question: After READ, is current record position implicit or explicit?

[ ] Implicit cursor position maintained by DB
    → DB auto-remembers last read position
    → REWRITE/DELETE can use that position
    Example: ISAM, BerkeleyDB cursor

[ ] Explicit cursor return value
    → Must store cursor handle after READ
    → Use same cursor for REWRITE/DELETE
    
[ ] Application must track key
    → READ returns key value
    → REWRITE/DELETE must re-specify key
    
Your DB: [✓] Implicit cursor position (BerkeleyDB cursor maintained in FCD3)
```

---

## Part 6: Implementation Constraints & Gotchas

### 6.1 Memory Management

```
Question: How does your DB allocate/deallocate record buffers?

[ ] Returns pointer to internal buffer
    → Caller must copy data before next operation
    → Buffer is invalidated after cursor move
    
[ ] Caller allocates, DB populates (USERMEM pattern)
    → Caller manages buffer lifetime
    → DB guarantees data validity until buffer reuse
    
[ ] Dynamic allocation (malloc)
    → DB returns allocated memory
    → Caller must free (memory leak risk)

Your DB: [✓] USERMEM pattern (caller allocates DBT buffer)
          ✗ Not: DB allocates DBT buffer
          
Implication:
  EXTFH must maintain buffer through operation sequence
  Issue: If buffer freed before REWRITE → data loss
  Solution: Keep record copy in file handle context
```

### 6.2 Null/Zero Handling

```
Question: How does your DB handle special values?

[ ] Null bytes in keys
    → Key is null-terminated string: Key\0
    → Binary keys may contain 0x00 bytes
    
[ ] Zero-length keys
    → Some DBs reject empty keys
    
[ ] Binary vs. ASCII keys
    → Difference in comparison order
    
[ ] Key modification safety
    → After START with key K, is K invalidated?

Your DB: [✓] Binary keys (arbitrary bytes, no null terminator)
         ✓ Zero-length keys allowed (but unusual)
         ✗ Not: ASCII-only keys
         ✓ Key comparison is byte-wise (0x00 valid)
```

### 6.3 File Naming & Paths

```
Question: How does your DB interpret filenames?

[ ] Single file per database
    → Filename = single physical file
    → All records in one file
    
[ ] Multiple files (index + data)
    → Filename = base name
    → Creates .idx, .dat, etc. internally
    
[ ] Directory-based
    → Filename = directory containing DB files
    
[ ] Network path support
    → Can filename be "user@host:database"?

Your DB: [✓] Single file (BerkeleyDB with DB_BTREE)
         ✓ Optional secondary indexes as separate files
         ✗ Not: Directory-based
         ✗ Not: Network paths (local only)

Implication for EXTFH:
  FCD3.filename must be valid for db_create()
  EXTEND directories as needed
  Handle relative vs. absolute paths
```

### 6.4 Record Size Limits

```
Question: What are the record size constraints?

Your DB:
  Minimum record size: [1 byte]
  Maximum record size: [BerkeleyDB: 4GB via DBT.size]
  Typical optimal size: [8 bytes - 64 KB]
  
VSAM KSDS typical: 1 - 4000 bytes

Implication:
  [ ] No changes needed - VSAM fits within DB limits
  [✓] Yes - VSAM records fit comfortably
  [ ] Possible - May hit limits with large records
      → Need to document max record size in error handling
```

---

## Part 7: Example: SQLite Backend Specification

### Complete Example

```
──────────────────────────────────────────────────────────────
BACKEND: SQLite3
──────────────────────────────────────────────────────────────

1. DATABASE SELECTION
   Name: SQLite 3
   Type: Relational DBMS
   Language: C library (libsqlite3)
   Version: 3.40+
   
   Capabilities: [✓ ACID] [✓ Indexes] [✓ Transactions] [✗ Multi-DB]

2. VSAM MAPPING
   File      → SQLite table: vsam_records
   Record    → Table row: (id INTEGER PRIMARY KEY, data BLOB)
   Primary Key → Table PRIMARY KEY column
   Sequential  → ORDER BY (id) in cursor
   
   Schema: CREATE TABLE vsam_records (
             id BLOB PRIMARY KEY,
             data BLOB NOT NULL
           )

3. API MAPPING
   OPEN     → sqlite3_open()
   READ     → SELECT * FROM vsam_records WHERE id = ? ORDER BY id
   WRITE    → INSERT INTO vsam_records (id, data) VALUES (?, ?)
   REWRITE  → UPDATE vsam_records SET data = ? WHERE id = ?
   DELETE   → DELETE FROM vsam_records WHERE id = ?
   START    → Prepare SELECT with WHERE id >= ?; sqlite3_step()
   
4. ERROR MAPPING
   SQLITE_CONSTRAINT_UNIQUE → IsamError.Duplicate
   SQLITE_NOTFOUND          → IsamError.NotFound (implicit via query result)
   SQLITE_CANTOPEN          → IsamError.IoError
   SQLITE_READONLY          → IsamError.Locked
   SQLITE_IOERR             → IsamError.IoError

5. NOTES
   - SQLite uses implicit cursors in prepared statements
   - Binary keys supported via BLOB type
   - ACID transactions via BEGIN/COMMIT/ROLLBACK
   - Single-file database (thread-safe with WAL mode)
```

---

## Part 8: Checklist for AI Implementation

Before asking AI to implement the backend, verify you have provided:

### Documentation Completeness

- [ ] **Backend Selection**: Name, type, version specified
- [ ] **Capabilities Matrix**: All EXTFH operations marked ✓/✗
- [ ] **Data Model**: VSAM concepts mapped to DB concepts
- [ ] **API Mapping**: Each EXTFH operation mapped to native DB APIs with examples
- [ ] **Error Codes**: Complete error translation table
- [ ] **Mode Conversions**: OpenMode, ReadMode, LockMode mappings with code examples
- [ ] **Concurrency**: Transaction support and locking mechanism documented
- [ ] **Edge Cases**: Memory management, null handling, path handling, size limits
- [ ] **Code Template**: At least one complete operation implementation shown

### Example for BerkeleyDB Specification

```zig
// Complete: Each operation has:
// 1. EXTFH Signature ✓
// 2. Native API calls ✓
// 3. Error mapping ✓
// 4. Working code example ✓

pub fn write(self: *BdbBackend, handle: IsamFileHandle, buffer: []const u8) IsamError!void {
    const bdb_handle: *c.DB = @ptrFromInt(@intCast(handle.handle));
    
    var key = std.mem.zeroes(c.DBT);
    var data = std.mem.zeroes(c.DBT);
    
    key.data = @constCast(buffer.ptr);
    key.size = @intCast(handle.key_size);
    data.data = @constCast(buffer.ptr);
    data.size = @intCast(buffer.len);
    
    const ret = bdb_handle.put(bdb_handle, null, &key, &data, c.DB_NOOVERWRITE);
    if (ret != 0) {
        return self.mapBdbError(ret);
    }
}
```

### Minimal Viable Specification

If time is limited, provide AT MINIMUM:

1. ✅ Database name and version
2. ✅ Capabilities matrix (✓/✗ for each EXTFH operation)
3. ✅ Data model mapping table (VSAM → Your DB)
4. ✅ Native API function names for: OPEN, READ, WRITE, DELETE
5. ✅ Error code mapping (at least 5 common errors)
6. ✅ One complete working code example (e.g., WRITE operation)

```
Minimal BerkeleyDB Spec (1 page):
  
Database: Berkeley DB 4.8+
Capabilities: [✓OPEN][✓READ][✓WRITE][✓DELETE][✓START][✓LOCK]

VSAM File    → BerkeleyDB (B-Tree)
VSAM Record  → (key, value) pair in DBT struct
Primary Key  → record[0:key_size]

Operations:
  OPEN    → db_create() + db->open()
  READ    → cursor->get() with DB_NEXT
  WRITE   → db->put() with DB_NOOVERWRITE flag
  DELETE  → cursor->del()
  START   → cursor->get() with DB_SET_RANGE

Error Mapping:
  DB_NOTFOUND  → NotFound
  DB_KEYEXIST  → Duplicate
  -1           → IoError (generic)

Example: WRITE
  db->put(db, NULL, &key_dbt, &data_dbt, DB_NOOVERWRITE);
  if (ret == DB_KEYEXIST) return IsamError.Duplicate;
```

---

## Part 9: Template for AI Prompt

Use this text to ask AI to implement your backend:

---

### AI Implementation Request Template

```
I need you to implement an EXTFH backend for [DATABASE NAME].

Please follow the specification below and create the file isam_[name].zig

SPECIFICATION:
──────────────
Database: [Name, version]
Type: [Key-Value / Relational / Other]

VSAM Mapping:
  File       → [Your DB representation]
  Record     → [How records are stored]
  Key        → [Key extraction from record]
  Ordering   → [Is sequential order guaranteed?]

Capabilities (mark ✓ or ✗):
  [ ] OPEN      → Native API: [function name] | Error: [map to IsamError]
  [ ] READ      → Native API: [function name] | Error: [map to IsamError]
  [ ] WRITE     → Native API: [function name] | Error: [map to IsamError]
  [ ] DELETE    → Native API: [function name] | Error: [map to IsamError]
  [ ] START     → Native API: [function name] | Error: [map to IsamError]
  [ ] REWRITE   → Native API: [function name] | Error: [map to IsamError]

Error Code Mapping:
  [DB Error]   → [IsamError equivalent]
  ...

Mode Conversions:
  OpenMode.INPUT    → [Your DB mode]
  ReadMode.FIRST    → [Your DB constant]
  ...

Concurrency:
  [ ] Transactions supported? [Yes/No]
  [ ] Lock mechanism: [Type]

Complete this function skeleton:

pub const [DbName]Backend = struct {
    allocator: std.mem.Allocator,
    
    pub fn open(self: *Backend, filename: []const u8, mode: OpenMode) !IsamFileHandle {
        // TEMPLATE PROVIDED:
        // [code example from this spec]
    }
    
    // ... (other operations)
};

Use the isam_interface.zig template for all function signatures.
```

---

## Summary

| Component | Purpose | When to Provide |
|-----------|---------|-----------------|
| **Part 1** | Backend selection & capabilities | Before implementation |
| **Part 2** | VSAM to DB mapping | Before implementation |
| **Part 3** | API mapping with examples | Essential - most important |
| **Part 4** | Type conversions | Before implementation |
| **Part 5** | Concurrency model | If transactions needed |
| **Part 6** | Edge cases & gotchas | During implementation |
| **Part 7** | Complete example | Reference only |
| **Part 8** | AI implementation checklist | When asking for code |

**Key Principle**: The more detailed your specification in Parts 1-4, the better AI can generate correct backend code without requiring iterations.
