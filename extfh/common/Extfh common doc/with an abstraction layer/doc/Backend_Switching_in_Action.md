# Backend Switching in Action - How Abstraction Really Works

**Core Principle**: With abstraction, only the **backend initialization** changes. Everything else stays the same.

---

## The Magic: extfh.zig is 100% Unchanged

### Scenario 1: VBISAM Backend

```zig
// main.zig - Production with VBISAM
const extfh = @import("extfh.zig");
const isam_vbisam = @import("isam_vbisam.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // Only change: Initialize VBISAM backend
    const backend = isam.IsamBackend{
        .VBISAM = isam_vbisam.VbisamBackend.init(allocator),
    };
    
    extfh.init(allocator, backend);
    defer extfh.deinit();
    
    // EXTFH handler is called by GnuCOBOL
    // ... COBOL program runs ...
}
```

### Scenario 2: SQLite Backend (Same extfh.zig!)

```zig
// main.zig - Development with SQLite
const extfh = @import("extfh.zig");
const isam_sqlite = @import("isam_sqlite.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // Only change: Initialize SQLite backend
    const backend = isam.IsamBackend{
        .SQLITE = isam_sqlite.SqliteBackend.init(allocator),
    };
    
    extfh.init(allocator, backend);
    defer extfh.deinit();
    
    // EXTFH handler is EXACTLY the same
    // ... COBOL program runs ...
}
```

### Scenario 3: Mock Backend for Testing

```zig
// test.zig - Testing with Mock
const extfh = @import("extfh.zig");
const isam_mock = @import("isam_mock.zig");

test "EXTFH read/write with mock" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // Only change: Initialize Mock backend
    const backend = isam.IsamBackend{
        .MOCK = isam_mock.MockBackend.init(allocator),
    };
    
    extfh.init(allocator, backend);
    defer extfh.deinit();
    
    // EXTFH handler is EXACTLY the same
    // Test code runs against in-memory mock
    var fcd: FCD3 = undefined;
    fcd.call_id = 1; // OPEN
    @memcpy(fcd.filename[0..8], "test.dat");
    fcd.filename[8] = 0;
    
    extfh.czippfh(@ptrCast(&fcd.call_id));
    
    try std.testing.expect(fcd.status == 0);
}
```

---

## The Proof: extfh.zig Looks Identical in All Scenarios

```zig
// extfh.zig - EXACTLY the same for VBISAM, SQLite, Mock
const std = @import("std");
const isam = @import("isam_interface.zig");

var allocator: std.mem.Allocator = undefined;
var backend: isam.IsamBackend = undefined;
var file_map: std.AutoHashMap(c_int, isam.IsamFileHandle) = undefined;

pub fn init(allocator_in: std.mem.Allocator, backend_in: isam.IsamBackend) void {
    allocator = allocator_in;
    backend = backend_in;  // ← Just store which backend
    file_map = std.AutoHashMap(c_int, isam.IsamFileHandle).init(allocator);
}

pub export fn czippfh(fcd_ptr: [*c]c_int) callconv(.C) void {
    const fcd: *FCD3 = @ptrCast(@alignCast(fcd_ptr));
    
    switch (fcd.call_id) {
        1 => handleOpen(fcd),
        2 => handleClose(fcd),
        3 => handleRead(fcd),
        4 => handleWrite(fcd),
        // ... etc
        else => fcd.status = 9,
    }
}

// Handlers use 'backend' - works the same for ANY backend!
fn handleOpen(fcd: *FCD3) void {
    const filename = std.mem.sliceTo(&fcd.filename, 0);
    const mode = mapCobolMode(fcd.file_open_mode);
    
    // This call works for VBISAM, SQLite, or Mock!
    const handle = backend.open(filename, mode) catch |err| {
        fcd.status = mapErrorToStatus(err);
        return;
    };
    
    file_map.put(fcd.handle, handle) catch { /* ... */ };
    fcd.status = 0;
}

fn handleRead(fcd: *FCD3) void {
    const handle = file_map.get(fcd.handle) orelse {
        fcd.status = 5;
        return;
    };
    
    const mode = mapCobolReadMode(fcd.option);
    const buffer = fcd.record_ptr[0..@intCast(fcd.record_size)];
    
    // This call works for VBISAM, SQLite, or Mock!
    backend.read(handle, buffer, mode) catch |err| {
        fcd.status = mapErrorToStatus(err);
        return;
    };
    
    fcd.status = 0;
}

fn handleWrite(fcd: *FCD3) void {
    const handle = file_map.get(fcd.handle) orelse {
        fcd.status = 5;
        return;
    };
    
    const buffer = fcd.record_ptr[0..@intCast(fcd.record_size)];
    
    // This call works for VBISAM, SQLite, or Mock!
    backend.write(handle, buffer) catch |err| {
        fcd.status = mapErrorToStatus(err);
        return;
    };
    
    fcd.status = 0;
}

// ... ALL other handlers identical ...
```

---

## Real Example: Build Config for Backend Selection

### build.zig - Compile-time Backend Selection

```zig
// build.zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    
    // Add build option for backend selection
    const backend_option = b.option(
        []const u8,
        "backend",
        "ISAM backend: vbisam, sqlite, mock (default: vbisam)"
    ) orelse "vbisam";
    
    const exe = b.addExecutable(.{
        .name = "cobol_app",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    
    // Different main.zig based on backend
    const main_source = switch (std.mem.eql(u8, backend_option, "sqlite")) {
        true => "src/main_sqlite.zig",      // SQLite version
        false => if (std.mem.eql(u8, backend_option, "mock"))
            "src/main_mock.zig"             // Mock version
        else
            "src/main_vbisam.zig",          // VBISAM version (default)
    };
    
    // Link appropriate backend libraries
    switch (std.mem.eql(u8, backend_option, "sqlite")) {
        true => exe.linkSystemLibrary("sqlite3"),
        false => exe.linkSystemLibrary("bdb"),  // VBISAM
    }
    
    b.installArtifact(exe);
}
```

### Build Command Usage

```bash
# Production: VBISAM backend
zig build -Dbackend=vbisam

# Development: SQLite backend
zig build -Dbackend=sqlite

# Testing: Mock backend
zig build -Dbackend=mock
```

---

## Practical Example: Environment-based Selection

### Dynamic Backend Selection at Runtime

```zig
// main.zig - Smart initialization
const std = @import("std");
const extfh = @import("extfh.zig");
const isam = @import("isam_interface.zig");
const isam_vbisam = @import("isam_vbisam.zig");
const isam_sqlite = @import("isam_sqlite.zig");
const isam_mock = @import("isam_mock.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // Read environment variable to choose backend
    const backend_name = std.os.getenv("ISAM_BACKEND") orelse "vbisam";
    
    // Initialize appropriate backend
    const backend: isam.IsamBackend = 
        if (std.mem.eql(u8, backend_name, "sqlite"))
            .{ .SQLITE = isam_sqlite.SqliteBackend.init(allocator) }
        else if (std.mem.eql(u8, backend_name, "mock"))
            .{ .MOCK = isam_mock.MockBackend.init(allocator) }
        else
            .{ .VBISAM = isam_vbisam.VbisamBackend.init(allocator) };
    
    extfh.init(allocator, backend);
    defer extfh.deinit();
    
    std.debug.print("Using backend: {s}\n", .{backend_name});
    
    // EXTFH handler works with any backend!
    // ... rest of program ...
}
```

### Runtime Backend Switching Example

```bash
# Production environment
export ISAM_BACKEND=vbisam
./cobol_app

# Development environment  
export ISAM_BACKEND=sqlite
./cobol_app

# Testing environment
export ISAM_BACKEND=mock
./cobol_app
```

---

## Comparison: Direct vs. Abstracted

### Without Abstraction (Direct VBISAM)

```
Code change needed to switch backends:
┌────────────────────────────────────────┐
│ extfh.zig (400 lines)                  │
│                                        │
│  All VBISAM calls hardcoded:           │
│  ├─ vbisam.isopen()                    │
│  ├─ vbisam.isread()                    │
│  ├─ vbisam.iswrite()                   │
│  └─ ...                                │
│                                        │
│  To switch to SQLite:                  │
│  ➜ REWRITE entire extfh.zig (400 lines)│
│  ➜ Change all calls to sqlite3_*()     │
│  ➜ Test everything again               │
│  ➜ Risk of subtle bugs                 │
└────────────────────────────────────────┘
```

### With Abstraction (Pluggable Backends)

```
Only initialization changes:
┌────────────────────────────────────────┐
│ main.zig (25 lines)                    │
│                                        │
│  const backend = isam.IsamBackend{     │
│    .VBISAM = ...  // Swap this line    │
│  };               // for .SQLITE = ... │
│                                        │
│  No other changes needed!              │
│                                        │
│ extfh.zig (400 lines)                  │
│  [UNCHANGED - uses abstract backend]   │
└────────────────────────────────────────┘
```

---

## Real-World Scenario: Multi-Environment Deployment

### Scenario: Bank's COBOL System

```
Requirements:
  - Production (Tokyo): VBISAM (existing VSAM compat)
  - Staging (Singapore): SQLite (lightweight)
  - Development (local): Mock (instant, no disk I/O)
  - Testing (CI/CD): Mock (parallel test execution)
```

### With Abstraction - Deploy Same Binary

```
build.zig produces ONE executable: cobol_app

Deploy to environments with different configs:

┌─ Production Server (Tokyo)
│  $ ISAM_BACKEND=vbisam ./cobol_app
│  → Uses VBISAM, legacy VSAM-compatible ✓
│
├─ Staging Server (Singapore)
│  $ ISAM_BACKEND=sqlite ./cobol_app
│  → Uses SQLite, lightweight ✓
│
├─ Developer Laptop
│  $ ISAM_BACKEND=mock ./cobol_app
│  → Uses Mock, instant startup ✓
│
└─ CI/CD Pipeline
   $ ISAM_BACKEND=mock zig build test
   → Uses Mock, 100x faster tests ✓
```

**Same binary. Same extfh.zig. Different backends per environment.**

### Without Abstraction - Build Multiple Binaries

```
Must compile separate binaries for each environment:

cobol_app_production  (VBISAM backend)
cobol_app_staging     (SQLite backend)
cobol_app_dev         (Mock backend)
cobol_app_test        (Mock backend)

Risk:
  ✗ Binary divergence (subtle bugs per version)
  ✗ Deployment complexity (which binary for which env?)
  ✗ Testing gap (prod uses VBISAM, test uses something else)
  ✗ Maintenance nightmare (4+ versions of extfh.zig)
```

---

## Code Size Comparison

### Switching Cost: Direct vs. Abstracted

#### Without Abstraction

```
To switch backends:
  
  1. Copy extfh.zig to extfh_sqlite.zig
  2. Replace all VBISAM calls:
     - vbisam.isopen() → sqlite3_open()
     - vbisam.isread() → sqlite3_prepare() + sqlite3_step()
     - vbisam.iswrite() → sqlite3_exec(INSERT...)
     - ... 20+ replacements
  3. Handle SQLite-specific errors
  4. Test entire workflow
  5. Keep both versions in source control
  6. Document which version to use where
  7. Risk: Divergent bug fixes (fix in one, forget in other)
  
  Effort: 1-2 weeks per new backend
  Files: 5+ versions of extfh.zig (production, staging, dev, test)
  Risk: High
```

#### With Abstraction

```
To switch backends:

  1. Create isam_sqlite.zig (implement standard interface)
  2. Modify main.zig (3-line change):
     .{ .VBISAM = ... }  →  .{ .SQLITE = ... }
  3. Test with new backend
  4. Done!
  
  Effort: 1 day (just implement backend, extfh.zig reused)
  Files: 1 extfh.zig (reused for all backends)
  Risk: Low (extfh.zig tested once, reused everywhere)
```

---

## Visual: Data Flow with Abstraction

### VBISAM Backend

```
COBOL Program
    ↓ (FCD3 struct: OPEN, filename="sales.dat")
czippfh()
    ↓
handleOpen(fcd)
    ↓
backend.open("sales.dat", INPUT)
    ↓ (dispatch to VBISAM impl)
VbisamBackend.open()
    ↓
vbisam.isopen("sales.dat", ISREAD)
    ↓
VBISAM C Library
    ↓
Read "sales.dat" from disk
```

### SQLite Backend (Same czippfh!)

```
COBOL Program
    ↓ (FCD3 struct: OPEN, filename="sales.dat")
czippfh()
    ↓
handleOpen(fcd)  [IDENTICAL CODE]
    ↓
backend.open("sales.dat", INPUT)
    ↓ (dispatch to SQLite impl)
SqliteBackend.open()
    ↓
sqlite3_open(":memory:")
    ↓
SQLite Library
    ↓
Open in-memory database
```

### Mock Backend (Same czippfh!)

```
COBOL Program
    ↓ (FCD3 struct: OPEN, filename="sales.dat")
czippfh()
    ↓
handleOpen(fcd)  [IDENTICAL CODE]
    ↓
backend.open("sales.dat", INPUT)
    ↓ (dispatch to Mock impl)
MockBackend.open()
    ↓
HashTable.put("sales.dat", {})
    ↓
In-memory HashMap
    ↓
Instant operation (no disk I/O)
```

---

## The Real Win: Testing

### Without Abstraction

```cobol
* COBOL test code
OPEN INPUT SALES-FILE.
READ SALES-FILE.
IF FILE-STATUS != "00"
    DISPLAY "ERROR"
    STOP RUN
END-IF.
DISPLAY "Record: " SALES-RECORD.
CLOSE SALES-FILE.

* Problem:
*   - Must have actual VSAM/VBISAM file on disk
*   - Test runs in seconds (disk I/O)
*   - Cannot test error conditions easily
*   - Sequential tests (one after another)
*   - Difficult to run 1000 test variants
```

### With Abstraction

```zig
test "read non-existent record" {
    // Initialize mock backend
    const backend = isam.IsamBackend{
        .MOCK = MockBackend.init(allocator),
    };
    extfh.init(allocator, backend);
    
    // Simulate COBOL READ with non-existent key
    var fcd: FCD3 = undefined;
    fcd.call_id = 3;  // READ
    fcd.handle = 1;
    fcd.option = 4;   // EQUAL
    
    extfh.czippfh(@ptrCast(&fcd.call_id));
    
    // Assert error status
    try std.testing.expectEqual(@as(c_short, 4), fcd.status);
    // ✓ No disk I/O, instant
    // ✓ Easy to simulate errors
    // ✓ Runs in parallel
    // ✓ 1000+ variants in seconds
}
```

---

## Summary: What Gets Swapped?

### Only These Lines Change

```zig
// main_vbisam.zig
const backend = isam.IsamBackend{
    .VBISAM = isam_vbisam.VbisamBackend.init(allocator),
};

// main_sqlite.zig (ONLY CHANGE)
const backend = isam.IsamBackend{
    .SQLITE = isam_sqlite.SqliteBackend.init(allocator),
};

// main_mock.zig (ONLY CHANGE)
const backend = isam.IsamBackend{
    .MOCK = isam_mock.MockBackend.init(allocator),
};
```

### Everything Else Stays the Same

```zig
// extfh.zig - 100% IDENTICAL for all backends
pub export fn czippfh(fcd_ptr: [*c]c_int) callconv(.C) void {
    // Works with VBISAM, SQLite, Mock - NO CHANGES
}

fn handleOpen(fcd: *FCD3) void {
    // Works with VBISAM, SQLite, Mock - NO CHANGES
}

fn handleRead(fcd: *FCD3) void {
    // Works with VBISAM, SQLite, Mock - NO CHANGES
}

// ... ALL other handlers identical ...
```

---

## The Abstraction Advantage: One Last Analogy

```
Direct VBISAM approach:
  "I built a COBOL system that only works with VBISAM"
  ➜ Tightly coupled
  ➜ Hard to change
  ➜ Risky to modify

Abstraction approach:
  "I built a COBOL system that works with ANY ISAM backend"
  ➜ Loosely coupled
  ➜ Easy to swap (3-line change)
  ➜ Safe to modify
  ➜ Same code, different environments
```

---

## Answer to Your Question

> **抽象化レイヤーがあると、BACKEND の差し替えだけで仕組みが入れ替えられる？**

**Yes, exactly.**

| Aspect | Change Required |
|--------|-----------------|
| **extfh.zig** | ✗ No changes |
| **Operation handlers** | ✗ No changes |
| **Mode mapping** | ✗ No changes |
| **Error handling** | ✗ No changes |
| **main.zig backend init** | ✓ 1 line change |
| **Rebuild** | ✓ Required |
| **Regression testing** | ✗ Minimal (extfh unchanged) |

**Result**: Change 1 line → Entire backend swapped → Same COBOL program runs on VBISAM, SQLite, or Mock.
