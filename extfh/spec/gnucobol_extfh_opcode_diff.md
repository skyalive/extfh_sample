# GnuCOBOL EXTFH opcode comparison (current vs next + extfh.zig)

This note compares the official opcode definitions from the **current release**
(`repo/gnucobol-osscons-patch/libcob/common.h`) and the **next release**
(`repo/gnucobol-3.3-dev/gnucobol-3.3-dev/libcob/common.h`) and highlights
differences with the local implementation (`extfh/src/extfh.zig`).

## Sources

- Current release: `repo/gnucobol-osscons-patch/libcob/common.h:2473`
- Next release: `repo/gnucobol-3.3-dev/gnucobol-3.3-dev/libcob/common.h:2514`
- Local implementation: `extfh/src/extfh.zig:41`

## Official opcode definitions (shared by current + next)

### Core/utility

- `0x0006` OP_GETINFO
- `0x0007` OP_CRE8_INDEX
- `0x000C` OP_FLUSH
- `0x000F` OP_UNLOCK_REC

### OPEN/CLOSE

- `0xFA00` OP_OPEN_INPUT
- `0xFA01` OP_OPEN_OUTPUT
- `0xFA02` OP_OPEN_IO
- `0xFA03` OP_OPEN_EXTEND
- `0xFA04` OP_OPEN_INPUT_NOREWIND
- `0xFA05` OP_OPEN_OUTPUT_NOREWIND
- `0xFA08` OP_OPEN_INPUT_REVERSED
- `0xFA80` OP_CLOSE
- `0xFA81` OP_CLOSE_LOCK
- `0xFA82` OP_CLOSE_NO_REWIND
- `0xFA84` OP_CLOSE_REEL
- `0xFA85` OP_CLOSE_REMOVE
- `0xFA86` OP_CLOSE_NOREWIND

### READ

- `0xFA8D` OP_READ_SEQ_NO_LOCK
- `0xFAD8` OP_READ_SEQ_LOCK
- `0xFAD9` OP_READ_SEQ_KEPT_LOCK
- `0xFAF5` OP_READ_SEQ
- `0xFA8C` OP_READ_PREV_NO_LOCK
- `0xFADE` OP_READ_PREV_LOCK
- `0xFADF` OP_READ_PREV_KEPT_LOCK
- `0xFAF9` OP_READ_PREV
- `0xFA8E` OP_READ_RAN_NO_LOCK
- `0xFADA` OP_READ_RAN_LOCK
- `0xFADB` OP_READ_RAN_KEPT_LOCK
- `0xFAF6` OP_READ_RAN
- `0xFA8F` OP_READ_DIR_NO_LOCK
- `0xFAD6` OP_READ_DIR_LOCK
- `0xFAD7` OP_READ_DIR_KEPT_LOCK
- `0xFAC9` OP_READ_DIR
- `0xFAF1` OP_READ_POSITION

### WRITE/REWRITE

- `0xFAE1` OP_WRITE_BEFORE
- `0xFAE3` OP_WRITE_BEFORE_TAB
- `0xFAE5` OP_WRITE_BEFORE_PAGE
- `0xFAE2` OP_WRITE_AFTER
- `0xFAE4` OP_WRITE_AFTER_TAB
- `0xFAE6` OP_WRITE_AFTER_PAGE
- `0xFAF3` OP_WRITE
- `0xFAF4` OP_REWRITE

### START

- `0xFAE8` OP_START_EQ
- `0xFAE9` OP_START_EQ_ANY
- `0xFAEA` OP_START_GT
- `0xFAEB` OP_START_GE
- `0xFAFE` OP_START_LT
- `0xFAFF` OP_START_LE
- `0xFAEC` OP_START_LA (LAST: not in MF standard)
- `0xFAED` OP_START_FI (FIRST: not in MF standard)

### STEP

- `0xFA90` OP_STEP_NEXT_NO_LOCK
- `0xFAD4` OP_STEP_NEXT_LOCK
- `0xFAD5` OP_STEP_NEXT_KEPT_LOCK
- `0xFACA` OP_STEP_NEXT
- `0xFA92` OP_STEP_FIRST_NO_LOCK
- `0xFAD0` OP_STEP_FIRST_LOCK
- `0xFAD1` OP_STEP_FIRST_KEPT_LOCK
- `0xFACC` OP_STEP_FIRST

### DELETE/LOCK/TRANSACTION

- `0xFAF7` OP_DELETE
- `0xFAF8` OP_DELETE_FILE
- `0xFA0E` OP_UNLOCK
- `0xFADC` OP_COMMIT
- `0xFADD` OP_ROLLBACK

## Local implementation coverage (extfh.zig)

Defined in `extfh/src/extfh.zig:41`:

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

## Differences between official definitions and extfh.zig

**Missing official opcodes in local enum**
- OPEN variants: `OP_OPEN_INPUT_NOREWIND`, `OP_OPEN_OUTPUT_NOREWIND`, `OP_OPEN_INPUT_REVERSED`
- CLOSE variants: `OP_CLOSE_LOCK`, `OP_CLOSE_NO_REWIND`, `OP_CLOSE_REEL`, `OP_CLOSE_REMOVE`, `OP_CLOSE_NOREWIND`
- READ lock variants: `OP_READ_*_NO_LOCK`, `OP_READ_*_LOCK`, `OP_READ_*_KEPT_LOCK`
- START variants: `OP_START_EQ_ANY`, `OP_START_LT`, `OP_START_LE`, `OP_START_LA`, `OP_START_FI`
- STEP operations: all `OP_STEP_*`
- Utility/transaction: `OP_GETINFO`, `OP_CRE8_INDEX`, `OP_FLUSH`, `OP_DELETE_FILE`, `OP_COMMIT`, `OP_ROLLBACK`

## Notes

- Both official trees (current + next) define the same opcode values for these entries.
- The local enum appears to target a minimal subset; gaps above represent either missing support
  or intentionally unimplemented operations.
- If/when aligning for strict compatibility, at minimum fix the `OP_DELETE` value and decide whether
  `OP_UNLOCK` vs `OP_UNLOCK_REC` should be handled (or both).

## Resolution Plan (draft)

- Done: aligned `OP_DELETE` to the official value (`0xFAF7`).
- Done: handle both `OP_UNLOCK` (`0xFA0E`) and `OP_UNLOCK_REC` (`0x000F`) as unlock operations.
- Deferred: additional opcode support (OPEN/CLOSE variants, lock variants, STEP, GETINFO, FLUSH, DELETE_FILE, COMMIT/ROLLBACK)
  until a concrete use case or COBOL test requires them.
