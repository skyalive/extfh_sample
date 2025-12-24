# GnuCOBOL ↔ COBOL4J Demo

This demo runs end-to-end interoperability on Podman.

Pipeline:
1) Line sequential -> VBISAM indexed (GnuCOBOL + EXTFH, extension .idx)
2) VBISAM indexed -> Line sequential (GnuCOBOL + EXTFH, extension .idx)
3) Line sequential -> SQLite indexed (GnuCOBOL + EXTFH, extension .db)
4) SQLite indexed -> Line sequential (COBOL4J cobj-idx unload)
5) Line sequential -> VBISAM indexed (Go + EXTFH)
6) VBISAM indexed -> Line sequential (Go + EXTFH)
7) Compare input and output (20 records)

Notes:
- EXTFH is built with `-Dbackend=both` and chooses the backend by file extension.
- The COBOL programs use `ASSIGN TO "IDXFILE"` and the demo sets `DD_IDXFILE`/`IDXFILE`.
- COBOL4J is cloned into `extfh/demo/work/cobol4j` and built on demand.

Run:
```
./extfh/demo/run_demo.sh
```

Quick steps:
1) Build the demo container image (uses `extfh/demo/container/Containerfile`)
2) Run the demo pipeline in the container
3) Check the summary line for `match=true`

Equivalent commands:
```
./extfh/demo/container/podman-build.sh
./extfh/demo/run_demo.sh
```

Artifacts are stored in `extfh/demo/work`.
