# GnuCOBOL ↔ COBOL4J Demo

This demo runs end-to-end interoperability on Podman.

Pipeline:
1) Line sequential -> VBISAM indexed (GnuCOBOL + EXTFH, extension .idx)
2) VBISAM indexed -> Line sequential (GnuCOBOL + EXTFH, extension .idx)
3) Line sequential -> SQLite indexed (GnuCOBOL + EXTFH, extension .db)
4) SQLite indexed -> Line sequential (COBOL4J cobj-idx unload)
5) Compare input and output (20 records)

Notes:
- EXTFH is built with `-Dbackend=both` and chooses the backend by file extension.
- The COBOL programs use `ASSIGN TO "IDXFILE"` and the demo sets `DD_IDXFILE`/`IDXFILE`.

Run:
```
./demo/run_demo.sh
```

Quick steps:
1) Build the demo container image (uses `demo/container/Containerfile`)
2) Run the demo pipeline in the container
3) Check the summary line for `match=true`

Equivalent commands:
```
./demo/container/podman-build.sh
./demo/run_demo.sh
```

Artifacts are stored in `demo/work`.
