#!/bin/bash
set -e

# Path to Zig output directory
ZIG_OUT="../../zig-out/lib"

# Check if libextfh.dylib (or .so) exists
if [ -f "$ZIG_OUT/libextfh.dylib" ]; then
    LIB_EXTFH="$ZIG_OUT/libextfh.dylib"
elif [ -f "$ZIG_OUT/libextfh.so" ]; then
    LIB_EXTFH="$ZIG_OUT/libextfh.so"
else
    echo "Error: libextfh not found in $ZIG_OUT. Run 'zig build' first."
    exit 1
fi

echo "Using library: $LIB_EXTFH"

# Compile COBOL program
# -x: Build executable
# -fcallfh=czippfh: Use custom file handler entry point
# -L... -lextfh: Link against our Zig library
# We might need to set DYLD_LIBRARY_PATH (MacOS) or LD_LIBRARY_PATH (Linux)

echo "Compiling sample.cob..."
cobc -x -fcallfh=czippfh src/sample.cob -L"$ZIG_OUT" -lextfh -o sample_app

echo "Running sample_app..."
export DYLD_LIBRARY_PATH="$ZIG_OUT:$DYLD_LIBRARY_PATH"
export LD_LIBRARY_PATH="$ZIG_OUT:$LD_LIBRARY_PATH"

./sample_app

echo "--- Directory Content ---"
ls -la
echo "--- seqfile.txt Content ---"
if [ -f seqfile.txt ]; then
    cat seqfile.txt
else
    echo "seqfile.txt NOT FOUND"
fi
echo "--- testfile.isam Content ---"
if [ -f testfile.isam ]; then
    ls -l testfile.isam
else
    echo "testfile.isam NOT FOUND"
fi

echo "Done."
