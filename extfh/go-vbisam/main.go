package main

/*
#cgo CFLAGS: -I.
#cgo LDFLAGS: -L../zig-out/lib -lextfh
#include <stdlib.h>
#include "vbisam.h"
*/
import "C"

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"unsafe"
)

const (
	recordSize = 100
	keyOffset  = 0
	keySize    = 10
)

func main() {
	workDir := filepath.Join(".", "work")
	if err := os.MkdirAll(workDir, 0o755); err != nil {
		panic(err)
	}

	basePath := filepath.Join(workDir, "poc_vbisam")
	if err := createSample(basePath); err != nil {
		panic(err)
	}

	if err := readSample(basePath); err != nil {
		panic(err)
	}
}

func createSample(basePath string) error {
	pathC := C.CString(basePath)
	defer C.free(unsafe.Pointer(pathC))

	var handle *C.GoVbisamHandle
	rc := C.gvbisam_create(
		pathC,
		C.int(C.GV_MODE_OUTPUT),
		C.size_t(recordSize),
		C.size_t(keyOffset),
		C.size_t(keySize),
		&handle,
	)
	if rc != C.GV_OK {
		return fmt.Errorf("create failed: %d", int(rc))
	}
	defer C.gvbisam_close(handle)

	for i := 1; i <= 5; i++ {
		key := fmt.Sprintf("%010d", i)
		payload := fmt.Sprintf("DATA-%d", i)
		rec := buildRecord(key, payload)
		rc = C.gvbisam_write(handle, unsafe.Pointer(&rec[0]), C.size_t(len(rec)))
		if rc != C.GV_OK {
			return fmt.Errorf("write failed: %d", int(rc))
		}
	}
	fmt.Println("write: ok")
	return nil
}

func readSample(basePath string) error {
	pathC := C.CString(basePath)
	defer C.free(unsafe.Pointer(pathC))

	var handle *C.GoVbisamHandle
	rc := C.gvbisam_open(
		pathC,
		C.int(C.GV_MODE_IO),
		C.size_t(keyOffset),
		C.size_t(keySize),
		&handle,
	)
	if rc != C.GV_OK {
		return fmt.Errorf("open failed: %d", int(rc))
	}
	defer C.gvbisam_close(handle)

	rec := make([]byte, recordSize)
	var outLen C.size_t

	rc = C.gvbisam_read(handle, unsafe.Pointer(&rec[0]), C.size_t(len(rec)), C.int(C.GV_READ_FIRST), &outLen)
	if rc != C.GV_OK {
		return fmt.Errorf("read first failed: %d", int(rc))
	}
	fmt.Println("read first:", formatRecord(rec))

	for {
		rc = C.gvbisam_read(handle, unsafe.Pointer(&rec[0]), C.size_t(len(rec)), C.int(C.GV_READ_NEXT), &outLen)
		if rc == C.GV_ERR_END_OF_FILE {
			break
		}
		if rc != C.GV_OK {
			return fmt.Errorf("read next failed: %d", int(rc))
		}
		fmt.Println("read next:", formatRecord(rec))
	}

	key := []byte("0000000003")
	rc = C.gvbisam_read_key(handle, unsafe.Pointer(&key[0]), C.size_t(len(key)), unsafe.Pointer(&rec[0]), C.size_t(len(rec)), &outLen)
	if rc != C.GV_OK {
		return fmt.Errorf("read key failed: %d", int(rc))
	}
	fmt.Println("read key:", formatRecord(rec))

	return nil
}

func buildRecord(key, payload string) []byte {
	rec := make([]byte, recordSize)
	keyField := fmt.Sprintf("%-10s", key)
	dataField := fmt.Sprintf("%-90s", payload)
	copy(rec[0:keySize], []byte(keyField))
	copy(rec[keySize:], []byte(dataField))
	return rec
}

func formatRecord(rec []byte) string {
	key := strings.TrimSpace(string(rec[0:keySize]))
	data := strings.TrimSpace(string(rec[keySize:]))
	return fmt.Sprintf("key=%s data=%s", key, data)
}
