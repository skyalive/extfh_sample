package main

/*
#cgo CFLAGS: -I.
#cgo LDFLAGS: -L../zig-out/lib -lextfh
#include <stdlib.h>
#include "vbisam.h"
*/
import "C"

import (
	"bufio"
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

	if len(os.Args) > 1 {
		switch os.Args[1] {
		case "write-text":
			if len(os.Args) != 4 {
				panic("usage: write-text <input.txt> <base_path>")
			}
			if err := createFromText(os.Args[2], os.Args[3]); err != nil {
				panic(err)
			}
			return
		case "read-text":
			if len(os.Args) != 4 {
				panic("usage: read-text <base_path> <output.txt>")
			}
			if err := readToText(os.Args[2], os.Args[3]); err != nil {
				panic(err)
			}
			return
		default:
			panic("unknown command")
		}
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

func createFromText(inputPath, basePath string) error {
	file, err := os.Open(inputPath)
	if err != nil {
		return err
	}
	defer file.Close()

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

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Bytes()
		rec := buildRecordFromLine(line)
		rc = C.gvbisam_write(handle, unsafe.Pointer(&rec[0]), C.size_t(len(rec)))
		if rc != C.GV_OK {
			return fmt.Errorf("write failed: %d", int(rc))
		}
	}
	if err := scanner.Err(); err != nil {
		return err
	}
	fmt.Println("write-text: ok")
	return nil
}

func readToText(basePath, outputPath string) error {
	pathC := C.CString(basePath)
	defer C.free(unsafe.Pointer(pathC))

	var handle *C.GoVbisamHandle
	rc := C.gvbisam_open(
		pathC,
		C.int(C.GV_MODE_INPUT),
		C.size_t(keyOffset),
		C.size_t(keySize),
		&handle,
	)
	if rc != C.GV_OK {
		return fmt.Errorf("open failed: %d", int(rc))
	}
	defer C.gvbisam_close(handle)

	outFile, err := os.Create(outputPath)
	if err != nil {
		return err
	}
	defer outFile.Close()

	rec := make([]byte, recordSize)
	var outLen C.size_t

	rc = C.gvbisam_read(handle, unsafe.Pointer(&rec[0]), C.size_t(len(rec)), C.int(C.GV_READ_FIRST), &outLen)
	if rc == C.GV_ERR_END_OF_FILE {
		return nil
	}
	if rc != C.GV_OK {
		return fmt.Errorf("read first failed: %d", int(rc))
	}
	if _, err := outFile.Write(append([]byte{}, rec...)); err != nil {
		return err
	}
	if _, err := outFile.Write([]byte("\n")); err != nil {
		return err
	}

	for {
		rc = C.gvbisam_read(handle, unsafe.Pointer(&rec[0]), C.size_t(len(rec)), C.int(C.GV_READ_NEXT), &outLen)
		if rc == C.GV_ERR_END_OF_FILE {
			break
		}
		if rc != C.GV_OK {
			return fmt.Errorf("read next failed: %d", int(rc))
		}
		if _, err := outFile.Write(append([]byte{}, rec...)); err != nil {
			return err
		}
		if _, err := outFile.Write([]byte("\n")); err != nil {
			return err
		}
	}

	fmt.Println("read-text: ok")
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

func buildRecordFromLine(line []byte) []byte {
	rec := make([]byte, recordSize)
	if len(line) >= recordSize {
		copy(rec, line[:recordSize])
		return rec
	}
	copy(rec, line)
	for i := len(line); i < recordSize; i++ {
		rec[i] = ' '
	}
	return rec
}

func formatRecord(rec []byte) string {
	key := strings.TrimSpace(string(rec[0:keySize]))
	data := strings.TrimSpace(string(rec[keySize:]))
	return fmt.Sprintf("key=%s data=%s", key, data)
}
