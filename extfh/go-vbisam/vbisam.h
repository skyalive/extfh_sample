#pragma once

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct GoVbisamHandle GoVbisamHandle;

enum {
    GV_OK = 0,
    GV_ERR_DUPLICATE = 1,
    GV_ERR_NOT_FOUND = 2,
    GV_ERR_END_OF_FILE = 3,
    GV_ERR_LOCKED = 4,
    GV_ERR_IO = 5,
    GV_ERR_NOT_SUPPORTED = 6,
    GV_ERR_INVALID_ARG = 7,
};

enum {
    GV_MODE_INPUT = 0,
    GV_MODE_OUTPUT = 1,
    GV_MODE_IO = 2,
    GV_MODE_EXTEND = 3,
};

enum {
    GV_READ_FIRST = 0,
    GV_READ_NEXT = 1,
    GV_READ_PREV = 2,
    GV_READ_LAST = 3,
    GV_READ_EQUAL = 4,
    GV_READ_GREAT = 5,
    GV_READ_GTEQ = 6,
};

int gvbisam_open(const char *path, int mode, size_t key_offset, size_t key_size, GoVbisamHandle **out_handle);
int gvbisam_create(const char *path, int mode, size_t record_size, size_t key_offset, size_t key_size, GoVbisamHandle **out_handle);
int gvbisam_close(GoVbisamHandle *handle);
size_t gvbisam_record_size(GoVbisamHandle *handle);
size_t gvbisam_key_offset(GoVbisamHandle *handle);
size_t gvbisam_key_size(GoVbisamHandle *handle);
int gvbisam_read(GoVbisamHandle *handle, void *buffer, size_t buffer_len, int mode, size_t *out_len);
int gvbisam_read_key(GoVbisamHandle *handle, const void *key, size_t key_len, void *buffer, size_t buffer_len, size_t *out_len);
int gvbisam_write(GoVbisamHandle *handle, const void *buffer, size_t buffer_len);
int gvbisam_rewrite(GoVbisamHandle *handle, const void *buffer, size_t buffer_len);
int gvbisam_delete(GoVbisamHandle *handle);

#ifdef __cplusplus
}
#endif
