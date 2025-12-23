#ifndef GNUCOBOL_COMMON_H
#define GNUCOBOL_COMMON_H

/*
 * Extracted from GnuCOBOL 3.2 libcob/common.h
 * Purpose: provide the exact FCD3 layout for EXTFH comparison.
 * This is a minimal subset (FCD3 + supporting pointer macro).
 */

#define MF_MAXKEYS 64

typedef struct {
    unsigned char count[2];
    unsigned char offset[2];
    unsigned char keyFlags;
    unsigned char compFlags;
    unsigned char sparse;
    unsigned char reserved[9];
} KDB_KEY;

typedef struct {
    unsigned char desc;
    unsigned char type;
    unsigned char pos[4];
    unsigned char len[4];
} EXTKEY;

typedef struct {
    unsigned char kdbLen[2];
    char filler[4];
    unsigned char nkeys[2];
    char filler2[6];
    KDB_KEY key[MF_MAXKEYS];
} KDB;

#define pointer_8byte(type, name) \
    union { \
        type *ptr_name; \
        char filler[8]; \
    } name

typedef struct __fcd3 {
    unsigned char fileStatus[2];
    unsigned char fcdLen[2];
    unsigned char fcdVer;
    unsigned char fileOrg;
    unsigned char accessFlags;
    unsigned char openMode;
    unsigned char recordMode;
    unsigned char fileFormat;
    unsigned char deviceFlag;
    unsigned char lockAction;
    unsigned char compType;
    unsigned char blocking;
    unsigned char idxCacheSz;
    unsigned char percent;
    unsigned char blockSize;
    unsigned char flags1;
    unsigned char flags2;
    unsigned char mvsFlags;
    unsigned char fstatusType;
    unsigned char otherFlags;
    unsigned char transLog;
    unsigned char lockTypes;
    unsigned char fsFlags;
    unsigned char confFlags;
    unsigned char miscFlags;
    unsigned char confFlags2;
    unsigned char lockMode;
    unsigned char fsv2Flags;
    unsigned char idxCacheArea;
    unsigned char fcdInternal1;
    unsigned char fcdInternal2;
    char res3[14];
    unsigned char gcFlags;
    unsigned char nlsId[2];
    char fsv2FileId[2];
    char retryOpenCount[2];
    unsigned char fnameLen[2];
    unsigned char idxNameLen[2];
    char retryCount[2];
    unsigned char refKey[2];
    unsigned char lineCount[2];
    unsigned char useFiles;
    unsigned char giveFiles;
    unsigned char effKeyLen[2];
    char res5[14];
    unsigned char eop[2];
    char opt[4];
    unsigned char curRecLen[4];
    unsigned char minRecLen[4];
    unsigned char maxRecLen[4];
    char fsv2SessionId[4];
    char res6[24];
    unsigned char relByteAdrs[8];
    unsigned char maxRelKey[8];
    unsigned char relKey[8];
    pointer_8byte(void, _fileHandle);
    pointer_8byte(unsigned char, _recPtr);
    pointer_8byte(char, _fnamePtr);
    pointer_8byte(char, _idxNamePtr);
    pointer_8byte(KDB, _kdbPtr);
    pointer_8byte(void, _colPtr);
    pointer_8byte(void, _fileDef);
    pointer_8byte(void, _dfSortPtr);
} FCD3;

#endif
