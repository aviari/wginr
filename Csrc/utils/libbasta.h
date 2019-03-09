// ----------------------------------------------------------------------- 
// $Id: libbasta.h 396 2019-01-02 22:53:10Z viari $
// ----------------------------------------------------------------------- 
//
//

#ifndef _H_LibBasta

#include "common.h"

// ----------------------------------------
// magic number for binary basta file
//

#define BASTA_MAGIC 0x62617332    // 'bas2'

// ----------------------------------------
// Fasta / Basta header structures
//
typedef struct {
    String name;        // fasta name
    String comment;     // fasta comment <unused>
    uint64_t size;      // sequence size
    uint32_t crc32;     // sequence crc32
    size_t _offset;     // <internal> offset in basta file
} BastaEntry;

typedef struct {
    size_t _capacity;   // <internal> current entries capacity
    size_t nbseq;
    BastaEntry **entry;
} BastaHeader;

// --------------------
// libbasta.c
//

BastaHeader *bastaNewHeader(size_t capacity);
BastaHeader *bastaResizeHeader(BastaHeader *header, size_t capacity);
BastaHeader *bastaFreeHeader(BastaHeader *header);

BastaHeader *bastaParseFasta(File *file, int verbose);

size_t bastaWriteHeader(BastaHeader *header, File *file);


#endif
