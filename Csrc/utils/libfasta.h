// ----------------------------------------------------------------------- 
// $Id: libfasta.h 396 2019-01-02 22:53:10Z viari $
// ----------------------------------------------------------------------- 
//
//

#ifndef _H_LibFasta

#define _H_LibFasta

#include "common.h"

// ----------------------------------------
// Fasta header / sequence structures
//
typedef struct {
    String   name;      // fasta name
    String   comment;   // fasta comment
    size_t   size;      // sequence size
    size_t   offset;    // fasta file offset
    uint32_t crc32;     // sequence crc32
    String   seq;       // fasta sequence (optional)
} FastaSequence;

// --------------------
// libfasta.c
//

FastaSequence *fastaNew(void);
FastaSequence *fastaFree(FastaSequence *fasta);

size_t fastaClean(FastaSequence *fasta);

int fastaRead(FastaSequence *fasta, File *file);
int fastaFetch(FastaSequence *fasta, File *file);

int fastaWrite(FastaSequence *fasta, unsigned char_per_line, File *file);

#endif
