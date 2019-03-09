// ----------------------------------------------------------------------- 
// $Id: libbasta.c 396 2019-01-02 22:53:10Z viari $
// ----------------------------------------------------------------------- 
//
//

#include "libbasta.h"
#include "libfasta.h"

// ----------------------------------------
// free basta entry
//
static BastaEntry *_freeEntry(BastaEntry *entry) {
  if (entry) {
    (void) stringFree(entry->name);
    (void) stringFree(entry->comment);
    Free(entry);
  }
  return NULL;
}

// ----------------------------------------
// init basta entry
//
static BastaEntry *_newEntry(FastaSequence *fasta) {
  BastaEntry *entry = New(BastaEntry);
  if (entry) {
    entry->name = stringNew(fasta->name);
    entry->comment = stringNew(fasta->comment);
    entry->size = fasta->size;
    entry->crc32 = fasta->crc32;
    entry->_offset = 0;
  }
  return   entry && entry->name && entry->comment 
         ? entry
         : _freeEntry(entry);
}

// ----------------------------------------
// API
// ----------------------------------------


// ----------------------------------------
// init basta header
// with initial capacity
//
BastaHeader *bastaNewHeader(size_t capacity) {
  capacity = Max(1, capacity);
  BastaHeader *header = New(BastaHeader);
  if (header) {
    header->_capacity = capacity;
    header->nbseq = 0;
    header->entry = NewN(BastaEntry*, capacity);
    AllocZero(header->entry);
  }
  return (    header && header->entry 
            ? header
            : bastaFreeHeader(header));
}

// ----------------------------------------
// resize basta header
//
BastaHeader *bastaResizeHeader(BastaHeader *header, size_t capacity) {
  capacity = Max(1, capacity);
  if (header) {
    BastaEntry **p = Realloc(header->entry, BastaEntry*, capacity);
    if (p) {
      header->entry = p;
      size_t previous = header->_capacity;
      while (previous < capacity)
        p[previous++] = NULL;
      header->_capacity = capacity; 
    }
  }
  return header;
}

// ----------------------------------------
// free basta header
//
BastaHeader *bastaFreeHeader(BastaHeader *header) {
  if (header) {
    if (header->entry) {
      size_t i;
      for (i = 0 ; i < header->nbseq ; i++) {
        (void) _freeEntry(header->entry[i]);
      }
      Free(header->entry);
    }
    Free(header);
  }
  return NULL;
}

// ----------------------------------------
// parse fasta file to extract basta header
//
BastaHeader *bastaParseFasta(File *file, int verbose) {
  FastaSequence *fasta = fastaNew();
  fasta->seq = stringFree(fasta->seq); // no sequence
  BastaHeader *header = bastaNewHeader(1);
  while (fastaRead(fasta, file)) {
    if (header->_capacity <= header->nbseq)
      header = bastaResizeHeader(header, 2 * header->nbseq);
    if (header->_capacity <= header->nbseq)
      continue;
    if ((header->entry[header->nbseq] = _newEntry(fasta)))
      header->nbseq++;
    if (verbose > 0)
      Notify("reading sequence %s %s\n", fasta->name, fasta->comment);
  }
  (void) fastaFree(fasta);
  return header;
}

// ----------------------------------------
// write header
//
size_t bastaWriteHeader(BastaHeader *header, File *file) {
  if (! header) return 0;
  size_t nout = 0, i;
  nout += binOutInt32(BASTA_MAGIC, file);
  nout += binOutInt32(header->nbseq, file);
  for (i = 0; i < header->nbseq ; i++) {
    nout += binOutString(header->entry[i]->name, file);
//    nout += binOutString(header->entry[i]->comment, file);
    nout += binOutInt64(header->entry[i]->size, file);
    nout += binOutInt32(header->entry[i]->crc32, file);
  }
  return nout;
}


