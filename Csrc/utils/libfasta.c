// ----------------------------------------------------------------------- 
// $Id: libfasta.c 396 2019-01-02 22:53:10Z viari $
// ----------------------------------------------------------------------- 
//
//

#include "libfasta.h"

// ----------------------------------------
// write single line in stream
//
static char *_putline(char *buffer, unsigned length, File *file) {
  char *end, keep;
  end = buffer + length;
  keep = *end;
  *end = '\000';
  (void) fputs(buffer, file->stream);
  (void) fputc('\n' , file->stream);
  *end = keep;
  return end;
}

// ----------------------------------------
// API
// ----------------------------------------

// ----------------------------------------
// allocate fasta sequence
//
FastaSequence *fastaNew() {
  FastaSequence *fasta = New(FastaSequence);
  if (fasta) {
    fasta->name    = stringNew("");
    fasta->comment = stringNew("");
    fasta->seq     = stringNew("");
    fasta->size    = 0;
    fasta->crc32   = 0;
    fasta->offset  = 0;
  }
  return   fasta && fasta->name && fasta->comment && fasta->seq
         ? fasta
         : fastaFree(fasta);
}

// ----------------------------------------
// free fasta sequence
//
FastaSequence *fastaFree(FastaSequence *fasta) {
  if (fasta) {
    (void) stringFree(fasta->name);
    (void) stringFree(fasta->comment);
    (void) stringFree(fasta->seq);
    Free(fasta);
  }
  return NULL;
}

// ----------------------------------------
// cleanup fasta sequence
//
size_t fastaClean(FastaSequence *fasta) {

  if (! (fasta && fasta->seq))
    return 0;
  
  char *s, *se;
  for (s = se = fasta->seq ; *s ; s++) {
    if (isalpha(*s))
        *se++ = *s;
  }
  *se = '\000';
  
  fasta->seq  = stringAdjust(fasta->seq, 0);
  fasta->size = stringLen(fasta->seq);
  
  return fasta->size;
}

// ----------------------------------------
// sequential read Fasta sequence from file
// file is assumed to be synched on '>'.
// if fasta->seq is not NULL keep sequence as well.
//
int fastaRead(FastaSequence *fasta, File *file) {

  String ioBuf = NULL;
  
  if (! stringGetLine(&ioBuf, file))
    return 0;

  if (*ioBuf != '>')
    return 0;
  
  fasta->size    = 0;
  fasta->crc32   = 0;
  fasta->offset  = file->offset;

  char *buf = ioBuf + 1;

  while (*buf && (! isspace(*buf)))
    buf++;

  char keep = *buf;
  *buf = '\000';
  fasta->name = stringCpy(fasta->name, ioBuf + 1);

  if (keep) {
    *buf = keep;
    fasta->comment = stringCpy(fasta->comment, buf+1);
  } else {
    fasta->comment = stringEmpty(fasta->comment, 0);
  }
  
  if (fasta->seq)
    fasta->seq = stringEmpty(fasta->seq, 0);

  while (stringGetLine(&ioBuf, file)) {
    if (*ioBuf == '>') {
      (void) sysPushBackLine(file);
      break;
    }
    size_t len = stringLen(ioBuf);
    fasta->crc32 = lxCrc32(ioBuf, len, fasta->crc32);
    fasta->size += len;
    if (fasta->seq)
      fasta->seq = stringCat(fasta->seq, ioBuf);
  }
  return 1;
}

// ----------------------------------------
// indexed read fasta sequence from file 
// using fasta->offset
//
int fastaFetch(FastaSequence *fasta, File *file) {
  (void) fseeko(file->stream, fasta->offset, SEEK_SET);
  return fastaRead(fasta, file);
}

// ----------------------------------------
// write fasta sequence to file
//
int fastaWrite(FastaSequence *fasta, unsigned char_per_line, File *file) {

  (void) fputc('>', file->stream);
  (void) fputs((fasta->name && *(fasta->name) ? fasta->name : "NO_NAME"), file->stream);
  if (fasta->comment && *(fasta->comment)) {
    (void) fputc(' ', file->stream);
    (void) fputs(fasta->comment, file->stream);
  }
  (void) fputc('\n', file->stream);

  if (! fasta->seq)
    return 1;

  char *buf = fasta->seq;

  size_t i;
  size_t nlines = fasta->size / char_per_line;
  size_t rest = fasta->size % char_per_line;
    
  for (i = 0 ; i < nlines ; i++)
    buf = _putline(buf, char_per_line, file);
    
  if (rest != 0)
    (void) _putline(buf, rest, file);
    
  return 1;
}





