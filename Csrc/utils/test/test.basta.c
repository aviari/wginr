// ----------------------------------------------------------------------- 
// $Id: test.basta.c 426 2019-01-07 22:20:53Z viari $
// ----------------------------------------------------------------------- 
//
//

#include "../common.h"
#include "../libbasta.h"

int main(int argn, char *argv[]) {

  Unused(argn);
  Unused(argv);
  
  File filin, filou;
  
  // ------------------------------------------
  // parse fasta
  // ------------------------------------------
  
  (void) sysOpenFile("test.basta.in", "r", &filin);

  BastaHeader *header = bastaParseFasta(&filin, 0);
  
  size_t i;
  
  for (i = 0 ; i < header->nbseq ; i++) {
   BastaEntry *entry = header->entry[i];
   (void) printf("%zu %s %zu %u\n", i, entry->name, (size_t) entry->size, entry->crc32);
  }
  
  (void) sysCloseFile(&filin);

  // ------------------------------------------
  // write basta header
  // ------------------------------------------
  
  (void) sysOpenFile("test.basta.bin", "wb", &filou);
  
  (void) bastaWriteHeader(header, &filou);
  
  (void) sysCloseFile(&filou);
  

  (void) bastaFreeHeader(header);

  exit(0);  
}
