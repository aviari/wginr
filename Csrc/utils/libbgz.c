// ----------------------------------------------------------------------- 
// $Id: libbgz.c 396 2019-01-02 22:53:10Z viari $
// bgz compression utility (using htslib bgzf)
// ----------------------------------------------------------------------- 
//
//

#include <stdarg.h>
#include <string.h>

#include "common.h"
#include "sam.h"

// ----------------------------------------
// compress filename to filename.bgz
//
int bgzCompressFile(const char *filename, int nthreads, int remove) {

    FILE *filin;
    BGZF *filou;
    size_t nread;
    char buffer[BGZF_BLOCK_SIZE];
    
    (void) strcat(strcpy(buffer, filename), ".bgz");
    
    if (! (filin = fopen(filename, "rb"))) {
      Warning("cannot open input file: %s\n", filename);
      return 1;
    }
      
    if (! (filou = bgzf_open(buffer, "w"))) {
      Warning("cannot open output file: %s\n", buffer);
      (void) fclose(filin);
      return 2;
    }
    
    if (nthreads > 1)
      bgzf_mt(filou, nthreads, 256);

    ssize_t status = -1;
    while ((nread = fread(buffer, 1, BGZF_BLOCK_SIZE, filin)) != 0) {
      status = bgzf_write(filou, buffer, nread);
      if (status < 0) {
        Warning("bgzf_write error\n");
        break;
      }
    }
    
    (void) fclose(filin);
    
    if (bgzf_flush(filou) != 0)
        Warning("bgzf_flush error\n");
    (void) bgzf_close(filou);

    if (remove && (status >= 0))
      (void) unlink(filename);
    
    return 0;
}


