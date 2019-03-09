// ----------------------------------------------------------------------- 
// $Id: test.fasta.c 396 2019-01-02 22:53:10Z viari $
// ----------------------------------------------------------------------- 
//
//

#include "../common.h"
#include "../libfasta.h"

static int _compar(const void *p1, const void *p2) {
  const FastaSequence *s1 = *(FastaSequence *const *) p1;
  const FastaSequence *s2 = *(FastaSequence *const *) p2;
  return (s1->size == s2->size) ? 0 : (s1->size > s2->size) ? 1 : -1;
}

int main(int argn, char *argv[]) {

  Unused(argn);
  Unused(argv);
  
  File filin, filou;
  FastaSequence *fasta = fastaNew();

  // ------------------------------------------
  // sequential read with sequence
  // ------------------------------------------
  
  (void) printf("+ sequential read with sequence\n");
  size_t iseq, nbseq = 0;

  (void) sysOpenFile("test.fasta.in", "r", &filin);

  (void) sysStream(stdout, &filou);
  
  while (fastaRead(fasta, &filin)) {
    (void) fastaClean(fasta);
    (void) printf("Seq-Info: %zu %zu %zu\n",
                    fasta->size, 
                    stringLen(fasta->seq), 
                    stringCapacity(fasta->seq));
    (void) fastaWrite(fasta, 50, &filou);

	if (fasta->size != stringLen(fasta->seq))
	  Notify("bad sequence length");
	  
	nbseq++;
  }

  // ------------------------------------------
  // sequential read without sequence
  // ------------------------------------------

  (void) printf("+ sequential read without sequence\n");
  FastaSequence **seqs = NewN(FastaSequence *, nbseq);
  
  rewind(filin.stream);
  
  for (iseq = 0 ; iseq < nbseq ; iseq++) {
    seqs[iseq] = fastaNew();
    seqs[iseq]->seq = stringFree(seqs[iseq]->seq); // no sequence
    (void) fastaRead(seqs[iseq], &filin);
    (void) printf("Seq-Info: %zu %zu\n",
                    seqs[iseq]->size, 
                    stringLen(seqs[iseq]->seq));
  }

  (void) qsort(seqs, nbseq, sizeof(FastaSequence *), _compar);

  // ------------------------------------------
  // indexed read with sequence
  // ------------------------------------------

  (void) printf("+ indexed read with sequence\n");

  for (iseq = 0 ; iseq < nbseq ; iseq++) {
    size_t off;
    off = fasta->offset = seqs[iseq]->offset;
    (void) fastaFetch(fasta, &filin);
    (void) fastaClean(fasta);
    (void) printf("Seq-Info: %zu %zu %zu %zu\n",
                    fasta->size, 
                    fasta->offset, 
                    stringLen(fasta->seq), 
                    stringCapacity(fasta->seq));
    (void) fastaWrite(fasta, 50, &filou);
    if (off != fasta->offset)
      Notify("bad offset");
  }

  // ------------------------------------------
  // free stuff
  // ------------------------------------------

  for (iseq = 0 ; iseq < nbseq ; iseq++) {
    (void) fastaFree(seqs[iseq]);
  }
  Free(seqs);
  
  (void) sysCloseFile(&filin);
  
  (void) fastaFree(fasta);

  exit(0);  
}
