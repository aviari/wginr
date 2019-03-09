// -----------------------------------------------------------------------
// $Id: testpileup.c 396 2019-01-02 22:53:10Z viari $
// -----------------------------------------------------------------------
//
//

#include "common.h"
#include "sam.h"
#include "mtreader.h"

typedef struct {
  int32_t tid;
} PileupUserArg;

// ----------------------------------------
// mpileup callback
//

static uint32_t _prev_pos = -1;

static int pileup_func(uint32_t tuid, uint32_t pos, int n,
                       const bam_pileup1_t *pl, void *data) {

    int32_t i, tid = (int32_t) tuid;
    
    PileupUserArg *args = (PileupUserArg *) data;

    if (tid < args->tid)
      Abort(10, "bam file is not sorted, please sort it first\n");

    if ((_prev_pos == pos) && (tid == args->tid))
      Notify("duplicate position detected at %ld\n", pos);
    _prev_pos = pos;

//    static char REF = "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT";
    
    printf("tid=%d pos=%d n=%d", tid, pos, n);
    for (i = 0 ; i < n ; i++) {
// bam_prob_realn(pl[i].b, REF);
      int c = pl[i].is_del ? 0 : bam1_seqi(bam1_seq(pl[i].b), pl[i].qpos) & 0x0f;
      printf(" %d-%d-%d", c, bam1_qual(pl[i].b)[pl[i].qpos], pl[i].b->core.qual);
    }
    printf("\n");
    
    return 0;  
}

// bam1_qual(p->b)[p->qpos] >= conf->min_baseQ

int main(int argn, char *argv[]) {

  int  carg, errflag, nthreads;

  samfile_t *bamfile;
  
  // ------------------------------------------
  // get user's args
  //

  errflag = 0;
  nthreads = 0;

  while ((carg = getopt(argn, argv, "@:")) != -1) {

    switch(carg) {

        case '@' :              /* # threads     */
        nthreads = strtol(optarg, 0, 0);
        break;

        case '?' :              /* misusage     */
        errflag++;
    }
  }

  if (errflag || ((argn - optind) != 1)) {
    Abort(1, "\n");
  }
  
  if (! (bamfile = samopen(argv[optind], "rb", 0)))
      Abort(2, "cannot open input bam file: %s\n", argv[optind]);

  if (nthreads <= 0)
    nthreads = getMTThreads();
  (void) setMTThreads(nthreads);
    
  // ------------------------------------------
  // pileup
  //

  PileupUserArg args;
  args.tid = -1;
  
  int err = sampileup(bamfile, -1, pileup_func, &args);
  
  Notify("mpileup status: %d\n", err);
 
  exit(0);
}
