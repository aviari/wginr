// -----------------------------------------------------------------------
// $Id: bam2baf.c 404 2019-01-06 17:22:12Z viari $
// -----------------------------------------------------------------------
//
//

#include "common.h"
#include "sam.h"
#include <htslib/thread_pool.h> // $$ needed ?

#define VERSION "1.4"

#define BAF_MAGIC 0x62616631    // 'baf1'

#define MIN_MAPQ 0      // like in samtools
#define MIN_BASQ 13     // like in samtools

// ----------------------------------------
// baf header structures
//
typedef struct {
    String name;        // sequence name
    uint64_t size;      // sequence size
    uint32_t code;      // encoding length in bytes = 0, 1, 2, 4
} BafEntry;

#define BAF_MAX_ENTRIES 4096

typedef struct {
    size_t nbseq;
    BafEntry entry[BAF_MAX_ENTRIES];
} BafHeader;

typedef struct {
  int32_t tid, mapq, basq;
  File *filou;
  BafHeader *header;
} PileupUserArg;

// ----------------------------------------
// allocate empty baf header
//
static BafHeader *_bafNewHeader(void) {
  BafHeader *header = New(BafHeader);
  if (header) {
    header->nbseq = 0;
    size_t i;
    for (i = 0 ; i < BAF_MAX_ENTRIES ; i++)
      header->entry[i].name = NULL;
  }
  return header;
}

// ----------------------------------------
// free baf header
//
static BafHeader *_bafFreeHeader(BafHeader *header) {
  if (header) {
    size_t i;
    for (i = 0 ; i < header->nbseq ; i++)
      (void) stringFree(header->entry[i].name);
    Free(header);
  }
  return NULL;
}

// ----------------------------------------
// bam -> baf header
//
static int _bam2bafHeader(bam_header_t *bamheader, BafHeader *bafheader) {
  int32_t i;
  bafheader->nbseq = bamheader->n_targets;
  for (i = 0 ; i < bamheader->n_targets ; i++) {
    BafEntry *entry = bafheader->entry + i;
    entry->name = stringNew(bamheader->target_name[i]);
    entry->size = bamheader->target_len[i];
    entry->code = 4;
  }
  return 1;
}

// ----------------------------------------
// write baf header
//
static size_t _bafWriteHeader(BafHeader *header, File *file) {
  size_t nout = 0, i;
  nout += binOutInt32(BAF_MAGIC, file);
  nout += binOutInt32(header->nbseq, file);
  for (i = 0; i < header->nbseq ; i++) {
    nout += binOutString(header->entry[i].name, file);
    nout += binOutInt64(header->entry[i].size, file);
    nout += binOutInt32(header->entry[i].code, file);
  }
  return nout;
}

// ----------------------------------------
// tid manamgment (Memory version)
// max memory needed ~ 4Gb
// ----------------------------------------
//
static uint32_t *_tidBuffer = NULL;

// ----------------------------------------
// init in memory buffer
//
static void _initTid(BafHeader *bafheader, int32_t tid) {
  size_t size = 4 * bafheader->entry[tid].size * sizeof(uint32_t);
  if (! (_tidBuffer =  AllocAssert(_tidBuffer, size)))
    Abort(20, "Not enough memory : %ld bytes needed\n", size);
  memZero(_tidBuffer, size);
}

// ----------------------------------------
// free in memory buffer
//
static void _freeTid(void) {
  (void) Free(_tidBuffer);
}

// ----------------------------------------
// update buffer
// add count to pos
//
static void _addTid(uint32_t pos, uint32_t *cnt) {
  pos *= 4;
  _tidBuffer[pos]   = cnt[1];
  _tidBuffer[pos+1] = cnt[2];
  _tidBuffer[pos+2] = cnt[4];
  _tidBuffer[pos+3] = cnt[8];
}

// ----------------------------------------
// determine proper buffer encoding length
//
static int _codeTid(BafHeader *bafheader, int32_t tid) {
  int32_t i, j, n = bafheader->entry[tid].size;
  uint32_t max = 0;
  uint32_t *buf = _tidBuffer;

  for (i = 0 ; i < n ; i++, buf += 2)
    for (j = 0 ; j < 4 ; j++)
      max = Max(max, buf[j]);

  Notify("encode tid=%d max: %d\n", tid, max);

  return    (max == 0)      ?  0
         :  (max <= 0xff)   ?  1
         :  (max <= 0xffff) ?  2
         :  4;
}

// ----------------------------------------
// convert buffer to proper encoding
//
static void _convertTid(int32_t code, int32_t size) {
  int32_t i, j;
  uint32_t *buf = _tidBuffer;
  if (code == 1) {
    uint8_t *b = (uint8_t *) _tidBuffer;
    for (i = 0 ; i < size ; i++, buf += 4, b += 4) {
      for (j = 0 ; j < 4 ; j++)
        b[j] = buf[j];
    }
  }
  if (code == 2) {
    uint16_t *b = (uint16_t *) _tidBuffer;
    for (i = 0 ; i < size ; i++, buf += 4, b += 4) {
      for (j = 0 ; j < 4 ; j++)
        b[j] = buf[j];
    }
  }
}

// ----------------------------------------
// flush buffer to file
//
static void _flushTid(File *file, int32_t code, int32_t size) {
  size_t sizelt = 0; // code == 0 => no output
  if (code == 1)
    sizelt = sizeof(uint8_t);
  else if (code == 2) 
    sizelt = sizeof(uint16_t);
  else if (code == 4)
    sizelt = sizeof(uint32_t);

  if (sizelt)
    (void) binOutBytes((const char *) _tidBuffer, size * 4 * sizelt, file);
}

// ----------------------------------------
// end buffer
// determine encoding
// write to file
//
static void _endTid(File *file, BafHeader *bafheader, int32_t tid) {
  int32_t n = bafheader->entry[tid].size;
  int32_t code = bafheader->entry[tid].code = _codeTid(bafheader, tid);
  Notify("encode tid=%d encoding: %d size: %ld\n", tid, code, n);
  _convertTid(code, n);
  _flushTid(file, code, n); 
}

// ----------------------------------------
// mpileup callback
//

static uint32_t _prev_pos = -1;

static int pileup_func(uint32_t tuid, uint32_t pos, int n,
                       const bam_pileup1_t *pl, void *data) {

    int32_t i, tid = (int32_t) tuid;
    uint32_t cnt[16];
    const bam_pileup1_t *p;
    
    PileupUserArg *args = (PileupUserArg *) data;

    if (tid < args->tid)
      Abort(10, "bam file is not sorted, please sort it first\n");

    if ((_prev_pos == pos) && (tid == args->tid))
      Notify("duplicate position detected at %ld\n", pos);
    _prev_pos = pos;

    cnt[1] = cnt[2] = cnt[4] = cnt[8] = 0;
    
    for (i = 0, p = pl ; i < n ; i++,  p++) {
      if (    (! p->is_del)
           && (! p->is_refskip)
           && (p->b->core.qual >= args->basq)
           && (bam1_qual(p->b)[p->qpos] >= args->mapq) ) {
        int c = bam1_seqi(bam1_seq(p->b), p->qpos) & 0x0f;
        cnt[c]++;
      }
    }

    if (tid > args->tid) {  // start new tid
      if (args->tid >= 0) { // end current tid if any
        _endTid(args->filou, args->header, args->tid);
      }
      for (i = ++args->tid ; i < tid ; i++) { // skip intermediary sequences
        _initTid(args->header, i);
        _endTid(args->filou, args->header, i);
      }
      Notify("starting tid=%d\n", tid);
      _initTid(args->header, tid); // start new tid
      args->tid = tid;
    }

    _addTid(pos, cnt);
    
    return 0;  
}

// ----------------------------------------
// help
//

#define PP (void) fprintf(stderr,

static void _printHelp(void) {
    PP  "------------------------------------------ \n");
    PP  " bam2baf Version %s\n", VERSION);
    PP  "------------------------------------------ \n");
    PP  "synopsis :                                 \n");
    PP  "  convert bam to baf format                \n");
    PP  "usage: bam2baf [options] in.bam out.baf    \n");
    PP  "------------------------------------------ \n");
    PP  "options:                                   \n");
    PP  "                                           \n");
    PP  "-h : this [H]elp                           \n");
    PP  "                                           \n");
    PP  "-p : generate [P]seudo baf (header only)   \n");
    PP  "                                           \n");
    PP  "-q INT : min map [q]uality (dft=%d)        \n", MIN_MAPQ);
    PP  "                                           \n");
    PP  "-q INT : min base [Q]uality (dft=%d)       \n", MIN_BASQ);
    PP  "                                           \n");
    PP  "-v : set [V]erbose mode                    \n");
    PP  "                                           \n");
    PP  "-z : compress outfile with bg[Z]f          \n");
    PP  "                                           \n");
    PP  "-@ INT : number of threads for compressing \n");
    PP  "         0=max available (dft=0)           \n");
    PP  "                                           \n");
    PP  "------------------------------------------ \n");
}

#undef PP

// ----------------------------------------
// main
//

int main(int argn, char *argv[]) {

  int  carg, errflag, vflag, pflag, zflag, mapq, basq, nthreads;

  File filou;
  
  samfile_t *bamfile;
  
  htsThreadPool threadpool = {NULL, 0};
  
  // ------------------------------------------
  // get user's args
  //

  errflag = 0;
  vflag = 0;
  zflag = 0;
  pflag = 0;
  mapq  = MIN_MAPQ;
  basq  = MIN_BASQ;
  nthreads = 0;

  while ((carg = getopt(argn, argv, "hpq:Q:vz@:")) != -1) {

    switch(carg) {

        case 'h' :              /* help             */
        _printHelp();
        exit(0);
        break;

        case 'p' :              /* pseudo baf       */
        pflag = 1;
        break;

        case 'q' :              /* min map quality  */
        mapq = atoi(optarg);
        break;

        case 'Q' :              /* min base quality */
        basq = atoi(optarg);
        break;

        case 'v' :              /* verbose          */
        vflag = 1;
        break;

        case 'z' :              /* compress         */
        zflag = 1;
        break;

        case '@' :              /* # threads        */
        nthreads = atoi(optarg);
        break;

        case '?' :              /* misusage         */
        errflag++;
    }
  }

  if (errflag || ((argn - optind) != 2)) {
    sysVerbose(1);
    Notify("usage: bam2baf [-h -p -q mapqual -Q basequal -v -z -@ nthreads] in.bam out.baf\n");
    Abort(1, "type bam2baf -h for help\n");
  }
  
  if (! (bamfile = samopen(argv[optind], "rb", 0)))
      Abort(2, "cannot open input bam file: %s\n", argv[optind]);

  if (! (sysOpenFile(argv[optind+1], "wb", &filou)))
      Abort(3, "cannot write output baf file: %s\n", argv[optind+1]);

  sysVerbose(vflag);

  if (nthreads == 0) nthreads = sysGetNbCores();
  if (nthreads < 0) {
    nthreads = sysGetNbCores() / -nthreads;
    nthreads = (nthreads > 1 ? nthreads : 1);
  }
  Notify("# threads : %d\n", nthreads);

  if (nthreads > 1) {
    if (!(threadpool.pool = hts_tpool_init(nthreads))) {
        fprintf(stderr, "Error creating thread pool\n");
        exit(EXIT_FAILURE);
    }
    hts_set_opt(bamfile->file, HTS_OPT_THREAD_POOL, &threadpool);
  }
  
  // ------------------------------------------
  // check max number of entries
  //
  
  if (bamfile->header->n_targets > BAF_MAX_ENTRIES)
    Abort(4, "too many target sequences : %d (max is %d) %s\n",
              bamfile->header->n_targets, BAF_MAX_ENTRIES,
              "\nPlease increase BAF_MAX_ENTRIES and recompile source code");

  Notify("# target sequences : %d\n", bamfile->header->n_targets);
  
  // ------------------------------------------
  // get baf header from bam header  
  //
  
  BafHeader *bafheader = _bafNewHeader();

  (void) _bam2bafHeader(bamfile->header, bafheader);

  // ------------------------------------------
  // write fake header
  //
  
  (void) _bafWriteHeader(bafheader, &filou);
  
  // ------------------------------------------
  // prepare pileup args
  //

  PileupUserArg args;
  args.tid = -1;
  args.mapq = mapq;
  args.basq = basq;
  args.filou = &filou;
  args.header = bafheader;
  
  if (! pflag) {
      // ------------------------------------------
      // pileup
      //
      int err = sampileup(bamfile, -1, pileup_func, &args);
      Notify("mpileup status: %d\n", err);
  }

  // ------------------------------------------
  // latest tid

  if (args.tid >= 0)
    _endTid(&filou, bafheader, args.tid);

  // ------------------------------------------
  // last tid's

  while (++args.tid < bamfile->header->n_targets) {
    _initTid(bafheader, args.tid);
    _endTid(&filou, bafheader, args.tid);
  }

  // ------------------------------------------
  // overwrite correct header
  // (i.e. with correct encodings)

  Notify("writing header\n");
  rewind(filou.stream);
  (void) _bafWriteHeader(bafheader, &filou);
  (void) sysCloseFile(&filou);

  // ------------------------------------------
  // free memory
  
  _bafFreeHeader(bafheader);
  _freeTid();

  samclose(bamfile);
  
  if (threadpool.pool) hts_tpool_destroy(threadpool.pool);
  
  // ------------------------------------------
  // compress with bgzf

  if (zflag) {
    Notify("compressing %s\n", argv[optind+1]);
    if (bgzCompressFile(argv[optind+1], nthreads, 1))
      Abort(11, "cannot compress file");
  }
  
  exit(0);
}
