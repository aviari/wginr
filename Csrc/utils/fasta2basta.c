// -----------------------------------------------------------------------
// $Id: fasta2basta.c 397 2019-01-02 23:12:25Z viari $
// -----------------------------------------------------------------------
//
//

#include "common.h"
#include "libbasta.h"

#define VERSION "1.1"

// ----------------------------------------
// help
//

#define PP (void) fprintf(stderr,

static void _printHelp(void) {
    PP  "------------------------------------------ \n");
    PP  " fasta2basta Version %s\n", VERSION);
    PP  "------------------------------------------ \n");
    PP  "synopsis :                                 \n");
    PP  "  convert fasta to basta format            \n");
    PP  "usage: fasta2basta [-h] [-v] [-z] [-@ n] in.fst out.bst\n");
    PP  "------------------------------------------ \n");
    PP  "options:                                   \n");
    PP  "                                           \n");
    PP  "-h : this [H]elp                           \n");
    PP  "                                           \n");
    PP  "-v : set [V]erbose mode                    \n");
    PP  "                                           \n");
    PP  "-z : compress outfile with bg[Z]f          \n");
    PP  "                                           \n");
    PP  "-@ INT : number of threads for compressing \n");
    PP  "         0:max available (dft=0)           \n");
    PP  "------------------------------------------ \n");
}

#undef PP

// ----------------------------------------
// main
//

int main(int argn, char *argv[]) {

  int  carg, errflag, vflag, zflag, nthreads;

  File filin, filou;

  // ------------------------------------------
  // get user's args
  //

  errflag = 0;
  vflag = 0;
  zflag = 0;
  nthreads = 0;

  while ((carg = getopt(argn, argv, "hvz@:")) != -1) {

    switch(carg) {

        case 'h' :              /* help     */
        _printHelp();
        exit(0);
        break;

        case 'v' :              /* verbose      */
        vflag = 1;
        break;

        case 'z' :              /* compress     */
        zflag = 1;
        break;

        case '@' :              /* nthreads     */
        nthreads = atoi(optarg);
        break;
        
        case '?' :              /* misusage     */
        errflag++;
    }
  }

  if (errflag || ((argn - optind) != 2)) {
    Abort(1, "usage: fasta2basta [-h] [-v] [-z] [-@ n] in.fasta out.basta\n");
  }
  
  sysVerbose(vflag);
  
  if (nthreads == 0) nthreads = sysGetNbCores();
  if (nthreads < 0) {
    nthreads = sysGetNbCores() / -nthreads;
    nthreads = (nthreads > 1 ? nthreads : 1);
  }
  Notify("# threads : %d\n", nthreads);
  
  if (! sysOpenFile(argv[optind], "r", &filin))
      Abort(2, "cannot open input fasta file: %s\n", argv[optind]);

  if (! (sysOpenFile(argv[optind+1], "wb", &filou)))
      Abort(3, "cannot open output basta file: %s\n", argv[optind+1]);

  // ------------------------------------------
  // traverse fasta and get header
  //

  BastaHeader *header = bastaParseFasta(&filin, vflag);

  (void) bastaWriteHeader(header, &filou);

  (void) bastaFreeHeader(header);

  // ------------------------------------------
  // retraverse fasta and output sequences
  //

  rewind(filin.stream);

  String buffer = stringNew("");

  while (stringGetLine(&buffer, &filin)) {
    if (*buffer == '>') {
      if (vflag)
        Notify("writing sequence %s\n", buffer+1);
      continue;
    }
    (void) binOutBytes(buffer, stringLen(buffer), &filou);
  }
  
  stringFree(buffer);

  (void) sysCloseFile(&filin);
  (void) sysCloseFile(&filou);

  // ------------------------------------------
  // compress with bgzf
  
  if (zflag) {
    Notify("compressing %s\n", argv[optind+1]);
    if (bgzCompressFile(argv[optind+1], nthreads, 1))
      Abort(11, "cannot compress file");
  }
  
  exit(0);
}
