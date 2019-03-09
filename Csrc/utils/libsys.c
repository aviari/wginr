// ----------------------------------------------------------------------- 
// $Id: libsys.c 396 2019-01-02 22:53:10Z viari $
// ----------------------------------------------------------------------- 
//
//

#include <stdarg.h>
#include <string.h>

#if defined(HAVE_SYSTIME)
  #include <sys/time.h>
#endif

#include "common.h"

static int _verbose = 0;

// ----------------------------------------
// set verbosity of sysNotify
//
int sysVerbose(int verbose) {
  int previous = _verbose;
  _verbose = verbose;
  return previous;
} 

// ----------------------------------------
// notify
//
void sysNotify(const char *fmt, ...) {
  if (_verbose == 0) return;
  
  va_list	args;
  va_start(args, fmt);
  
  (void) fprintf(stderr, "// [info] ");
  (void) vfprintf(stderr, fmt, args);
}

// ----------------------------------------
// warning
//
void sysWarning(const char *fmt, ...) {
  va_list	args;
  va_start(args, fmt);
  
  (void) fprintf(stderr, "// [warning] ");
  (void) vfprintf(stderr, fmt, args);
}

// ----------------------------------------
// exit on error
//
void sysAbort(int errno, const char *fmt, ...) {
  va_list	args;
  va_start(args, fmt);

  (void) fprintf(stderr, "// [abort] ");
  (void) vfprintf(stderr, fmt, args);
  
  exit(errno);
}

// ----------------------------------------
// open File
//
int sysOpenFile(const char *filename, const char *mode, File *file) {
   file->offset = 0;
   file->stream = fopen(filename, mode);
   return (file->stream != NULL);
}

// ----------------------------------------
// close File
//
int sysCloseFile(File *file) {
   return (file ? (fclose(file->stream) == 0) : 1);
}

// ----------------------------------------
// assign stdin/stdout/stderr to File
//
int sysStream(FILE *stream, File *file) {
  file->stream = stream;
  file->offset = 0;
  return 1;
}

// ----------------------------------------
// similar to fgets but also keeps track of 
// current position (before line read)
// in order to push it back later if necessary.
//
char *sysGetLine(char *s, int n, File *file) {
  file->offset = ftello(file->stream);
  return fgets(s, n, file->stream);
}

// ----------------------------------------
// push back last read line 
//
int sysPushBackLine(File *file) {
  return fseeko(file->stream, file->offset, SEEK_SET);
}

// ----------------------------------------
// get number of cores
// pass n=0 to get maximum available
int sysGetNbCores(void) {
  return sysconf(_SC_NPROCESSORS_ONLN);
}

// ----------------------------------------
// get time
//
double sysGetTime(void) {
#if defined(HAVE_SYSTIME)
  struct timeval clok;
  gettimeofday(&clok, NULL);
  double now = (double) clok.tv_sec + ((double) clok.tv_usec / 1e6);
#else
  double now = 0;
#endif
  return now;
}

// ----------------------------------------
// timer in seconds with microsecond precision
//
double sysTimer(int reset) {
  static double _previous = 0;
  double now = sysGetTime();
  double delta = now - _previous;
  if (reset) _previous = now;
  return delta;
}
