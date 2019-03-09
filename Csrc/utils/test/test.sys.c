// ----------------------------------------------------------------------- 
// $Id: test.sys.c 396 2019-01-02 22:53:10Z viari $
// ----------------------------------------------------------------------- 
//
//

#include "../common.h"

int main(int argn, char *argv[]) {

  int st;
  char buffer[BUFSIZ+1];

  Unused(argv);
  
  (void) sysVerbose(1);
  
  (void) sysTimer(1);
   
  Notify("test.sys: argn=%d\n", argn);

  // files
  
  File filin;
  
  st = sysOpenFile("test.sys.nofile", "r", &filin);
  Notify("test.sys: sysOpen=%d\n", st);

  st = sysOpenFile("test.sys.in", "r", &filin);
  Notify("test.sys: sysOpen=%d\n", st);

  (void) sysGetLine(buffer, BUFSIZ, &filin);
  (void) sysPushBackLine(&filin);
  
  while (sysGetLine(buffer, BUFSIZ, &filin))
    Notify("test.sys: buffer=%s", buffer);

  Notify("test.sys: file.offset=%d\n", filin.offset);

  (void) sysCloseFile(&filin);


  // standard streams
  
  File fstdin;
  
  (void) sysStream(stdin, &fstdin);

  while (sysGetLine(buffer, BUFSIZ, &fstdin))
    Notify("test.sys: stdin=%s", buffer);

  // timer
  
  (void) fprintf(stderr, "@ test.sys: timer=%f\n", sysTimer(0));

  Abort(0, "Normal End\n");
  
  exit(0); // not reached
}
