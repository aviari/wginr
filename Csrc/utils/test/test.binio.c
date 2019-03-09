// ----------------------------------------------------------------------- 
// $Id: test.binio.c 396 2019-01-02 22:53:10Z viari $
// ----------------------------------------------------------------------- 
//
//

#include "../common.h"

int main(int argn, char *argv[]) {

  Unused(argn);
  Unused(argv);

  File filou;
  
  (void) sysOpenFile("test.binio.bak", "wb", &filou);
  
  (void) fprintf(stderr, "@ isLittleEndian: %d\n", isLittleEndian());
  
  (void) binOutInt32(1234, &filou);
  (void) binOutInt64(1234, &filou);
  (void) binOutString("abcdefgh", &filou);
  (void) binOutBytes("end", 3, &filou);
  
  (void) sysCloseFile(&filou);
  

  FILE *filin = fopen("test.binio.bak", "rb");
  int i=0, c;
  
  while ((c = fgetc(filin)) != EOF)
    (void) printf("%2.2d : 0x%2.2x\n", i++, c);

  fclose(filin);

  exit(0);  
}
