// ----------------------------------------------------------------------- 
// $Id: test.memory.c 396 2019-01-02 22:53:10Z viari $
// ----------------------------------------------------------------------- 
//
//

#include "../common.h"

#define N 100000000

static void _print(const char *where, const Ptr p) {
  printf("@ %s: memAllocUsed: %zu\n", where, memAllocUsed());
  printf("  %s: AllocSize: %zu\n", where, p ? AllocSize(p) : 0UL);
}

int main(int argn, char *argv[]) {

  Unused(argn);
  Unused(argv);

  char *buf = NULL;
  
  _print("start", NULL);
  
  buf = NewN(char, N);

  _print("alloc", buf);

  buf = Realloc(buf, char, 2*N);

  _print("alloc", buf);

  buf = AllocAssert(buf, 3*N);

  _print("alloc", buf);

  buf = Free(buf);

  _print("end", buf);
     
  exit(0);  
  
}
