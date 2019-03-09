// ----------------------------------------------------------------------- 
// $Id: test.crc32.c 396 2019-01-02 22:53:10Z viari $
// ----------------------------------------------------------------------- 
//
//

#include "../common.h"

static uint32_t _crc(const char *data, uint32_t previous) {
  return lxCrc32(data, strlen(data), previous);
}


int main(int argn, char *argv[]) {

  Unused(argn);
  Unused(argv);

  uint32_t crc = 0;
  
  crc = _crc("The ", crc);
  crc = _crc("quick brown fox jumps ", crc);
  crc = _crc("over the lazy dog", crc);
  
  (void) printf("crc32: %x\n", crc); // 414fa339

  exit(0);  
}
