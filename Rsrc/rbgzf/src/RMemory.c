//
// R memory managment
// add stamp to malloc to check nobody tamper with adresses...
//
#include <R.h>
#include "RMemory.h"

#define SIZ sizeof(long)

static unsigned long sStamp = 0x5262677a66L;  // 'Rbgzf'

int r_check(Ptr ptr) {
  if (ptr == NULL) return 0;
  unsigned long *pl = (unsigned long *)ptr - 1;
  return (*pl == sStamp);
}

Ptr r_malloc(size_t size) {
  Ptr ptr = Calloc(size+SIZ, char);
  (void) memcpy(ptr, &sStamp, SIZ);
  return ptr + SIZ;
}

Ptr r_calloc(size_t count, size_t size) {
  return r_malloc(count*size);
}

Ptr r_realloc(Ptr ptr, size_t size) {
  if (ptr == NULL) return r_malloc(size); //  ISO-IEC 9899 - 7.22.3.5
  if (r_check(ptr)) {
    ptr -= SIZ;
    ptr = Realloc(ptr, size+SIZ, char);
    (void) memcpy(ptr, &sStamp, SIZ);
    ptr += SIZ;
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) ptr);
  }
  return ptr;
}

void r_free(Ptr ptr) {
  if (ptr == NULL) return; // ISO-IEC 9899 - 7.20.3.2
  if (r_check(ptr)) {
    ptr -= SIZ;
    bzero(ptr, SIZ);
    Free(ptr);
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) ptr);
  }
}

void r_ptr2str(Ptr ptr, char *buffer) {
  (void) sprintf(buffer, "0x%lx", (unsigned long) ptr);
}

Ptr r_str2ptr(const char *buffer) {
  return (Ptr) strtoul(buffer, NULL, 16);
}
