/* -----------------------------------------------------------------------
 * $Id: libmemory.c 396 2019-01-02 22:53:10Z viari $
 * -----------------------------------------------------------------------
 *
 * @brief memory allocation
 *
 */
 
#include "common.h"

#if defined(HAVE_MEMSET)
  #include <string.h>
#endif

/* ---------------------------------------------------- */
/* should we use dlmalloc or internal libc malloc ?     */
/* ---------------------------------------------------- */
 
#if defined(DLMALLOC)

  #include "dlmalloc.c"

  #define _MALLOC  dlmalloc
  #define _REALLOC dlrealloc
  #define _FREE    dlfree

#else

  #define _MALLOC  malloc
  #define _REALLOC realloc
  #define _FREE    free
  
  /*                                        */
  /* for mallinfo, linux uses <malloc.h>    */
  /* whereas apple uses <malloc/malloc.h>   */
  /*                                        */

  #if defined(LINUX)
    #include <malloc.h>
  #elif defined(OSX)
    #include <malloc/malloc.h>
  #endif

#endif

#define _OFFSET          sizeof(size_t)
#define _SIZE(ptr)       *((size_t *) ((ptr) - _OFFSET))
#define _CSIZE(ptr)      *((const size_t *) ((ptr) - _OFFSET))

/* ---------------------------------------------------- */
/* API                                                  */
/* these malloc/realloc/free functions wrap the libc    */
/* functions to add information about allocated size    */
/* in front of the pointer                              */
/* doc is in LXMemory.h                                 */
/* ---------------------------------------------------- */

/* -------------------------------------------- */
/* malloc                                       */
/* -------------------------------------------- */

Ptr memMalloc(size_t size) {
  char *ptr = _MALLOC(size + _OFFSET);
  if (ptr) {
    ptr += _OFFSET;
    _SIZE(ptr) = size;
  }
  return (Ptr) ptr;
}

/* -------------------------------------------- */
/* realloc                                      */
/* -------------------------------------------- */

Ptr memRealloc(Ptr aptr, size_t size) {
  char *ptr = (char *) aptr;
  if (ptr) {
    ptr -= _OFFSET;
    ptr = _REALLOC(ptr, size + _OFFSET);
  } else {
    ptr = _MALLOC(size + _OFFSET);
  }
  if (ptr) {
    ptr += _OFFSET;
    _SIZE(ptr) = size;
  }
  return (Ptr) ptr;
}

/* -------------------------------------------- */
/* assert alloc                                 */
/* -------------------------------------------- */

Ptr memAllocAssert(Ptr aptr, size_t size) {
  char *ptr = (char *) aptr;
  if (ptr) {
    size_t osize = _SIZE(ptr);
    if (osize < size)
      ptr = memRealloc(ptr, size);
  } else {
      ptr = memMalloc(size);
  }
  return (Ptr) ptr;
}

/* -------------------------------------------- */
/* free alloc. block                            */
/* -------------------------------------------- */

Ptr memFree(Ptr aptr) {
  char *ptr = (char *) aptr;
  if (ptr) {
    _SIZE(ptr) = 0;
    _FREE(ptr - _OFFSET);
  }
  return NULL;
}

/* -------------------------------------------- */
/* zero memory block                            */
/* -------------------------------------------- */

void memZero(Ptr aptr, size_t size) {
  char *ptr = (char *) aptr;
  if (ptr) {
    if (size > _SIZE(ptr))
      size = _SIZE(ptr);
#if defined(HAVE_MEMSET)
    (void) memset(ptr, 0, size);
#else
    while(size != 0) {
      *ptr++ = 0;
      size--;
    }
#endif
  }
}

/* -------------------------------------------- */
/* zero all alloc. block                        */
/* -------------------------------------------- */

void memAllocZero(Ptr aptr) {
  memZero(aptr, _SIZE((char *) aptr));
}

/* -------------------------------------------- */
/* get alloc size of ptr                        */
/* -------------------------------------------- */

size_t memAllocSize(const Ptr aptr) {
  const char *ptr = (const char *) aptr;
  return (ptr ? _CSIZE(ptr) : 0);
}

/* -------------------------------------------- */
/* get total allocated memory size              */
/* -------------------------------------------- */

size_t memAllocUsed(void) {

#if defined(DLMALLOC)

  struct mallinfo info = dlmallinfo();
  return info.uordblks;

#elif defined(OSX)  // Apple specific version

  struct mstats mem = mstats();
  return mem.bytes_used;

#elif defined(LINUX)

  struct mallinfo info = mallinfo();
  return info.uordblks;
  
#else
  return 0;

#endif
}
