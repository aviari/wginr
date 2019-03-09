// ----------------------------------------------------------------------- 
// $Id: common.h 396 2019-01-02 22:53:10Z viari $
// ----------------------------------------------------------------------- 
//
//

#ifndef _H_Common

#define _H_Common

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>
#include <unistd.h>

// ----------------------------------------
// memory pointers
//
// libmemory provides basic memory managment functions wrapping
// Clib's malloc, realloc, free, etc...
// The only difference is that the allocated blocks of memory
// also keep track of their allocated size (thus simplifying some
// operations).
// Therefore instead of a void*, these functions 
// take as parameter(s) and return Ptr's.
//
// A Ptr is functionnaly equivalent to a void*
// and may be passed to any function that accepts
// a pointer (e.g. to memcpy(2)).
// But you should not pass them to functions
// that modify the pointer allocation itself (e.g. to realloc(2)).
// Instead, you should use the corresponding memMalloc, memRealloc,
// memAllocAssert and memFree or (better) the equivalent macros
// defined herein New, NewN, Realloc, AllocAssert and Free.
// In the same way, you should not pass any pointer other
// than Ptr to a function that expect a Ptr.
//
// You may retrieve the allocated size of a Ptr by 
// memAllocSize or equivalent macro AllocSize.
//
// note: libmemory can optionaly use the Doug Lea 'dlmalloc'
//       library instead of usual Clib by specifying the DLMALLOC
//       compilation flag.
//

typedef void *Ptr;


// ----------------------------------------
// Strings
//
// A String is just like a char* with additional information about
// its length (and its memory capacity). This makes some operations
// quicker or easier, since String can automatically resize as needed (see below). 
// 
// For convenience and transparency with other function acting on char*
// String is not a struct but a char* pointer. 
// You may see String as dynamic char*
//
// You should allocate Strings's thru stringNew or stringAssert,
// and free String's thru stringFree.
// The string length is the actual length (i.e. number of chars before
// the null).
// The String capacity is the size in bytes of allocated memory, including
// the place for the null mark.
// Therefore, for any string we have capacity >= length + 1.
// You may retrieve the string length (in O(1)) by stringLen.
// Since String's are dynamic, all the stringCat, stringCpy and similar
// operations will automatically resize the String as needed, so you don't
// need to worry about the memory size.
//
// You may use a String as argument in any function than accept
// a char*. However if this function modifies the position of the null mark,
// you should call stringAdjust immediately after in order to
// readjust the String internal information.
// On the other hand, you should never pass any char*
// other than String to a function that expect a String. In most cases
// this will result in a crash.
//

typedef char *String;

// ----------------------------------------
// indexed file structure
//

typedef struct {
  FILE *stream; // io stream
  off_t offset; // offset of latest read/write op
} File;

// ----------------------------------------
// macros
//

#define Notify(format, ...) sysNotify(format, ##__VA_ARGS__)
#define Warning(format, ...) sysWarning(format, ##__VA_ARGS__)
#define Abort(errno, format, ...) sysAbort(errno, format, ##__VA_ARGS__)

#define Unused(arg) (void)(arg)

#define New(typ) (typ*) memMalloc(sizeof(typ))
#define NewN(typ, size) memMalloc((size_t)(size) * sizeof(typ))
#define Realloc(ptr, typ, size)  memRealloc((Ptr) (ptr), (size) * sizeof(typ))
#define Free(ptr) memFree(ptr)
#define AllocAssert(ptr, size) memAllocAssert((Ptr) (ptr), (size_t)(size))
#define AllocSize(ptr) memAllocSize((const Ptr) (ptr))
#define AllocZero(ptr)  memAllocZero((Ptr) (ptr))

#define Min(x, y) ((x) <= (y) ? (x) : (y))
#define Max(x, y) ((x) >= (y) ? (x) : (y))

// ----------------------------------------
// prototypes
//

// --------------------
// libsys.c

int    sysVerbose(int verbose);
void   sysNotify(const char *fmt, ...);
void   sysWarning(const char *fmt, ...);
void   sysAbort(int errno, const char *fmt, ...);
int    sysOpenFile(const char *filename, const char *mode, File *file);
int    sysCloseFile(File *file);
int    sysStream(FILE *stream, File *file);
char  *sysGetLine(char *s, int n, File *file);
int    sysPushBackLine(File *file);
int    sysGetNbCores(void);
double sysGetTime(void);
double sysTimer(int reset);

// --------------------
// libmalloc.c

Ptr    memMalloc(size_t size);
Ptr    memRealloc(Ptr aptr, size_t size);
Ptr    memAllocAssert(Ptr aptr, size_t size);
Ptr    memFree(Ptr aptr);
void   memZero(Ptr aptr, size_t size);
void   memAllocZero(Ptr aptr);
size_t memAllocSize(const Ptr aptr);
size_t memAllocUsed(void);

// --------------------
// libstring.c

String  stringNew(const char *src);
String  stringFree(String str);
String  stringAssert(String str, size_t size);
size_t  stringLen(const String str);
size_t  stringCapacity(const String str);
String  stringCpy(String dst, const char *src);
String  stringCat(String dst, const char *src);
String  stringEmpty(String str, int shrinkit);
String  stringAdjust(String str, int shrinkit);
String  stringErase(String str, int c);
String  stringReplace(String str, int cfrom, int cto);
String  stringInsert(String dst, const char *src, size_t pos);
String  stringExtract(const String str, size_t from, size_t to);
String  stringTrim(String str);
char    *stringUpper(char *str);
char    *stringLower(char *str);
char    *stringReverse(char *str);
size_t   stringGetLine(String *str, File *file);

// --------------------
// libbinio.c

int  isLittleEndian(void);

size_t binOutInt8(uint8_t i, File *file);
size_t binOutInt16(uint16_t i, File *file);
size_t binOutInt32(uint32_t i, File *file);
size_t binOutInt64(uint64_t i, File *file);
size_t binOutString(const char *s, File *file);
size_t binOutBytes(const char *s, size_t size, File *file);

// --------------------
// libcrc32.c

uint32_t lxCrc32(const char* data, size_t length, uint32_t previous);

// --------------------
// libbgz.c  (using sam)

int bgzCompressFile(const char *filename, int nthreads, int remove);


#endif

