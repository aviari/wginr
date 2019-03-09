// ----------------------------------------------------------------------- 
// $Id: libbinio.c 396 2019-01-02 22:53:10Z viari $
// ----------------------------------------------------------------------- 
//
//

#include "common.h"

#include <string.h>

// ----------------------------------------
// swap endian for 16 bits value
//
#define ENDIAN_SWAP16(x)  (((x) & 0x00FF) << 8) | \
                          (((x) & 0xFF00) >> 8)
// ----------------------------------------
// swap endian for 32 bits value
//
#define ENDIAN_SWAP32(x)   (((x) & 0x000000FF) << 24) | \
                           (((x) & 0x0000FF00) << 8)  | \
                           (((x) & 0x00FF0000) >> 8)  | \
                           (((x) & 0xFF000000) >> 24)
// ----------------------------------------
// swap endian for 64 bits value
//
#define ENDIAN_SWAP64(x)   ((x)>>56)                           | \
                           (((x)<<40) & 0x00FF000000000000LLU) | \
                           (((x)<<24) & 0x0000FF0000000000LLU) | \
                           (((x)<<8)  & 0x000000FF00000000LLU) | \
                           (((x)>>8)  & 0x00000000FF000000LLU) | \
                           (((x)>>24) & 0x0000000000FF0000LLU) | \
                           (((x)>>40) & 0x000000000000FF00LLU) | \
                           ((x)<<56)

// ----------------------------------------
// test machine endianness
//
int isLittleEndian(void) {
  static union {
    int integer;
    unsigned char byte[sizeof(int)];
  } pack_ = {1};
  return (pack_.byte[0] != 0);
}

// ----------------------------------------
// bin out uint8
//
size_t binOutInt8(uint8_t i, File *file) {
  return fwrite(&i, sizeof(uint8_t), 1, file->stream);
}

// ----------------------------------------
// bin out uint16
//
size_t binOutInt16(uint16_t i, File *file) {
  uint16_t xout = isLittleEndian() ? i : ENDIAN_SWAP16(i);
  return fwrite(&xout, sizeof(uint16_t), 1, file->stream);
}

// ----------------------------------------
// bin out uint32
//
size_t binOutInt32(uint32_t i, File *file) {
  uint32_t xout = isLittleEndian() ? i : ENDIAN_SWAP32(i);
  return fwrite(&xout, sizeof(uint32_t), 1, file->stream);
}

// ----------------------------------------
// bin out uint64
//
size_t binOutInt64(uint64_t i, File *file) {
  uint64_t xout = isLittleEndian() ? i : ENDIAN_SWAP64(i);
  return fwrite(&xout, sizeof(uint64_t), 1, file->stream);
}

// ----------------------------------------
// bin out string
// as: size size*char null
//
size_t binOutString(const char *s, File *file) {
  size_t nout = 0;
  uint32_t len = s ? (uint32_t) strlen(s) : 0;
  nout += binOutInt32(len, file);
  nout += fwrite(s, 1, len+1, file->stream);
  return nout;
}

// ----------------------------------------
// bin out raw bytes
//
size_t binOutBytes(const char *s, size_t size, File *file) {
  return fwrite(s, 1, size, file->stream);  
}


