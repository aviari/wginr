/* -----------------------------------------------------------------------
 * $Id: libstring.c 396 2019-01-02 22:53:10Z viari $
 * -----------------------------------------------------------------------
 *
 * @brief strings operations
 *
 */

#include "common.h"

#include <string.h>

/* ---------------------------------------------------- */
/* local utilities                                      */
/* we currently write String length, just before the    */
/* actual char* pointer. i.e. at address :              */
/*   ((size_t *) ((str) - _OFFSET))                     */
/* where _OFFSET = sizeof(size_t)                       */
/* ---------------------------------------------------- */

#define _OFFSET         sizeof(size_t)
#define _STRLEN(str)    *((size_t *) ((str) - _OFFSET))

/* -------------------------------------------- */
/* assert string capacity or create string      */
/* return a String                              */
/* -------------------------------------------- */

static String _assert(String str, size_t size) {

  size += _OFFSET + 1;  /* _OFFSET is for size and 1 is for '\0' */

  if (str) {
    str = AllocAssert(str - _OFFSET, size);
  }
  else if ((str = NewN(char, size)) != NULL) {
      *((size_t *) (str)) = 0;
      *(str + _OFFSET) = '\000';
  }

  return (str ? str + _OFFSET : str);
}

/* -------------------------------------------- */
/* reallocate string                            */
/* -------------------------------------------- */
static String _realloc(String str, size_t size) {

  if (str)
    str = (String) Realloc(str - _OFFSET, char, size + _OFFSET + 1);
  
  return (str ? str + _OFFSET : str);
}

/* -------------------------------------------- */
/* copy len bytes from char *src into string    */
/* dst at offset.                               */
/* note: dst size must fit                      */
/* return a String                              */
/* -------------------------------------------- */
static String _copy(String dst, const char *src,
                        size_t offset, size_t len) {
  if (dst) {

    size_t totlen = offset + len;

    _STRLEN(dst) = totlen;

    (void) memcpy(dst + offset, src, len);

    dst[totlen] = '\000';
  }
    
  return dst;
}

/* ---------------------------------------------------- */
/* API                                                  */
/* ---------------------------------------------------- */

/* -------------------------------------------- */
/* create new string                            */
/* -------------------------------------------- */

String stringNew(const char *src) {
  const char *psrc = src ? src : "";
  
  size_t len = strlen(psrc);

  return _copy(_assert(NULL, len), psrc, 0, len);
}

/* -------------------------------------------- */
/* free string                                  */
/* -------------------------------------------- */

String stringFree(String str) {
  if (str) Free(str - _OFFSET);
  return NULL;
}

/* -------------------------------------------- */
/* assert string alloc size is at least         */
/* siz+1 bytes                                  */
/* -------------------------------------------- */

String stringAssert(String str, size_t size) {
   return _assert(str, size);
}

/* -------------------------------------------- */
/* string length                                */
/* -------------------------------------------- */

size_t stringLen(const String str) {
  return str ? _STRLEN(str) : 0;
}

/* -------------------------------------------- */
/* string capacity in bytes                     */
/* -------------------------------------------- */

size_t stringCapacity(const String str) {
  return (str ? AllocSize(str - _OFFSET) - _OFFSET - 1: 0);
}

/* -------------------------------------------- */
/* copy char* into string, increasing memory    */
/* size if necessary.                           */
/* -------------------------------------------- */

String stringCpy(String dst, const char *src) {

  const char *psrc = src ? src : "";
  
  size_t srclen = strlen(psrc);

  return _copy(_assert(dst, srclen), psrc, 0, srclen);
}

/* -------------------------------------------- */
/* catenate string, increasing memory size if   */
/* necessary                                    */
/* -------------------------------------------- */

String stringCat(String dst, const char *src) {

  const char *psrc = src ? src : "";

  size_t srclen = strlen(psrc);
  size_t dstlen = stringLen(dst);
  size_t totlen = srclen + dstlen;

  return _copy(_assert(dst, totlen), psrc, dstlen, srclen);
}

/* -------------------------------------------- */
/* empty string                                 */
/* -------------------------------------------- */

String stringEmpty(String str, int shrinkit) {

  if (str) {
    _STRLEN(str) = 0;
    *str = '\000';
    
    if (shrinkit)
      str = _realloc(str, 0);
  }
  else {
    str = stringNew(NULL);
  }
  
  return str;
}


/* -------------------------------------------- */
/* adjust string length and capacity            */
/* -------------------------------------------- */

String stringAdjust(String str, int shrinkit) {

  if (str) {
    size_t len = strlen(str);
    _STRLEN(str) = len;
    
    if (shrinkit)
      str = _realloc(str, len);
  }
  
  return str;
}

/* -------------------------------------------- */
/* delete char from string                      */
/* -------------------------------------------- */

String stringErase(String str, int c) {

  if (! str) return str;

  char *s, *se;
  size_t len = 0;
        
  for (se = s = str ; *s ; s++) {
    if ((*se = *s) != c) {
      se++; len++;
    }
  }
    
  *se = '\000';
    
  _STRLEN(str) = len;
    
  return str;
}

/* -------------------------------------------- */
/* replace char into string                     */
/* -------------------------------------------- */

String stringReplace(String str, int cfrom, int cto) {

  if (! str) return str;

  char *s;
        
  for (s = str ; *s ; s++) {
    if (*s == cfrom)
      *s = cto;
  }

  return str;
}

/* -------------------------------------------- */
/* insert string                                */
/* -------------------------------------------- */

String stringInsert(String dst, const char *src, size_t pos) {

  const char *psrc = src ? src : "";
    
  size_t srclen = strlen(psrc);
  size_t dstlen = stringLen(dst);

  if ((srclen == 0) || (dstlen < pos))
    return dst;

  size_t totlen = srclen + dstlen;

  String nstr = stringAssert(dst, totlen);
  
  if (! nstr) return dst;
      
  char *s = nstr + dstlen;
  char *t = s + srclen;
    
  *t = '\000';

  dstlen -= pos;
    
  while (dstlen--)
    *--t = *--s;

  while(srclen--)
    *s++ = *psrc++;

  _STRLEN(nstr) = totlen;

  return nstr;
}

/* -------------------------------------------- */
/* extract substring                            */
/* create new string                            */
/* -------------------------------------------- */

String stringExtract(const String str, size_t from, size_t to) {

  String sub = stringNew(NULL);  // may be NULL if memory shortage

  if (to < from) return sub;
  
  size_t len = stringLen(str);

  if (from >= len) return sub;
  
  if (to >= len) to = len - 1;  // len is > 0 so (len - 1) is ok
  
  size_t size = to - from + 1;
  
  sub = stringAssert(sub, size);

  if (! sub) return sub;

  _STRLEN(sub) = size;

  char *s = sub;
  const char *pstr = str + from;

  while (size--)
    *s++ = *pstr++;

  *s = '\000';
  
  return sub;
}

/* -------------------------------------------- */
/* trim trailing spaces                         */
/* -------------------------------------------- */

String stringTrim(String str) {

  if (! str) return str;

  size_t len = stringLen(str);
  char *s = str + len;
    
  for (--s ; (s >= str) && isspace(*s) ; s--)
    len--;

  *++s = '\000';

  _STRLEN(str) = len;

  return str;     
}

/* -------------------------------------------- */
/* upper case                                   */
/* -------------------------------------------- */

#define IS_LOWER(c) (((c) >= 'a') && ((c) <= 'z'))
#define TO_UPPER(c) ((c) - 'a' + 'A')

char *stringUpper(char *str) {

    if (! str) return str;

    char *s;

    for (s = str ; *s ; s++)
        if (IS_LOWER(*s))
            *s = TO_UPPER(*s);

    return str;    
}

#undef IS_LOWER
#undef TO_UPPER

/* -------------------------------------------- */
/* lower case                                   */
/* -------------------------------------------- */

#define IS_UPPER(c) (((c) >= 'A') && ((c) <= 'Z'))
#define TO_LOWER(c) ((c) - 'A' + 'a')

char *stringLower(char *str) {

    if (! str) return str;

    char *s;

    for (s = str ; *s ; s++)
        if (IS_UPPER(*s))
            *s = TO_LOWER(*s);
        
    return str;    
}

#undef IS_UPPER
#undef TO_LOWER

/* -------------------------------------------- */
/* reverse string                               */
/* -------------------------------------------- */

char *stringReverse(char *str) {

    if (! str) return str;

    char *sb, *se, c;

    sb = str;
    se = str + strlen(str) - 1;

    while(sb <= se) {
       c    = *sb;
      *sb++ = *se;
      *se-- = c;
    }

    return str;
}

/* -------------------------------------------- */
/* read arbitrary long String from File         */
/* remove trailing \r \n \t \s                  */
/* return number of chars read, 0 on EOF        */
/* -------------------------------------------- */

size_t stringGetLine(String *str, File *file) {

  char buffer[BUFSIZ+1];    // BUFSIZ in <stdio.h>
  
  size_t readlen = 0;

  *str = stringEmpty(*str, 0);   // reset or create string
  
  while (sysGetLine(buffer, sizeof(buffer), file)) {
  
    size_t len = strlen(buffer);

    readlen += len;

    String tmp = stringCat(*str, buffer);
    
    if (! tmp) // not enough memory
        break;

    *str = tmp;
    
    if ((len == 0) || (buffer[len-1] == '\n'))  // finished
        break;
  }

  *str = stringTrim(*str); // remove trailing spaces
  
  return readlen;
}
