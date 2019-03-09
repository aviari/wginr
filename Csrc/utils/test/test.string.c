// ----------------------------------------------------------------------- 
// $Id: test.string.c 396 2019-01-02 22:53:10Z viari $
// ----------------------------------------------------------------------- 
//
//

#include "../common.h"

static void _print(const char *where, const String s) {
  (void) printf("%s: s=>%s<\n", where, s);
  (void) printf("    len(s)=%zu strlen(s)=%zu capacity(s)=%zu\n", 
                     stringLen(s), strlen(s), stringCapacity(s));
}

int main(int argn, char *argv[]) {

  Unused(argn);
  Unused(argv);

  String s, ss;

  // ------------------  

  s = stringNew(NULL);
  
  _print("LXStrNew_null", s);

  s = stringFree(s);

  // ------------------  

  s = stringNew("abcdef");
  
  _print("stringNew", s);
  
  s = stringCat(s, s);
  s = stringCat(s, s);
  s = stringCat(s, s);

  _print("stringCat", s);
  
  s = stringCpy(s, "ab");

  _print("stringCpy", s);

  s = stringEmpty(s, 0);

  _print("stringEmpty", s);

  s = stringEmpty(s, 1);

  _print("stringEmpty", s);

  s = stringFree(s);
  
  s = stringEmpty(NULL, 1);

  _print("stringEmpty_NULL", s);

  s = stringFree(s);

  s = stringEmpty(NULL, 0);

  _print("stringEmpty_NULL", s);

  s = stringFree(s);

  // ------------------  
  
  s = stringAssert(NULL, 10);
  
  _print("stringAssert_NULL", s);

  s = stringCpy(s, "abc");

  s = stringAssert(s, 20);

  _print("stringAssert", s);

  s = stringFree(s);

  s = stringAssert(NULL, 0);
  
  _print("stringAssert_NULL_0", s);

  s = stringFree(s);

  // ------------------  
  
  s = stringCpy(NULL, "abc");
  
  _print("LXStrcpy_null", s);

  s[0] = '\000';
  
  s = stringAdjust(s, 1);

  _print("stringAdjust", s);
  
  s = stringFree(s);

  // ------------------  

  s = stringNew("abcabcabcabca");
  
  s = stringErase(s, 'a');

  _print("stringErase", s);

  s = stringReplace(s, 'b', 'x');

  _print("stringReplace", s);

  s = stringReplace(s, 'c', 'x');

  _print("stringReplace", s);

  s = stringErase(s, 'x');

  _print("stringErase", s);
  
  s = stringFree(s);
  
  // ------------------  

  s = stringNew("  abc  ");
  
  s = stringTrim(s);

  _print("stringTrim", s);

  s = stringFree(s);

  s = stringNew("  abc");
  
  s = stringTrim(s);

  _print("stringTrim", s);

  s = stringFree(s);

  s = stringNew("   ");
  
  s = stringTrim(s);

  _print("stringTrim", s);

  s = stringFree(s);

  s = stringNew(NULL);
  
  s = stringTrim(s);

  _print("stringTrim", s);

  s = stringFree(s);

  // ------------------  

  s = stringNew("abc");
  
  s = stringInsert(s, "xyz", 1);

  _print("stringInsert", s);

  s = stringInsert(s, "ooo", 0);

  _print("stringInsert", s);

  s = stringInsert(s, "ooo", 10);

  _print("stringInsert", s);

  s = stringInsert(s, "ooo", 9);

  _print("stringInsert", s);

  s = stringFree(s);

  s = stringNew(NULL);

  s = stringInsert(s, "ooo", 0);

  _print("stringInsert", s);

  s = stringFree(s);

  // ------------------  

  s = stringNew("abcdefghi");
  
  ss = stringExtract(s, 0, 2);

  _print("stringExtract", ss);

  ss = stringFree(ss);

  ss = stringExtract(s, 0, 100);

  _print("stringExtract", ss);

  ss = stringFree(ss);

  ss = stringExtract(s, 0, 0);

  _print("stringExtract", ss);

  ss = stringFree(ss);

  ss = stringExtract(s, 8, 8);

  _print("stringExtract", ss);

  ss = stringFree(ss);

  ss = stringExtract(s, 9, 9);

  _print("stringExtract", ss);

  ss = stringFree(ss);

  ss = stringExtract(s, 3, 2);

  _print("stringExtract", ss);

  ss = stringFree(ss);

  s = stringFree(s);

  // ------------------  

  s = stringNew("abcdefghi");
  
  s = stringUpper(s);

  _print("stringUpper", s);

  s = stringLower(s);

  _print("stringLower", s);

  s = stringFree(s);

  // ------------------  

  s = stringNew("abcdefghi");
  
  s = stringReverse(s);

  _print("stringReverse", s);

  s = stringFree(s);

  char buf[] = "abcdefghi";
  
  printf("buffer: %s\n", stringReverse(buf));

  // ------------------ 
  
  s = NULL;
  
  File filin;
  
  (void) printf("sysOpenFile: %d\n",
                 sysOpenFile("test.string.in", "r", &filin));
  
  while (stringGetLine(&s, &filin) > 0) {
    _print("stringGetLine", s);
  }

  (void) sysCloseFile(&filin);
  
  s = stringFree(s);
  
  exit(0);
}
