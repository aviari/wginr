//
#include <stdlib.h>

typedef void *Ptr;

Ptr r_malloc(size_t size);
Ptr r_calloc(size_t count, size_t size);
Ptr r_realloc(Ptr ptr, size_t size);
int r_check(Ptr ptr);
void r_free(Ptr ptr);

void r_ptr2str(Ptr ptr, char *buffer);
Ptr  r_str2ptr(const char *buffer);
