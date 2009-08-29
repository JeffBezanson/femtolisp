#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <alloca.h>
#include "dtypes.h"
#include "utils.h"

#define ALIGNED_TO_ACTUAL(p) (((char*)p) - ((long*)p)[-1])

static void *aligned_ptr(char *ptr, size_t align_size)
{
    char *ptr2, *aligned_ptr;

    ptr2 = ptr + sizeof(long);
    aligned_ptr = (char*)LLT_ALIGN(((uptrint_t)ptr2), align_size);

    ((long*)aligned_ptr)[-1] = (long)(aligned_ptr - ptr);

    return aligned_ptr;
}

/* align_size has to be a power of two */
void *malloc_aligned(size_t size, size_t align_size)
{
    char *ptr;

    ptr = (char*)malloc(size + align_size-1 + sizeof(long));
    if (ptr == NULL)
        return NULL;

    return aligned_ptr(ptr, align_size);
}

void free_aligned(void *ptr)
{
    free(ALIGNED_TO_ACTUAL(ptr));
}

void *realloc_aligned(void *ptr, size_t size, size_t align_size)
{
    char *pnew;

    if (ptr != NULL)
        ptr = ALIGNED_TO_ACTUAL(ptr);
    pnew = realloc(ptr, size + align_size-1 + sizeof(long));
    if (pnew == NULL)
        return NULL;

    return aligned_ptr(pnew, align_size);
}
