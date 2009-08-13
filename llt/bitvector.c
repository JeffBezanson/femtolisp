/*
  bit vector primitives

  todo:
  * reverse
  * nreverse
 (- rotate left/right)
  * shl_to
  * not
  - shr_row, shl_row

  These routines are the back end supporting bit matrices. Many operations
  on bit matrices are slow (such as accessing or setting a single element!)
  but certain operations are privileged and lend themselves to extremely
  efficient implementation due to the bit-vector nature of machine integers.
  These are:
  done:
    &  |  $  ~  copy  reverse  fill  sum  prod
  todo:
    shift  trans  rowswap
  would be nice:
    channel  interleave

  Important note:
  Out-of-place functions always assume dest and source have the same amount
  of space available.

  shr_to, shl_to, not_to, and reverse_to assume source and dest don't overlap
  and_to, or_to, and xor_to allow overlap.
*/

#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "dtypes.h"
#include "bitvector.h"

#ifdef WIN32
#include <malloc.h>
#endif

u_int32_t *bitvector_resize(u_int32_t *b, u_int64_t n, int initzero)
{
    u_int32_t *p;
    size_t sz = ((n+31)>>5) * 4;
    p = realloc(b, sz);
    if (p == NULL) return NULL;
    if (initzero) memset(p, 0, sz);
    return p;
}

u_int32_t *bitvector_new(u_int64_t n, int initzero)
{
    return bitvector_resize(NULL, n, initzero);
}

size_t bitvector_nwords(u_int64_t nbits)
{
    return ((nbits+31)>>5) * 4;
}

void bitvector_set(u_int32_t *b, u_int64_t n, u_int32_t c)
{
    if (c)
        b[n>>5] |= (1<<(n&31));
    else
        b[n>>5] &= ~(1<<(n&31));
}

u_int32_t bitvector_get(u_int32_t *b, u_int64_t n)
{
    return b[n>>5] & (1<<(n&31));
}

// set nbits to c, starting at given bit offset
// assumes offs < 32
void bitvector_fill(u_int32_t *b, u_int32_t offs, u_int32_t c, u_int32_t nbits)
{
    index_t i;
    u_int32_t nw, tail;
    u_int32_t mask;

    if (nbits == 0) return;
    nw = (offs+nbits+31)>>5;

    if (nw == 1) {
        mask = (lomask(nbits)<<offs);
        if (c) b[0]|=mask; else b[0]&=(~mask);
        return;
    }

    mask = lomask(offs);
    if (c) b[0]|=(~mask); else b[0]&=mask;

    if (c) mask=ONES32; else mask = 0;
    for(i=1; i < nw-1; i++)
        b[i] = mask;

    tail = (offs+nbits)&31;
    if (tail==0) {
        b[i] = mask;
    }
    else {
        mask = lomask(tail);
        if (c) b[i]|=mask; else b[i]&=(~mask);
    }
}
