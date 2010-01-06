#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "dtypes.h"
#include "bitvector.h"

#ifdef WIN32
#include <malloc.h>
#define alloca _alloca
#endif

// greater than this # of words we use malloc instead of alloca
#define MALLOC_CUTOFF 2000

u_int32_t bitreverse(u_int32_t x)
{
    u_int32_t m;

#ifdef __INTEL_COMPILER
    x = _bswap(x);
#else
    x = (x >> 16)      | (x << 16);        m = 0xff00ff00;
    x = ((x & m) >> 8) | ((x & ~m) << 8);
#endif
    m = 0xf0f0f0f0;
    x = ((x & m) >> 4) | ((x & ~m) << 4);  m = 0xcccccccc;
    x = ((x & m) >> 2) | ((x & ~m) << 2);  m = 0xaaaaaaaa;
    x = ((x & m) >> 1) | ((x & ~m) << 1);

    return x;
}

// shift all bits in a long bit vector
// n is # of int32s to consider, s is shift distance
// lowest bit-index is bit 0 of word 0
// TODO: handle boundary case of shift distance >= data size?
void bitvector_shr(u_int32_t *b, size_t n, u_int32_t s)
{
    u_int32_t i;
    if (s == 0 || n == 0) return;
    i = (s>>5);
    if (i) {
        n -= i;
        memmove(b, &b[i], n*4);
        memset(&b[n], 0, i*4);
        s &= 31;
    }
    for(i=0; i < n-1; i++) {
        b[i] = (b[i]>>s) | (b[i+1]<<(32-s));
    }
    b[i]>>=s;
}

// out-of-place version, good for re-aligning a strided submatrix to
// linear representation when a copy is needed
// assumes that dest has the same amount of space as source, even if it
// wouldn't have been necessary to hold the shifted bits
void bitvector_shr_to(u_int32_t *dest, u_int32_t *b, size_t n, u_int32_t s)
{
    u_int32_t i, j;
    if (n == 0) return;
    if (s == 0) {
        memcpy(dest, b, n*4);
        return;
    }
    j = (s>>5);
    if (j) {
        n -= j;
        memset(&dest[n], 0, j*4);
        s &= 31;
        b = &b[j];
    }
    for(i=0; i < n-1; i++) {
        dest[i] = (b[i]>>s) | (b[i+1]<<(32-s));
    }
    dest[i] = b[i]>>s;
}

void bitvector_shl(u_int32_t *b, size_t n, u_int32_t s)
{
    u_int32_t i, scrap=0, temp;
    if (s == 0 || n == 0) return;
    i = (s>>5);
    if (i) {
        n -= i;
        memmove(&b[i], b, n*4);
        memset(b, 0, i*4);
        s &= 31;
        b = &b[i];
    }
    for(i=0; i < n; i++) {
        temp = (b[i]<<s) | scrap;
        scrap = b[i]>>(32-s);
        b[i] = temp;
    }
}

// if dest has more space than source, set scrap to true to keep the
// top bits that would otherwise be shifted out
void bitvector_shl_to(u_int32_t *dest, u_int32_t *b, size_t n, u_int32_t s,
                      bool_t scrap)
{
    u_int32_t i, j, sc=0;
    if (n == 0) return;
    if (s == 0) {
        memcpy(dest, b, n*4);
        return;
    }
    j = (s>>5);
    if (j) {
        n -= j;
        memset(dest, 0, j*4);
        s &= 31;
        dest = &dest[j];
    }
    for(i=0; i < n; i++) {
        dest[i] = (b[i]<<s) | sc;
        sc = b[i]>>(32-s);
    }
    if (scrap)
        dest[i] = sc;
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

void bitvector_not(u_int32_t *b, u_int32_t offs, u_int32_t nbits)
{
    index_t i;
    u_int32_t nw, tail;
    u_int32_t mask;

    if (nbits == 0) return;
    nw = (offs+nbits+31)>>5;

    if (nw == 1) {
        mask = (lomask(nbits)<<offs);
        b[0] ^= mask;
        return;
    }

    mask = ~lomask(offs);
    b[0]^=mask;

    for(i=1; i < nw-1; i++)
        b[i] = ~b[i];

    tail = (offs+nbits)&31;
    if (tail==0) {
        b[i] = ~b[i];
    }
    else {
        mask = lomask(tail);
        b[i]^=mask;
    }
}

// constant-space bit vector copy in a single pass, with arbitrary
// offsets and lengths. to get this right, there are 16 cases to handle!
#define BITVECTOR_COPY_OP(name, OP)                                          \
void bitvector_##name(u_int32_t *dest, u_int32_t doffs,                      \
                      u_int32_t *src, u_int32_t soffs, u_int32_t nbits)      \
{                                                                            \
    index_t i;                                                               \
    u_int32_t s, nw, tail, snw;                                              \
    u_int32_t mask, scrap;                                                   \
                                                                             \
    if (nbits == 0) return;                                                  \
    nw = (doffs+nbits+31)>>5;                                                \
                                                                             \
    if (soffs == doffs) {                                                    \
        if (nw == 1) {                                                       \
            mask = (lomask(nbits)<<doffs);                                   \
            dest[0] = (dest[0] & ~mask) | (OP(src[0]) & mask);               \
            return;                                                          \
        }                                                                    \
        mask = ~lomask(doffs);                                               \
        dest[0] = (dest[0] & ~mask) | (OP(src[0]) & mask);                   \
        for(i=1; i < nw-1; i++)                                              \
            dest[i] = OP(src[i]);                                            \
        tail = (doffs+nbits)&31;                                             \
        if (tail==0) { dest[i]=src[i]; } else {                              \
            mask = lomask(tail);                                             \
            dest[i] = (dest[i] & ~mask) | (OP(src[i]) & mask); }             \
        return;                                                              \
    }                                                                        \
    snw = (soffs+nbits+31)>>5;                                               \
    if (soffs < doffs) {                                                     \
        s = doffs-soffs;                                                     \
        if (nw == 1) {                                                       \
            mask = (lomask(nbits)<<doffs);                                   \
            dest[0] = (dest[0] & ~mask) | ((OP(src[0])<<s) & mask);          \
            return;                                                          \
        }                                                                    \
        mask = ~lomask(doffs);                                               \
        dest[0] = (dest[0] & ~mask) | ((OP(src[0])<<s) & mask);              \
        scrap = OP(src[0])>>(32-s);                                          \
        for(i=1; i < snw-1; i++) {                                           \
            dest[i] = (OP(src[i])<<s) | scrap;                               \
            scrap = OP(src[i])>>(32-s);                                      \
        }                                                                    \
        tail = (doffs+nbits)&31;                                             \
        if (tail==0) { mask=ONES32; } else { mask = lomask(tail); }          \
        if (snw == nw) {                                                     \
            dest[i] = (dest[i] & ~mask) | (((OP(src[i])<<s)|scrap) & mask);  \
        }                                                                    \
        else /* snw < nw */ {                                                \
            if (snw == 1) {                                                  \
                dest[i] = (dest[i] & ~mask) |                                \
                    (((OP(src[i])<<s) | scrap) & mask);                      \
            }                                                                \
            else {                                                           \
                dest[i] = (OP(src[i])<<s) | scrap;                           \
                scrap = OP(src[i])>>(32-s);                                  \
                i++;                                                         \
                dest[i] = (dest[i] & ~mask) | (scrap & mask);                \
            }                                                                \
        }                                                                    \
    }                                                                        \
    else {                                                                   \
        s = soffs-doffs;                                                     \
        if (snw == 1) {                                                      \
            mask = (lomask(nbits)<<doffs);                                   \
            dest[0] = (dest[0] & ~mask) | ((OP(src[0])>>s) & mask);          \
            return;                                                          \
        }                                                                    \
        if (nw == 1) {                                                       \
            mask = (lomask(nbits)<<doffs);                                   \
            dest[0] = (dest[0] & ~mask) |                                    \
                (((OP(src[0])>>s)|(OP(src[1])<<(32-s))) & mask);             \
            return;                                                          \
        }                                                                    \
        mask = ~lomask(doffs);                                               \
        dest[0] = (dest[0] & ~mask) |                                        \
            (((OP(src[0])>>s)|(OP(src[1])<<(32-s))) & mask);                 \
        for(i=1; i < nw-1; i++) {                                            \
            dest[i] = (OP(src[i])>>s) | (OP(src[i+1])<<(32-s));              \
        }                                                                    \
        tail = (doffs+nbits)&31;                                             \
        if (tail==0) { mask=ONES32; } else { mask = lomask(tail); }          \
        if (snw == nw) {                                                     \
            dest[i] = (dest[i] & ~mask) | ((OP(src[i])>>s) & mask);          \
        }                                                                    \
        else /* snw > nw */ {                                                \
            dest[i] = (dest[i] & ~mask) |                                    \
                (((OP(src[i])>>s)|(OP(src[i+1])<<(32-s))) & mask);           \
        }                                                                    \
    }                                                                        \
}

#define BV_COPY(a) (a)
#define BV_NOT(a) (~(a))
BITVECTOR_COPY_OP(copy, BV_COPY)
BITVECTOR_COPY_OP(not_to, BV_NOT)

// right-shift the bits in one logical "row" of a long 2d bit vector
/*
void bitvector_shr_row(u_int32_t *b, u_int32_t offs, size_t nbits, u_int32_t s)
{
}
*/

// copy from source to dest while reversing bit-order
// assumes dest offset == 0
// assumes source and dest don't overlap
// assumes offset < 32
void bitvector_reverse_to(u_int32_t *dest, u_int32_t *src, u_int32_t soffs,
                          u_int32_t nbits)
{
    index_t i;
    u_int32_t nw, tail;

    if (nbits == 0) return;

    nw = (soffs+nbits+31)>>5;
    // first, reverse the words while reversing bit order within each word
    for(i=0; i < nw/2; i++) {
        dest[i]      = bitreverse(src[nw-i-1]);
        dest[nw-i-1] = bitreverse(src[i]);
    }
    if (nw&0x1)
        dest[i] = bitreverse(src[i]);

    tail = (soffs+nbits)&31;
    if (tail)
        bitvector_shr(dest, nw, 32-tail);
}

void bitvector_reverse(u_int32_t *b, u_int32_t offs, u_int32_t nbits)
{
    index_t i;
    u_int32_t nw, tail;
    u_int32_t *temp;

    if (nbits == 0) return;

    nw = (offs+nbits+31)>>5;
    temp = (nw > MALLOC_CUTOFF) ? malloc(nw*4) : alloca(nw*4);
    for(i=0; i < nw/2; i++) {
        temp[i]      = bitreverse(b[nw-i-1]);
        temp[nw-i-1] = bitreverse(b[i]);
    }
    if (nw&0x1)
        temp[i] = bitreverse(b[i]);

    tail = (offs+nbits)&31;
    bitvector_copy(b, offs, temp, (32-tail)&31, nbits);
    if (nw > MALLOC_CUTOFF) free(temp);
}

u_int64_t bitvector_count(u_int32_t *b, u_int32_t offs, u_int64_t nbits)
{
    size_t i, nw;
    u_int32_t ntail;
    u_int64_t ans;

    if (nbits == 0) return 0;
    nw = ((u_int64_t)offs+nbits+31)>>5;

    if (nw == 1) {
        return count_bits(b[0] & (lomask(nbits)<<offs));
    }

    ans = count_bits(b[0]>>offs);  // first end cap

    for(i=1; i < nw-1; i++) {
        /* popcnt can be computed branch-free, so these special cases
           probably don't help much */
        /*
        v = b[i];
        if (v == 0)
            continue;
        if (v == ONES32)
            ans += 32;
        else
        */
        ans += count_bits(b[i]);
    }

    ntail = (offs+(u_int32_t)nbits)&31;
    ans += count_bits(b[i]&(ntail>0?lomask(ntail):ONES32));  // last end cap

    return ans;
}

u_int32_t bitvector_any0(u_int32_t *b, u_int32_t offs, u_int32_t nbits)
{
    index_t i;
    u_int32_t nw, tail;
    u_int32_t mask;

    if (nbits == 0) return 0;
    nw = (offs+nbits+31)>>5;

    if (nw == 1) {
        mask = (lomask(nbits)<<offs);
        if ((b[0] & mask) != mask) return 1;
        return 0;
    }

    mask = ~lomask(offs);
    if ((b[0] & mask) != mask) return 1;

    for(i=1; i < nw-1; i++) {
        if (b[i] != ONES32) return 1;
    }

    tail = (offs+nbits)&31;
    if (tail==0) {
        if (b[i] != ONES32) return 1;
    }
    else {
        mask = lomask(tail);
        if ((b[i] & mask) != mask) return 1;
    }
    return 0;
}

u_int32_t bitvector_any1(u_int32_t *b, u_int32_t offs, u_int32_t nbits)
{
    index_t i;
    u_int32_t nw, tail;
    u_int32_t mask;

    if (nbits == 0) return 0;
    nw = (offs+nbits+31)>>5;

    if (nw == 1) {
        mask = (lomask(nbits)<<offs);
        if ((b[0] & mask) != 0) return 1;
        return 0;
    }

    mask = ~lomask(offs);
    if ((b[0] & mask) != 0) return 1;

    for(i=1; i < nw-1; i++) {
        if (b[i] != 0) return 1;
    }

    tail = (offs+nbits)&31;
    if (tail==0) {
        if (b[i] != 0) return 1;
    }
    else {
        mask = lomask(tail);
        if ((b[i] & mask) != 0) return 1;
    }
    return 0;
}

static void adjust_offset_to(u_int32_t *dest, u_int32_t *src, u_int32_t nw,
                             u_int32_t soffs, u_int32_t newoffs)
{
    if (newoffs > soffs)
        bitvector_shl_to(dest, src, nw, newoffs-soffs, 1);
    else
        bitvector_shr_to(dest, src, nw, soffs-newoffs);
}

#define BITVECTOR_BINARY_OP_TO(opname, OP)                                   \
void bitvector_##opname##_to(u_int32_t *dest, u_int32_t doffs,               \
                             u_int32_t *a, u_int32_t aoffs,                  \
                             u_int32_t *b, u_int32_t boffs, u_int32_t nbits) \
{                                                                            \
    u_int32_t nw = (doffs+nbits+31)>>5;                                      \
    u_int32_t *temp = nw>MALLOC_CUTOFF ? malloc((nw+1)*4) : alloca((nw+1)*4);\
    u_int32_t i, anw, bnw;                                                   \
    if (aoffs == boffs) {                                                    \
        anw = (aoffs+nbits+31)>>5;                                           \
    }                                                                        \
    else if (aoffs == doffs) {                                               \
        bnw = (boffs+nbits+31)>>5;                                           \
        adjust_offset_to(temp, b, bnw, boffs, aoffs);                        \
        b = temp; anw = nw;                                                  \
    }                                                                        \
    else {                                                                   \
        anw = (aoffs+nbits+31)>>5;                                           \
        bnw = (boffs+nbits+31)>>5;                                           \
        adjust_offset_to(temp, a, anw, aoffs, boffs);                        \
        a = temp; aoffs = boffs; anw = bnw;                                  \
    }                                                                        \
    for(i=0; i < anw; i++) temp[i] = OP(a[i], b[i]);                         \
    bitvector_copy(dest, doffs, temp, aoffs, nbits);                         \
    if (nw>MALLOC_CUTOFF) free(temp);                                        \
}

#define BV_AND(a,b) ((a)&(b))
#define BV_OR(a,b)  ((a)|(b))
#define BV_XOR(a,b) ((a)^(b))
BITVECTOR_BINARY_OP_TO(and, BV_AND)
BITVECTOR_BINARY_OP_TO(or,  BV_OR)
BITVECTOR_BINARY_OP_TO(xor, BV_XOR)
