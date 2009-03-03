#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "ieee754.h"
#include "dtypes.h"
#include "hashing.h"

static uint64_t max_ulps;
static uint32_t flt_max_ulps;

static uint64_t nexti64pow2(uint64_t i)
{
    if (i==0) return 1;
    if ((i&(i-1))==0) return i;
    if (i&BIT63) return BIT63;
    // repeatedly clear bottom bit
    while (i&(i-1))
        i = i&(i-1);
    return i<<1;
}

static uint32_t nexti32pow2(uint32_t i)
{
    if (i==0) return 1;
    if ((i&(i-1))==0) return i;
    if (i&BIT31) return BIT31;
    // repeatedly clear bottom bit
    while (i&(i-1))
        i = i&(i-1);
    return i<<1;
}

void dbl_tolerance(double tol)
{
    max_ulps = nexti64pow2((uint64_t)(tol/DBL_EPSILON));
}

void flt_tolerance(float tol)
{
    flt_max_ulps = nexti32pow2((uint32_t)(tol/FLT_EPSILON));
}

#ifdef __INTEL_COMPILER
static inline int64_t llabs(int64_t j)
{
    return NBABS(j, 64);
}
#else
extern int64_t llabs(int64_t j);
#endif

int dbl_equals(double a, double b)
{
    int64_t aint, bint;

    if (a == b)
        return 1;
    aint = *(int64_t*)&a;
    bint = *(int64_t*)&b;
    if (aint < 0)
        aint = BIT63 - aint;
    if (bint < 0)
        bint = BIT63 - bint;
    /* you'd think it makes no difference whether the result of llabs is
       signed or unsigned, but if it's signed then the case of
       0x8000000000000000 blows up, making 4 == -1 :) */
    if ((uint64_t)llabs(aint-bint) <= max_ulps)
        return 1;
    return 0;
}

int flt_equals(float a, float b)
{
    int32_t aint, bint;

    if (a == b)
        return 1;
    aint = *(int32_t*)&a;
    bint = *(int32_t*)&b;
    if (aint < 0)
        aint = BIT31 - aint;
    if (bint < 0)
        bint = BIT31 - bint;
    if ((uint32_t)abs(aint-bint) <= flt_max_ulps)
        return 1;
    return 0;
}
