/*
  Hashing and random numbers
*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "ieee754.h"
#include "dtypes.h"
#include "utils.h"
#include "hashing.h"
#include "timefuncs.h"

uint_t nextipow2(uint_t i)
{
    if (i==0) return 1;
    if ((i&(i-1))==0) return i;
    if (i&TOP_BIT) return TOP_BIT;

    // repeatedly clear bottom bit
    while (i&(i-1))
        i = i&(i-1);

    return i<<1;
}

u_int32_t int32hash(u_int32_t a)
{
    a = (a+0x7ed55d16) + (a<<12);
    a = (a^0xc761c23c) ^ (a>>19);
    a = (a+0x165667b1) + (a<<5);
    a = (a+0xd3a2646c) ^ (a<<9);
    a = (a+0xfd7046c5) + (a<<3);
    a = (a^0xb55a4f09) ^ (a>>16);
    return a;
}

u_int64_t int64hash(u_int64_t key)
{
    key = (~key) + (key << 21);            // key = (key << 21) - key - 1;
    key =   key  ^ (key >> 24);
    key = (key + (key << 3)) + (key << 8); // key * 265
    key =  key ^ (key >> 14);
    key = (key + (key << 2)) + (key << 4); // key * 21
    key =  key ^ (key >> 28);
    key =  key + (key << 31);
    return key;
}

u_int32_t int64to32hash(u_int64_t key)
{
    key = (~key) + (key << 18); // key = (key << 18) - key - 1;
    key =   key  ^ (key >> 31);
    key = key * 21;             // key = (key + (key << 2)) + (key << 4);
    key = key ^ (key >> 11);
    key = key + (key << 6);
    key = key ^ (key >> 22);
    return (u_int32_t)key;
}

#include "lookup3.c"

u_int64_t memhash(char* buf, size_t n)
{
    u_int32_t c=0xcafe8881, b=0x4d6a087c;

    hashlittle2(buf, n, &c, &b);
    return (u_int64_t)c | (((u_int64_t)b)<<32);
}

#include "mt19937ar.c"

double rand_double()
{
    union ieee754_double d;

    d.ieee.mantissa0 = random();
    d.ieee.mantissa1 = random();
    d.ieee.negative = 0;
    d.ieee.exponent = IEEE754_DOUBLE_BIAS + 0;    /* 2^0 */
    return d.d - 1.0;
}

float rand_float()
{
    union ieee754_float f;

    f.ieee.mantissa = random();
    f.ieee.negative = 0;
    f.ieee.exponent = IEEE754_FLOAT_BIAS + 0;     /* 2^0 */
    return f.f - 1.0;
}

double randn()
{
    double s, vre, vim, ure, uim;
    static double next = -42;

    if (next != -42) {
        s = next;
        next = -42;
        return s;
    }
    do {
        ure = rand_double();
        uim = rand_double();
        vre = 2*ure - 1;
        vim = 2*uim - 1;
        s = vre*vre + vim*vim;
    } while (s >= 1);
    s = sqrt(-2*log(s)/s);
    next = s * vre;
    return s * vim;
}

void randomize()
{
    u_int64_t tm = i64time();
    init_by_array((unsigned long*)&tm, 2);
}

void llt_init()
{
    /*
      I used this function to guess good values based on epsilon:
      tol(eps) = exp(ln(eps)*-.2334012088721472)*eps
      I derived the constant by hallucinating freely.
    */
    dbl_tolerance(1e-12);
    flt_tolerance(5e-6);

    randomize();
}
