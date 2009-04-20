/*
  Hashing
*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "ieee754.h"
#include "dtypes.h"
#include "utils.h"
#include "hashing.h"
#include "timefuncs.h"
#include "ios.h"
#include "random.h"

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

u_int32_t memhash32(char* buf, size_t n)
{
    u_int32_t c=0xcafe8881, b=0x4d6a087c;

    hashlittle2(buf, n, &c, &b);
    return c;
}

double D_PNAN;
double D_NNAN;
double D_PINF;
double D_NINF;
float  F_PNAN;
float  F_NNAN;
float  F_PINF;
float  F_NINF;

void llt_init()
{
    randomize();

    ios_init_stdstreams();

    D_PNAN = strtod("+NaN",NULL);
    D_NNAN = strtod("-NaN",NULL);
    D_PINF = strtod("+Inf",NULL);
    D_NINF = strtod("-Inf",NULL);
    F_PNAN = strtof("+NaN",NULL);
    F_NNAN = strtof("-NaN",NULL);
    F_PINF = strtof("+Inf",NULL);
    F_NINF = strtof("-Inf",NULL);
}
