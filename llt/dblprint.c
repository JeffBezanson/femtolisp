#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "ieee754.h"
#include "dtypes.h"

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

int double_exponent(double d)
{
    union ieee754_double dl;

    dl.d = d;
    return dl.ieee.exponent - IEEE754_DOUBLE_BIAS;
}

double double_mantissa(double d)
{
    union ieee754_double dl;

    dl.d = d;
    dl.ieee.exponent = IEEE754_DOUBLE_BIAS;
    dl.ieee.negative = 0;
    return dl.d;
}

int float_exponent(float f)
{
    union ieee754_float fl;

    fl.f = f;
    return fl.ieee.exponent - IEEE754_FLOAT_BIAS;
}

float float_mantissa(float f)
{
    union ieee754_float fl;

    fl.f = f;
    fl.ieee.exponent = IEEE754_FLOAT_BIAS;
    fl.ieee.negative = 0;
    return fl.f;
}

void snprint_real(char *s, size_t cnt, double r,
                  int width,    // printf field width, or 0
                  int dec,      // # decimal digits desired, recommend 16
                  // # of zeros in .00...0x before using scientific notation
                  // recommend 3-4 or so
                  int max_digs_rt,
                  // # of digits left of decimal before scientific notation
                  // recommend 10
                  int max_digs_lf)
{
    int mag;
    double fpart, temp;
    char format[8];
    char num_format[3];
    int sz, keepz=0;

    s[0] = '\0';
    if (width == -1) {
        width = 0;
        keepz=1;
    }
    if (isnan(r)) {
        if (sign_bit(r))
            strncpy(s, "-nan", cnt);
        else
            strncpy(s, "nan", cnt);
        return;
    }
    if (r == 0) {
        strncpy(s, "0", cnt);
        return;
    }

    num_format[0] = 'l';
    num_format[2] = '\0';

    mag = double_exponent(r);

    mag = (int)(((double)mag)/LOG2_10 + 0.5);
    if (r == 0)
        mag = 0;
    if ((mag > max_digs_lf-1) || (mag < -max_digs_rt)) {
        num_format[1] = 'e';
        temp = r/pow(10, mag);      /* see if number will have a decimal */
        fpart = temp - floor(temp); /* when written in scientific notation */
    }
    else {
        num_format[1] = 'f';
        fpart = r - floor(r);
    }
    if (fpart == 0)
        dec = 0;
    if (width == 0) {
        snprintf(format, 8, "%%.%d%s", dec, num_format);
    }
    else {
        snprintf(format, 8, "%%%d.%d%s", width, dec, num_format);
    }
    sz = snprintf(s, cnt, format, r);
    /* trim trailing zeros from fractions. not when using scientific
       notation, since we might have e.g. 1.2000e+100. also not when we
       need a specific output width */
    if (width == 0 && !keepz) {
        if (sz > 2 && fpart && num_format[1]!='e') {
            while (s[sz-1] == '0') {
                s[sz-1]='\0';
                sz--;
            }
            // don't need trailing .
            if (s[sz-1] == '.') {
                s[sz-1] = '\0';
                sz--;
            }
        }
    }
    // TODO. currently 1.1e20 prints as 1.1000000000000000e+20; be able to
    // get rid of all those zeros.
}
