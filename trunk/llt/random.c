/*
  random numbers
*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "ieee754.h"
#include "dtypes.h"
#include "utils.h"
#include "random.h"
#include "timefuncs.h"
#include "ios.h"

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
