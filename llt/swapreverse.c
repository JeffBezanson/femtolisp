#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <alloca.h>
#include "dtypes.h"
#include "utils.h"

void memswap(char *a, char *b, size_t sz)
{
    int8_t i8;
    int32_t i32;
    int32_t *a4, *b4;

    if (sz < 4) {
        while (sz--) {
            i8 = *a;
            *a++ = *b;
            *b++ = i8;
        }
    }
    else {
        while (sz & 0x3) {
            i8 = *a;
            *a++ = *b;
            *b++ = i8;
            sz--;
        }
        a4 = (int32_t*)a;
        b4 = (int32_t*)b;
        sz >>= 2;
        while (sz--) {
            i32 = *a4;
            *a4++ = *b4;
            *b4++ = i32;
        }
    }
}

void memreverse(char *a, size_t n, size_t elsz)
{
    int64_t i64, *pi64;
    int32_t i32, *pi32;
    int16_t i16, *pi16;
    int8_t i8;
    size_t i;
    char *temp;
    size_t eli, tot;

    if (n==0 || elsz==0) return;
    switch(elsz) {
    case 16:
        pi64 = (int64_t*)a;
        for(i=0; i < n/2; i++) {
            i64 = pi64[2*i];
            pi64[2*i] = pi64[2*(n-i-1)];
            pi64[2*(n-i-1)] = i64;

            i64 = pi64[2*i+1];
            pi64[2*i+1] = pi64[2*(n-i-1)+1];
            pi64[2*(n-i-1)+1] = i64;
        }
        break;
    case 8:
        pi64 = (int64_t*)a;
        for(i=0; i < n/2; i++) {
            i64 = pi64[i];
            pi64[i] = pi64[n-i-1];
            pi64[n-i-1] = i64;
        }
        break;
    case 4:
        pi32 = (int32_t*)a;
        for(i=0; i < n/2; i++) {
            i32 = pi32[i];
            pi32[i] = pi32[n-i-1];
            pi32[n-i-1] = i32;
        }
        break;
    case 2:
        pi16 = (int16_t*)a;
        for(i=0; i < n/2; i++) {
            i16 = pi16[i];
            pi16[i] = pi16[n-i-1];
            pi16[n-i-1] = i16;
        }
        break;
    case 1:
        for(i=0; i < n/2; i++) {
            i8 = a[i];
            a[i] = a[n-i-1];
            a[n-i-1] = i8;
        }
        break;
    default:
        tot = n*elsz;
        if (elsz < 4097)
            temp = alloca(elsz);
        else
            temp = malloc(elsz);

        if (temp != NULL) {
            for(i=0, eli=0; i < n/2; i++, eli+=elsz) {
                memcpy(temp, &a[eli], elsz);
                memcpy(&a[eli], &a[tot-eli-elsz], elsz);
                memcpy(&a[tot-eli-elsz], temp, elsz);
            }

            if (elsz >= 4097)
                free(temp);
        }
        break;
    }
}

void memreverse_to(char *dest, char *a, size_t n, size_t elsz)
{
    int64_t *pi64, *di64;
    int32_t *pi32, *di32;
    int16_t *pi16, *di16;
    size_t i;
    size_t eli, tot;
    if (n==0 || elsz==0) return;
    switch(elsz) {
    case 16:
        pi64 = (int64_t*)a;
        di64 = (int64_t*)dest;
        for(i=0; i < n/2; i++) {
            di64[2*i] = pi64[2*(n-i-1)];
            di64[2*(n-i-1)] = pi64[2*i];

            di64[2*i+1] = pi64[2*(n-i-1)+1];
            di64[2*(n-i-1)+1] = pi64[2*i+1];
        }
        if (n&0x1) {
            di64[2*i] = pi64[2*i];
            di64[2*i+1] = pi64[2*i+1];
        }
        break;
    case 8:
        pi64 = (int64_t*)a;
        di64 = (int64_t*)dest;
        for(i=0; i < n/2; i++) {
            di64[i] = pi64[n-i-1];
            di64[n-i-1] = pi64[i];
        }
        if (n&0x1)
            di64[i] = pi64[i];
        break;
    case 4:
        pi32 = (int32_t*)a;
        di32 = (int32_t*)dest;
        for(i=0; i < n/2; i++) {
            di32[i] = pi32[n-i-1];
            di32[n-i-1] = pi32[i];
        }
        if (n&0x1)
            di32[i] = pi32[i];
        break;
    case 2:
        pi16 = (int16_t*)a;
        di16 = (int16_t*)dest;
        for(i=0; i < n/2; i++) {
            di16[i] = pi16[n-i-1];
            di16[n-i-1] = pi16[i];
        }
        if (n&0x1)
            di16[i] = pi16[i];
        break;
    case 1:
        for(i=0; i < n/2; i++) {
            dest[i] = a[n-i-1];
            dest[n-i-1] = a[i];
        }
        if (n&0x1)
            dest[i] = a[i];
        break;
    default:
        tot = n*elsz;
        for(i=0, eli=0; i < n/2; i++, eli+=elsz) {
            memcpy(&dest[eli], &a[tot - eli - elsz], elsz);
            memcpy(&dest[tot - eli - elsz], &a[eli], elsz);
        }
        if (n&0x1)
            memcpy(&dest[eli], &a[eli], elsz);
        break;
    }
}
