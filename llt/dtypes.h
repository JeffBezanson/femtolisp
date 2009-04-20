#ifndef __DTYPES_H_
#define __DTYPES_H_

/*
  This file defines sane integer types for our target platforms. This
  library only runs on machines with the following characteristics:

  - supports integer word sizes of 8, 16, 32, and 64 bits
  - uses unsigned and signed 2's complement representations
  - all pointer types are the same size
  - there is an integer type with the same size as a pointer

  Some features require:
  - IEEE 754 single- and double-precision floating point

  We assume the LP64 convention for 64-bit platforms.
*/

#include "config.h"

typedef int bool_t;
/* unfortunately we can't make this an enum, since false is an invalid
   enum label in C++ (since it's a keyword) */
#define false (0)
#define true (1)

#if defined(__INTEL_COMPILER) && defined(WIN32)
# define STATIC_INLINE static
# define INLINE
# ifdef BITS64
typedef unsigned long size_t;
# else
typedef unsigned int size_t;
# endif
#else
# define STATIC_INLINE static inline
# define INLINE inline
#endif

typedef unsigned char  byte_t;   /* 1 byte */
#if defined(WIN32)
typedef short int16_t;
typedef int int32_t;
typedef long long int64_t;
typedef unsigned char u_int8_t;
typedef unsigned short u_int16_t;
typedef unsigned int u_int32_t;
#ifdef BITS64
typedef unsigned long u_int64_t;
#else
typedef unsigned long long u_int64_t;
#endif
#ifdef __INTEL_COMPILER
typedef signed char int8_t;
typedef short int16_t;
typedef int int32_t;
#endif
#else
#include <sys/types.h>
#endif

#ifdef BITS64
#define TOP_BIT 0x8000000000000000
#define NBITS 64
typedef unsigned long uint_t;  // preferred int type on platform
typedef long int_t;
typedef int64_t offset_t;
typedef u_int64_t index_t;
typedef int64_t ptrint_t; // pointer-size int
typedef u_int64_t u_ptrint_t
#else
#define TOP_BIT 0x80000000
#define NBITS 32
typedef unsigned long uint_t;
typedef long int_t;
typedef int32_t offset_t;
typedef u_int32_t index_t;
typedef int32_t ptrint_t;
typedef u_int32_t u_ptrint_t;
#endif

typedef u_int8_t  uint8_t;
typedef u_int16_t uint16_t;
typedef u_int32_t uint32_t;
typedef u_int64_t uint64_t;
typedef u_ptrint_t uptrint_t;

#define ALIGN(x, sz) (((x) + (sz-1)) & (-sz))

// branch prediction annotations
#ifdef __GNUC__
#define __unlikely(x) __builtin_expect(!!(x), 0)
#define __likely(x)   __builtin_expect(!!(x), 1)
#else
#define __unlikely(x) (x)
#define __likely(x)   (x)
#endif

#define DBL_MAXINT 9007199254740992LL
#define FLT_MAXINT 16777216
#define U64_MAX    18446744073709551615ULL
#define S64_MAX    9223372036854775807LL
#define S64_MIN    (-S64_MAX - 1LL)
#define BIT63      0x8000000000000000LL
#define BIT31      0x80000000

#define DBL_EPSILON      2.2204460492503131e-16
#define FLT_EPSILON      1.1920928e-7
#define DBL_MAX          1.7976931348623157e+308
#define DBL_MIN          2.2250738585072014e-308
#define FLT_MAX          3.402823466e+38
#define FLT_MIN          1.175494351e-38
#define LOG2_10          3.3219280948873626
#define rel_zero(a, b) (fabs((a)/(b)) < DBL_EPSILON)
#define sign_bit(r) ((*(int64_t*)&(r)) & BIT63)
#define LABS(n) (((n)^((n)>>(NBITS-1))) - ((n)>>(NBITS-1)))
#define NBABS(n,nb) (((n)^((n)>>((nb)-1))) - ((n)>>((nb)-1)))
#define DFINITE(d) (((*(int64_t*)&(d))&0x7ff0000000000000LL)!=0x7ff0000000000000LL)

extern double D_PNAN;
extern double D_NNAN;
extern double D_PINF;
extern double D_NINF;
extern float  F_PNAN;
extern float  F_NNAN;
extern float  F_PINF;
extern float  F_NINF;

typedef enum { T_INT8, T_UINT8, T_INT16, T_UINT16, T_INT32, T_UINT32,
               T_INT64, T_UINT64, T_FLOAT, T_DOUBLE } numerictype_t;

#define N_NUMTYPES ((int)T_DOUBLE+1)

#ifdef BITS64
# define T_LONG T_INT64
# define T_ULONG T_UINT64
#else
# define T_LONG T_INT32
# define T_ULONG T_UINT32
#endif

#endif
