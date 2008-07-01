#ifndef __UTILS_H_
#define __UTILS_H_

/* these functions byteswap any-size units --------------------- */
void bswap(byte_t *s, size_t n);
void bswap_to(byte_t *dest, byte_t *src, size_t n);
void bswap_buffer(byte_t *data, size_t sz, size_t npts);
/* ------------------------------------------------------------- */

/* reverse the order of elements of any size, in place or out of place */
/* n is the number of elements */
void memreverse(char *a, size_t n, size_t elsz);
void memreverse_to(char *dest, char *a, size_t n, size_t elsz);

/* swap the contents of two buffers */
void memswap(char *a, char *b, size_t sz);

/* allocating aligned blocks ----------------------------------- */
void *malloc_aligned(size_t size, size_t align_size);
void free_aligned(void *ptr);
void *realloc_aligned(void *ptr, size_t size, size_t align_size);
/* ------------------------------------------------------------- */

int dbl_equals(double a, double b);
int flt_equals(float a, float b);
void dbl_tolerance(double tol);
void flt_tolerance(float tol);
int double_exponent(double d);
double double_mantissa(double d);
int float_exponent(float f);
float float_mantissa(float f);
void snprint_real(char *s, size_t cnt, double r,
                  int width,    // printf field width, or 0
                  int dec,      // # decimal digits desired, recommend 16
                  // # of zeros in .00...0x before using scientific notation
                  // recommend 3-4 or so
                  int max_digs_rt,
                  // # of digits left of decimal before scientific notation
                  // recommend 10
                  int max_digs_lf);
void snprint_cplx(char *s, size_t cnt, double re, double im,
                  // args to pass on to snprint_real
                  int width, int dec,
                  int max_digs_rt, int max_digs_lf,
                  // print spaces around sign in a+bi
                  int spflag);

extern double trunc(double x);

STATIC_INLINE double fpart(double arg)
{
    return arg - trunc(arg);
}

#define ipart(x) trunc(x)

numerictype_t effective_numerictype(double r);
double conv_to_double(void *data, numerictype_t tag);
void conv_from_double(void *data, double d, numerictype_t tag);
int64_t conv_to_int64(void *data, numerictype_t tag);
uint64_t conv_to_uint64(void *data, numerictype_t tag);
int32_t conv_to_int32(void *data, numerictype_t tag);
uint32_t conv_to_uint32(void *data, numerictype_t tag);
#ifdef BITS64
#define conv_to_long conv_to_int64
#define conv_to_ulong conv_to_uint64
#else
#define conv_to_long conv_to_int32
#define conv_to_ulong conv_to_uint32
#endif
int cmp_same_lt(void *a, void *b, numerictype_t tag);
int cmp_same_eq(void *a, void *b, numerictype_t tag);
int cmp_lt(void *a, numerictype_t atag, void *b, numerictype_t btag);
int cmp_eq(void *a, numerictype_t atag, void *b, numerictype_t btag);

#ifdef ARCH_X86_64
#  define LEGACY_REGS "=Q"
#else
#  define LEGACY_REGS "=q"
#endif

#if !defined(__INTEL_COMPILER) && (defined(ARCH_X86) || defined(ARCH_X86_64))
STATIC_INLINE u_int16_t ByteSwap16(u_int16_t x)
{
  __asm("xchgb %b0,%h0" :
        LEGACY_REGS (x)	:
        "0" (x));
    return x;
}
#define bswap_16(x) ByteSwap16(x)

STATIC_INLINE u_int32_t ByteSwap32(u_int32_t x)
{
#if __CPU__ > 386
 __asm("bswap	%0":
      "=r" (x)     :
#else
 __asm("xchgb	%b0,%h0\n"\
      "	rorl	$16,%0\n"
      "	xchgb	%b0,%h0":
      LEGACY_REGS (x)		:
#endif
      "0" (x));
  return x;
}

#define bswap_32(x) ByteSwap32(x)

STATIC_INLINE u_int64_t ByteSwap64(u_int64_t x)
{
#ifdef ARCH_X86_64
  __asm("bswap	%0":
        "=r" (x)     :
        "0" (x));
  return x;
#else
  register union { __extension__ u_int64_t __ll;
          u_int32_t __l[2]; } __x;
  asm("xchgl	%0,%1":
      "=r"(__x.__l[0]),"=r"(__x.__l[1]):
      "0"(bswap_32((unsigned long)x)),"1"(bswap_32((unsigned long)(x>>32))));
  return __x.__ll;
#endif
}
#define bswap_64(x) ByteSwap64(x)

#else

#define bswap_16(x) (((x) & 0x00ff) << 8 | ((x) & 0xff00) >> 8)

#ifdef __INTEL_COMPILER
#define bswap_32(x) _bswap(x)
#else
#define bswap_32(x) \
     ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) | \
      (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24))
#endif

STATIC_INLINE u_int64_t ByteSwap64(u_int64_t x)
{
    union { 
        u_int64_t ll;
        u_int32_t l[2]; 
    } w, r;
    w.ll = x;
    r.l[0] = bswap_32 (w.l[1]);
    r.l[1] = bswap_32 (w.l[0]);
    return r.ll;
}
#define bswap_64(x) ByteSwap64(x)

#endif

#endif
