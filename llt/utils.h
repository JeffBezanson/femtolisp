#ifndef __UTILS_H_
#define __UTILS_H_


#if defined( __amd64__ ) || defined( _M_AMD64 )
#  define ARCH_X86_64
#  define __CPU__ 686
#elif defined( _M_IX86 )//msvs, intel, digital mars, watcom
#  if ! defined( __386__ )
#    error "unsupported target: 16-bit x86"
#  endif
#  define ARCH_X86
#  define __CPU__  ( _M_IX86 + 86 )
#elif defined( __i686__ )//gnu c
#  define ARCH_X86
#  define __CPU__ 686
#elif defined( __i586__ )//gnu c
#  define ARCH_X86
#  define __CPU__ 586
#elif defined( __i486__ )//gnu c
#  define ARCH_X86
#  define __CPU__ 486
#elif defined( __i386__ )//gnu c
#  define ARCH_X86
#  define __CPU__ 386
#else
#  error "unknown architecture"
#endif


char *uint2str(char *dest, size_t len, uint64_t num, uint32_t base);
int str2int(char *str, size_t len, int64_t *res, uint32_t base);
int isdigit_base(char c, int base);

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
