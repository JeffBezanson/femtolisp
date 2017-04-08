/*
  string functions
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <wchar.h>
#include <wctype.h>
#include <sys/types.h>
#include <errno.h>

#include "flisp.h"

#if !defined(_OS_WINDOWS_)
#include <sys/time.h>
#endif /* !_OS_WINDOWS_ */

#undef JL_DLLEXPORT /* avoid conflicting definition */
#include "utf8proc.h"

#ifdef __cplusplus
extern "C" {
#endif

value_t fl_stringp(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "string?", nargs, 1);
    return fl_isstring(fl_ctx, args[0]) ? fl_ctx->T : fl_ctx->F;
}

value_t fl_string_count(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    size_t start = 0;
    if (nargs < 1 || nargs > 3)
        argcount(fl_ctx, "string.count", nargs, 1);
    if (!fl_isstring(fl_ctx, args[0]))
        type_error(fl_ctx, "string.count", "string", args[0]);
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t stop = len;
    if (nargs > 1) {
        start = tosize(fl_ctx, args[1], "string.count");
        if (start > len)
            bounds_error(fl_ctx, "string.count", args[0], args[1]);
        if (nargs > 2) {
            stop = tosize(fl_ctx, args[2], "string.count");
            if (stop > len)
                bounds_error(fl_ctx, "string.count", args[0], args[2]);
            if (stop <= start)
                return fixnum(0);
        }
    }
    char *str = (char*)cvalue_data(args[0]);
    return size_wrap(fl_ctx, u8_charnum(str+start, stop-start));
}

value_t fl_string_width(fl_context_t *fl_ctx, value_t *args, u_int32_t nargs)
{
    argcount(fl_ctx, "string.width", nargs, 1);
    if (iscprim(args[0])) {
        cprim_t *cp = (cprim_t*)ptr(args[0]);
        if (cp_class(cp) == fl_ctx->wchartype) {
            int w = wcwidth(*(uint32_t*)cp_data(cp));
            if (w < 0)
                return fl_ctx->F;
            return fixnum(w);
        }
    }
    char *s = tostring(fl_ctx, args[0], "string.width");
    return size_wrap(fl_ctx, u8_strwidth(s));
}

value_t fl_string_reverse(fl_context_t *fl_ctx, value_t *args, u_int32_t nargs)
{
    argcount(fl_ctx, "string.reverse", nargs, 1);
    if (!fl_isstring(fl_ctx, args[0]))
        type_error(fl_ctx, "string.reverse", "string", args[0]);
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    value_t ns = cvalue_string(fl_ctx, len);
    u8_reverse(cvalue_data(ns), cvalue_data(args[0]), len);
    return ns;
}

extern value_t fl_buffer(fl_context_t *fl_ctx, value_t *args, uint32_t nargs);
extern value_t stream_to_string(fl_context_t *fl_ctx, value_t *ps);

value_t fl_string_encode(fl_context_t *fl_ctx, value_t *args, u_int32_t nargs)
{
    argcount(fl_ctx, "string.encode", nargs, 1);
    if (iscvalue(args[0])) {
        cvalue_t *cv = (cvalue_t*)ptr(args[0]);
        fltype_t *t = cv_class(cv);
        if (t->eltype == fl_ctx->wchartype) {
            size_t nc = cv_len(cv) / sizeof(uint32_t);
            uint32_t *ptr = (uint32_t*)cv_data(cv);
            size_t nbytes = u8_codingsize(ptr, nc);
            value_t str = cvalue_string(fl_ctx, nbytes);
            ptr = cv_data((cvalue_t*)ptr(args[0]));  // relocatable pointer
            u8_toutf8(cvalue_data(str), nbytes, ptr, nc);
            return str;
        }
    }
    type_error(fl_ctx, "string.encode", "wchar array", args[0]);
}

value_t fl_string_decode(fl_context_t *fl_ctx, value_t *args, u_int32_t nargs)
{
    int term=0;
    if (nargs == 2) {
        term = (args[1] != fl_ctx->F);
    }
    else {
        argcount(fl_ctx, "string.decode", nargs, 1);
    }
    if (!fl_isstring(fl_ctx, args[0]))
        type_error(fl_ctx, "string.decode", "string", args[0]);
    cvalue_t *cv = (cvalue_t*)ptr(args[0]);
    char *ptr = (char*)cv_data(cv);
    size_t nb = cv_len(cv);
    size_t nc = u8_charnum(ptr, nb);
    size_t newsz = nc*sizeof(uint32_t);
    if (term) newsz += sizeof(uint32_t);
    value_t wcstr = cvalue(fl_ctx, fl_ctx->wcstringtype, newsz);
    ptr = cv_data((cvalue_t*)ptr(args[0]));  // relocatable pointer
    uint32_t *pwc = cvalue_data(wcstr);
    u8_toucs(pwc, nc, ptr, nb);
    if (term) pwc[nc] = 0;
    return wcstr;
}

value_t fl_string(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs == 1 && fl_isstring(fl_ctx, args[0]))
        return args[0];
    value_t arg, buf = fl_buffer(fl_ctx, NULL, 0);
    fl_gc_handle(fl_ctx, &buf);
    ios_t *s = value2c(ios_t*,buf);
    uint32_t i;
    value_t oldpr = symbol_value(fl_ctx->printreadablysym);
    value_t oldpp = symbol_value(fl_ctx->printprettysym);
    set(fl_ctx->printreadablysym, fl_ctx->F);
    set(fl_ctx->printprettysym, fl_ctx->F);
    FOR_ARGS(i,0,arg,args) {
        fl_print(fl_ctx, s, args[i]);
    }
    set(fl_ctx->printreadablysym, oldpr);
    set(fl_ctx->printprettysym, oldpp);
    value_t outp = stream_to_string(fl_ctx, &buf);
    fl_free_gc_handles(fl_ctx, 1);
    return outp;
}

value_t fl_string_split(fl_context_t *fl_ctx, value_t *args, u_int32_t nargs)
{
    argcount(fl_ctx, "string.split", nargs, 2);
    char *s = tostring(fl_ctx, args[0], "string.split");
    char *delim = tostring(fl_ctx, args[1], "string.split");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t dlen = cv_len((cvalue_t*)ptr(args[1]));
    size_t ssz, tokend=0, tokstart=0, i=0;
    value_t first=fl_ctx->NIL, c=fl_ctx->NIL, last;
    size_t junk;
    fl_gc_handle(fl_ctx, &first);
    fl_gc_handle(fl_ctx, &last);

    do {
        // find and allocate next token
        tokstart = tokend = i;
        while (i < len &&
               !u8_memchr(delim, u8_nextmemchar(s, &i), dlen, &junk))
            tokend = i;
        ssz = tokend - tokstart;
        last = c;  // save previous cons cell
        c = fl_cons(fl_ctx, cvalue_string(fl_ctx, ssz), fl_ctx->NIL);

        // we've done allocation; reload movable pointers
        s = cv_data((cvalue_t*)ptr(args[0]));
        delim = cv_data((cvalue_t*)ptr(args[1]));

        if (ssz) memcpy(cv_data((cvalue_t*)ptr(car_(c))), &s[tokstart], ssz);

        // link new cell
        if (last == fl_ctx->NIL)
            first = c;   // first time, save first cons
        else
            ((cons_t*)ptr(last))->cdr = c;

        // note this tricky condition: if the string ends with a
        // delimiter, we need to go around one more time to add an
        // empty string. this happens when (i==len && tokend<i)
    } while (i < len || (i==len && (tokend!=i)));
    fl_free_gc_handles(fl_ctx, 2);
    return first;
}
value_t fl_string_sub(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs != 2)
        argcount(fl_ctx, "string.sub", nargs, 3);
    char *s = tostring(fl_ctx, args[0], "string.sub");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t i1, i2;
    i1 = tosize(fl_ctx, args[1], "string.sub");
    if (i1 > len)
        bounds_error(fl_ctx, "string.sub", args[0], args[1]);
    if (nargs == 3) {
        i2 = tosize(fl_ctx, args[2], "string.sub");
        if (i2 > len)
            bounds_error(fl_ctx, "string.sub", args[0], args[2]);
    }
    else {
        i2 = len;
    }
    if (i2 <= i1)
        return cvalue_string(fl_ctx, 0);
    value_t ns = cvalue_string(fl_ctx, i2-i1);
    s = (char*)cvalue_data(args[0]); // reload after alloc
    memcpy(cv_data((cvalue_t*)ptr(ns)), &s[i1], i2-i1);
    return ns;
}

value_t fl_string_char(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "string.char", nargs, 2);
    char *s = tostring(fl_ctx, args[0], "string.char");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t i = tosize(fl_ctx, args[1], "string.char");
    if (i >= len)
        bounds_error(fl_ctx, "string.char", args[0], args[1]);
    size_t sl = u8_seqlen(&s[i]);
    if (sl > len || i > len-sl)
        bounds_error(fl_ctx, "string.char", args[0], args[1]);
    return mk_wchar(fl_ctx, u8_nextchar(s, &i));
}

value_t fl_char_upcase(fl_context_t *fl_ctx, value_t *args, u_int32_t nargs)
{
    argcount(fl_ctx, "char.upcase", nargs, 1);
    cprim_t *cp = (cprim_t*)ptr(args[0]);
    if (!iscprim(args[0]) || cp_class(cp) != fl_ctx->wchartype)
      type_error(fl_ctx, "char.upcase", "wchar", args[0]);
    return mk_wchar(fl_ctx, towupper(*(int32_t*)cp_data(cp)));
}
value_t fl_char_downcase(fl_context_t *fl_ctx, value_t *args, u_int32_t nargs)
{
    argcount(fl_ctx, "char.downcase", nargs, 1);
    cprim_t *cp = (cprim_t*)ptr(args[0]);
    if (!iscprim(args[0]) || cp_class(cp) != fl_ctx->wchartype)
      type_error(fl_ctx, "char.downcase", "wchar", args[0]);
    return mk_wchar(fl_ctx, towlower(*(int32_t*)cp_data(cp)));
}
static value_t mem_find_byte(fl_context_t *fl_ctx, char *s, char c, size_t start, size_t len)
{
    char *p = (char*)memchr(s+start, c, len-start);
    if (p == NULL)
        return fl_ctx->F;
    return size_wrap(fl_ctx, (size_t)(p - s));
}

value_t fl_string_find(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    char cbuf[8];
    size_t start = 0;
    if (nargs == 3)
        start = tosize(fl_ctx, args[2], "string.find");
    else
        argcount(fl_ctx, "string.find", nargs, 2);
    char *s = tostring(fl_ctx, args[0], "string.find");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    if (start > len)
        bounds_error(fl_ctx, "string.find", args[0], args[2]);
    char *needle; size_t needlesz;

    value_t v = args[1];
    cprim_t *cp = (cprim_t*)ptr(v);
    if (iscprim(v) && cp_class(cp) == fl_ctx->wchartype) {
        uint32_t c = *(uint32_t*)cp_data(cp);
        if (c <= 0x7f)
            return mem_find_byte(fl_ctx, s, (char)c, start, len);
        needlesz = u8_toutf8(cbuf, sizeof(cbuf), &c, 1);
        needle = cbuf;
    }
    else if (iscprim(v) && cp_class(cp) == fl_ctx->bytetype) {
        return mem_find_byte(fl_ctx, s, *(char*)cp_data(cp), start, len);
    }
    else if (fl_isstring(fl_ctx, v)) {
        cvalue_t *cv = (cvalue_t*)ptr(v);
        needlesz = cv_len(cv);
        needle = (char*)cv_data(cv);
    }
    else {
        type_error(fl_ctx, "string.find", "string", args[1]);
    }
    if (needlesz > len-start)
        return fl_ctx->F;
    else if (needlesz == 1)
        return mem_find_byte(fl_ctx, s, needle[0], start, len);
    else if (needlesz == 0)
        return size_wrap(fl_ctx, start);
    size_t i;
    for(i=start; i < len-needlesz+1; i++) {
        if (s[i] == needle[0]) {
            if (!memcmp(&s[i+1], needle+1, needlesz-1))
                return size_wrap(fl_ctx, i);
        }
    }
    return fl_ctx->F;
}

value_t fl_string_inc(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs < 2 || nargs > 3)
        argcount(fl_ctx, "string.inc", nargs, 2);
    char *s = tostring(fl_ctx, args[0], "string.inc");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t i = tosize(fl_ctx, args[1], "string.inc");
    size_t cnt = 1;
    if (nargs == 3)
        cnt = tosize(fl_ctx, args[2], "string.inc");
    while (cnt--) {
        if (i >= len)
            bounds_error(fl_ctx, "string.inc", args[0], args[1]);
        (void)(isutf(s[++i]) || isutf(s[++i]) || isutf(s[++i]) || ++i);
    }
    return size_wrap(fl_ctx, i);
}

value_t fl_string_dec(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs < 2 || nargs > 3)
        argcount(fl_ctx, "string.dec", nargs, 2);
    char *s = tostring(fl_ctx, args[0], "string.dec");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t i = tosize(fl_ctx, args[1], "string.dec");
    size_t cnt = 1;
    if (nargs == 3)
        cnt = tosize(fl_ctx, args[2], "string.dec");
    // note: i is allowed to start at index len
    if (i > len)
        bounds_error(fl_ctx, "string.dec", args[0], args[1]);
    while (cnt--) {
        if (i == 0)
            bounds_error(fl_ctx, "string.dec", args[0], args[1]);
        (void)(isutf(s[--i]) || isutf(s[--i]) || isutf(s[--i]) || --i);
    }
    return size_wrap(fl_ctx, i);
}

static unsigned long get_radix_arg(fl_context_t *fl_ctx, value_t arg, char *fname)
{
    unsigned long radix = (unsigned long)tosize(fl_ctx, arg, fname);
    if (radix < 2 || radix > 36)
        lerrorf(fl_ctx, fl_ctx->ArgError, "%s: invalid radix", fname);
    return radix;
}

value_t fl_numbertostring(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs < 1 || nargs > 2)
        argcount(fl_ctx, "number->string", nargs, 2);
    value_t n = args[0];
    int neg = 0;
    uint64_t num;
    if (isfixnum(n))      num = numval(n);
    else if (!iscprim(n)) type_error(fl_ctx, "number->string", "integer", n);
    else num = conv_to_uint64(cp_data((cprim_t*)ptr(n)),
                              cp_numtype((cprim_t*)ptr(n)));
    if (numval(fl_compare(fl_ctx, args[0],fixnum(0))) < 0) {
        num = -num;
        neg = 1;
    }
    unsigned long radix = 10;
    if (nargs == 2)
        radix = get_radix_arg(fl_ctx, args[1], "number->string");
    char buf[128];
    char *str = uint2str(buf, sizeof(buf), num, radix);
    if (neg && str > &buf[0])
        *(--str) = '-';
    return string_from_cstr(fl_ctx, str);
}

value_t fl_stringtonumber(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs < 1 || nargs > 2)
        argcount(fl_ctx, "string->number", nargs, 2);
    char *str = tostring(fl_ctx, args[0], "string->number");
    value_t n;
    unsigned long radix = 0;
    if (nargs == 2)
        radix = get_radix_arg(fl_ctx, args[1], "string->number");
    if (!isnumtok_base(fl_ctx, str, &n, (int)radix))
        return fl_ctx->F;
    return n;
}

value_t fl_string_isutf8(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "string.isutf8", nargs, 1);
    char *s = tostring(fl_ctx, args[0], "string.isutf8");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    return u8_isvalid(s, len) ? fl_ctx->T : fl_ctx->F;
}

static const builtinspec_t stringfunc_info[] = {
    { "string", fl_string },
    { "string?", fl_stringp },
    { "string.count", fl_string_count },
    { "string.width", fl_string_width },
    { "string.split", fl_string_split },
    { "string.sub", fl_string_sub },
    { "string.find", fl_string_find },
    { "string.char", fl_string_char },
    { "string.inc", fl_string_inc },
    { "string.dec", fl_string_dec },
    { "string.reverse", fl_string_reverse },
    { "string.encode", fl_string_encode },
    { "string.decode", fl_string_decode },
    { "string.isutf8", fl_string_isutf8 },

    { "char.upcase", fl_char_upcase },
    { "char.downcase", fl_char_downcase },

    { "number->string", fl_numbertostring },
    { "string->number", fl_stringtonumber },

    { NULL, NULL }
};

void stringfuncs_init(fl_context_t *fl_ctx)
{
    assign_global_builtins(fl_ctx, stringfunc_info);
}

#ifdef __cplusplus
}
#endif
