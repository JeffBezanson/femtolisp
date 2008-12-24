/*
  string functions
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#include <errno.h>
#include "llt.h"
#include "flisp.h"

value_t fl_intern(value_t *args, u_int32_t nargs)
{
    argcount("intern", nargs, 1);
    if (!isstring(args[0]))
        type_error("intern", "string", args[0]);
    return symbol(cvalue_data(args[0]));
}

value_t fl_stringp(value_t *args, u_int32_t nargs)
{
    argcount("stringp", nargs, 1);
    return isstring(args[0]) ? T : NIL;
}

value_t fl_string_length(value_t *args, u_int32_t nargs)
{
    argcount("string.length", nargs, 1);
    if (!isstring(args[0]))
        type_error("string.length", "string", args[0]);
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    return size_wrap(u8_charnum(cvalue_data(args[0]), len));
}

value_t fl_string_reverse(value_t *args, u_int32_t nargs)
{
    argcount("string.reverse", nargs, 1);
    if (!isstring(args[0]))
        type_error("string.reverse", "string", args[0]);
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    value_t ns = cvalue_string(len);
    u8_reverse(cvalue_data(ns), cvalue_data(args[0]), len);
    return ns;
}

value_t fl_string_encode(value_t *args, u_int32_t nargs)
{
    argcount("string.encode", nargs, 1);
    if (iscvalue(args[0])) {
        cvalue_t *cv = (cvalue_t*)ptr(args[0]);
        value_t t = cv_type(cv);
        if (iscons(t) && car_(t) == arraysym &&
            iscons(cdr_(t)) && car_(cdr_(t)) == wcharsym) {
            size_t nc = cv_len(cv) / sizeof(uint32_t);
            uint32_t *ptr = (uint32_t*)cv_data(cv);
            size_t nbytes = u8_codingsize(ptr, nc);
            value_t str = cvalue_string(nbytes);
            ptr = cv_data((cvalue_t*)ptr(args[0]));  // relocatable pointer
            u8_toutf8(cvalue_data(str), nbytes, ptr, nc);
            return str;
        }
    }
    type_error("string.encode", "wide character array", args[0]);
}

value_t fl_string_decode(value_t *args, u_int32_t nargs)
{
    int term=0;
    if (nargs == 2) {
        term = (POP() != NIL);
        nargs--;
    }
    argcount("string.decode", nargs, 1);
    if (!isstring(args[0]))
        type_error("string.decode", "string", args[0]);
    cvalue_t *cv = (cvalue_t*)ptr(args[0]);
    char *ptr = (char*)cv_data(cv);
    size_t nb = cv_len(cv);
    size_t nc = u8_charnum(ptr, nb);
    size_t newsz = nc*sizeof(uint32_t);
    if (term) newsz += sizeof(uint32_t);
    value_t wcstr = cvalue(wcstringtype, newsz);
    ptr = cv_data((cvalue_t*)ptr(args[0]));  // relocatable pointer
    uint32_t *pwc = cvalue_data(wcstr);
    u8_toucs(pwc, nc, ptr, nb);
    if (term) pwc[nc] = 0;
    return wcstr;
}

value_t fl_string(value_t *args, u_int32_t nargs)
{
    value_t cv, t;
    u_int32_t i;
    size_t len, sz = 0;
    cvalue_t *temp;
    char *data;
    uint32_t wc;

    for(i=0; i < nargs; i++) {
        if (issymbol(args[i])) {
            sz += strlen(symbol_name(args[i]));
            continue;
        }
        else if (iscvalue(args[i])) {
            temp = (cvalue_t*)ptr(args[i]);
            t = cv_type(temp);
            if (t == bytesym) {
                sz++;
                continue;
            }
            else if (t == wcharsym) {
                wc = *(uint32_t*)cv_data(temp);
                sz += u8_charlen(wc);
                continue;
            }
            else if (cv_isstr(temp)) {
                sz += cv_len(temp);
                continue;
            }
        }
        lerror(ArgError, "string: expected string, symbol or character");
    }
    cv = cvalue_string(sz);
    char *ptr = cvalue_data(cv);
    for(i=0; i < nargs; i++) {
        if (issymbol(args[i])) {
            char *name = symbol_name(args[i]);
            while (*name) *ptr++ = *name++;
        }
        else {
            temp = (cvalue_t*)ptr(args[i]);
            t = cv_type(temp);
            data = cvalue_data(args[i]);
            if (t == bytesym) {
                *ptr++ = *(char*)data;
            }
            else if (t == wcharsym) {
                ptr += u8_wc_toutf8(ptr, *(uint32_t*)data);
            }
            else {
                len = cv_len(temp);
                memcpy(ptr, data, len);
                ptr += len;
            }
        }
    }
    return cv;
}

value_t fl_string_split(value_t *args, u_int32_t nargs)
{
    argcount("string.split", nargs, 2);
    char *s = tostring(args[0], "string.split");
    char *delim = tostring(args[1], "string.split");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t dlen = cv_len((cvalue_t*)ptr(args[1]));
    PUSH(NIL);
    size_t ssz, tokend=0, tokstart=0, i=0;
    value_t c=NIL;
    size_t junk;
    do {
        // find and allocate next token
        tokstart = tokend = i;
        while (i < len &&
               !u8_memchr(delim, u8_nextmemchar(s, &i), dlen, &junk))
            tokend = i;
        ssz = tokend - tokstart;
        PUSH(c);  // save previous cons cell
        c = fl_cons(cvalue_string(ssz), NIL);

        // we've done allocation; reload movable pointers
        s = cv_data((cvalue_t*)ptr(args[0]));
        delim = cv_data((cvalue_t*)ptr(args[1]));

        if (ssz) memcpy(cv_data((cvalue_t*)ptr(car_(c))), &s[tokstart], ssz);

        // link new cell
        if (Stack[SP-1] == NIL) {
            Stack[SP-2] = c;   // first time, save first cons
            (void)POP();
        }
        else {
            ((cons_t*)ptr(POP()))->cdr = c;
        }

        // note this tricky condition: if the string ends with a
        // delimiter, we need to go around one more time to add an
        // empty string. this happens when (i==len && tokend<i)
    } while (i < len || (i==len && (tokend!=i)));
    return POP();
}

value_t fl_string_sub(value_t *args, u_int32_t nargs)
{
    argcount("string.sub", nargs, 3);
    char *s = tostring(args[0], "string.sub");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t i1, i2;
    i1 = toulong(args[1], "string.sub");
    if (i1 > len)
        bounds_error("string.sub", args[0], args[1]);
    i2 = toulong(args[2], "string.sub");
    if (i2 > len)
        bounds_error("string.sub", args[0], args[2]);
    if (i2 <= i1)
        return cvalue_string(0);
    value_t ns = cvalue_string(i2-i1);
    memcpy(cv_data((cvalue_t*)ptr(ns)), &s[i1], i2-i1);
    return ns;
}

value_t fl_string_char(value_t *args, u_int32_t nargs)
{
    argcount("string.char", nargs, 2);
    char *s = tostring(args[0], "string.char");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t i = toulong(args[1], "string.char");
    if (i >= len)
        bounds_error("string.char", args[0], args[1]);
    size_t sl = u8_seqlen(&s[i]);
    if (sl > len || i > len-sl)
        bounds_error("string.char", args[0], args[1]);
    return mk_wchar(u8_nextchar(s, &i));
}

static value_t mem_find_byte(char *s, char c, size_t start, size_t len)
{
    char *p = memchr(s+start, c, len-start);
    if (p == NULL)
        return NIL;
    return size_wrap((size_t)(p - s));
}

value_t fl_string_find(value_t *args, u_int32_t nargs)
{
    char cbuf[8];
    size_t start = 0;
    if (nargs == 3)
        start = toulong(args[2], "string.find");
    else
        argcount("string.find", nargs, 2);
    char *s = tostring(args[0], "string.find");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    if (start > len)
        bounds_error("string.find", args[0], args[2]);
    char *needle; size_t needlesz;
    if (!iscvalue(args[1]))
        type_error("string.find", "string", args[1]);
    cvalue_t *cv = (cvalue_t*)ptr(args[1]);
    if (cv_class(cv) == wchartype) {
        uint32_t c = *(uint32_t*)cv_data(cv);
        if (c <= 0x7f)
            return mem_find_byte(s, (char)c, start, len);
        needlesz = u8_toutf8(cbuf, sizeof(cbuf), &c, 1);
        needle = cbuf;
    }
    else if (cv_class(cv) == bytetype) {
        return mem_find_byte(s, *(char*)cv_data(cv), start, len);
    }
    else if (isstring(args[1])) {
        needlesz = cv_len(cv);
        needle = (char*)cv_data(cv);
    }
    else {
        type_error("string.find", "string", args[1]);
    }
    if (needlesz > len-start)
        return NIL;
    else if (needlesz == 1)
        return mem_find_byte(s, needle[0], start, len);
    else if (needlesz == 0)
        return size_wrap(start);
    size_t i;
    for(i=start; i < len-needlesz+1; i++) {
        if (s[i] == needle[0]) {
            if (!memcmp(&s[i+1], needle+1, needlesz-1))
                return size_wrap(i);
        }
    }
    return NIL;
}

value_t fl_string_inc(value_t *args, u_int32_t nargs)
{
    if (nargs < 2 || nargs > 3)
        argcount("string.inc", nargs, 2);
    char *s = tostring(args[0], "string.inc");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t i = toulong(args[1], "string.inc");
    size_t cnt = 1;
    if (nargs == 3)
        cnt = toulong(args[2], "string.inc");
    while (cnt--) {
        if (i >= len)
            bounds_error("string.inc", args[0], args[1]);
        u8_inc(s, &i);
    }
    return size_wrap(i);
}

value_t fl_string_dec(value_t *args, u_int32_t nargs)
{
    if (nargs < 2 || nargs > 3)
        argcount("string.dec", nargs, 2);
    char *s = tostring(args[0], "string.dec");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t i = toulong(args[1], "string.dec");
    size_t cnt = 1;
    if (nargs == 3)
        cnt = toulong(args[2], "string.dec");
    // note: i is allowed to start at index len
    if (i > len)
        bounds_error("string.dec", args[0], args[1]);
    while (cnt--) {
        if (i == 0)
            bounds_error("string.dec", args[0], args[1]);
        u8_dec(s, &i);
    }
    return size_wrap(i);
}

static builtinspec_t stringfunc_info[] = {
    { "intern", fl_intern },
    { "string", fl_string },
    { "stringp", fl_stringp },
    { "string.length", fl_string_length },
    { "string.split", fl_string_split },
    { "string.sub", fl_string_sub },
    { "string.find", fl_string_find },
    { "string.char", fl_string_char },
    { "string.inc", fl_string_inc },
    { "string.dec", fl_string_dec },
    { "string.reverse", fl_string_reverse },
    { "string.encode", fl_string_encode },
    { "string.decode", fl_string_decode },
    { NULL, NULL }
};

void stringfuncs_init()
{
    assign_global_builtins(stringfunc_info);
}
