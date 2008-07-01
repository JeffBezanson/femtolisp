/*
  Extra femtoLisp builtin functions
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

size_t llength(value_t v)
{
    size_t n = 0;
    while (iscons(v)) {
        n++;
        v = cdr_(v);
    }
    return n;
}

value_t list_nth(value_t l, size_t n)
{
    while (n && iscons(l)) {
        l = cdr_(l);
        n--;
    }
    if (iscons(l)) return car_(l);
    return NIL;
}

value_t fl_print(value_t *args, u_int32_t nargs)
{
    unsigned i;
    for (i=0; i < nargs; i++)
        print(stdout, args[i], 0);
    fputc('\n', stdout);
    return nargs ? args[nargs-1] : NIL;
}

value_t fl_princ(value_t *args, u_int32_t nargs)
{
    unsigned i;
    for (i=0; i < nargs; i++)
        print(stdout, args[i], 1);
    return nargs ? args[nargs-1] : NIL;
}

value_t fl_read(value_t *args, u_int32_t nargs)
{
    (void)args;
    argcount("read", nargs, 0);
    return read_sexpr(stdin);
}

value_t fl_load(value_t *args, u_int32_t nargs)
{
    argcount("load", nargs, 1);
    return load_file(tostring(args[0], "load"));
}

value_t fl_exit(value_t *args, u_int32_t nargs)
{
    if (nargs > 0)
        exit(tofixnum(args[0], "exit"));
    exit(0);
    return NIL;
}

extern value_t LAMBDA;

value_t fl_setsyntax(value_t *args, u_int32_t nargs)
{
    argcount("set-syntax", nargs, 2);
    symbol_t *sym = tosymbol(args[0], "set-syntax");
    if (sym->syntax && (sym->syntax == TAG_CONST || isspecial(sym->syntax)))
        lerror(ArgError, "set-syntax: cannot define syntax for %s",
               symbol_name(args[0]));
    if (args[1] == NIL) {
        sym->syntax = 0;
    }
    else {
        if (!iscons(args[1]) || car_(args[1])!=LAMBDA)
            type_error("set-syntax", "function", args[1]);
        sym->syntax = args[1];
    }
    return args[1];
}

value_t fl_symbolsyntax(value_t *args, u_int32_t nargs)
{
    argcount("symbol-syntax", nargs, 1);
    symbol_t *sym = tosymbol(args[0], "symbol-syntax");
    // must avoid returning built-in syntax expanders, because they
    // don't behave like functions (they take their arguments directly
    // from the form rather than from the stack of evaluated arguments)
    if (sym->syntax == TAG_CONST || isspecial(sym->syntax))
        return NIL;
    return sym->syntax;
}

static void syntax_env_assoc_list(symbol_t *root, value_t *pv)
{
    while (root != NULL) {
        if (root->syntax && root->syntax != TAG_CONST &&
            !isspecial(root->syntax)) {
            PUSH(fl_cons(tagptr(root,TAG_SYM), root->syntax));
            *pv = fl_cons(POP(), *pv);
        }
        syntax_env_assoc_list(root->left, pv);
        root = root->right;
    }
}
static void global_env_assoc_list(symbol_t *root, value_t *pv)
{
    while (root != NULL) {
        if (root->binding != UNBOUND) {
            PUSH(fl_cons(tagptr(root,TAG_SYM), root->binding));
            *pv = fl_cons(POP(), *pv);
        }
        global_env_assoc_list(root->left, pv);
        root = root->right;
    }
}

extern symbol_t *symtab;

value_t fl_syntax_env(value_t *args, u_int32_t nargs)
{
    (void)args;
    argcount("syntax-environment", nargs, 0);
    PUSH(NIL);
    syntax_env_assoc_list(symtab, &Stack[SP-1]);
    return POP();
}
value_t fl_global_env(value_t *args, u_int32_t nargs)
{
    (void)args;
    argcount("environment", nargs, 0);
    PUSH(NIL);
    global_env_assoc_list(symtab, &Stack[SP-1]);
    return POP();
}

value_t fl_constantp(value_t *args, u_int32_t nargs)
{
    argcount("constantp", nargs, 1);
    if (issymbol(args[0]))
        return (isconstant(args[0]) ? T : NIL);
    if (iscons(args[0]))
        return NIL;
    return T;
}

value_t fl_fixnum(value_t *args, u_int32_t nargs)
{
    argcount("fixnum", nargs, 1);
    if (isfixnum(args[0]))
        return args[0];
    if (iscvalue(args[0])) {
        cvalue_t *cv = (cvalue_t*)ptr(args[0]);
        long i;
        if (cv->flags.cstring) {
            char *pend;
            errno = 0;
            i = strtol(cv_data(cv), &pend, 0);
            if (*pend != '\0' || errno!=0)
                lerror(ArgError, "fixnum: invalid string");
            return fixnum(i);
        }
        else if (valid_numtype(cv_numtype(cv))) {
            i = conv_to_long(cv_data(cv), cv_numtype(cv));
            return fixnum(i);
        }
    }
    lerror(ArgError, "fixnum: cannot convert argument");
}

value_t fl_truncate(value_t *args, u_int32_t nargs)
{
    argcount("truncate", nargs, 1);
    if (isfixnum(args[0]))
        return args[0];
    if (iscvalue(args[0])) {
        cvalue_t *cv = (cvalue_t*)ptr(args[0]);
        void *data = cv_data(cv);
        numerictype_t nt = cv_numtype(cv);
        if (valid_numtype(nt)) {
            double d;
            if (nt == T_FLOAT)
                d = (double)*(float*)data;
            else if (nt == T_DOUBLE)
                d = *(double*)data;
            else
                return args[0];
            if (d > 0)
                return return_from_uint64((uint64_t)d);
            return return_from_int64((int64_t)d);
        }
    }
    type_error("truncate", "number", args[0]);
}

value_t fl_vector_alloc(value_t *args, u_int32_t nargs)
{
    fixnum_t i;
    value_t f, v;
    if (nargs == 0)
        lerror(ArgError, "vector.alloc: too few arguments");
    i = tofixnum(args[0], "vector.alloc");
    if (i < 0)
        lerror(ArgError, "vector.alloc: invalid size");
    if (nargs == 2)
        f = args[1];
    else
        f = NIL;
    v = alloc_vector((unsigned)i, f==NIL);
    if (f != NIL) {
        int k;
        for(k=0; k < i; k++)
            vector_elt(v,k) = f;
    }
    return v;
}

int isstring(value_t v)
{
    return (iscvalue(v) && ((cvalue_t*)ptr(v))->flags.cstring);
}

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
    value_t wcstr = cvalue(symbol_value(wcstringtypesym), newsz);
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
    wchar_t wc;

    for(i=0; i < nargs; i++) {
        if (issymbol(args[i])) {
            sz += strlen(symbol_name(args[i]));
            continue;
        }
        else if (iscvalue(args[i])) {
            temp = (cvalue_t*)ptr(args[i]);
            t = cv_type(temp);
            if (t == charsym) {
                sz++;
                continue;
            }
            else if (t == wcharsym) {
                wc = *(wchar_t*)cv_data(temp);
                sz += u8_charlen(wc);
                continue;
            }
            else if (temp->flags.cstring) {
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
            if (t == charsym) {
                *ptr++ = *(char*)data;
            }
            else if (t == wcharsym) {
                ptr += u8_wc_toutf8(ptr, *(wchar_t*)data);
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

value_t fl_time_now(value_t *args, u_int32_t nargs)
{
    argcount("time.now", nargs, 0);
    (void)args;
    return mk_double(clock_now());
}

static double value_to_double(value_t a, char *fname)
{
    if (isfixnum(a))
        return (double)numval(a);
    if (iscvalue(a)) {
        cvalue_t *cv = (cvalue_t*)ptr(a);
        numerictype_t nt = cv_numtype(cv);
        if (valid_numtype(nt))
            return conv_to_double(cv_data(cv), nt);
    }
    type_error(fname, "number", a);
}

static value_t return_from_cstr(char *str)
{
    size_t n = strlen(str);
    value_t v = cvalue_string(n);
    memcpy(cvalue_data(v), str, n);
    return v;
}

value_t fl_time_string(value_t *args, uint32_t nargs)
{
    argcount("time.string", nargs, 1);
    double t = value_to_double(args[0], "time.string");
    char buf[64];
    timestring(t, buf, sizeof(buf));
    return return_from_cstr(buf);
}

value_t fl_path_cwd(value_t *args, uint32_t nargs)
{
    if (nargs > 1)
        argcount("path.cwd", nargs, 1);
    if (nargs == 0) {
        char buf[1024];
        get_cwd(buf, sizeof(buf));
        return return_from_cstr(buf);
    }
    char *ptr = tostring(args[0], "path.cwd");
    if (set_cwd(ptr))
        lerror(IOError, "could not cd to %s", ptr);
    return T;
}

value_t fl_os_getenv(value_t *args, uint32_t nargs)
{
    argcount("os.getenv", nargs, 1);
    char *name = tostring(args[0], "os.getenv");
    char *val = getenv(name);
    if (val == NULL) return NIL;
    if (*val == 0)
        return symbol_value(emptystringsym);
    return cvalue_pinned_cstring(val);
}

value_t fl_os_setenv(value_t *args, uint32_t nargs)
{
    argcount("os.setenv", nargs, 2);
    char *name = tostring(args[0], "os.setenv");
    int result;
    if (args[1] == NIL) {
        result = unsetenv(name);
    }
    else {
        char *val = tostring(args[1], "os.setenv");
        result = setenv(name, val, 1);
    }
    if (result != 0)
        lerror(ArgError, "os.setenv: invalid environment variable");
    return T;
}

value_t fl_rand(value_t *args, u_int32_t nargs)
{
    (void)args;
    (void)nargs;
    return fixnum(random()&0x1fffffff);
}
value_t fl_rand32(value_t *args, u_int32_t nargs)
{
    (void)args;
    (void)nargs;
    return mk_uint32(random());
}
value_t fl_rand64(value_t *args, u_int32_t nargs)
{
    (void)args;
    (void)nargs;
    return mk_uint64(((uint64_t)random())<<32 | ((uint64_t)random()));
}
value_t fl_randd(value_t *args, u_int32_t nargs)
{
    (void)args;
    (void)nargs;
    return mk_double(rand_double());
}

void builtins_init()
{
    set(symbol("set-syntax"), guestfunc(fl_setsyntax));
    set(symbol("symbol-syntax"), guestfunc(fl_symbolsyntax));
    set(symbol("syntax-environment"), guestfunc(fl_syntax_env));
    set(symbol("environment"), guestfunc(fl_global_env));
    set(symbol("constantp"), guestfunc(fl_constantp));

    set(symbol("print"), guestfunc(fl_print));
    set(symbol("princ"), guestfunc(fl_princ));
    set(symbol("read"), guestfunc(fl_read));
    set(symbol("load"), guestfunc(fl_load));
    set(symbol("exit"), guestfunc(fl_exit));
    set(symbol("intern"), guestfunc(fl_intern));
    set(symbol("fixnum"), guestfunc(fl_fixnum));
    set(symbol("truncate"), guestfunc(fl_truncate));

    set(symbol("vector.alloc"), guestfunc(fl_vector_alloc));

    set(symbol("string"), guestfunc(fl_string));
    set(symbol("stringp"), guestfunc(fl_stringp));
    set(symbol("string.length"), guestfunc(fl_string_length));
    set(symbol("string.split"), guestfunc(fl_string_split));
    set(symbol("string.sub"), guestfunc(fl_string_sub));
    set(symbol("string.reverse"), guestfunc(fl_string_reverse));
    set(symbol("string.encode"), guestfunc(fl_string_encode));
    set(symbol("string.decode"), guestfunc(fl_string_decode));

    set(symbol("time.now"), guestfunc(fl_time_now));
    set(symbol("time.string"), guestfunc(fl_time_string));

    set(symbol("rand"), guestfunc(fl_rand));
    set(symbol("rand.uint32"), guestfunc(fl_rand32));
    set(symbol("rand.uint64"), guestfunc(fl_rand64));
    set(symbol("rand.double"), guestfunc(fl_randd));

    set(symbol("path.cwd"), guestfunc(fl_path_cwd));

    set(symbol("os.getenv"), guestfunc(fl_os_getenv));
    set(symbol("os.setenv"), guestfunc(fl_os_setenv));
}
