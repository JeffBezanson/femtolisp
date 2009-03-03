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
#include "random.h"

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

value_t fl_exit(value_t *args, u_int32_t nargs)
{
    if (nargs > 0)
        exit(tofixnum(args[0], "exit"));
    exit(0);
    return NIL;
}

value_t fl_intern(value_t *args, u_int32_t nargs)
{
    argcount("intern", nargs, 1);
    if (!isstring(args[0]))
        type_error("intern", "string", args[0]);
    return symbol(cvalue_data(args[0]));
}

value_t fl_setconstant(value_t *args, u_int32_t nargs)
{
    argcount("set-constant!", nargs, 2);
    symbol_t *sym = tosymbol(args[0], "set-constant!");
    if (isconstant(args[0]) || sym->binding != UNBOUND)
        lerror(ArgError, "set-constant!: cannot redefine %s",
               symbol_name(args[0]));
    setc(args[0], args[1]);
    return args[1];
}

extern value_t LAMBDA;

value_t fl_setsyntax(value_t *args, u_int32_t nargs)
{
    argcount("set-syntax!", nargs, 2);
    symbol_t *sym = tosymbol(args[0], "set-syntax!");
    if (sym->syntax && (sym->syntax == TAG_CONST || isspecial(sym->syntax)))
        lerror(ArgError, "set-syntax!: cannot define syntax for %s",
               symbol_name(args[0]));
    if (args[1] == FL_F) {
        sym->syntax = 0;
    }
    else {
        if (!iscons(args[1]) || car_(args[1])!=LAMBDA)
            type_error("set-syntax!", "function", args[1]);
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
        return FL_F;
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

extern value_t QUOTE;

value_t fl_constantp(value_t *args, u_int32_t nargs)
{
    argcount("constant?", nargs, 1);
    if (issymbol(args[0]))
        return (isconstant(args[0]) ? FL_T : FL_F);
    if (iscons(args[0])) {
        if (car_(args[0]) == QUOTE)
            return FL_T;
        return FL_F;
    }
    return FL_T;
}

value_t fl_integerp(value_t *args, u_int32_t nargs)
{
    argcount("integer?", nargs, 1);
    value_t v = args[0];
    if (isfixnum(v)) {
        return FL_T;
    }
    else if (iscprim(v)) {
        numerictype_t nt = cp_numtype((cprim_t*)ptr(v));
        if (nt < T_FLOAT)
            return FL_T;
        void *data = cp_data((cprim_t*)ptr(v));
        if (nt == T_FLOAT) {
            float f = *(float*)data;
            if (f < 0) f = -f;
            if (f <= FLT_MAXINT && (float)(int32_t)f == f)
                return FL_T;
        }
        else {
            assert(nt == T_DOUBLE);
            double d = *(double*)data;
            if (d < 0) d = -d;
            if (d <= DBL_MAXINT && (double)(int64_t)d == d)
                return FL_T;
        }
    }
    return FL_F;
}

value_t fl_fixnum(value_t *args, u_int32_t nargs)
{
    argcount("fixnum", nargs, 1);
    if (isfixnum(args[0])) {
        return args[0];
    }
    else if (iscprim(args[0])) {
        cprim_t *cp = (cprim_t*)ptr(args[0]);
        return fixnum(conv_to_long(cp_data(cp), cp_numtype(cp)));
    }
    else if (isstring(args[0])) {
        cvalue_t *cv = (cvalue_t*)ptr(args[0]);
        char *pend;
        errno = 0;
        long i = strtol(cv_data(cv), &pend, 0);
        if (*pend != '\0' || errno!=0)
            lerror(ArgError, "fixnum: invalid string");
        return fixnum(i);
    }
    lerror(ArgError, "fixnum: cannot convert argument");
}

value_t fl_truncate(value_t *args, u_int32_t nargs)
{
    argcount("truncate", nargs, 1);
    if (isfixnum(args[0]))
        return args[0];
    if (iscprim(args[0])) {
        cprim_t *cp = (cprim_t*)ptr(args[0]);
        void *data = cp_data(cp);
        numerictype_t nt = cp_numtype(cp);
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

value_t fl_time_now(value_t *args, u_int32_t nargs)
{
    argcount("time.now", nargs, 0);
    (void)args;
    return mk_double(clock_now());
}

static double todouble(value_t a, char *fname)
{
    if (isfixnum(a))
        return (double)numval(a);
    if (iscprim(a)) {
        cprim_t *cp = (cprim_t*)ptr(a);
        numerictype_t nt = cp_numtype(cp);
        return conv_to_double(cp_data(cp), nt);
    }
    type_error(fname, "number", a);
}

value_t fl_time_string(value_t *args, uint32_t nargs)
{
    argcount("time.string", nargs, 1);
    double t = todouble(args[0], "time.string");
    char buf[64];
    timestring(t, buf, sizeof(buf));
    return string_from_cstr(buf);
}

value_t fl_path_cwd(value_t *args, uint32_t nargs)
{
    if (nargs > 1)
        argcount("path.cwd", nargs, 1);
    if (nargs == 0) {
        char buf[1024];
        get_cwd(buf, sizeof(buf));
        return string_from_cstr(buf);
    }
    char *ptr = tostring(args[0], "path.cwd");
    if (set_cwd(ptr))
        lerror(IOError, "path.cwd: could not cd to %s", ptr);
    return FL_T;
}

value_t fl_os_getenv(value_t *args, uint32_t nargs)
{
    argcount("os.getenv", nargs, 1);
    char *name = tostring(args[0], "os.getenv");
    char *val = getenv(name);
    if (val == NULL) return FL_F;
    if (*val == 0)
        return symbol_value(emptystringsym);
    return cvalue_static_cstring(val);
}

value_t fl_os_setenv(value_t *args, uint32_t nargs)
{
    argcount("os.setenv", nargs, 2);
    char *name = tostring(args[0], "os.setenv");
    int result;
    if (args[1] == FL_F) {
        result = unsetenv(name);
    }
    else {
        char *val = tostring(args[1], "os.setenv");
        result = setenv(name, val, 1);
    }
    if (result != 0)
        lerror(ArgError, "os.setenv: invalid environment variable");
    return FL_T;
}

value_t fl_rand(value_t *args, u_int32_t nargs)
{
    (void)args; (void)nargs;
    fixnum_t r;
#ifdef BITS64
    r = ((((uint64_t)random())<<32) | random()) & 0x1fffffffffffffffLL;
#else
    r = random() & 0x1fffffff;
#endif
    return fixnum(r);
}
value_t fl_rand32(value_t *args, u_int32_t nargs)
{
    (void)args; (void)nargs;
    ulong r = random();
#ifdef BITS64
    return fixnum(r);
#else
    return mk_uint32(r);
#endif
}
value_t fl_rand64(value_t *args, u_int32_t nargs)
{
    (void)args; (void)nargs;
    ulong r = (((uint64_t)random())<<32) | random();
    return mk_uint64(r);
}
value_t fl_randd(value_t *args, u_int32_t nargs)
{
    (void)args; (void)nargs;
    return mk_double(rand_double());
}
value_t fl_randf(value_t *args, u_int32_t nargs)
{
    (void)args; (void)nargs;
    return mk_float(rand_float());
}

extern void stringfuncs_init();
extern void table_init();
extern void iostream_init();

static builtinspec_t builtin_info[] = {
    { "set-constant!", fl_setconstant },
    { "set-syntax!", fl_setsyntax },
    { "symbol-syntax", fl_symbolsyntax },
    { "syntax-environment", fl_syntax_env },
    { "environment", fl_global_env },
    { "constant?", fl_constantp },

    { "exit", fl_exit },
    { "intern", fl_intern },
    { "fixnum", fl_fixnum },
    { "truncate", fl_truncate },
    { "integer?", fl_integerp },

    { "vector.alloc", fl_vector_alloc },

    { "time.now", fl_time_now },
    { "time.string", fl_time_string },

    { "rand", fl_rand },
    { "rand.uint32", fl_rand32 },
    { "rand.uint64", fl_rand64 },
    { "rand.double", fl_randd },
    { "rand.float", fl_randf },

    { "path.cwd", fl_path_cwd },

    { "os.getenv", fl_os_getenv },
    { "os.setenv", fl_os_setenv },
    { NULL, NULL }
};

void builtins_init()
{
    assign_global_builtins(builtin_info);
    stringfuncs_init();
    table_init();
    iostream_init();
}
