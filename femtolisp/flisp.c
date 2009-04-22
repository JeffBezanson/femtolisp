/*
  femtoLisp

  a minimal interpreter for a minimal lisp dialect

  this lisp dialect uses lexical scope and self-evaluating lambda.
  it supports 30-bit integers, symbols, conses, and full macros.
  it is case-sensitive.
  it features a simple compacting copying garbage collector.
  it uses a Scheme-style evaluation rule where any expression may appear in
    head position as long as it evaluates to a function.
  it uses Scheme-style varargs (dotted formal argument lists)
  lambdas can have only 1 body expression; use (begin ...) for multiple
    expressions. this is due to the closure representation
    (lambda args body . env)

  This is a fully fleshed-out lisp built up from femtoLisp. It has all the
  remaining features needed to be taken seriously:
  * circular structure can be printed and read
  * #. read macro for eval-when-read and correctly printing builtins
  * read macros for backquote
  * symbol character-escaping printer
  * vectors
  * exceptions
  * gensyms (can be usefully read back in, too)
  * #| multiline comments |#
  * generic compare function, cyclic equal
  * cvalues system providing C data types and a C FFI
  * constructor notation for nicely printing arbitrary values
  * strings
  * hash tables
  * I/O streams

  by Jeff Bezanson (C) 2009
  Distributed under the BSD License
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <wctype.h>
#include <sys/types.h>
#include <locale.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include "llt.h"
#include "flisp.h"
#include "opcodes.h"

static char *builtin_names[] =
    { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      NULL, NULL, NULL, NULL, NULL,
      // predicates
      "eq?", "eqv?", "equal?", "atom?", "not", "null?", "boolean?", "symbol?",
      "number?", "bound?", "pair?", "builtin?", "vector?", "fixnum?",

      // lists
      "cons", "list", "car", "cdr", "set-car!", "set-cdr!",

      // execution
      "apply",

      // arithmetic
      "+", "-", "*", "/", "=", "<", "compare",

      // sequences
      "vector", "aref", "aset!",
      "", "", "" };

#define ANYARGS -10000

static short builtin_arg_counts[] =
    { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      2, ANYARGS, 1, 1, 2, 2,
      2,
      ANYARGS, -1, ANYARGS, -1, 2, 2, 2,
      ANYARGS, 2, 3 };

#define N_STACK 262144
value_t StaticStack[N_STACK];
value_t *Stack = StaticStack;
uint32_t SP = 0;

value_t NIL, FL_T, FL_F, LAMBDA, QUOTE, IF, TRYCATCH;
value_t BACKQUOTE, COMMA, COMMAAT, COMMADOT, FUNCTION;
value_t IOError, ParseError, TypeError, ArgError, UnboundError, MemoryError;
value_t DivideError, BoundsError, Error, KeyError, EnumerationError;
value_t conssym, symbolsym, fixnumsym, vectorsym, builtinsym, vu8sym;
value_t definesym, defmacrosym, forsym, labelsym, printprettysym, setqsym;
value_t printwidthsym, tsym, Tsym, fsym, Fsym, booleansym, nullsym, evalsym;
static fltype_t *functiontype;

static value_t apply_cl(uint32_t nargs);
static value_t *alloc_words(int n);
static value_t relocate(value_t v);

typedef struct _readstate_t {
    htable_t backrefs;
    htable_t gensyms;
    value_t source;
    struct _readstate_t *prev;
} readstate_t;
static readstate_t *readstate = NULL;

static void free_readstate(readstate_t *rs)
{
    htable_free(&rs->backrefs);
    htable_free(&rs->gensyms);
}

static unsigned char *fromspace;
static unsigned char *tospace;
static unsigned char *curheap;
static unsigned char *lim;
static uint32_t heapsize = 512*1024;//bytes
static uint32_t *consflags;

// error utilities ------------------------------------------------------------

// saved execution state for an unwind target
typedef struct _ectx_t {
    jmp_buf buf;
    uint32_t sp;
    readstate_t *rdst;
    struct _ectx_t *prev;
} exception_context_t;

static exception_context_t *ctx = NULL;
static value_t lasterror;

#define FL_TRY \
  exception_context_t _ctx; int l__tr, l__ca; \
  _ctx.sp=SP; _ctx.rdst=readstate; _ctx.prev=ctx; \
  ctx = &_ctx; \
  if (!setjmp(_ctx.buf)) \
      for (l__tr=1; l__tr; l__tr=0, (void)(ctx->prev && (ctx=ctx->prev)))

#define FL_CATCH \
  else \
      for (l__ca=1; l__ca; l__ca=0, lasterror=NIL)

void raise(value_t e)
{
    lasterror = e;
    // unwind read state
    while (readstate != ctx->rdst) {
        free_readstate(readstate);
        readstate = readstate->prev;
    }
    SP = ctx->sp;
    exception_context_t *thisctx = ctx;
    if (ctx->prev)   // don't throw past toplevel
        ctx = ctx->prev;
    longjmp(thisctx->buf, 1);
}

static value_t make_error_msg(char *format, va_list args)
{
    char msgbuf[512];
    vsnprintf(msgbuf, sizeof(msgbuf), format, args);
    return string_from_cstr(msgbuf);
}

void lerrorf(value_t e, char *format, ...)
{
    va_list args;
    PUSH(e);
    va_start(args, format);
    value_t msg = make_error_msg(format, args);
    va_end(args);

    e = POP();
    raise(list2(e, msg));
}

void lerror(value_t e, const char *msg)
{
    PUSH(e);
    value_t m = cvalue_static_cstring(msg);
    e = POP();
    raise(list2(e, m));
}

void type_error(char *fname, char *expected, value_t got)
{
    raise(listn(4, TypeError, symbol(fname), symbol(expected), got));
}

void bounds_error(char *fname, value_t arr, value_t ind)
{
    lerrorf(listn(3, BoundsError, arr, ind), "%s: index out of bounds", fname);
}

// safe cast operators --------------------------------------------------------

#define SAFECAST_OP(type,ctype,cnvt)                                          \
ctype to##type(value_t v, char *fname)                                        \
{                                                                             \
    if (__likely(is##type(v)))                                                \
        return (ctype)cnvt(v);                                                \
    type_error(fname, #type, v);                                              \
}
SAFECAST_OP(cons,  cons_t*,  ptr)
SAFECAST_OP(symbol,symbol_t*,ptr)
SAFECAST_OP(fixnum,fixnum_t, numval)
SAFECAST_OP(cvalue,cvalue_t*,ptr)
SAFECAST_OP(string,char*,    cvalue_data)

// symbol table ---------------------------------------------------------------

symbol_t *symtab = NULL;

static symbol_t *mk_symbol(char *str)
{
    symbol_t *sym;
    size_t len = strlen(str);

    sym = (symbol_t*)malloc(sizeof(symbol_t)-sizeof(void*) + len + 1);
    assert(((uptrint_t)sym & 0x7) == 0); // make sure malloc aligns 8
    sym->left = sym->right = NULL;
    if (str[0] == ':') {
        value_t s = tagptr(sym, TAG_SYM);
        setc(s, s);
    }
    else {
        sym->binding = UNBOUND;
        sym->syntax = 0;
    }
    sym->type = sym->dlcache = NULL;
    sym->hash = memhash32(str, len)^0xAAAAAAAA;
    strcpy(&sym->name[0], str);
    return sym;
}

static symbol_t **symtab_lookup(symbol_t **ptree, char *str)
{
    int x;

    while(*ptree != NULL) {
        x = strcmp(str, (*ptree)->name);
        if (x == 0)
            return ptree;
        if (x < 0)
            ptree = &(*ptree)->left;
        else
            ptree = &(*ptree)->right;
    }
    return ptree;
}

value_t symbol(char *str)
{
    symbol_t **pnode;

    pnode = symtab_lookup(&symtab, str);
    if (*pnode == NULL)
        *pnode = mk_symbol(str);
    return tagptr(*pnode, TAG_SYM);
}

typedef struct {
    value_t syntax;    // syntax environment entry
    value_t binding;   // global value binding
    fltype_t *type;
    uint32_t id;
} gensym_t;

static uint32_t _gensym_ctr=0;
// two static buffers for gensym printing so there can be two
// gensym names available at a time, mostly for compare()
static char gsname[2][16];
static int gsnameno=0;
value_t gensym(value_t *args, uint32_t nargs)
{
    (void)args;
    (void)nargs;
    gensym_t *gs = (gensym_t*)alloc_words(sizeof(gensym_t)/sizeof(void*));
    gs->id = _gensym_ctr++;
    gs->binding = UNBOUND;
    gs->syntax = 0;
    gs->type = NULL;
    return tagptr(gs, TAG_SYM);
}

value_t fl_gensym()
{
    return gensym(NULL, 0);
}

char *symbol_name(value_t v)
{
    if (ismanaged(v)) {
        gensym_t *gs = (gensym_t*)ptr(v);
        gsnameno = 1-gsnameno;
        char *n = uint2str(gsname[gsnameno]+1, sizeof(gsname[0])-1, gs->id, 10);
        *(--n) = 'g';
        return n;
    }
    return ((symbol_t*)ptr(v))->name;
}

// conses ---------------------------------------------------------------------

void gc(int mustgrow);

static value_t mk_cons(void)
{
    cons_t *c;

    if (__unlikely(curheap > lim))
        gc(0);
    c = (cons_t*)curheap;
    curheap += sizeof(cons_t);
    return tagptr(c, TAG_CONS);
}

static value_t *alloc_words(int n)
{
    value_t *first;

    assert(n > 0);
    n = ALIGN(n, 2);   // only allocate multiples of 2 words
    if (__unlikely((value_t*)curheap > ((value_t*)lim)+2-n)) {
        gc(0);
        while ((value_t*)curheap > ((value_t*)lim)+2-n) {
            gc(1);
        }
    }
    first = (value_t*)curheap;
    curheap += (n*sizeof(value_t));
    return first;
}

// allocate n consecutive conses
#define cons_reserve(n) tagptr(alloc_words((n)*2), TAG_CONS)

#define cons_index(c)  (((cons_t*)ptr(c))-((cons_t*)fromspace))
#define ismarked(c)    bitvector_get(consflags, cons_index(c))
#define mark_cons(c)   bitvector_set(consflags, cons_index(c), 1)
#define unmark_cons(c) bitvector_set(consflags, cons_index(c), 0)

static value_t the_empty_vector;

value_t alloc_vector(size_t n, int init)
{
    if (n == 0) return the_empty_vector;
    value_t *c = alloc_words(n+1);
    value_t v = tagptr(c, TAG_VECTOR);
    vector_setsize(v, n);
    if (init) {
        unsigned int i;
        for(i=0; i < n; i++)
            vector_elt(v, i) = NIL;
    }
    return v;
}

// cvalues --------------------------------------------------------------------

#include "cvalues.c"
#include "types.c"

// print ----------------------------------------------------------------------

static int isnumtok(char *tok, value_t *pval);
static int symchar(char c);

#include "print.c"

// collector ------------------------------------------------------------------

static value_t relocate(value_t v)
{
    value_t a, d, nc, first, *pcdr;
    uptrint_t t = tag(v);

    if (t == TAG_CONS) {
        // iterative implementation allows arbitrarily long cons chains
        pcdr = &first;
        do {
            if ((a=car_(v)) == TAG_FWD) {
                *pcdr = cdr_(v);
                return first;
            }
            *pcdr = nc = mk_cons();
            d = cdr_(v);
            car_(v) = TAG_FWD; cdr_(v) = nc;
            car_(nc) = relocate(a);
            pcdr = &cdr_(nc);
            v = d;
        } while (iscons(v));
        *pcdr = (d==NIL) ? NIL : relocate(d);
        return first;
    }

    if ((t&3) == 0) return v;
    if (!ismanaged(v)) return v;
    if (isforwarded(v)) return forwardloc(v);

    if (t == TAG_VECTOR) {
        // N.B.: 0-length vectors secretly have space for a first element
        size_t i, newsz, sz = vector_size(v);
        newsz = sz;
        if (vector_elt(v,-1) & 0x1)
            newsz += vector_grow_amt(sz);
        nc = tagptr(alloc_words(newsz+1), TAG_VECTOR);
        vector_setsize(nc, newsz);
        a = vector_elt(v,0);
        forward(v, nc);
        i = 0;
        if (sz > 0) {
            vector_elt(nc,0) = relocate(a); i++;
            for(; i < sz; i++)
                vector_elt(nc,i) = relocate(vector_elt(v,i));
        }
        for(; i < newsz; i++)
            vector_elt(nc,i) = NIL;
        return nc;
    }
    else if (t == TAG_CPRIM) {
        cprim_t *pcp = (cprim_t*)ptr(v);
        size_t nw = CPRIM_NWORDS-1+NWORDS(cp_class(pcp)->size);
        cprim_t *ncp = (cprim_t*)alloc_words(nw);
        while (nw--)
            ((value_t*)ncp)[nw] = ((value_t*)pcp)[nw];
        nc = tagptr(ncp, TAG_CPRIM);
        forward(v, nc);
        return nc;
    }
    else if (t == TAG_CVALUE) {
        return cvalue_relocate(v);
    }
    else if (t == TAG_SYM) {
        gensym_t *gs = (gensym_t*)ptr(v);
        gensym_t *ng = (gensym_t*)alloc_words(sizeof(gensym_t)/sizeof(void*));
        ng->id = gs->id;
        ng->binding = gs->binding;
        ng->syntax = gs->syntax;
        nc = tagptr(ng, TAG_SYM);
        forward(v, nc);
        if (ng->binding != UNBOUND)
            ng->binding = relocate(ng->binding);
        if (iscons(ng->syntax))
            ng->syntax = relocate(ng->syntax);
        return nc;
    }
    return v;
}

value_t relocate_lispvalue(value_t v)
{
    return relocate(v);
}

static void trace_globals(symbol_t *root)
{
    while (root != NULL) {
        if (root->binding != UNBOUND)
            root->binding = relocate(root->binding);
        if (iscons(root->syntax) || iscvalue(root->syntax))
            root->syntax = relocate(root->syntax);
        trace_globals(root->left);
        root = root->right;
    }
}

static value_t memory_exception_value;

void gc(int mustgrow)
{
    static int grew = 0;
    void *temp;
    uint32_t i;
    readstate_t *rs;

    curheap = tospace;
    lim = curheap+heapsize-sizeof(cons_t);

    for (i=0; i < SP; i++)
        Stack[i] = relocate(Stack[i]);
    trace_globals(symtab);
    relocate_typetable();
    rs = readstate;
    while (rs) {
        for(i=0; i < rs->backrefs.size; i++)
            rs->backrefs.table[i] = (void*)relocate((value_t)rs->backrefs.table[i]);
        for(i=0; i < rs->gensyms.size; i++)
            rs->gensyms.table[i] = (void*)relocate((value_t)rs->gensyms.table[i]);
        rs->source = relocate(rs->source);
        rs = rs->prev;
    }
    lasterror = relocate(lasterror);
    memory_exception_value = relocate(memory_exception_value);
    the_empty_vector = relocate(the_empty_vector);

    sweep_finalizers();

#ifdef VERBOSEGC
    printf("GC: found %d/%d live conses\n",
           (curheap-tospace)/sizeof(cons_t), heapsize/sizeof(cons_t));
#endif
    temp = tospace;
    tospace = fromspace;
    fromspace = temp;

    // if we're using > 80% of the space, resize tospace so we have
    // more space to fill next time. if we grew tospace last time,
    // grow the other half of the heap this time to catch up.
    if (grew || ((lim-curheap) < (int)(heapsize/5)) || mustgrow) {
        temp = realloc(tospace, grew ? heapsize : heapsize*2);
        if (temp == NULL)
            raise(memory_exception_value);
        tospace = temp;
        if (!grew) {
            heapsize*=2;
        }
        else {
            temp = bitvector_resize(consflags, heapsize/sizeof(cons_t), 1);
            if (temp == NULL)
                raise(memory_exception_value);
            consflags = (uint32_t*)temp;
        }
        grew = !grew;
    }
    if (curheap > lim)  // all data was live
        gc(0);
}

// utils ----------------------------------------------------------------------

// apply function with n args on the stack
static value_t _applyn(uint32_t n)
{
    value_t f = Stack[SP-n-1];
    uint32_t saveSP = SP;
    value_t v;
    if (isbuiltinish(f)) {
        if (uintval(f) > N_BUILTINS) {
            v = ((builtin_t)ptr(f))(&Stack[SP-n], n);
            SP = saveSP;
            return v;
        }
    }
    else if (isfunction(f)) {
        v = apply_cl(n);
        SP = saveSP;
        return v;
    }
    type_error("apply", "function", f);
}

value_t apply(value_t f, value_t l)
{
    value_t v = l;
    uint32_t n = SP;

    PUSH(f);
    while (iscons(v)) {
        if ((SP-n-1) == MAX_ARGS) {
            PUSH(v);
            break;
        }
        PUSH(car_(v));
        v = cdr_(v);
    }
    n = SP - n - 1;
    assert(n <= MAX_ARGS+1);
    v = _applyn(n);
    POPN(n+1);
    return v;
}

value_t applyn(uint32_t n, value_t f, ...)
{
    va_list ap;
    va_start(ap, f);
    size_t i;

    PUSH(f);
    for(i=0; i < n; i++) {
        value_t a = va_arg(ap, value_t);
        PUSH(a);
    }
    value_t v = _applyn(n);
    POPN(n+1);
    return v;
}

value_t listn(size_t n, ...)
{
    va_list ap;
    va_start(ap, n);
    uint32_t si = SP;
    size_t i;

    for(i=0; i < n; i++) {
        value_t a = va_arg(ap, value_t);
        PUSH(a);
    }
    cons_t *c = (cons_t*)alloc_words(n*2);
    cons_t *l = c;
    for(i=0; i < n; i++) {
        c->car = Stack[si++];
        c->cdr = tagptr(c+1, TAG_CONS);
        c++;
    }
    (c-1)->cdr = NIL;

    POPN(n);
    va_end(ap);
    return tagptr(l, TAG_CONS);
}

value_t list2(value_t a, value_t b)
{
    PUSH(a);
    PUSH(b);
    cons_t *c = (cons_t*)alloc_words(4);
    b = POP();
    a = POP();
    c[0].car = a;
    c[0].cdr = tagptr(c+1, TAG_CONS);
    c[1].car = b;
    c[1].cdr = NIL;
    return tagptr(c, TAG_CONS);
}

value_t fl_cons(value_t a, value_t b)
{
    PUSH(a);
    PUSH(b);
    value_t c = mk_cons();
    cdr_(c) = POP();
    car_(c) = POP();
    return c;
}

// NOTE: this is NOT an efficient operation. it is only used by the
// reader; vectors should not generally be resized.
// vector_grow requires at least 1 and up to 3 garbage collections!
static value_t vector_grow(value_t v)
{
    size_t s = vector_size(v);
    size_t d = vector_grow_amt(s);
    PUSH(v);
    // first allocate enough space to guarantee the heap will be big enough
    // for the new vector
    alloc_words(d);
    // setting low bit of vector's size acts as a flag to the collector
    // to grow this vector as it is relocated
    ((size_t*)ptr(Stack[SP-1]))[0] |= 0x1;
    gc(0);
    return POP();
}

int isnumber(value_t v)
{
    return (isfixnum(v) || iscprim(v));
}

// read -----------------------------------------------------------------------

#include "read.c"

// eval -----------------------------------------------------------------------

/*
  there is one interesting difference between this and (lambda x x).
  (eq a (apply list a)) is always false for nonempty a, while
  (eq a (apply (lambda x x) a)) is always true. the justification for this
  is that a vararg lambda often needs to recur by applying itself to the
  tail of its argument list, so copying the list would be unacceptable.
*/
static value_t list(value_t *args, uint32_t nargs)
{
    cons_t *c;
    uint32_t i;
    value_t v;
    v = cons_reserve(nargs);
    c = (cons_t*)ptr(v);
    for(i=0; i < nargs; i++) {
        c->car = args[i];
        c->cdr = tagptr(c+1, TAG_CONS);
        c++;
    }
    if (nargs > MAX_ARGS)
        (c-2)->cdr = (c-1)->car;
    else
        (c-1)->cdr = NIL;
    return v;
}

static value_t do_trycatch()
{
    uint32_t saveSP = SP;
    value_t v;
    value_t thunk = Stack[SP-2];
    Stack[SP-2] = Stack[SP-1];
    Stack[SP-1] = thunk;

    FL_TRY {
        v = apply_cl(0);
    }
    FL_CATCH {
        Stack[SP-1] = lasterror;
        v = apply_cl(1);
    }
    SP = saveSP;
    return v;
}

/*
  stack on entry: <func>  <args...>
  caller's responsibility:
  - put the stack in this state
  - provide arg count
  - respect tail position
  - call correct entry point (either eval_sexpr or apply_cl)
  - restore SP

  callee's responsibility:
  - check arg counts
  - allocate vararg array
  - push closed env, set up new environment

  ** need 'copyenv' instruction that moves env to heap, installs
     heap version as the current env, and pushes the result vector.
     this can be used to implement the copy-closure op in terms of
     other ops. and it can be the first instruction in lambdas in
     head position (let optimization).
*/
static value_t apply_cl(uint32_t nargs)
{
    uint32_t i, n, ip, bp, envsz, captured, op;
    fixnum_t s, lo, hi;
    int64_t accum;
    uint8_t *code;
    value_t func, v, x, e;
    function_t *fn;
    value_t *pvals, *lenv, *pv;
    symbol_t *sym;
    cons_t *c;

 apply_cl_top:
    captured = 0;
    func = Stack[SP-nargs-1];
    fn = value2c(function_t*,func);
    code = cv_data((cvalue_t*)ptr(fn->bcode));
    assert(!ismanaged((uptrint_t)code));
    assert(ismanaged(func));
    assert(ismanaged(fn->bcode));
    if (nargs < code[1])
        lerror(ArgError, "apply: too few arguments");

    bp = SP-nargs;
    PUSH(fn->env);
    PUSH(fn->vals);
    pvals = &Stack[SP-1];

    ip = 0;
    while (1) {
        op = code[ip++];
    dispatch:
        switch (op) {
        case OP_ARGC:
            if (nargs > code[ip++]) {
                lerror(ArgError, "apply: too many arguments");
            }
            break;
        case OP_VARGC:
            i = code[ip++];
            s = (fixnum_t)nargs - (fixnum_t)i;
            v = NIL;
            if (s > 0) {
                v = list(&Stack[bp+i], s);
                if (nargs > MAX_ARGS) {
                    c = (cons_t*)curheap;
                    (c-2)->cdr = (c-1)->car;
                }
                Stack[bp+i] = v;
                Stack[bp+i+1] = Stack[bp+nargs];
                Stack[bp+i+2] = Stack[bp+nargs+1];
                pvals = &Stack[bp+i+2];
            }
            else {
                PUSH(NIL);
                Stack[SP-1] = Stack[SP-2];
                Stack[SP-2] = Stack[SP-3];
                Stack[SP-3] = NIL;
                pvals = &Stack[SP-1];
            }
            nargs = i+1;
            break;
        case OP_LET:
            ip++;
            // last arg is closure environment to use
            nargs--;
            Stack[SP-2] = Stack[SP-1];
            POPN(1);
            pvals = &Stack[SP-1];
            break;
        case OP_NOP: break;
        case OP_DUP: v = Stack[SP-1]; PUSH(v); break;
        case OP_POP: POPN(1); break;
        case OP_TCALL:
        case OP_CALL:
            n = code[ip++];  // nargs
        do_call:
            s = SP;
            func = Stack[SP-n-1];
            if (isfunction(func)) {
                if (op == OP_TCALL) {
                    for(s=-1; s < (fixnum_t)n; s++)
                        Stack[bp+s] = Stack[SP-n+s];
                    SP = bp+n;
                    nargs = n;
                    goto apply_cl_top;
                }
                else {
                    v = apply_cl(n);
                }
            }
            else if (isbuiltinish(func)) {
                op = uintval(func);
                if (op > N_BUILTINS) {
                    v = ((builtin_t)ptr(func))(&Stack[SP-n], n);
                }
                else {
                    s = builtin_arg_counts[op];
                    if (s >= 0)
                        argcount(builtin_names[op], n, s);
                    else if (s != ANYARGS && (signed)n < -s)
                        argcount(builtin_names[op], n, -s);
                    // remove function arg
                    for(s=SP-n-1; s < (int)SP-1; s++)
                        Stack[s] = Stack[s+1];
                    SP--;
                    switch (op) {
                    case OP_LIST:   goto apply_list;
                    case OP_ADD:    goto apply_add;
                    case OP_SUB:    goto apply_sub;
                    case OP_MUL:    goto apply_mul;
                    case OP_DIV:    goto apply_div;
                    case OP_VECTOR: goto apply_vector;
                    default:
                        goto dispatch;
                    }
                }
            }
            else {
                type_error("apply", "function", func);
            }
            SP = s-n-1;
            PUSH(v);
            break;
        case OP_JMP: ip = (uint32_t)*(uint16_t*)&code[ip]; break;
        case OP_BRF:
            v = POP();
            if (v == FL_F) ip = (uint32_t)*(uint16_t*)&code[ip];
            else ip += 2;
            break;
        case OP_BRT:
            v = POP();
            if (v != FL_F) ip = (uint32_t)*(uint16_t*)&code[ip];
            else ip += 2;
            break;
        case OP_JMPL: ip = *(uint32_t*)&code[ip]; break;
        case OP_BRFL:
            v = POP();
            if (v == FL_F) ip = *(uint32_t*)&code[ip];
            else ip += 4;
            break;
        case OP_BRTL:
            v = POP();
            if (v != FL_F) ip = *(uint32_t*)&code[ip];
            else ip += 4;
            break;
        case OP_RET: v = POP(); return v;

        case OP_EQ:
            Stack[SP-2] = ((Stack[SP-2] == Stack[SP-1]) ? FL_T : FL_F);
            POPN(1); break;
        case OP_EQV:
            if (Stack[SP-2] == Stack[SP-1]) {
                v = FL_T;
            }
            else if (!leafp(Stack[SP-2]) || !leafp(Stack[SP-1])) {
                v = FL_F;
            }
            else {
                v = (numval(compare(Stack[SP-2], Stack[SP-1]))==0) ?
                    FL_T : FL_F;
            }
            Stack[SP-2] = v; POPN(1);
            break;
        case OP_EQUAL:
            if (Stack[SP-2] == Stack[SP-1]) {
                v = FL_T;
            }
            else if (eq_comparable(Stack[SP-2],Stack[SP-1])) {
                v = FL_F;
            }
            else {
                v = (numval(compare(Stack[SP-2], Stack[SP-1]))==0) ?
                    FL_T : FL_F;
            }
            Stack[SP-2] = v; POPN(1);
            break;
        case OP_PAIRP:
            Stack[SP-1] = (iscons(Stack[SP-1]) ? FL_T : FL_F); break;
        case OP_ATOMP:
            Stack[SP-1] = (iscons(Stack[SP-1]) ? FL_F : FL_T); break;
        case OP_NOT:
            Stack[SP-1] = ((Stack[SP-1]==FL_F) ? FL_T : FL_F); break;
        case OP_NULLP:
            Stack[SP-1] = ((Stack[SP-1]==NIL) ? FL_T : FL_F); break;
        case OP_BOOLEANP:
            v = Stack[SP-1];
            Stack[SP-1] = ((v == FL_T || v == FL_F) ? FL_T : FL_F); break;
        case OP_SYMBOLP:
            Stack[SP-1] = (issymbol(Stack[SP-1]) ? FL_T : FL_F); break;
        case OP_NUMBERP:
            v = Stack[SP-1];
            Stack[SP-1] = (isfixnum(v) || iscprim(v) ? FL_T : FL_F); break;
        case OP_FIXNUMP:
            Stack[SP-1] = (isfixnum(Stack[SP-1]) ? FL_T : FL_F); break;
        case OP_BOUNDP:
            sym = tosymbol(Stack[SP-1], "bound?");
            Stack[SP-1] = ((sym->binding == UNBOUND) ? FL_F : FL_T);
            break;
        case OP_BUILTINP:
            v = Stack[SP-1];
            Stack[SP-1] = ((isbuiltinish(v) && v!=FL_F && v!=FL_T && v!=NIL)
                           ? FL_T : FL_F);
            break;
        case OP_VECTORP:
            Stack[SP-1] = (isvector(Stack[SP-1]) ? FL_T : FL_F); break;

        case OP_CONS:
            if (curheap > lim)
                gc(0);
            c = (cons_t*)curheap;
            curheap += sizeof(cons_t);
            c->car = Stack[SP-2];
            c->cdr = Stack[SP-1];
            Stack[SP-2] = tagptr(c, TAG_CONS);
            POPN(1); break;
        case OP_CAR:
            v = Stack[SP-1];
            if (!iscons(v)) type_error("car", "cons", v);
            Stack[SP-1] = car_(v);
            break;
        case OP_CDR:
            v = Stack[SP-1];
            if (!iscons(v)) type_error("cdr", "cons", v);
            Stack[SP-1] = cdr_(v);
            break;
        case OP_SETCAR:
            car(Stack[SP-2]) = Stack[SP-1];
            POPN(1); break;
        case OP_SETCDR:
            cdr(Stack[SP-2]) = Stack[SP-1];
            POPN(1); break;
        case OP_LIST:
            n = code[ip++];
        apply_list:
            if (n > 0) {
                v = list(&Stack[SP-n], n);
                POPN(n);
                PUSH(v);
            }
            else {
                PUSH(NIL);
            }
            break;

        case OP_TAPPLY:
        case OP_APPLY:
            v = POP();  // arglist
            n = SP;
            while (iscons(v)) {
                if (SP-n == MAX_ARGS) {
                    PUSH(v);
                    break;
                }
                PUSH(car_(v));
                v = cdr_(v);
            }
            n = SP-n;
            if (op==OP_TAPPLY) op = OP_TCALL;
            goto do_call;

        case OP_ADD:
            n = code[ip++];
        apply_add:
            s = 0;
            i = SP-n;
            if (n > MAX_ARGS) goto add_ovf;
            for (; i < SP; i++) {
                if (__likely(isfixnum(Stack[i]))) {
                    s += numval(Stack[i]);
                    if (__unlikely(!fits_fixnum(s))) {
                        i++;
                        goto add_ovf;
                    }
                }
                else {
                add_ovf:
                    v = fl_add_any(&Stack[i], SP-i, s);
                    break;
                }
            }
            if (i==SP)
                v = fixnum(s);
            POPN(n);
            PUSH(v);
            break;
        case OP_SUB:
            n = code[ip++];
        apply_sub:
            i = SP-n;
            if (n == 1) {
                if (__likely(isfixnum(Stack[i])))
                    Stack[SP-1] = fixnum(-numval(Stack[i]));
                else
                    Stack[SP-1] = fl_neg(Stack[i]);
                break;
            }
            if (n == 2) {
                if (__likely(bothfixnums(Stack[i], Stack[i+1]))) {
                    s = numval(Stack[i]) - numval(Stack[i+1]);
                    if (__likely(fits_fixnum(s))) {
                        POPN(1);
                        Stack[SP-1] = fixnum(s);
                        break;
                    }
                    Stack[i+1] = fixnum(-numval(Stack[i+1]));
                }
                else {
                    Stack[i+1] = fl_neg(Stack[i+1]);
                }
            }
            else {
                // we need to pass the full arglist on to fl_add_any
                // so it can handle rest args properly
                PUSH(Stack[i]);
                Stack[i] = fixnum(0);
                Stack[i+1] = fl_neg(fl_add_any(&Stack[i], n, 0));
                Stack[i] = POP();
            }
            v = fl_add_any(&Stack[i], 2, 0);
            POPN(n);
            PUSH(v);
            break;
        case OP_MUL:
            n = code[ip++];
        apply_mul:
            accum = 1;
            i = SP-n;
            if (n > MAX_ARGS) goto mul_ovf;
            for (; i < SP; i++) {
                if (__likely(isfixnum(Stack[i]))) {
                    accum *= numval(Stack[i]);
                }
                else {
                mul_ovf:
                    v = fl_mul_any(&Stack[i], SP-i, accum);
                    break;
                }
            }
            if (i == SP) {
                if (__likely(fits_fixnum(accum)))
                    v = fixnum(accum);
                else
                    v = return_from_int64(accum);
            }
            POPN(n);
            PUSH(v);
            break;
        case OP_DIV:
            n = code[ip++];
        apply_div:
            i = SP-n;
            if (n == 1) {
                Stack[SP-1] = fl_div2(fixnum(1), Stack[i]);
            }
            else {
                if (n > 2) {
                    PUSH(Stack[i]);
                    Stack[i] = fixnum(1);
                    Stack[i+1] = fl_mul_any(&Stack[i], n, 1);
                    Stack[i] = POP();
                }
                v = fl_div2(Stack[i], Stack[i+1]);
                POPN(n);
                PUSH(v);
            }
            break;
        case OP_NUMEQ:
            v = Stack[SP-2]; e = Stack[SP-1];
            if (bothfixnums(v, e)) {
                v = (v == e) ? FL_T : FL_F;
            }
            else {
                v = (!numeric_compare(v,e,1,0,"=")) ? FL_T : FL_F;
            }
            POPN(1);
            Stack[SP-1] = v;
            break;
        case OP_LT:
            if (bothfixnums(Stack[SP-2], Stack[SP-1])) {
                v = (numval(Stack[SP-2]) < numval(Stack[SP-1])) ? FL_T : FL_F;
            }
            else {
                v = (numval(compare(Stack[SP-2], Stack[SP-1])) < 0) ?
                    FL_T : FL_F;
            }
            POPN(1);
            Stack[SP-1] = v;
            break;
        case OP_COMPARE:
            Stack[SP-2] = compare(Stack[SP-2], Stack[SP-1]);
            POPN(1);
            break;

        case OP_VECTOR:
            n = code[ip++];
        apply_vector:
            if (n > MAX_ARGS) {
                i = llength(Stack[SP-1])-1;
            }
            else i = 0;
            v = alloc_vector(n+i, 0);
            if (n) {
                memcpy(&vector_elt(v,0), &Stack[SP-n], n*sizeof(value_t));
                e = POP();
                POPN(n-1);
            }
            if (n > MAX_ARGS) {
                i = n-1;
                while (iscons(e)) {
                    vector_elt(v,i) = car_(e);
                    i++;
                    e = cdr_(e);
                }
            }
            PUSH(v);
            break;

        case OP_AREF:
            v = Stack[SP-2];
            if (isvector(v)) {
                i = tofixnum(Stack[SP-1], "aref");
                if (__unlikely((unsigned)i >= vector_size(v)))
                    bounds_error("aref", v, Stack[SP-1]);
                v = vector_elt(v, i);
            }
            else if (isarray(v)) {
                v = cvalue_array_aref(&Stack[SP-2]);
            }
            else {
                type_error("aref", "sequence", v);
            }
            POPN(1);
            Stack[SP-1] = v;
            break;
        case OP_ASET:
            e = Stack[SP-3];
            if (isvector(e)) {
                i = tofixnum(Stack[SP-2], "aset!");
                if (__unlikely((unsigned)i >= vector_size(e)))
                    bounds_error("aset!", v, Stack[SP-1]);
                vector_elt(e, i) = (v=Stack[SP-1]);
            }
            else if (isarray(e)) {
                v = cvalue_array_aset(&Stack[SP-3]);
            }
            else {
                type_error("aset!", "sequence", e);
            }
            POPN(2);
            Stack[SP-1] = v;
            break;
        case OP_FOR:
            lo = tofixnum(Stack[SP-3], "for");
            hi = tofixnum(Stack[SP-2], "for");
            //f = Stack[SP-1];
            v = FL_F;
            SP += 2;
            i = SP;
            for(s=lo; s <= hi; s++) {
                Stack[SP-2] = Stack[SP-3];
                Stack[SP-1] = fixnum(s);
                v = apply_cl(1);
                SP = i;
            }
            POPN(4);
            Stack[SP-1] = v;
            break;

        case OP_LOADT: PUSH(FL_T); break;
        case OP_LOADF: PUSH(FL_F); break;
        case OP_LOADNIL: PUSH(NIL); break;
        case OP_LOAD0: PUSH(fixnum(0)); break;
        case OP_LOAD1: PUSH(fixnum(1)); break;
        case OP_LOADI8: s = (int8_t)code[ip++]; PUSH(fixnum(s)); break;
        case OP_LOADV:
            assert(code[ip] < vector_size(*pvals));
            v = vector_elt(*pvals, code[ip]); ip++;
            PUSH(v);
            break;
        case OP_LOADVL:
            v = vector_elt(*pvals, *(uint32_t*)&code[ip]); ip+=4;
            PUSH(v);
            break;
        case OP_LOADGL:
            v = vector_elt(*pvals, *(uint32_t*)&code[ip]); ip+=4;
            goto do_loadg;
        case OP_LOADG:
            assert(code[ip] < vector_size(*pvals));
            v = vector_elt(*pvals, code[ip]); ip++;
        do_loadg:
            assert(issymbol(v));
            sym = (symbol_t*)ptr(v);
            if (sym->binding == UNBOUND)
                raise(list2(UnboundError, v));
            PUSH(sym->binding);
            break;

        case OP_SETGL:
            v = vector_elt(*pvals, *(uint32_t*)&code[ip]); ip+=4;
            goto do_setg;
        case OP_SETG:
            assert(code[ip] < vector_size(*pvals));
            v = vector_elt(*pvals, code[ip]); ip++;
        do_setg:
            assert(issymbol(v));
            sym = (symbol_t*)ptr(v);
            v = Stack[SP-1];
            if (sym->syntax != TAG_CONST)
                sym->binding = v;
            break;

        case OP_LOADA:
            assert(nargs > 0);
            i = code[ip++];
            if (captured) {
                x = Stack[bp];
                assert(isvector(x));
                assert(i < vector_size(x));
                v = vector_elt(x, i);
            }
            else {
                assert(bp+i < SP);
                v = Stack[bp+i];
            }
            PUSH(v);
            break;
        case OP_SETA:
            assert(nargs > 0);
            v = Stack[SP-1];
            i = code[ip++];
            if (captured) {
                x = Stack[bp];
                assert(isvector(x));
                assert(i < vector_size(x));
                vector_elt(x, i) = v;
            }
            else {
                assert(bp+i < SP);
                Stack[bp+i] = v;
            }
            break;
        case OP_LOADC:
        case OP_SETC:
            s = code[ip++];
            i = code[ip++];
            v = Stack[bp+nargs];
            while (s--)
                v = vector_elt(v, vector_size(v)-1);
            assert(isvector(v));
            assert(i < vector_size(v));
            if (op == OP_SETC)
                vector_elt(v, i) = Stack[SP-1];
            else
                PUSH(vector_elt(v, i));
            break;

        case OP_CLOSURE:
        case OP_CLOSE:
            // build a closure (lambda args body . env)
            if (nargs > 0 && !captured) {
                // save temporary environment to the heap
                lenv = &Stack[bp];
                envsz = nargs+1;
                pv = alloc_words(envsz + 1);
                PUSH(tagptr(pv, TAG_VECTOR));
                pv[0] = fixnum(envsz);
                pv++;
                while (envsz--)
                    *pv++ = *lenv++;
                // environment representation changed; install
                // the new representation so everybody can see it
                captured = 1;
                Stack[bp] = Stack[SP-1];
            }
            else {
                PUSH(Stack[bp]); // env has already been captured; share
            }
            if (op == OP_CLOSURE) {
                pv = alloc_words(6);
                x = Stack[SP-2];  // closure to copy
                assert(isfunction(x));
                pv[0] = ((value_t*)ptr(x))[0];
                pv[1] = (value_t)&pv[3];
                pv[2] = ((value_t*)ptr(x))[2];
                pv[3] = ((value_t*)ptr(x))[3];
                pv[4] = ((value_t*)ptr(x))[4];
                pv[5] = Stack[SP-1];  // env
                POPN(1);
                Stack[SP-1] = tagptr(pv, TAG_CVALUE);
            }
            break;

        case OP_TRYCATCH:
            v = do_trycatch();
            POPN(1);
            Stack[SP-1] = v;
            break;
        }
    }
}

// initialization -------------------------------------------------------------

extern void builtins_init();
extern void comparehash_init();

static char *EXEDIR = NULL;

void assign_global_builtins(builtinspec_t *b)
{
    while (b->name != NULL) {
        set(symbol(b->name), cbuiltin(b->name, b->fptr));
        b++;
    }
}

static void print_function(value_t v, ios_t *f, int princ)
{
    (void)princ;
    function_t *fn = value2c(function_t*,v);
    outs("#function(", f);
    char *data = cvalue_data(fn->bcode);
    size_t sz = cvalue_len(fn->bcode);
    outc('"', f);
    size_t i; uint8_t c;
    for(i=0; i < sz; i++) {
        c = data[i]+48;
        if (c == '\\')
            outsn("\\\\", f, 2);
        else if (c == '"')
            outsn("\\\"", f, 2);
        else if (c >= 32 && c < 0x7f)
            outc(c, f);
        else
            ios_printf(f, "\\x%02x", c);
    }
    outsn("\" ", f, 2);
    //fl_print_child(f, fn->bcode, 0);
    //outc(' ', f);
    fl_print_child(f, fn->vals, 0);
    if (fn->env != NIL) {
        outc(' ', f);
        fl_print_child(f, fn->env, 0);
    }
    outc(')', f);
}

static void print_traverse_function(value_t v)
{
    function_t *fn = value2c(function_t*,v);
    print_traverse(fn->bcode);
    print_traverse(fn->vals);
    print_traverse(fn->env);
}

static void relocate_function(value_t oldv, value_t newv)
{
    (void)oldv;
    function_t *fn = value2c(function_t*,newv);
    fn->bcode = relocate(fn->bcode);
    fn->vals = relocate(fn->vals);
    fn->env = relocate(fn->env);
}

static value_t fl_function(value_t *args, uint32_t nargs)
{
    if (nargs != 3)
        argcount("function", nargs, 2);
    if (!isvector(args[1]))
        type_error("function", "vector", args[1]);
    cvalue_t *arr = (cvalue_t*)ptr(args[0]);
    cv_pin(arr);
    char *data = cv_data(arr);
    if (data[0] >= N_OPCODES) {
        // read syntax, shifted 48 for compact text representation
        size_t i, sz = cv_len(arr);
        for(i=0; i < sz; i++)
            data[i] -= 48;
    }
    value_t fv = cvalue(functiontype, sizeof(function_t));
    function_t *fn = value2c(function_t*,fv);
    fn->bcode = args[0];
    fn->vals = args[1];
    if (nargs == 3)
        fn->env = args[2];
    else
        fn->env = NIL;
    return fv;
}

static value_t fl_function2vector(value_t *args, uint32_t nargs)
{
    argcount("function->vector", nargs, 1);
    value_t v = args[0];
    if (!iscvalue(v) || cv_class((cvalue_t*)ptr(v)) != functiontype)
        type_error("function->vector", "function", v);
    value_t vec = alloc_vector(3, 0);
    function_t *fn = value2c(function_t*,args[0]);
    vector_elt(vec,0) = fn->bcode;
    vector_elt(vec,1) = fn->vals;
    vector_elt(vec,2) = fn->env;
    return vec;
}

static cvtable_t function_vtable = { print_function, relocate_function,
                                     NULL, print_traverse_function };

static builtinspec_t core_builtin_info[] = {
    { "function", fl_function },
    { "function->vector", fl_function2vector },
    { "gensym", gensym },
    { "hash", fl_hash },
    { NULL, NULL }
};

static void lisp_init(void)
{
    int i;

    llt_init();

    fromspace = malloc(heapsize);
    tospace   = malloc(heapsize);
    curheap = fromspace;
    lim = curheap+heapsize-sizeof(cons_t);
    consflags = bitvector_new(heapsize/sizeof(cons_t), 1);
    htable_new(&printconses, 32);
    comparehash_init();

    NIL = builtin(F_NIL);
    FL_T = builtin(F_TRUE);
    FL_F = builtin(F_FALSE);
    LAMBDA = symbol("lambda");
    FUNCTION = symbol("function");
    QUOTE = symbol("quote");
    TRYCATCH = symbol("trycatch");
    BACKQUOTE = symbol("backquote");
    COMMA = symbol("*comma*");
    COMMAAT = symbol("*comma-at*");
    COMMADOT = symbol("*comma-dot*");
    IOError = symbol("io-error");
    ParseError = symbol("parse-error");
    TypeError = symbol("type-error");
    ArgError = symbol("arg-error");
    UnboundError = symbol("unbound-error");
    KeyError = symbol("key-error");
    MemoryError = symbol("memory-error");
    BoundsError = symbol("bounds-error");
    DivideError = symbol("divide-error");
    EnumerationError = symbol("enumeration-error");
    Error = symbol("error");
    conssym = symbol("cons");
    symbolsym = symbol("symbol");
    fixnumsym = symbol("fixnum");
    vectorsym = symbol("vector");
    builtinsym = symbol("builtin");
    booleansym = symbol("boolean");
    nullsym = symbol("null");
    definesym = symbol("define");
    defmacrosym = symbol("define-macro");
    forsym = symbol("for");
    labelsym = symbol("label");
    setqsym = symbol("set!");
    evalsym = symbol("eval");
    vu8sym = symbol("vu8");
    tsym = symbol("t"); Tsym = symbol("T");
    fsym = symbol("f"); Fsym = symbol("F");
    set(printprettysym=symbol("*print-pretty*"), FL_T);
    set(printwidthsym=symbol("*print-width*"), fixnum(SCR_WIDTH));
    lasterror = NIL;
    i = 0;
    for (i=F_EQ; i < F_TRUE; i++) {
        setc(symbol(builtin_names[i]), builtin(i));
    }
    setc(symbol("eq"), builtin(F_EQ));
    setc(symbol("equal"), builtin(F_EQUAL));

#ifdef LINUX
    setc(symbol("*os-name*"), symbol("linux"));
#elif defined(WIN32) || defined(WIN64)
    setc(symbol("*os-name*"), symbol("win32"));
#elif defined(MACOSX)
    setc(symbol("*os-name*"), symbol("macos"));
#else
    setc(symbol("*os-name*"), symbol("unknown"));
#endif

    cvalues_init();

    char buf[1024];
    char *exename = get_exename(buf, sizeof(buf));
    if (exename != NULL) {
        path_to_dirname(exename);
        EXEDIR = strdup(exename);
        setc(symbol("*install-dir*"), cvalue_static_cstring(EXEDIR));
    }

    memory_exception_value = list2(MemoryError,
                                   cvalue_static_cstring("out of memory"));

    the_empty_vector = tagptr(alloc_words(1), TAG_VECTOR);
    vector_setsize(the_empty_vector, 0);

    functiontype = define_opaque_type(FUNCTION, sizeof(function_t),
                                      &function_vtable, NULL);

    assign_global_builtins(core_builtin_info);

    builtins_init();
}

// repl -----------------------------------------------------------------------

value_t toplevel_eval(value_t expr)
{
    value_t v;
    uint32_t saveSP = SP;
    PUSH(symbol_value(evalsym));
    PUSH(expr);
    v = apply_cl(1);
    SP = saveSP;
    return v;
}

static value_t argv_list(int argc, char *argv[])
{
    int i;
    PUSH(NIL);
    for(i=argc-1; i >= 0; i--) {
        PUSH(cvalue_static_cstring(argv[i]));
        Stack[SP-2] = fl_cons(Stack[SP-1], Stack[SP-2]);
        POPN(1);
    }
    return POP();
}

int locale_is_utf8;

extern value_t fl_file(value_t *args, uint32_t nargs);

int main(int argc, char *argv[])
{
    value_t e, v;
    int saveSP;
    symbol_t *sym;
    char fname_buf[1024];

    locale_is_utf8 = u8_is_locale_utf8(setlocale(LC_ALL, ""));

    lisp_init();

    fname_buf[0] = '\0';
    if (EXEDIR != NULL) {
        strcat(fname_buf, EXEDIR);
        strcat(fname_buf, PATHSEPSTRING);
    }
    strcat(fname_buf, "flisp.boot");

    FL_TRY {
        // install toplevel exception handler
        PUSH(cvalue_static_cstring(fname_buf));
        PUSH(symbol(":read"));
        value_t f = fl_file(&Stack[SP-2], 2);
        POPN(2);
        PUSH(f); saveSP = SP;
        while (1) {
            e = read_sexpr(Stack[SP-1]);
            if (ios_eof(value2c(ios_t*,Stack[SP-1]))) break;
            if (isfunction(e)) {
                // stage 0 format: series of thunks
                PUSH(e);
                (void)_applyn(0);
                SP = saveSP;
            }
            else {
                // stage 1 format: symbol/value pairs
                sym = tosymbol(e, "bootstrap");
                v = read_sexpr(Stack[SP-1]);
                sym->binding = v;
            }
        }
        ios_close(value2c(ios_t*,Stack[SP-1]));
        POPN(1);

        PUSH(symbol_value(symbol("__start")));
        PUSH(argv_list(argc, argv));
        (void)_applyn(1);
    }
    FL_CATCH {
        ios_puts("fatal error during bootstrap:\n", ios_stderr);
        print(ios_stderr, lasterror, 0);
        ios_putc('\n', ios_stderr);
        return 1;
    }

    return 0;
}
