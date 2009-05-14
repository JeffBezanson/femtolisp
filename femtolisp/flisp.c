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
      NULL, NULL, NULL, NULL,
      // predicates
      "eq?", "eqv?", "equal?", "atom?", "not", "null?", "boolean?", "symbol?",
      "number?", "bound?", "pair?", "builtin?", "vector?", "fixnum?",
      "function?",

      // lists
      "cons", "list", "car", "cdr", "set-car!", "set-cdr!",

      // execution
      "apply",

      // arithmetic
      "+", "-", "*", "/", "div0", "=", "<", "compare",

      // sequences
      "vector", "aref", "aset!",
      "", "", "" };

#define ANYARGS -10000

static short builtin_arg_counts[] =
    { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      2, ANYARGS, 1, 1, 2, 2,
      -2,
      ANYARGS, -1, ANYARGS, -1, 2,  2, 2, 2,
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
value_t printwidthsym, printreadablysym;
value_t tsym, Tsym, fsym, Fsym, booleansym, nullsym, evalsym;

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
    if (is##type(v))                                                          \
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
value_t fl_gensym(value_t *args, uint32_t nargs)
{
    argcount("gensym", nargs, 0);
    (void)args;
    gensym_t *gs = (gensym_t*)alloc_words(sizeof(gensym_t)/sizeof(void*));
    gs->id = _gensym_ctr++;
    gs->binding = UNBOUND;
    gs->syntax = 0;
    gs->type = NULL;
    return tagptr(gs, TAG_SYM);
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
        size_t i, sz = vector_size(v);
        if (vector_elt(v,-1) & 0x1) {
            // grown vector
            nc = relocate(vector_elt(v,0));
            forward(v, nc);
        }
        else {
            nc = tagptr(alloc_words(sz+1), TAG_VECTOR);
            vector_setsize(nc, sz);
            a = vector_elt(v,0);
            forward(v, nc);
            if (sz > 0) {
                vector_elt(nc,0) = relocate(a);
                for(i=1; i < sz; i++)
                    vector_elt(nc,i) = relocate(vector_elt(v,i));
            }
        }
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
    else if (t == TAG_FUNCTION) {
        function_t *fn = (function_t*)ptr(v);
        function_t *nfn = (function_t*)alloc_words(4);
        nfn->bcode = fn->bcode;
        nfn->vals = fn->vals;
        nc = tagptr(nfn, TAG_FUNCTION);
        forward(v, nc);
        nfn->env = relocate(fn->env);
        nfn->vals = relocate(nfn->vals);
        nfn->bcode = relocate(nfn->bcode);
        return nc;
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
    if (iscbuiltin(f)) {
        v = ((builtin_t*)ptr(f))[3](&Stack[SP-n], n);
    }
    else if (isfunction(f)) {
        v = apply_cl(n);
    }
    else {
        type_error("apply", "function", f);
    }
    SP = saveSP;
    return v;
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

int isnumber(value_t v)
{
    return (isfixnum(v) || iscprim(v));
}

// read -----------------------------------------------------------------------

#include "read.c"

// eval -----------------------------------------------------------------------

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

// perform (apply list* L)
// like the function list() above, but takes arguments from a list
// rather than from an array (the stack)
static value_t apply_liststar(value_t L)
{
    PUSH(NIL);
    PUSH(NIL);
    PUSH(L);
    value_t *pfirst = &Stack[SP-3];
    value_t *plcons = &Stack[SP-2];
    value_t *pL = &Stack[SP-1];
    value_t c;
    while (iscons(*pL)) {
        if (iscons(cdr_(*pL))) {
            c = mk_cons();
            car_(c) = car_(*pL);
            cdr_(c) = NIL;
        }
        else {
            // last element; becomes final CDR
            c = car_(*pL);
        }
        if (*pfirst == NIL)
            *pfirst = c;
        else
            cdr_(*plcons) = c;
        *plcons = c;
        *pL = cdr_(*pL);
    }
    POPN(2);
    return POP();
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

#define fn_bcode(f) (((value_t*)ptr(f))[0])
#define fn_vals(f) (((value_t*)ptr(f))[1])
#define fn_env(f) (((value_t*)ptr(f))[2])

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
*/
static value_t apply_cl(uint32_t nargs)
{
    // frame variables
    uint32_t i, n, ip, bp, captured;
    fixnum_t s, hi;
    uint8_t *code;

    // temporary variables (not necessary to preserve across calls)
    uint32_t op, envsz;
    int64_t accum;
    symbol_t *sym;
    cons_t *c;
    value_t func, v, x, e;
    value_t *lenv, *pv;

 apply_cl_top:
    captured = 0;
    func = Stack[SP-nargs-1];
    code = cv_data((cvalue_t*)ptr(fn_bcode(func)));
    assert(!ismanaged((uptrint_t)code));
    assert(ismanaged(func));

    bp = SP-nargs;
    PUSH(fn_env(func));

    ip = 0;
    { 
    next_op:
        op = code[ip++];
    dispatch:
        switch (op) {
        case OP_ARGC:
            n = code[ip++];
            if (nargs != n) {
                if (nargs > n)
                    lerror(ArgError, "apply: too many arguments");
                else
                    lerror(ArgError, "apply: too few arguments");
            }
            goto next_op;
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
            }
            else if (s < 0) {
                lerror(ArgError, "apply: too few arguments");
            }
            else {
                PUSH(NIL);
                Stack[SP-1] = Stack[SP-2];
                Stack[SP-2] = NIL;
            }
            nargs = i+1;
            goto next_op;
        case OP_LET:
            // last arg is closure environment to use
            nargs--;
            POPN(1);
            goto next_op;
        case OP_NOP: goto next_op;
        case OP_DUP: SP++; Stack[SP-1] = Stack[SP-2]; goto next_op;
        case OP_POP: POPN(1); goto next_op;
        case OP_TCALL:
            n = code[ip++];  // nargs
        do_tcall:
            if (isfunction(Stack[SP-n-1])) {
                for(s=-1; s < (fixnum_t)n; s++)
                    Stack[bp+s] = Stack[SP-n+s];
                SP = bp+n;
                nargs = n;
                goto apply_cl_top;
            }
            goto do_call;
        case OP_CALL:
            n = code[ip++];  // nargs
        do_call:
            func = Stack[SP-n-1];
            s = SP;
            if (tag(func) == TAG_FUNCTION) {
                if (func > (N_BUILTINS<<3)) {
                    v = apply_cl(n);
                }
                else {
                    op = uintval(func);
                    if (op > OP_ASET)
                        type_error("apply", "function", func);
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
                    case OP_VECTOR: goto apply_vector;
                    case OP_APPLY:  goto apply_apply;
                    case OP_ADD:    goto apply_add;
                    case OP_SUB:    goto apply_sub;
                    case OP_MUL:    goto apply_mul;
                    case OP_DIV:    goto apply_div;
                    default:
                        goto dispatch;
                    }
                }
            }
            else if (iscbuiltin(func)) {
                v = (((builtin_t*)ptr(func))[3])(&Stack[SP-n], n);
            }
            else {
                type_error("apply", "function", func);
            }
            SP = s-n;
            Stack[SP-1] = v;
            goto next_op;
        case OP_JMP: ip = (uint32_t)*(uint16_t*)&code[ip]; goto next_op;
        case OP_BRF:
            v = POP();
            if (v == FL_F) ip = (uint32_t)*(uint16_t*)&code[ip];
            else ip += 2;
            goto next_op;
        case OP_BRT:
            v = POP();
            if (v != FL_F) ip = (uint32_t)*(uint16_t*)&code[ip];
            else ip += 2;
            goto next_op;
        case OP_JMPL: ip = *(uint32_t*)&code[ip]; goto next_op;
        case OP_BRFL:
            v = POP();
            if (v == FL_F) ip = *(uint32_t*)&code[ip];
            else ip += 4;
            goto next_op;
        case OP_BRTL:
            v = POP();
            if (v != FL_F) ip = *(uint32_t*)&code[ip];
            else ip += 4;
            goto next_op;
        case OP_RET: v = POP(); return v;

        case OP_EQ:
            Stack[SP-2] = ((Stack[SP-2] == Stack[SP-1]) ? FL_T : FL_F);
            POPN(1); goto next_op;
        case OP_EQV:
            if (Stack[SP-2] == Stack[SP-1]) {
                v = FL_T;
            }
            else if (!leafp(Stack[SP-2]) || !leafp(Stack[SP-1])) {
                v = FL_F;
            }
            else {
                v = equal(Stack[SP-2], Stack[SP-1]);
            }
            Stack[SP-2] = v; POPN(1);
            goto next_op;
        case OP_EQUAL:
            if (Stack[SP-2] == Stack[SP-1]) {
                v = FL_T;
            }
            else {
                v = equal(Stack[SP-2], Stack[SP-1]);
            }
            Stack[SP-2] = v; POPN(1);
            goto next_op;
        case OP_PAIRP:
            Stack[SP-1] = (iscons(Stack[SP-1]) ? FL_T : FL_F); goto next_op;
        case OP_ATOMP:
            Stack[SP-1] = (iscons(Stack[SP-1]) ? FL_F : FL_T); goto next_op;
        case OP_NOT:
            Stack[SP-1] = ((Stack[SP-1]==FL_F) ? FL_T : FL_F); goto next_op;
        case OP_NULLP:
            Stack[SP-1] = ((Stack[SP-1]==NIL) ? FL_T : FL_F); goto next_op;
        case OP_BOOLEANP:
            v = Stack[SP-1];
            Stack[SP-1] = ((v == FL_T || v == FL_F) ? FL_T:FL_F); goto next_op;
        case OP_SYMBOLP:
            Stack[SP-1] = (issymbol(Stack[SP-1]) ? FL_T : FL_F); goto next_op;
        case OP_NUMBERP:
            v = Stack[SP-1];
            Stack[SP-1] = (isfixnum(v) || iscprim(v) ? FL_T:FL_F); goto next_op;
        case OP_FIXNUMP:
            Stack[SP-1] = (isfixnum(Stack[SP-1]) ? FL_T : FL_F); goto next_op;
        case OP_BOUNDP:
            sym = tosymbol(Stack[SP-1], "bound?");
            Stack[SP-1] = ((sym->binding == UNBOUND) ? FL_F : FL_T);
            goto next_op;
        case OP_BUILTINP:
            v = Stack[SP-1];
            Stack[SP-1] = (isbuiltin(v) || iscbuiltin(v)) ? FL_T : FL_F;
            goto next_op;
        case OP_FUNCTIONP:
            v = Stack[SP-1];
            Stack[SP-1] = ((tag(v)==TAG_FUNCTION &&v!=FL_F&&v!=FL_T&&v!=NIL) ||
                           iscbuiltin(v)) ? FL_T : FL_F;
            goto next_op;
        case OP_VECTORP:
            Stack[SP-1] = (isvector(Stack[SP-1]) ? FL_T : FL_F); goto next_op;

        case OP_CONS:
            if (curheap > lim)
                gc(0);
            c = (cons_t*)curheap;
            curheap += sizeof(cons_t);
            c->car = Stack[SP-2];
            c->cdr = Stack[SP-1];
            Stack[SP-2] = tagptr(c, TAG_CONS);
            POPN(1); goto next_op;
        case OP_CAR:
            v = Stack[SP-1];
            if (!iscons(v)) type_error("car", "cons", v);
            Stack[SP-1] = car_(v);
            goto next_op;
        case OP_CDR:
            v = Stack[SP-1];
            if (!iscons(v)) type_error("cdr", "cons", v);
            Stack[SP-1] = cdr_(v);
            goto next_op;
        case OP_SETCAR:
            car(Stack[SP-2]) = Stack[SP-1];
            POPN(1); goto next_op;
        case OP_SETCDR:
            cdr(Stack[SP-2]) = Stack[SP-1];
            POPN(1); goto next_op;
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
            goto next_op;

        case OP_TAPPLY:
        case OP_APPLY:
            n = code[ip++];
        apply_apply:
            v = POP();     // arglist
            if (n > MAX_ARGS) {
                v = apply_liststar(v);
            }
            n = SP-(n-2);  // n-2 == # leading arguments not in the list
            while (iscons(v)) {
                if (SP-n == MAX_ARGS) {
                    PUSH(v);
                    break;
                }
                PUSH(car_(v));
                v = cdr_(v);
            }
            n = SP-n;
            if (op==OP_TAPPLY) goto do_tcall;
            else goto do_call;

        case OP_ADD:
            n = code[ip++];
        apply_add:
            s = 0;
            i = SP-n;
            if (n > MAX_ARGS) goto add_ovf;
            for (; i < SP; i++) {
                if (isfixnum(Stack[i])) {
                    s += numval(Stack[i]);
                    if (!fits_fixnum(s)) {
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
            goto next_op;
        case OP_ADD2:
            if (bothfixnums(Stack[SP-1], Stack[SP-2])) {
                s = numval(Stack[SP-1]) + numval(Stack[SP-2]);
                if (fits_fixnum(s))
                    v = fixnum(s);
                else
                    v = mk_long(s);
            }
            else {
                v = fl_add_any(&Stack[SP-2], 2, 0);
            }
            POPN(1);
            Stack[SP-1] = v;
            goto next_op;
        case OP_SUB:
            n = code[ip++];
        apply_sub:
            if (n == 2) goto do_sub2;
            if (n == 1) goto do_neg;
            i = SP-n;
            // we need to pass the full arglist on to fl_add_any
            // so it can handle rest args properly
            PUSH(Stack[i]);
            Stack[i] = fixnum(0);
            Stack[i+1] = fl_neg(fl_add_any(&Stack[i], n, 0));
            Stack[i] = POP();
            v = fl_add_any(&Stack[i], 2, 0);
            POPN(n);
            PUSH(v);
            goto next_op;
        case OP_NEG:
        do_neg:
            if (isfixnum(Stack[SP-1]))
                Stack[SP-1] = fixnum(-numval(Stack[SP-1]));
            else
                Stack[SP-1] = fl_neg(Stack[SP-1]);
            goto next_op;
        case OP_SUB2:
        do_sub2:
            if (bothfixnums(Stack[SP-2], Stack[SP-1])) {
                s = numval(Stack[SP-2]) - numval(Stack[SP-1]);
                if (fits_fixnum(s))
                    v = fixnum(s);
                else
                    v = mk_long(s);
            }
            else {
                Stack[SP-1] = fl_neg(Stack[SP-1]);
                v = fl_add_any(&Stack[SP-2], 2, 0);
            }
            POPN(1);
            Stack[SP-1] = v;
            goto next_op;
        case OP_MUL:
            n = code[ip++];
        apply_mul:
            accum = 1;
            i = SP-n;
            if (n > MAX_ARGS) goto mul_ovf;
            for (; i < SP; i++) {
                if (isfixnum(Stack[i])) {
                    accum *= numval(Stack[i]);
                }
                else {
                mul_ovf:
                    v = fl_mul_any(&Stack[i], SP-i, accum);
                    break;
                }
            }
            if (i == SP) {
                if (fits_fixnum(accum))
                    v = fixnum(accum);
                else
                    v = return_from_int64(accum);
            }
            POPN(n);
            PUSH(v);
            goto next_op;
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
            goto next_op;
        case OP_IDIV:
            v = Stack[SP-2]; e = Stack[SP-1];
            if (bothfixnums(v, e)) {
                if (e==0) DivideByZeroError();
                v = fixnum(numval(v) / numval(e));
            }
            else
                v = fl_idiv2(v, e);
            POPN(1);
            Stack[SP-1] = v;
            goto next_op;
        case OP_NUMEQ:
            v = Stack[SP-2]; e = Stack[SP-1];
            if (bothfixnums(v, e))
                v = (v == e) ? FL_T : FL_F;
            else
                v = (!numeric_compare(v,e,1,0,"=")) ? FL_T : FL_F;
            POPN(1);
            Stack[SP-1] = v;
            goto next_op;
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
            goto next_op;
        case OP_COMPARE:
            Stack[SP-2] = compare(Stack[SP-2], Stack[SP-1]);
            POPN(1);
            goto next_op;

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
            goto next_op;

        case OP_AREF:
            v = Stack[SP-2];
            if (isvector(v)) {
                i = tofixnum(Stack[SP-1], "aref");
                if ((unsigned)i >= vector_size(v))
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
            goto next_op;
        case OP_ASET:
            e = Stack[SP-3];
            if (isvector(e)) {
                i = tofixnum(Stack[SP-2], "aset!");
                if ((unsigned)i >= vector_size(e))
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
            goto next_op;
        case OP_FOR:
            s  = tofixnum(Stack[SP-3], "for");
            hi = tofixnum(Stack[SP-2], "for");
            //f = Stack[SP-1];
            v = FL_F;
            SP += 2;
            i = SP;
            for(; s <= hi; s++) {
                Stack[SP-2] = Stack[SP-3];
                Stack[SP-1] = fixnum(s);
                v = apply_cl(1);
                SP = i;
            }
            POPN(4);
            Stack[SP-1] = v;
            goto next_op;

        case OP_LOADT: PUSH(FL_T); goto next_op;
        case OP_LOADF: PUSH(FL_F); goto next_op;
        case OP_LOADNIL: PUSH(NIL); goto next_op;
        case OP_LOAD0: PUSH(fixnum(0)); goto next_op;
        case OP_LOAD1: PUSH(fixnum(1)); goto next_op;
        case OP_LOADI8: s = (int8_t)code[ip++]; PUSH(fixnum(s)); goto next_op;
        case OP_LOADV:
            v = fn_vals(Stack[bp-1]);
            assert(code[ip] < vector_size(v));
            v = vector_elt(v, code[ip]); ip++;
            PUSH(v);
            goto next_op;
        case OP_LOADVL:
            v = fn_vals(Stack[bp-1]);
            v = vector_elt(v, *(uint32_t*)&code[ip]); ip+=4;
            PUSH(v);
            goto next_op;
        case OP_LOADGL:
            v = fn_vals(Stack[bp-1]);
            v = vector_elt(v, *(uint32_t*)&code[ip]); ip+=4;
            goto do_loadg;
        case OP_LOADG:
            v = fn_vals(Stack[bp-1]);
            assert(code[ip] < vector_size(v));
            v = vector_elt(v, code[ip]); ip++;
        do_loadg:
            assert(issymbol(v));
            sym = (symbol_t*)ptr(v);
            if (sym->binding == UNBOUND)
                raise(list2(UnboundError, v));
            PUSH(sym->binding);
            goto next_op;

        case OP_SETGL:
            v = fn_vals(Stack[bp-1]);
            v = vector_elt(v, *(uint32_t*)&code[ip]); ip+=4;
            goto do_setg;
        case OP_SETG:
            v = fn_vals(Stack[bp-1]);
            assert(code[ip] < vector_size(v));
            v = vector_elt(v, code[ip]); ip++;
        do_setg:
            assert(issymbol(v));
            sym = (symbol_t*)ptr(v);
            v = Stack[SP-1];
            if (sym->syntax != TAG_CONST)
                sym->binding = v;
            goto next_op;

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
            goto next_op;
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
            goto next_op;
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
            goto next_op;

        case OP_CLOSURE:
        case OP_COPYENV:
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
                pv = alloc_words(4);
                x = Stack[SP-2];  // closure to copy
                assert(isfunction(x));
                pv[0] = ((value_t*)ptr(x))[0];
                pv[1] = ((value_t*)ptr(x))[1];
                pv[2] = Stack[SP-1];  // env
                POPN(1);
                Stack[SP-1] = tagptr(pv, TAG_FUNCTION);
            }
            goto next_op;

        case OP_TRYCATCH:
            v = do_trycatch();
            POPN(1);
            Stack[SP-1] = v;
            goto next_op;
        }
    }
    assert(0);
    return UNBOUND;
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

static value_t fl_function(value_t *args, uint32_t nargs)
{
    if (nargs != 3)
        argcount("function", nargs, 2);
    if (!isstring(args[0]))
        type_error("function", "string", args[0]);
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
    function_t *fn = (function_t*)alloc_words(4);
    value_t fv = tagptr(fn, TAG_FUNCTION);
    fn->bcode = args[0];
    fn->vals = args[1];
    if (nargs == 3)
        fn->env = args[2];
    else
        fn->env = NIL;
    return fv;
}

static value_t fl_function_code(value_t *args, uint32_t nargs)
{
    argcount("function:code", nargs, 1);
    value_t v = args[0];
    if (!isclosure(v)) type_error("function:code", "function", v);
    return fn_bcode(v);
}
static value_t fl_function_vals(value_t *args, uint32_t nargs)
{
    argcount("function:vals", nargs, 1);
    value_t v = args[0];
    if (!isclosure(v)) type_error("function:vals", "function", v);
    return fn_vals(v);
}
static value_t fl_function_env(value_t *args, uint32_t nargs)
{
    argcount("function:env", nargs, 1);
    value_t v = args[0];
    if (!isclosure(v)) type_error("function:env", "function", v);
    return fn_env(v);
}

static builtinspec_t core_builtin_info[] = {
    { "function", fl_function },
    { "function:code", fl_function_code },
    { "function:vals", fl_function_vals },
    { "function:env", fl_function_env },
    { "gensym", fl_gensym },
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

    NIL = builtin(OP_THE_EMPTY_LIST);
    FL_T = builtin(OP_BOOL_CONST_T);
    FL_F = builtin(OP_BOOL_CONST_F);
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
    set(printreadablysym=symbol("*print-readably*"), FL_T);
    set(printwidthsym=symbol("*print-width*"), fixnum(SCR_WIDTH));
    lasterror = NIL;
    i = 0;
    for (i=OP_EQ; i <= OP_ASET; i++) {
        setc(symbol(builtin_names[i]), builtin(i));
    }
    setc(symbol("eq"), builtin(OP_EQ));
    setc(symbol("equal"), builtin(OP_EQUAL));
    setc(symbol("procedure?"), builtin(OP_FUNCTIONP));

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
        print(ios_stderr, lasterror);
        ios_putc('\n', ios_stderr);
        return 1;
    }

    return 0;
}
