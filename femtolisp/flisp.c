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
  lambdas can have only 1 body expression; use (progn ...) for multiple
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
  - hash tables

  by Jeff Bezanson (C) 2008
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

static char *builtin_names[] =
    { "quote", "cond", "if", "and", "or", "while", "lambda",
      "trycatch", "%apply", "setq", "progn",

      "eq", "atom", "not", "symbolp", "numberp", "boundp", "consp",
      "builtinp", "vectorp", "fixnump", "equal",
      "cons", "car", "cdr", "rplaca", "rplacd",
      "eval", "eval*", "apply", "prog1", "raise",
      "+", "-", "*", "/", "<", "~", "&", "!", "$",
      "vector", "aref", "aset", "length", "assoc", "compare",
      "for" };

#define N_STACK 98304
value_t Stack[N_STACK];
uint32_t SP = 0;

value_t NIL, T, LAMBDA, QUOTE, IF, TRYCATCH;
value_t BACKQUOTE, COMMA, COMMAAT, COMMADOT;
value_t IOError, ParseError, TypeError, ArgError, UnboundError, MemoryError;
value_t DivideError, BoundsError, Error, KeyError;
value_t conssym, symbolsym, fixnumsym, vectorsym, builtinsym;
value_t defunsym, defmacrosym, forsym, labelsym, printprettysym;
value_t printwidthsym;

static value_t eval_sexpr(value_t e, uint32_t penv, int tail);
static value_t *alloc_words(int n);
static value_t relocate(value_t v);

typedef struct _readstate_t {
    htable_t backrefs;
    htable_t gensyms;
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
static uint32_t heapsize = 256*1024;//bytes
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
static char lerrorbuf[512];

#define FL_TRY \
  exception_context_t _ctx; int l__tr, l__ca; \
  _ctx.sp=SP; _ctx.rdst=readstate; _ctx.prev=ctx; \
  ctx = &_ctx; \
  if (!setjmp(_ctx.buf)) \
      for (l__tr=1; l__tr; l__tr=0, (void)(ctx->prev && (ctx=ctx->prev)))

#define FL_CATCH \
  else \
      for (l__ca=1; l__ca; l__ca=0, lerrorbuf[0]='\0', lasterror=NIL)

void raise(value_t e)
{
    if (e != lasterror) {
        lasterror = e;
        lerrorbuf[0] = '\0';  // overwriting exception; clear error buf
    }
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

void lerror(value_t e, char *format, ...)
{
    va_list args;
    va_start(args, format);
    vsnprintf(lerrorbuf, sizeof(lerrorbuf), format, args);
    va_end(args);

    lasterror = e;
    raise(e);
}

void type_error(char *fname, char *expected, value_t got)
{
    raise(listn(4, TypeError, symbol(fname), symbol(expected), got));
}

void bounds_error(char *fname, value_t arr, value_t ind)
{
    lerror(listn(3, BoundsError, arr, ind), "%s: index out of bounds", fname);
}

// safe cast operators --------------------------------------------------------

#define SAFECAST_OP(type,ctype,cnvt)                                          \
ctype to##type(value_t v, char *fname)                                        \
{                                                                             \
    if (is##type(v))                                                          \
        return (ctype)cnvt(v);                                                \
    type_error(fname, #type, v);                                              \
    return (ctype)0;                                                          \
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

    sym = (symbol_t*)malloc_aligned(sizeof(symbol_t)-sizeof(void*) + len + 1,
                                    8);
    sym->left = sym->right = NULL;
    if (str[0] == ':') {
        value_t s = tagptr(sym, TAG_SYM);
        setc(s, s);
    }
    else {
        sym->binding = UNBOUND;
        sym->syntax = 0;
    }
    sym->type = NULL;
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

static char *snprintf_gensym_id(char *nbuf, size_t n, uint32_t g)
{
    size_t i=n-1;

    nbuf[i--] = '\0';
    do {
        nbuf[i--] = '0' + g%10;
        g/=10;
    } while (g && i);
    nbuf[i] = 'g';
    return &nbuf[i];
}

char *symbol_name(value_t v)
{
    if (ismanaged(v)) {
        gensym_t *gs = (gensym_t*)ptr(v);
        gsnameno = 1-gsnameno;
        return snprintf_gensym_id(gsname[gsnameno], sizeof(gsname[0]), gs->id);
    }
    return ((symbol_t*)ptr(v))->name;
}

// conses ---------------------------------------------------------------------

void gc(int mustgrow);

static value_t mk_cons(void)
{
    cons_t *c;

    if (curheap > lim)
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
    if ((value_t*)curheap > ((value_t*)lim)+2-n) {
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

value_t alloc_vector(size_t n, int init)
{
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

    if (iscons(v)) {
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
    uptrint_t t = tag(v);
    if ((t&(t-1)) == 0) return v;  // tags 0,1,2,4
    if (isforwarded(v))
        return forwardloc(v);
    if (isvector(v)) {
        // N.B.: 0-length vectors secretly have space for a first element
        size_t i, newsz, sz = vector_size(v);
        newsz = sz;
        if (vector_elt(v,-1) & 0x1)
            newsz += vector_grow_amt(sz);
        nc = alloc_vector(newsz, 0);
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
    else if (iscvalue(v)) {
        return cvalue_relocate(v);
    }
    else if (ismanaged(v)) {
        assert(issymbol(v));
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
        if (iscons(root->syntax))
            root->syntax = relocate(root->syntax);
        trace_globals(root->left);
        root = root->right;
    }
}

static value_t special_apply_form;

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
        rs = rs->prev;
    }
    lasterror = relocate(lasterror);
    special_apply_form = relocate(special_apply_form);

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
        temp = realloc_aligned(tospace, grew ? heapsize : heapsize*2, 16);
        if (temp == NULL)
            lerror(MemoryError, "out of memory");
        tospace = temp;
        if (!grew) {
            heapsize*=2;
        }
        else {
            temp = bitvector_resize(consflags, heapsize/sizeof(cons_t), 1);
            if (temp == NULL)
                lerror(MemoryError, "out of memory");
            consflags = (uint32_t*)temp;
        }
        grew = !grew;
    }
    if (curheap > lim)  // all data was live
        gc(0);
}

// utils ----------------------------------------------------------------------

value_t apply(value_t f, value_t l)
{
    PUSH(f);
    PUSH(l);
    value_t v = toplevel_eval(special_apply_form);
    POPN(2);
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
    return (isfixnum(v) ||
            (iscvalue(v) &&
             valid_numtype(cv_numtype((cvalue_t*)ptr(v)))));
}

// read -----------------------------------------------------------------------

#include "read.c"

// eval -----------------------------------------------------------------------

// return a cons element of v whose car is item
static value_t assoc(value_t item, value_t v)
{
    value_t bind;

    while (iscons(v)) {
        bind = car_(v);
        if (iscons(bind) && car_(bind) == item)
            return bind;
        v = cdr_(v);
    }
    return NIL;
}

#define eval(e)         (selfevaluating(e) ? (e) : eval_sexpr((e),penv,0))
#define topeval(e, env) (selfevaluating(e) ? (e) : eval_sexpr((e),env,1))
#define tail_eval(xpr) do { SP = saveSP;  \
    if (selfevaluating(xpr)) { return (xpr); }  \
    else { e=(xpr); goto eval_top; } } while (0)

static value_t do_trycatch(value_t expr, uint32_t penv)
{
    value_t v;

    FL_TRY {
        v = eval(expr);
    }
    FL_CATCH {
        v = cdr_(Stack[SP-1]);
        if (!iscons(v)) {
            v = NIL;   // 1-argument form
        }
        else {
            Stack[SP-1] = car_(v);
            value_t quoted = list2(QUOTE, lasterror);
            expr = list2(Stack[SP-1], quoted);
            v = eval(expr);
        }
    }
    return v;
}

/* stack setup on entry:
  n     n+1   ...
 +-----+-----+-----+-----+-----+-----+-----+-----+
 | LL  | VAL | VAL | CLO |     |     |     |     |
 +-----+-----+-----+-----+-----+-----+-----+-----+
  ^                                                   ^
  |                                                   |
  penv                                                SP (who knows where)

 where LL is the lambda list, CLO is a closed-up environment vector
 (which can be empty, i.e. NIL). An environment vector is just a copy
 of the stack from LL through CLO.
 There might be zero values, in which case LL is NIL.

 Stack[penv-1] is the size of the whole environment (as a fixnum)

 if tail==1, you are allowed (indeed encouraged) to overwrite this
 environment, otherwise you have to put any new environment on the top
 of the stack.
*/
static value_t eval_sexpr(value_t e, uint32_t penv, int tail)
{
    value_t f, v, *pv, *argsyms, *body;
    cons_t *c;
    symbol_t *sym;
    uint32_t saveSP, envsz, lenv;
    int i, nargs=0, noeval=0;
    fixnum_t s, lo, hi;
    cvalue_t *cv;
    int64_t accum;

 eval_top:
    if (issymbol(e)) {
        sym = (symbol_t*)ptr(e);
        if (sym->syntax == TAG_CONST) return sym->binding;
        pv = &Stack[penv];
        while (1) {
            v = *pv++;
            while (iscons(v)) {
                if (car_(v)==e) return *pv;
                v = cdr_(v); pv++;
            }
            if (v == e) return *pv;  // dotted list
            if (v != NIL) pv++;
            if (*pv == NIL) break;
            pv = &vector_elt(*pv, 0);
        }
        if ((v = sym->binding) == UNBOUND)
            raise(list2(UnboundError, e));
        return v;
    }
    if (SP >= (N_STACK-64))
        lerror(MemoryError, "eval: stack overflow");
    saveSP = SP;
    v = car_(e);
    PUSH(cdr_(e));
    if (selfevaluating(v)) f=v;
    else if (issymbol(v) && (f=((symbol_t*)ptr(v))->syntax)) {
        // handle special syntax forms
        if (isspecial(f))
            goto apply_special;
        else if (f == TAG_CONST)
            f = ((symbol_t*)ptr(v))->binding;
        else
            noeval = 2;
    }
    else f = eval(v);
    v = Stack[saveSP];
    if (isbuiltinish(f)) {
        // handle builtin function
        // evaluate argument list, placing arguments on stack
        while (iscons(v)) {
            v = eval(car_(v));
            PUSH(v);
            v = Stack[saveSP] = cdr_(Stack[saveSP]);
        }
    apply_builtin:
        nargs = SP - saveSP - 1;
    apply_special:
        switch (uintval(f)) {
        // special forms
        case F_QUOTE:
            if (!iscons(Stack[saveSP]))
                lerror(ArgError, "quote: expected argument");
            v = car_(Stack[saveSP]);
            break;
        case F_SETQ:
            e = car(Stack[saveSP]);
            v = eval(car(cdr_(Stack[saveSP])));
            pv = &Stack[penv];
            while (1) {
                f = *pv++;
                while (iscons(f)) {
                    if (car_(f)==e) {
                        *pv = v;
                        SP = saveSP;
                        return v;
                    }
                    f = cdr_(f); pv++;
                }
                if (f == e) {
                    *pv = v;
                    SP = saveSP;
                    return v;
                }
                if (f != NIL) pv++;
                if (*pv == NIL) break;
                pv = &vector_elt(*pv, 0);
            }
            sym = tosymbol(e, "setq");
            if (sym->syntax != TAG_CONST)
                sym->binding = v;
            break;
        case F_LAMBDA:
            // build a closure (lambda args body . env)
            if (Stack[penv] != NIL) {
                // save temporary environment to the heap
                lenv = penv;
                envsz = numval(Stack[penv-1]);
                pv = alloc_words(envsz + 1);
                PUSH(tagptr(pv, TAG_VECTOR));
                pv[0] = fixnum(envsz);
                pv++;
                while (envsz--)
                    *pv++ = Stack[penv++];
                // environment representation changed; install
                // the new representation so everybody can see it
                Stack[lenv]   = NIL;
                Stack[lenv+1] = Stack[SP-1];
            }
            else {
                PUSH(Stack[penv+1]); // env has already been captured; share
            }
            c = (cons_t*)ptr(v=cons_reserve(3));
            c->car = LAMBDA;
            c->cdr = tagptr(c+1, TAG_CONS); c++;
            c->car = car(Stack[saveSP]); //argsyms
            c->cdr = tagptr(c+1, TAG_CONS); c++;
            c->car = car(cdr_(Stack[saveSP])); //body
            c->cdr = Stack[SP-1]; //env
            break;
        case F_IF:
            v = car(Stack[saveSP]);
            if (eval(v) != NIL)
                v = car(cdr_(Stack[saveSP]));
            else
                v = car(cdr(cdr_(Stack[saveSP])));
            tail_eval(v);
            break;
        case F_COND:
            pv = &Stack[saveSP]; v = NIL;
            while (iscons(*pv)) {
                c = tocons(car_(*pv), "cond");
                v = eval(c->car);
                if (v != NIL) {
                    *pv = cdr_(car_(*pv));
                    // evaluate body forms
                    if (iscons(*pv)) {
                        while (iscons(cdr_(*pv))) {
                            v = eval(car_(*pv));
                            *pv = cdr_(*pv);
                        }
                        tail_eval(car_(*pv));
                    }
                    break;
                }
                *pv = cdr_(*pv);
            }
            break;
        case F_AND:
            pv = &Stack[saveSP]; v = T;
            if (iscons(*pv)) {
                while (iscons(cdr_(*pv))) {
                    if ((v=eval(car_(*pv))) == NIL) {
                        SP = saveSP; return NIL;
                    }
                    *pv = cdr_(*pv);
                }
                tail_eval(car_(*pv));
            }
            break;
        case F_OR:
            pv = &Stack[saveSP]; v = NIL;
            if (iscons(*pv)) {
                while (iscons(cdr_(*pv))) {
                    if ((v=eval(car_(*pv))) != NIL) {
                        SP = saveSP; return v;
                    }
                    *pv = cdr_(*pv);
                }
                tail_eval(car_(*pv));
            }
            break;
        case F_WHILE:
            PUSH(cdr(Stack[saveSP]));
            body = &Stack[SP-1];
            PUSH(*body);
            Stack[saveSP] = car_(Stack[saveSP]);
            value_t *cond = &Stack[saveSP];
            PUSH(NIL);
            pv = &Stack[SP-1];
            while (eval(*cond) != NIL) {
                *body = Stack[SP-2];
                while (iscons(*body)) {
                    *pv = eval(car_(*body));
                    *body = cdr_(*body);
                }
            }
            v = *pv;
            break;
        case F_PROGN:
            // return last arg
            pv = &Stack[saveSP];
            if (iscons(*pv)) {
                while (iscons(cdr_(*pv))) {
                    (void)eval(car_(*pv));
                    *pv = cdr_(*pv);
                }
                tail_eval(car_(*pv));
            }
            v = NIL;
            break;
        case F_TRYCATCH:
            v = do_trycatch(car(Stack[saveSP]), penv);
            break;

        // ordinary functions
        case F_BOUNDP:
            argcount("boundp", nargs, 1);
            sym = tosymbol(Stack[SP-1], "boundp");
            v = (sym->binding == UNBOUND) ? NIL : T;
            break;
        case F_EQ:
            argcount("eq", nargs, 2);
            v = ((Stack[SP-2] == Stack[SP-1]) ? T : NIL);
            break;
        case F_CONS:
            argcount("cons", nargs, 2);
            if (curheap > lim)
                gc(0);
            c = (cons_t*)curheap;
            curheap += sizeof(cons_t);
            c->car = Stack[SP-2];
            c->cdr = Stack[SP-1];
            v = tagptr(c, TAG_CONS);
            break;
        case F_CAR:
            argcount("car", nargs, 1);
            v = car(Stack[SP-1]);
            break;
        case F_CDR:
            argcount("cdr", nargs, 1);
            v = cdr(Stack[SP-1]);
            break;
        case F_RPLACA:
            argcount("rplaca", nargs, 2);
            car(v=Stack[SP-2]) = Stack[SP-1];
            break;
        case F_RPLACD:
            argcount("rplacd", nargs, 2);
            cdr(v=Stack[SP-2]) = Stack[SP-1];
            break;
        case F_VECTOR:
            v = alloc_vector(nargs, 0);
            memcpy(&vector_elt(v,0), &Stack[saveSP+1], nargs*sizeof(value_t));
            break;
        case F_LENGTH:
            argcount("length", nargs, 1);
            if (isvector(Stack[SP-1])) {
                v = fixnum(vector_size(Stack[SP-1]));
                break;
            }
            else if (iscvalue(Stack[SP-1])) {
                cv = (cvalue_t*)ptr(Stack[SP-1]);
                v = cv_type(cv);
                if (iscons(v) && car_(v) == arraysym) {
                    v = size_wrap(cvalue_arraylen(Stack[SP-1]));
                    break;
                }
                else if (v == bytesym) {
                    v = fixnum(1);
                    break;
                }
                else if (v == wcharsym) {
                    v = fixnum(u8_charlen(*(uint32_t*)cv_data(cv)));
                    break;
                }
            }
            else if (Stack[SP-1] == NIL) {
                v = fixnum(0); break;
            }
            else if (iscons(Stack[SP-1])) {
                v = fixnum(llength(Stack[SP-1])); break;
            }
            type_error("length", "sequence", Stack[SP-1]);
            break;
        case F_AREF:
            argcount("aref", nargs, 2);
            v = Stack[SP-2];
            i = tofixnum(Stack[SP-1], "aref");
            if (isvector(v)) {
                if ((unsigned)i >= vector_size(v))
                    bounds_error("aref", v, Stack[SP-1]);
                v = vector_elt(v, i);
            }
            else {
                // TODO other sequence types?
                type_error("aref", "sequence", v);
            }
            break;
        case F_ASET:
            argcount("aset", nargs, 3);
            e = Stack[SP-3];
            i = tofixnum(Stack[SP-2], "aset");
            if (isvector(e)) {
                if ((unsigned)i >= vector_size(e))
                    bounds_error("aref", v, Stack[SP-1]);
                vector_elt(e, i) = (v=Stack[SP-1]);
            }
            else {
                type_error("aset", "sequence", e);
            }
            break;
        case F_ATOM:
            argcount("atom", nargs, 1);
            v = ((!iscons(Stack[SP-1])) ? T : NIL);
            break;
        case F_CONSP:
            argcount("consp", nargs, 1);
            v = (iscons(Stack[SP-1]) ? T : NIL);
            break;
        case F_SYMBOLP:
            argcount("symbolp", nargs, 1);
            v = ((issymbol(Stack[SP-1])) ? T : NIL);
            break;
        case F_NUMBERP:
            argcount("numberp", nargs, 1);
            v = ((isfixnum(Stack[SP-1]) ||
                  (iscvalue(Stack[SP-1]) &&
                   valid_numtype(cv_numtype((cvalue_t*)ptr(Stack[SP-1]))) ))
                 ? T : NIL);
            break;
        case F_FIXNUMP:
            argcount("fixnump", nargs, 1);
            v = (isfixnum(Stack[SP-1]) ? T : NIL);
            break;
        case F_BUILTINP:
            argcount("builtinp", nargs, 1);
            v = (isbuiltinish(Stack[SP-1]) ? T : NIL);
            break;
        case F_VECTORP:
            argcount("vectorp", nargs, 1);
            v = ((isvector(Stack[SP-1])) ? T : NIL);
            break;
        case F_NOT:
            argcount("not", nargs, 1);
            v = ((Stack[SP-1] == NIL) ? T : NIL);
            break;
        case F_ADD:
            s = 0;
            for (i=saveSP+1; i < (int)SP; i++) {
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
                    SP = saveSP;
                    return v;
                }
            }
            v = fixnum(s);
            break;
        case F_SUB:
            if (nargs < 1) lerror(ArgError, "-: too few arguments");
            i = saveSP+1;
            if (nargs == 1) {
                if (isfixnum(Stack[i]))
                    v = fixnum(-numval(Stack[i]));
                else
                    v = fl_neg(Stack[i]);
                break;
            }
            if (nargs == 2) {
                if (bothfixnums(Stack[i], Stack[i+1])) {
                    s = numval(Stack[i]) - numval(Stack[i+1]);
                    if (fits_fixnum(s)) {
                        v = fixnum(s);
                        break;
                    }
                    Stack[i+1] = fixnum(-numval(Stack[i+1]));
                }
                else {
                    Stack[i+1] = fl_neg(Stack[i+1]);
                }
            }
            else {
                Stack[i+1] = fl_neg(fl_add_any(&Stack[i+1], nargs-1, 0));
            }
            v = fl_add_any(&Stack[i], 2, 0);
            break;
        case F_MUL:
            accum = 1;
            for (i=saveSP+1; i < (int)SP; i++) {
                if (isfixnum(Stack[i])) {
                    accum *= numval(Stack[i]);
                }
                else {
                    v = fl_mul_any(&Stack[i], SP-i, accum);
                    SP = saveSP;
                    return v;
                }
            }
            if (fits_fixnum(accum))
                v = fixnum(accum);
            else
                v = return_from_int64(accum);
            break;
        case F_DIV:
            if (nargs < 1) lerror(ArgError, "/: too few arguments");
            i = saveSP+1;
            if (nargs == 1) {
                v = fl_div2(fixnum(1), Stack[i]);
            }
            else {
                if (nargs > 2)
                    Stack[i+1] = fl_mul_any(&Stack[i+1], nargs-1, 1);
                v = fl_div2(Stack[i], Stack[i+1]);
            }
            break;
        case F_BNOT:
            argcount("~", nargs, 1);
            if (isfixnum(Stack[SP-1]))
                v = fixnum(~numval(Stack[SP-1]));
            else
                v = fl_bitwise_not(Stack[SP-1]);
            break;
        case F_BAND:
            argcount("&", nargs, 2);
            if (bothfixnums(Stack[SP-1], Stack[SP-2]))
                v = Stack[SP-1] & Stack[SP-2];
            else
                v = fl_bitwise_op(Stack[SP-2], Stack[SP-1], 0, "&");
            break;
        case F_BOR:
            argcount("!", nargs, 2);
            if (bothfixnums(Stack[SP-1], Stack[SP-2]))
                v = Stack[SP-1] | Stack[SP-2];
            else
                v = fl_bitwise_op(Stack[SP-2], Stack[SP-1], 1, "!");
            break;
        case F_BXOR:
            argcount("$", nargs, 2);
            if (bothfixnums(Stack[SP-1], Stack[SP-2]))
                v = fixnum(numval(Stack[SP-1]) ^ numval(Stack[SP-2]));
            else
                v = fl_bitwise_op(Stack[SP-2], Stack[SP-1], 2, "$");
            break;
        case F_COMPARE:
            argcount("compare", nargs, 2);
            v = compare(Stack[SP-2], Stack[SP-1]);
            break;
        case F_LT:
            argcount("<", nargs, 2);
            if (bothfixnums(Stack[SP-2], Stack[SP-1])) {
                v = (numval(Stack[SP-2]) < numval(Stack[SP-1])) ? T : NIL;
            }
            else {
                v = (numval(compare(Stack[SP-2], Stack[SP-1])) < 0) ? T : NIL;
            }
            break;
        case F_EQUAL:
            argcount("equal", nargs, 2);
            if (eq_comparable(Stack[SP-2],Stack[SP-1])) {
                v = (Stack[SP-2] == Stack[SP-1]) ? T : NIL;
            }
            else {
                v = (numval(compare(Stack[SP-2], Stack[SP-1]))==0) ? T : NIL;
            }
            break;
        case F_EVAL:
            argcount("eval", nargs, 1);
            v = Stack[SP-1];
            if (selfevaluating(v)) { SP=saveSP; return v; }
            if (tail) {
                Stack[penv-1] = fixnum(2);
                Stack[penv] = NIL;
                Stack[penv+1] = NIL;
                SP = penv + 2;
                e=v;
                goto eval_top;
            }
            else {
                PUSH(fixnum(2));
                PUSH(NIL);
                PUSH(NIL);
                v = eval_sexpr(v, SP-2, 1);
            }
            break;
        case F_EVALSTAR:
            argcount("eval*", nargs, 1);
            e = Stack[SP-1];
            if (selfevaluating(e)) { SP=saveSP; return e; }
            SP = penv+2;
            goto eval_top;
        case F_RAISE:
            argcount("raise", nargs, 1);
            raise(Stack[SP-1]);
            break;
        case F_PROG1:
            // return first arg
            if (nargs < 1) lerror(ArgError, "prog1: too few arguments");
            v = Stack[saveSP+1];
            break;
        case F_ASSOC:
            argcount("assoc", nargs, 2);
            v = assoc(Stack[SP-2], Stack[SP-1]);
            break;
        case F_FOR:
            argcount("for", nargs, 3);
            lo = tofixnum(Stack[SP-3], "for");
            hi = tofixnum(Stack[SP-2], "for");
            f = Stack[SP-1];
            v = car(cdr(f));
            if (!iscons(v) || !iscons(cdr_(cdr_(f))) || cdr_(v) != NIL)
                lerror(ArgError, "for: expected 1 argument lambda");
            f = cdr_(f);
            PUSH(f);  // save function cdr
            SP += 4;  // make space
            Stack[SP-4] = fixnum(3);       // env size
            Stack[SP-1] = cdr_(cdr_(f));   // cloenv
            v = NIL;
            for(s=lo; s <= hi; s++) {
                f = Stack[SP-5];
                Stack[SP-3] = car_(f);     // lambda list
                Stack[SP-2] = fixnum(s);   // argument value
                v = car_(cdr_(f));
                if (!selfevaluating(v)) v = eval_sexpr(v, SP-3, 0);
            }
            break;
        case F_SPECIAL_APPLY:
            v = Stack[saveSP-4];
            f = Stack[saveSP-5];
            PUSH(f);
            PUSH(v);
            nargs = 2;
            // falls through!!
        case F_APPLY:
            argcount("apply", nargs, 2);
            v = Stack[saveSP] = Stack[SP-1];  // second arg is new arglist
            f = Stack[SP-2];            // first arg is new function
            POPN(2);                    // pop apply's args
            if (isbuiltinish(f)) {
                assert(!isspecial(f));
                // unpack arglist onto the stack
                while (iscons(v)) {
                    PUSH(car_(v));
                    v = cdr_(v);
                }
                goto apply_builtin;
            }
            noeval = 1;
            goto apply_lambda;
        default:
            // function pointer tagged as a builtin
            v = ((builtin_t)ptr(f))(&Stack[saveSP+1], nargs);
        }
        SP = saveSP;
        return v;
    }
 apply_lambda:
    if (iscons(f)) {
        // apply lambda expression
        f = cdr_(f);
        PUSH(f);
        PUSH(car(f)); // arglist
        argsyms = &Stack[SP-1];
        // build a calling environment for the lambda
        // the environment is the argument binds on top of the captured
        // environment
        if (noeval) {
            while (iscons(v)) {
                // bind args
                if (!iscons(*argsyms)) {
                    if (*argsyms == NIL)
                        lerror(ArgError, "apply: too many arguments");
                    break;
                }
                PUSH(car_(v));
                *argsyms = cdr_(*argsyms);
                v = cdr_(v);
            }
            if (*argsyms != NIL && issymbol(*argsyms))
                PUSH(v);
        }
        else {
            while (iscons(v)) {
                // bind args
                if (!iscons(*argsyms)) {
                    if (*argsyms == NIL)
                        lerror(ArgError, "apply: too many arguments");
                    break;
                }
                v = eval(car_(v));
                PUSH(v);
                *argsyms = cdr_(*argsyms);
                v = Stack[saveSP] = cdr_(Stack[saveSP]);
            }
            if (*argsyms != NIL && issymbol(*argsyms)) {
                PUSH(Stack[saveSP]);
                // this version uses collective allocation. about 7-10%
                // faster for lists with > 2 elements, but uses more
                // stack space
                i = SP;
                while (iscons(Stack[saveSP])) {
                    v = car_(Stack[saveSP]);
                    v = eval(v);
                    PUSH(v);
                    Stack[saveSP] = cdr_(Stack[saveSP]);
                }
                nargs = SP-i;
                if (nargs) {
                    Stack[i-1] = cons_reserve(nargs);
                    c = (cons_t*)ptr(Stack[i-1]);
                    for(; i < (int)SP; i++) {
                        c->car = Stack[i];
                        c->cdr = tagptr(c+1, TAG_CONS);
                        c++;
                    }
                    (c-1)->cdr = Stack[saveSP];
                    POPN(nargs);
                }
            }
        }
        if (iscons(*argsyms)) {
            lerror(ArgError, "apply: too few arguments");
        }
        f = cdr_(Stack[saveSP+1]);
        e = car(f);
        if (selfevaluating(e)) { SP=saveSP; return(e); }
        PUSH(cdr_(f));                     // add closed environment
        *argsyms = car_(Stack[saveSP+1]);  // put lambda list

        if (noeval == 2) {
            // macro: evaluate body in lambda environment
            Stack[saveSP+1] = fixnum(SP-saveSP-2);
            e = eval_sexpr(e, saveSP+2, 1);
            SP = saveSP;
            if (selfevaluating(e)) return(e);
            noeval = 0;
            // macro: evaluate expansion in calling environment
            goto eval_top;
        }
        else {
            envsz = SP - saveSP - 2;
            if (tail) {
                noeval = 0;
                // ok to overwrite environment
                for(i=0; i < (int)envsz; i++)
                    Stack[penv+i] = Stack[saveSP+2+i];
                SP = penv+envsz;
                Stack[penv-1] = fixnum(envsz);
                goto eval_top;
            }
            else {
                Stack[saveSP+1] = fixnum(envsz);
                v = eval_sexpr(e, saveSP+2, 1);
                SP = saveSP;
                return v;
            }
        }
        // not reached
    }
    type_error("apply", "function", f);
    return NIL;
}

// initialization -------------------------------------------------------------

extern void builtins_init();
extern void comparehash_init();

static char *EXEDIR;

void assign_global_builtins(builtinspec_t *b)
{
    while (b->name != NULL) {
        set(symbol(b->name), cbuiltin(b->name, b->fptr));
        b++;
    }
}

void lisp_init(void)
{
    int i;

    llt_init();

    fromspace = malloc_aligned(heapsize, 16);
    tospace   = malloc_aligned(heapsize, 16);
    curheap = fromspace;
    lim = curheap+heapsize-sizeof(cons_t);
    consflags = bitvector_new(heapsize/sizeof(cons_t), 1);
    htable_new(&printconses, 32);
    comparehash_init();

    NIL = symbol("nil"); setc(NIL, NIL);
    T   = symbol("T");   setc(T,   T);
    LAMBDA = symbol("lambda");
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
    Error = symbol("error");
    conssym = symbol("cons");
    symbolsym = symbol("symbol");
    fixnumsym = symbol("fixnum");
    vectorsym = symbol("vector");
    builtinsym = symbol("builtin");
    defunsym = symbol("defun");
    defmacrosym = symbol("defmacro");
    forsym = symbol("for");
    labelsym = symbol("label");
    set(printprettysym=symbol("*print-pretty*"), T);
    set(printwidthsym=symbol("*print-width*"), fixnum(SCR_WIDTH));
    lasterror = NIL;
    lerrorbuf[0] = '\0';
    special_apply_form = fl_cons(builtin(F_SPECIAL_APPLY), NIL);
    i = 0;
    while (isspecial(builtin(i))) {
        if (i != F_SPECIAL_APPLY)
            ((symbol_t*)ptr(symbol(builtin_names[i])))->syntax = builtin(i);
        i++;
    }
    for (; i < N_BUILTINS; i++) {
        setc(symbol(builtin_names[i]), builtin(i));
    }

#ifdef LINUX
    set(symbol("os.name"), symbol("linux"));
#elif defined(WIN32) || defined(WIN64)
    set(symbol("os.name"), symbol("win32"));
#elif defined(MACOSX)
    set(symbol("os.name"), symbol("macos"));
#else
    set(symbol("os.name"), symbol("unknown"));
#endif

    cvalues_init();
    set(symbol("gensym"), cbuiltin("gensym", gensym));
    set(symbol("hash"), cbuiltin("hash", fl_hash));

    char buf[1024];
    char *exename = get_exename(buf, sizeof(buf));
    if (exename != NULL) {
        path_to_dirname(exename);
        EXEDIR = strdup(exename);
        setc(symbol("*install-dir*"), cvalue_static_cstring(EXEDIR));
    }

    builtins_init();
}

// repl -----------------------------------------------------------------------

value_t toplevel_eval(value_t expr)
{
    value_t v;
    uint32_t saveSP = SP;
    PUSH(fixnum(2));
    PUSH(NIL);
    PUSH(NIL);
    v = topeval(expr, SP-2);
    SP = saveSP;
    return v;
}

static void print_toplevel_exception()
{
    if (iscons(lasterror) && car_(lasterror) == TypeError &&
        llength(lasterror) == 4) {
        ios_printf(ios_stderr, "type-error: ");
        print(ios_stderr, car_(cdr_(lasterror)), 1);
        ios_printf(ios_stderr, ": expected ");
        print(ios_stderr, car_(cdr_(cdr_(lasterror))), 1);
        ios_printf(ios_stderr, ", got ");
        print(ios_stderr, car_(cdr_(cdr_(cdr_(lasterror)))), 0);
    }
    else if (iscons(lasterror) && car_(lasterror) == UnboundError &&
             iscons(cdr_(lasterror))) {
        ios_printf(ios_stderr, "unbound-error: eval: variable %s has no value",
                   (symbol_name(car_(cdr_(lasterror)))));
    }
    else if (iscons(lasterror) && car_(lasterror) == Error) {
        value_t v = cdr_(lasterror);
        ios_printf(ios_stderr, "error: ");
        while (iscons(v)) {
            print(ios_stderr, car_(v), 1);
            v = cdr_(v);
        }
    }
    else {
        if (lasterror != NIL) {
            if (!lerrorbuf[0])
                ios_printf(ios_stderr, "*** Unhandled exception: ");
            print(ios_stderr, lasterror, 0);
            if (lerrorbuf[0])
                ios_printf(ios_stderr, ": ");
        }
    }

    if (lerrorbuf[0])
        ios_printf(ios_stderr, "%s", lerrorbuf);
}

value_t load_file(char *fname)
{
    value_t volatile e, v=NIL;
    ios_t fi;
    ios_t * volatile f;
    fname = strdup(fname);
    f = &fi; f = ios_file(f, fname, 0, 0);
    if (f == NULL) lerror(IOError, "file \"%s\" not found", fname);
    FL_TRY {
        while (1) {
            e = read_sexpr(f);
            //print(ios_stdout,e,0); ios_putc('\n', ios_stdout);
            if (ios_eof(f)) break;
            v = toplevel_eval(e);
        }
    }
    FL_CATCH {
        ios_close(f);
        size_t msglen = strlen(lerrorbuf);
        snprintf(&lerrorbuf[msglen], sizeof(lerrorbuf)-msglen,
                 "\nin file \"%s\"", fname);
        lerrorbuf[sizeof(lerrorbuf)-1] = '\0';
        free(fname);
        raise(lasterror);
    }
    free(fname);
    ios_close(f);
    return v;
}

static value_t argv_list(int argc, char *argv[])
{
    int i;
    PUSH(NIL);
    if (argc > 1) { argc--; argv++; }
    for(i=argc-1; i >= 0; i--)
        Stack[SP-1] = fl_cons(cvalue_static_cstring(argv[i]), Stack[SP-1]);
    return POP();
}

int locale_is_utf8;

int main(int argc, char *argv[])
{
    value_t v;

    locale_is_utf8 = u8_is_locale_utf8(setlocale(LC_ALL, ""));

    lisp_init();
    set(symbol("argv"), argv_list(argc, argv));
    FL_TRY {
        // install toplevel exception handler
    }
    FL_CATCH {
        print_toplevel_exception();
        lerrorbuf[0] = '\0';
        lasterror = NIL;
        ios_puts("\n\n", ios_stderr);
        if (argc > 1) return 1;
        else goto repl;
    }
    load_file("system.lsp");
    if (argc > 1) { load_file(argv[1]); return 0; }
    printf(";  _                   \n");
    printf("; |_ _ _ |_ _ |  . _ _\n");
    printf("; | (-||||_(_)|__|_)|_)\n");
    printf(";-------------------|----------------------------------------------------------\n\n");
 repl:
    while (1) {
        ios_puts("> ", ios_stdout); ios_flush(ios_stdout);
        FL_TRY {
            v = read_sexpr(ios_stdin);
        }
        FL_CATCH {
            ios_purge(ios_stdin);
            raise(lasterror);
        }
        if (ios_eof(ios_stdin)) break;
        print(ios_stdout, v=toplevel_eval(v), 0);
        set(symbol("that"), v);
        ios_puts("\n\n", ios_stdout);
    }
    ios_putc('\n', ios_stdout);
    return 0;
}
