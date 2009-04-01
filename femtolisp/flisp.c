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

static char *builtin_names[] =
    { // special forms
      "quote", "cond", "if", "and", "or", "while", "lambda",
      "trycatch", "%apply", "set!", "prog1", "begin",

      // predicates
      "eq?", "eqv?", "equal?", "atom?", "not", "null?", "boolean?", "symbol?",
      "number?", "bound?", "pair?", "builtin?", "vector?", "fixnum?",

      // lists
      "cons", "list", "car", "cdr", "set-car!", "set-cdr!",

      // execution
      "eval", "eval*", "apply",

      // arithmetic
      "+", "-", "*", "/", "<", "lognot", "compare",

      // sequences
      "vector", "aref", "aset!", "length", "for",
      "", "", "" };

#define N_STACK 262144
value_t StaticStack[N_STACK];
value_t *Stack = StaticStack;
uint32_t SP = 0;

typedef struct _stackseg_t {
    value_t *Stack;
    uint32_t SP;
    struct _stackseg_t *prev;
} stackseg_t;

stackseg_t stackseg0 = { StaticStack, 0, NULL };
stackseg_t *current_stack_seg = &stackseg0;

value_t NIL, FL_T, FL_F, LAMBDA, QUOTE, IF, TRYCATCH;
value_t BACKQUOTE, COMMA, COMMAAT, COMMADOT;
value_t IOError, ParseError, TypeError, ArgError, UnboundError, MemoryError;
value_t DivideError, BoundsError, Error, KeyError, EnumerationError;
value_t conssym, symbolsym, fixnumsym, vectorsym, builtinsym;
value_t definesym, defmacrosym, forsym, labelsym, printprettysym, setqsym;
value_t printwidthsym, tsym, Tsym, fsym, Fsym, booleansym, nullsym, elsesym;

static value_t eval_sexpr(value_t e, value_t *penv, int tail);
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
        if (iscons(root->syntax))
            root->syntax = relocate(root->syntax);
        trace_globals(root->left);
        root = root->right;
    }
}

static value_t special_apply_form;
static value_t apply1_args;
static value_t memory_exception_value;

void gc(int mustgrow)
{
    static int grew = 0;
    void *temp;
    uint32_t i;
    readstate_t *rs;
    stackseg_t *ss;

    curheap = tospace;
    lim = curheap+heapsize-sizeof(cons_t);

    ss = current_stack_seg;
    ss->SP = SP;
    while (ss) {
        for (i=0; i < ss->SP; i++)
            ss->Stack[i] = relocate(ss->Stack[i]);
        ss = ss->prev;
    }
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
    special_apply_form = relocate(special_apply_form);
    apply1_args = relocate(apply1_args);
    memory_exception_value = relocate(memory_exception_value);

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

value_t apply(value_t f, value_t l)
{
    PUSH(f);
    PUSH(l);
    value_t v = toplevel_eval(special_apply_form);
    POPN(2);
    return v;
}

value_t apply1(value_t f, value_t a0)
{
    car_(apply1_args) = a0;
    return apply(f, apply1_args);
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
  take the final cdr as an argument so the list builtin can give
  the same result as (lambda x x).

  however, there is still one interesting difference.
  (eq a (apply list a)) is always false for nonempty a, while
  (eq a (apply (lambda x x) a)) is always true. the justification for this
  is that a vararg lambda often needs to recur by applying itself to the
  tail of its argument list, so copying the list would be unacceptable.
*/
static void list(value_t *pv, uint32_t nargs, value_t *plastcdr)
{
    cons_t *c;
    uint32_t i;
    *pv = cons_reserve(nargs);
    c = (cons_t*)ptr(*pv);
    for(i=SP-nargs; i < SP; i++) {
        c->car = Stack[i];
        c->cdr = tagptr(c+1, TAG_CONS);
        c++;
    }
    if (nargs > MAX_ARGS)
        (c-2)->cdr = (c-1)->car;
    else
        (c-1)->cdr = *plastcdr;
}

#define eval(e)         (selfevaluating(e) ? (e) : eval_sexpr((e),penv,0))
#define topeval(e, env) (selfevaluating(e) ? (e) : eval_sexpr((e),env,1))
#define tail_eval(xpr) do { SP = saveSP;  \
    if (selfevaluating(xpr)) { return (xpr); }  \
    else { e=(xpr); goto eval_top; } } while (0)

/* eval a list of expressions, giving a list of the results */
static value_t evlis(value_t *pv, value_t *penv)
{
    PUSH(NIL);
    PUSH(NIL);
    value_t *rest = &Stack[SP-1];
    value_t a, v = *pv;
    while (iscons(v)) {
        a = car_(v);
        v = eval(a);
        PUSH(v);
        v = mk_cons();
        car_(v) = Stack[SP-1];
        cdr_(v) = NIL;
        (void)POP();
        if (*rest == NIL)
            Stack[SP-2] = v;
        else
            cdr_(*rest) = v;
        *rest = v;
        v = *pv = cdr_(*pv);
    }
    (void)POP();
    return POP();
}

/*
  If we start to run out of space on the lisp value stack, we allocate
  a new stack array and put it on the top of the chain. The new stack
  is active until this function returns. Any return past this function
  must free the new segment.
*/
static value_t new_stackseg(value_t e, value_t *penv, int tail)
{
    stackseg_t s;

    s.prev = current_stack_seg;
    s.Stack = (value_t*)malloc(N_STACK * sizeof(value_t));
    if (s.Stack == NULL)
        lerror(MemoryError, "eval: stack overflow");
    current_stack_seg->SP = SP;
    current_stack_seg = &s;
    SP = 0;
    Stack = s.Stack;
    value_t v = NIL;
    int err = 0;
    FL_TRY {
        v = eval_sexpr(e, penv, tail);
    }
    FL_CATCH {
        err = 1;
        v = lasterror;
    }
    free(s.Stack);
    current_stack_seg = s.prev;
    SP = current_stack_seg->SP;
    Stack = current_stack_seg->Stack;
    if (err) raise(v);
    return v;
}

static value_t do_trycatch(value_t expr, value_t *penv)
{
    value_t v;

    FL_TRY {
        v = eval(expr);
    }
    FL_CATCH {
        v = cdr_(Stack[SP-1]);
        if (!iscons(v)) {
            v = FL_F;   // 1-argument form
        }
        else {
            v = car_(v);
            Stack[SP-1] = eval(v);
            v = apply1(Stack[SP-1], lasterror);
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

 penv[-1] tells you the environment size, from LL through CLO, as a fixnum.
*/
static value_t eval_sexpr(value_t e, value_t *penv, int tail)
{
    value_t f, v, *pv, *lenv;
    cons_t *c;
    symbol_t *sym;
    uint32_t saveSP, envsz, nargs;
    int i, noeval=0;
    fixnum_t s, lo, hi;
    cvalue_t *cv;
    int64_t accum;

 eval_top:
    if (issymbol(e)) {
        sym = (symbol_t*)ptr(e);
        if (sym->syntax == TAG_CONST) return sym->binding;
        while (1) {
            v = *penv++;
            while (iscons(v)) {
                if (car_(v)==e) return *penv;
                v = cdr_(v); penv++;
            }
            if (v != NIL) {
                if (v == e) return *penv;  // dotted list
                penv++;
            }
            if (*penv == NIL) break;
            penv = &vector_elt(*penv, 0);
        }
        if (__unlikely((v = sym->binding) == UNBOUND))
            raise(list2(UnboundError, e));
        return v;
    }
    if (__unlikely(SP >= (N_STACK-MAX_ARGS-4)))
        return new_stackseg(e, penv, tail);
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
        else {
            noeval = 2;
            PUSH(f);
            v = Stack[saveSP];
            goto move_args;
        }
    }
    else f = eval(v);
    PUSH(f);
    v = Stack[saveSP];
    // evaluate argument list, placing arguments on stack
    while (iscons(v)) {
        if (SP-saveSP-2 == MAX_ARGS) {
            v = evlis(&Stack[saveSP], penv);
            PUSH(v);
            break;
        }
        v = car_(v);
        v = eval(v);
        PUSH(v);
        v = Stack[saveSP] = cdr_(Stack[saveSP]);
    }
 do_apply:
    nargs = SP - saveSP - 2;
    if (isbuiltinish(f)) {
        // handle builtin function
    apply_special:
        switch (uintval(f)) {
        // special forms
        case F_QUOTE:
            if (__unlikely(!iscons(Stack[saveSP])))
                lerror(ArgError, "quote: expected argument");
            v = car_(Stack[saveSP]);
            break;
        case F_SETQ:
            e = car(Stack[saveSP]);
            v = car(cdr_(Stack[saveSP]));
            v = eval(v);
            while (1) {
                f = *penv++;
                while (iscons(f)) {
                    if (car_(f)==e) {
                        *penv = v;
                        SP = saveSP;
                        return v;
                    }
                    f = cdr_(f); penv++;
                }
                if (f != NIL) {
                    if (f == e) {
                        *penv = v;
                        SP = saveSP;
                        return v;
                    }
                    penv++;
                }
                if (*penv == NIL) break;
                penv = &vector_elt(*penv, 0);
            }
            sym = tosymbol(e, "set!");
            if (sym->syntax != TAG_CONST)
                sym->binding = v;
            break;
        case F_LAMBDA:
            // build a closure (lambda args body . env)
            if (*penv != NIL) {
                // save temporary environment to the heap
                lenv = penv;
                envsz = numval(penv[-1]);
                pv = alloc_words(envsz + 1);
                PUSH(tagptr(pv, TAG_VECTOR));
                pv[0] = fixnum(envsz);
                pv++;
                while (envsz--)
                    *pv++ = *penv++;
                // environment representation changed; install
                // the new representation so everybody can see it
                lenv[0] = NIL;
                lenv[1] = Stack[SP-1];
            }
            else {
                PUSH(penv[1]); // env has already been captured; share
            }
            c = (cons_t*)ptr(v=cons_reserve(3));
            e = Stack[saveSP];
            if (!iscons(e)) goto notpair;
            c->car = LAMBDA;
            c->cdr = tagptr(c+1, TAG_CONS); c++;
            c->car = car_(e);      //argsyms
            c->cdr = tagptr(c+1, TAG_CONS); c++;
            if (!iscons(e=cdr_(e))) goto notpair;
            c->car = car_(e);      //body
            c->cdr = Stack[SP-1];  //env
            break;
        case F_IF:
            if (!iscons(Stack[saveSP])) goto notpair;
            v = car_(Stack[saveSP]);
            if (eval(v) != FL_F) {
                v = cdr_(Stack[saveSP]);
                if (!iscons(v)) goto notpair;
                v = car_(v);
            }
            else {
                v = cdr_(Stack[saveSP]);
                if (!iscons(v)) goto notpair;
                if (!iscons(v=cdr_(v))) v = FL_F;  // allow 2-arg form
                else v = car_(v);
            }
            tail_eval(v);
            break;
        case F_COND:
            pv = &Stack[saveSP]; v = FL_F;
            while (iscons(*pv)) {
                c = tocons(car_(*pv), "cond");
                v = c->car;
                // allow last condition to be 'else'
                if (iscons(cdr_(*pv)) || v != elsesym)
                    v = eval(v);
                if (v != FL_F) {
                    *pv = cdr_(car_(*pv));
                    // evaluate body forms
                    if (iscons(*pv)) {
                        while (iscons(cdr_(*pv))) {
                            v = car_(*pv);
                            v = eval(v);
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
            pv = &Stack[saveSP]; v = FL_T;
            if (iscons(*pv)) {
                while (iscons(cdr_(*pv))) {
                    if ((v=eval(car_(*pv))) == FL_F) {
                        SP = saveSP; return FL_F;
                    }
                    *pv = cdr_(*pv);
                }
                tail_eval(car_(*pv));
            }
            break;
        case F_OR:
            pv = &Stack[saveSP]; v = FL_F;
            if (iscons(*pv)) {
                while (iscons(cdr_(*pv))) {
                    if ((v=eval(car_(*pv))) != FL_F) {
                        SP = saveSP; return v;
                    }
                    *pv = cdr_(*pv);
                }
                tail_eval(car_(*pv));
            }
            break;
        case F_WHILE:
            PUSH(cdr(Stack[saveSP]));
            lenv = &Stack[SP-1];
            PUSH(*lenv);
            Stack[saveSP] = car_(Stack[saveSP]);
            value_t *cond = &Stack[saveSP];
            PUSH(FL_F);
            pv = &Stack[SP-1];
            while (eval(*cond) != FL_F) {
                *lenv = Stack[SP-2];
                while (iscons(*lenv)) {
                    *pv = eval(car_(*lenv));
                    *lenv = cdr_(*lenv);
                }
            }
            v = *pv;
            break;
        case F_BEGIN:
            // return last arg
            pv = &Stack[saveSP];
            if (iscons(*pv)) {
                while (iscons(cdr_(*pv))) {
                    v = car_(*pv);
                    (void)eval(v);
                    *pv = cdr_(*pv);
                }
                tail_eval(car_(*pv));
            }
            v = FL_F;
            break;
        case F_PROG1:
            // return first arg
            pv = &Stack[saveSP];
            if (__unlikely(!iscons(*pv)))
                lerror(ArgError, "prog1: too few arguments");
            PUSH(eval(car_(*pv)));
            *pv = cdr_(*pv);
            while (iscons(*pv)) {
                (void)eval(car_(*pv));
                *pv = cdr_(*pv);
            }
            v = POP();
            break;
        case F_TRYCATCH:
            v = do_trycatch(car(Stack[saveSP]), penv);
            break;

        // ordinary functions
        case F_BOUNDP:
            argcount("bound?", nargs, 1);
            sym = tosymbol(Stack[SP-1], "bound?");
            v = (sym->binding == UNBOUND) ? FL_F : FL_T;
            break;
        case F_EQ:
            argcount("eq?", nargs, 2);
            v = ((Stack[SP-2] == Stack[SP-1]) ? FL_T : FL_F);
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
        case F_LIST:
            if (nargs) {
                Stack[saveSP] = v;
                list(&v, nargs, &Stack[saveSP]);
            }
            // else v is already set to the final cdr, which is the result
            break;
        case F_CAR:
            argcount("car", nargs, 1);
            v = Stack[SP-1];
            if (!iscons(v)) goto notpair;
            v = car_(v);
            break;
        case F_CDR:
            argcount("cdr", nargs, 1);
            v = Stack[SP-1];
            if (!iscons(v)) goto notpair;
            v = cdr_(v);
            break;
        case F_SETCAR:
            argcount("set-car!", nargs, 2);
            car(v=Stack[SP-2]) = Stack[SP-1];
            break;
        case F_SETCDR:
            argcount("set-cdr!", nargs, 2);
            cdr(v=Stack[SP-2]) = Stack[SP-1];
            break;
        case F_VECTOR:
            if (nargs > MAX_ARGS) {
                i = llength(Stack[SP-1]);
                nargs--;
            }
            else i = 0;
            v = alloc_vector(nargs+i, 0);
            memcpy(&vector_elt(v,0), &Stack[saveSP+2], nargs*sizeof(value_t));
            if (i > 0) {
                e = Stack[SP-1];
                while (iscons(e)) {
                    vector_elt(v,nargs) = car_(e);
                    nargs++;
                    e = cdr_(e);
                }
            }
            break;
        case F_LENGTH:
            argcount("length", nargs, 1);
            if (isvector(Stack[SP-1])) {
                v = fixnum(vector_size(Stack[SP-1]));
                break;
            }
            else if (iscprim(Stack[SP-1])) {
                cv = (cvalue_t*)ptr(Stack[SP-1]);
                if (cp_class(cv) == bytetype) {
                    v = fixnum(1);
                    break;
                }
                else if (cp_class(cv) == wchartype) {
                    v = fixnum(u8_charlen(*(uint32_t*)cp_data((cprim_t*)cv)));
                    break;
                }
            }
            else if (iscvalue(Stack[SP-1])) {
                cv = (cvalue_t*)ptr(Stack[SP-1]);
                if (cv_class(cv)->eltype != NULL) {
                    v = size_wrap(cvalue_arraylen(Stack[SP-1]));
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
                // TODO other sequence types?
                type_error("aref", "sequence", v);
            }
            break;
        case F_ASET:
            argcount("aset!", nargs, 3);
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
            break;
        case F_ATOM:
            argcount("atom?", nargs, 1);
            v = ((!iscons(Stack[SP-1])) ? FL_T : FL_F);
            break;
        case F_CONSP:
            argcount("pair?", nargs, 1);
            v = (iscons(Stack[SP-1]) ? FL_T : FL_F);
            break;
        case F_SYMBOLP:
            argcount("symbol?", nargs, 1);
            v = ((issymbol(Stack[SP-1])) ? FL_T : FL_F);
            break;
        case F_NUMBERP:
            argcount("number?", nargs, 1);
            v = (isfixnum(Stack[SP-1]) || iscprim(Stack[SP-1]) ? FL_T : FL_F);
            break;
        case F_FIXNUMP:
            argcount("fixnum?", nargs, 1);
            v = (isfixnum(Stack[SP-1]) ? FL_T : FL_F);
            break;
        case F_BUILTINP:
            argcount("builtin?", nargs, 1);
            v = Stack[SP-1];
            v = ((isbuiltinish(v) && v!=FL_F && v!=FL_T && v!=NIL)
                 ? FL_T : FL_F);
            break;
        case F_VECTORP:
            argcount("vector?", nargs, 1);
            v = ((isvector(Stack[SP-1])) ? FL_T : FL_F);
            break;
        case F_NOT:
            argcount("not", nargs, 1);
            v = ((Stack[SP-1] == FL_F) ? FL_T : FL_F);
            break;
        case F_NULL:
            argcount("null?", nargs, 1);
            v = ((Stack[SP-1] == NIL) ? FL_T : FL_F);
            break;            
        case F_BOOLEANP:
            argcount("boolean?", nargs, 1);
            v = Stack[SP-1];
            v = ((v == FL_T || v == FL_F) ? FL_T : FL_F);
            break;
        case F_ADD:
            s = 0;
            i = saveSP+2;
            if (nargs > MAX_ARGS) goto add_ovf;
            for (; i < (int)SP; i++) {
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
                    SP = saveSP;
                    return v;
                }
            }
            v = fixnum(s);
            break;
        case F_SUB:
            if (__unlikely(nargs < 1)) lerror(ArgError, "-: too few arguments");
            i = saveSP+2;
            if (nargs == 1) {
                if (__likely(isfixnum(Stack[i])))
                    v = fixnum(-numval(Stack[i]));
                else
                    v = fl_neg(Stack[i]);
                break;
            }
            if (nargs == 2) {
                if (__likely(bothfixnums(Stack[i], Stack[i+1]))) {
                    s = numval(Stack[i]) - numval(Stack[i+1]);
                    if (__likely(fits_fixnum(s))) {
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
                // we need to pass the full arglist on to fl_add_any
                // so it can handle rest args properly
                PUSH(Stack[i]);
                Stack[i] = fixnum(0);
                Stack[i+1] = fl_neg(fl_add_any(&Stack[i], nargs, 0));
                Stack[i] = POP();
            }
            v = fl_add_any(&Stack[i], 2, 0);
            break;
        case F_MUL:
            accum = 1;
            i = saveSP+2;
            if (nargs > MAX_ARGS) goto mul_ovf;
            for (; i < (int)SP; i++) {
                if (__likely(isfixnum(Stack[i]))) {
                    accum *= numval(Stack[i]);
                }
                else {
                mul_ovf:
                    v = fl_mul_any(&Stack[i], SP-i, accum);
                    SP = saveSP;
                    return v;
                }
            }
            if (__likely(fits_fixnum(accum)))
                v = fixnum(accum);
            else
                v = return_from_int64(accum);
            break;
        case F_DIV:
            if (__unlikely(nargs < 1)) lerror(ArgError, "/: too few arguments");
            i = saveSP+2;
            if (nargs == 1) {
                v = fl_div2(fixnum(1), Stack[i]);
            }
            else {
                if (nargs > 2) {
                    PUSH(Stack[i]);
                    Stack[i] = fixnum(1);
                    Stack[i+1] = fl_mul_any(&Stack[i], nargs, 1);
                    Stack[i] = POP();
                }
                v = fl_div2(Stack[i], Stack[i+1]);
            }
            break;
        case F_BNOT:
            argcount("lognot", nargs, 1);
            if (isfixnum(Stack[SP-1]))
                v = fixnum(~numval(Stack[SP-1]));
            else
                v = fl_bitwise_not(Stack[SP-1]);
            break;
        case F_COMPARE:
            argcount("compare", nargs, 2);
            v = compare(Stack[SP-2], Stack[SP-1]);
            break;
        case F_LT:
            argcount("<", nargs, 2);
            if (bothfixnums(Stack[SP-2], Stack[SP-1])) {
                v = (numval(Stack[SP-2]) < numval(Stack[SP-1])) ? FL_T : FL_F;
            }
            else {
                v = (numval(compare(Stack[SP-2], Stack[SP-1])) < 0) ?
                    FL_T : FL_F;
            }
            break;
        case F_EQUAL:
            argcount("equal?", nargs, 2);
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
            break;
        case F_EQV:
            argcount("eqv?", nargs, 2);
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
            break;
        case F_EVAL:
            argcount("eval", nargs, 1);
            v = Stack[SP-1];
            if (selfevaluating(v)) { SP=saveSP; return v; }
            if (tail) {
                assert((ulong_t)(penv-Stack)<N_STACK);
                penv[-1] = fixnum(2);
                penv[0] = NIL;
                penv[1] = NIL;
                SP = (penv-Stack) + 2;
                e=v;
                goto eval_top;
            }
            else {
                PUSH(NIL);
                PUSH(NIL);
                v = eval_sexpr(v, &Stack[SP-2], 1);
            }
            break;
        case F_EVALSTAR:
            argcount("eval*", nargs, 1);
            e = Stack[SP-1];
            if (selfevaluating(e)) { SP=saveSP; return e; }
            POPN(3);
            goto eval_top;
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
            v = FL_F;
            for(s=lo; s <= hi; s++) {
                f = Stack[SP-5];
                Stack[SP-3] = car_(f);     // lambda list
                Stack[SP-2] = fixnum(s);   // argument value
                v = car_(cdr_(f));
                if (!selfevaluating(v)) v = eval_sexpr(v, &Stack[SP-3], 0);
            }
            break;
        case F_SPECIAL_APPLY:
            f = Stack[saveSP-4];
            v = Stack[saveSP-3];
            PUSH(f);
            PUSH(v);
            nargs = 2;
            // falls through!!
        case F_APPLY:
            argcount("apply", nargs, 2);
            v = Stack[saveSP]   = Stack[SP-1]; // second arg is new arglist
            f = Stack[saveSP+1] = Stack[SP-2]; // first arg is new function
            POPN(2);                    // pop apply's args
        move_args:
            while (iscons(v)) {
                if (SP-saveSP-2 == MAX_ARGS) {
                    PUSH(v);
                    break;
                }
                PUSH(car_(v));
                v = cdr_(v);
            }
            goto do_apply;
        case F_TRUE:
        case F_FALSE:
        case F_NIL:
            goto apply_type_error;
        default:
            // function pointer tagged as a builtin
            v = ((builtin_t)ptr(f))(&Stack[saveSP+2], nargs);
        }
        SP = saveSP;
        return v;
    }
    if (__likely(iscons(f))) {
        // apply lambda expression
        f = Stack[saveSP+1];
        f = Stack[saveSP+1] = cdr_(f);
        if (!iscons(f)) goto notpair;
        v = car_(f); // arglist
        i = nargs;
        while (iscons(v)) {
            if (i == 0)
                lerror(ArgError, "apply: too few arguments");
            i--;
            v = cdr_(v);
        }
        if (v == NIL) {
            if (i > 0)
                lerror(ArgError, "apply: too many arguments");
        }
        else {
            if (i > 0) {
                list(&v, i, &NIL);
                if (nargs > MAX_ARGS) {
                    c = (cons_t*)curheap;
                    (c-2)->cdr = (c-1)->car;
                }
                Stack[SP-i] = v;
                SP -= (i-1);
            }
            else {
                PUSH(NIL);
            }
        }
        f = cdr_(Stack[saveSP+1]);
        if (!iscons(f)) goto notpair;
        e = car_(f);
        if (selfevaluating(e)) { SP=saveSP; return(e); }
        PUSH(cdr_(f));                     // add closed environment
        Stack[saveSP+1] = car_(Stack[saveSP+1]);  // put lambda list
        envsz = SP - saveSP - 1;

        if (noeval == 2) {
            // macro: evaluate body in lambda environment
            Stack[saveSP] = fixnum(envsz);
            e = eval_sexpr(e, &Stack[saveSP+1], 1);
            SP = saveSP;
            if (selfevaluating(e)) return(e);
            noeval = 0;
            // macro: evaluate expansion in calling environment
            goto eval_top;
        }
        else {
            if (tail) {
                // ok to overwrite environment
                penv[-1] = fixnum(envsz);
                for(i=0; i < (int)envsz; i++)
                    penv[i] = Stack[saveSP+1+i];
                SP = (penv-Stack)+envsz;
                goto eval_top;
            }
            else {
                Stack[saveSP] = fixnum(envsz);
                v = eval_sexpr(e, &Stack[saveSP+1], 1);
                SP = saveSP;
                return v;
            }
        }
        // not reached
    }
 apply_type_error:
    type_error("apply", "function", f);
 notpair:
    lerror(TypeError, "expected cons");
    return NIL;
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
    elsesym = symbol("else");
    tsym = symbol("t"); Tsym = symbol("T");
    fsym = symbol("f"); Fsym = symbol("F");
    set(printprettysym=symbol("*print-pretty*"), FL_T);
    set(printwidthsym=symbol("*print-width*"), fixnum(SCR_WIDTH));
    lasterror = NIL;
    special_apply_form = fl_cons(builtin(F_SPECIAL_APPLY), NIL);
    apply1_args = fl_cons(NIL, NIL);
    i = 0;
    while (isspecial(builtin(i))) {
        if (i != F_SPECIAL_APPLY)
            ((symbol_t*)ptr(symbol(builtin_names[i])))->syntax = builtin(i);
        i++;
    }
    for (; i < F_TRUE; i++) {
        setc(symbol(builtin_names[i]), builtin(i));
    }
    setc(symbol("eq"), builtin(F_EQ));
    setc(symbol("equal"), builtin(F_EQUAL));

#ifdef LINUX
    set(symbol("*os-name*"), symbol("linux"));
#elif defined(WIN32) || defined(WIN64)
    set(symbol("*os-name*"), symbol("win32"));
#elif defined(MACOSX)
    set(symbol("*os-name*"), symbol("macos"));
#else
    set(symbol("*os-name*"), symbol("unknown"));
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

    memory_exception_value = list2(MemoryError,
                                   cvalue_static_cstring("out of memory"));

    builtins_init();
}

// repl -----------------------------------------------------------------------

value_t toplevel_eval(value_t expr)
{
    value_t v;
    uint32_t saveSP = SP;
    PUSH(NIL);
    PUSH(NIL);
    v = topeval(expr, &Stack[SP-2]);
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
        (void)POP();
    }
    return POP();
}

int locale_is_utf8;

extern value_t fl_file(value_t *args, uint32_t nargs);

int main(int argc, char *argv[])
{
    value_t e, v;
    char fname_buf[1024];

    locale_is_utf8 = u8_is_locale_utf8(setlocale(LC_ALL, ""));

    lisp_init();

    fname_buf[0] = '\0';
    if (EXEDIR != NULL) {
        strcat(fname_buf, EXEDIR);
        strcat(fname_buf, PATHSEPSTRING);
    }
    strcat(fname_buf, "system.lsp");

    FL_TRY {
        // install toplevel exception handler
        PUSH(cvalue_static_cstring(fname_buf));
        PUSH(symbol(":read"));
        value_t f = fl_file(&Stack[SP-2], 2);
        POPN(2);
        PUSH(f);
        while (1) {
            e = read_sexpr(Stack[SP-1]);
            if (ios_eof(value2c(ios_t*,Stack[SP-1]))) break;
            v = toplevel_eval(e);
        }
        ios_close(value2c(ios_t*,Stack[SP-1]));
        (void)POP();

        PUSH(symbol_value(symbol("__start")));
        PUSH(argv_list(argc, argv));
        (void)toplevel_eval(special_apply_form);
    }
    FL_CATCH {
        ios_puts("fatal error during bootstrap:\n", ios_stderr);
        print(ios_stderr, lasterror, 0);
        ios_putc('\n', ios_stderr);
        return 1;
    }

    return 0;
}
