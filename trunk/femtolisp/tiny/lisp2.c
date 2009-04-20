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

  This is a fork of femtoLisp with advanced reading and printing facilities:
  * circular structure can be printed and read
  * #. read macro for eval-when-read and correctly printing builtins
  * read macros for backquote
  * symbol character-escaping printer

  * new print algorithm
     1. traverse & tag all conses to be printed. when you encounter a cons
        that is already tagged, add it to a table to give it a #n# index
     2. untag a cons when printing it. if cons is in the table, print
        "#n=" before it in the car, " . #n=" in the cdr. if cons is in the
        table but already untagged, print #n# in car or " . #n#" in the cdr.
  * read macros for #n# and #n= using the same kind of table
    * also need a table of read labels to translate from input indexes to
      normalized indexes (0 for first label, 1 for next, etc.)
  * read macro #. for eval-when-read. use for printing builtins, e.g. "#.eq"

  The value of this extra complexity, and what makes this fork worthy of
  the femtoLisp brand, is that the interpreter is fully "closed" in the
  sense that all representable values can be read and printed.

  by Jeff Bezanson
  Public Domain
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>
#include <ctype.h>
#include <sys/types.h>

typedef u_int32_t value_t;
typedef int32_t number_t;

typedef struct {
    value_t car;
    value_t cdr;
} cons_t;

typedef struct _symbol_t {
    value_t binding;   // global value binding
    value_t constant;  // constant binding (used only for builtins)
    struct _symbol_t *left;
    struct _symbol_t *right;
    char name[1];
} symbol_t;

#define TAG_NUM      0x0
#define TAG_BUILTIN  0x1
#define TAG_SYM      0x2
#define TAG_CONS     0x3
#define UNBOUND      ((value_t)TAG_SYM) // an invalid symbol pointer
#define tag(x) ((x)&0x3)
#define ptr(x) ((void*)((x)&(~(value_t)0x3)))
#define tagptr(p,t) (((value_t)(p)) | (t))
#define number(x) ((value_t)((x)<<2))
#define numval(x)  (((number_t)(x))>>2)
#define intval(x)  (((int)(x))>>2)
#define builtin(n) tagptr((((int)n)<<2), TAG_BUILTIN)
#define iscons(x)    (tag(x) == TAG_CONS)
#define issymbol(x)  (tag(x) == TAG_SYM)
#define isnumber(x)  (tag(x) == TAG_NUM)
#define isbuiltin(x) (tag(x) == TAG_BUILTIN)
// functions ending in _ are unsafe, faster versions
#define car_(v) (((cons_t*)ptr(v))->car)
#define cdr_(v) (((cons_t*)ptr(v))->cdr)
#define car(v)  (tocons((v),"car")->car)
#define cdr(v)  (tocons((v),"cdr")->cdr)
#define set(s, v)  (((symbol_t*)ptr(s))->binding = (v))
#define setc(s, v) (((symbol_t*)ptr(s))->constant = (v))

enum {
    // special forms
    F_QUOTE=0, F_COND, F_IF, F_AND, F_OR, F_WHILE, F_LAMBDA, F_MACRO, F_LABEL,
    F_PROGN,
    // functions
    F_EQ, F_ATOM, F_CONS, F_CAR, F_CDR, F_READ, F_EVAL, F_PRINT, F_SET, F_NOT,
    F_LOAD, F_SYMBOLP, F_NUMBERP, F_ADD, F_SUB, F_MUL, F_DIV, F_LT, F_PROG1,
    F_APPLY, F_RPLACA, F_RPLACD, F_BOUNDP, F_ERROR, F_EXIT, F_PRINC, F_CONSP,
    F_ASSOC, N_BUILTINS
};
#define isspecial(v) (intval(v) <= (number_t)F_PROGN)

static char *builtin_names[] =
    { "quote", "cond", "if", "and", "or", "while", "lambda", "macro", "label",
      "progn",
      "eq", "atom", "cons", "car", "cdr", "read", "eval", "print",
      "set", "not", "load", "symbolp", "numberp", "+", "-", "*", "/", "<",
      "prog1", "apply", "rplaca", "rplacd", "boundp", "error", "exit", "princ",
      "consp", "assoc" };

static char *stack_bottom;
#define PROCESS_STACK_SIZE (2*1024*1024)
#define N_STACK 98304
static value_t Stack[N_STACK];
static u_int32_t SP = 0;
#define PUSH(v) (Stack[SP++] = (v))
#define POP()   (Stack[--SP])
#define POPN(n) (SP-=(n))

value_t NIL, T, LAMBDA, MACRO, LABEL, QUOTE;
value_t BACKQUOTE, COMMA, COMMAAT, COMMADOT;

value_t read_sexpr(FILE *f);
void print(FILE *f, value_t v, int princ);
value_t eval_sexpr(value_t e, value_t *penv, int tail, u_int32_t envend);
value_t load_file(char *fname);
value_t toplevel_eval(value_t expr);

#include "flutils.c"

typedef struct _readstate_t {
    ltable_t labels;
    ltable_t exprs;
    struct _readstate_t *prev;
} readstate_t;
static readstate_t *readstate = NULL;

// error utilities ------------------------------------------------------------

jmp_buf toplevel;

void lerror(char *format, ...)
{
    va_list args;
    va_start(args, format);

    while (readstate) {
        free(readstate->labels.items);
        free(readstate->exprs.items);
        readstate = readstate->prev;
    }

    vfprintf(stderr, format, args);
    va_end(args);
    longjmp(toplevel, 1);
}

void type_error(char *fname, char *expected, value_t got)
{
    fprintf(stderr, "%s: error: expected %s, got ", fname, expected);
    print(stderr, got, 0); lerror("\n");
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
SAFECAST_OP(number,number_t, numval)

// symbol table ---------------------------------------------------------------

static symbol_t *symtab = NULL;

static symbol_t *mk_symbol(char *str)
{
    symbol_t *sym;

    sym = (symbol_t*)malloc(sizeof(symbol_t) + strlen(str));
    sym->left = sym->right = NULL;
    sym->constant = sym->binding = UNBOUND;
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

// initialization -------------------------------------------------------------

static unsigned char *fromspace;
static unsigned char *tospace;
static unsigned char *curheap;
static unsigned char *lim;
static u_int32_t heapsize = 128*1024;//bytes
static u_int32_t *consflags;
static ltable_t printconses;

void lisp_init(void)
{
    int i;

    fromspace = malloc(heapsize);
    tospace   = malloc(heapsize);
    curheap = fromspace;
    lim = curheap+heapsize-sizeof(cons_t);
    consflags = mk_bitvector(heapsize/sizeof(cons_t));

    ltable_init(&printconses, 32);

    NIL = symbol("nil"); setc(NIL, NIL);
    T   = symbol("t");   setc(T,   T);
    LAMBDA = symbol("lambda");
    MACRO = symbol("macro");
    LABEL = symbol("label");
    QUOTE = symbol("quote");
    BACKQUOTE = symbol("backquote");
    COMMA = symbol("*comma*");
    COMMAAT = symbol("*comma-at*");
    COMMADOT = symbol("*comma-dot*");
    for (i=0; i < (int)N_BUILTINS; i++)
        setc(symbol(builtin_names[i]), builtin(i));
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

// allocate n consecutive conses
static value_t cons_reserve(int n)
{
    cons_t *first;

    n--;
    if ((cons_t*)curheap > ((cons_t*)lim)-n) {
        gc(0);
        while ((cons_t*)curheap > ((cons_t*)lim)-n) {
            gc(1);
        }
    }
    first = (cons_t*)curheap;
    curheap += ((n+1)*sizeof(cons_t));
    return tagptr(first, TAG_CONS);
}

#define cons_index(c)  (((cons_t*)ptr(c))-((cons_t*)fromspace))
#define ismarked(c)    bitvector_get(consflags, cons_index(c))
#define mark_cons(c)   bitvector_set(consflags, cons_index(c), 1)
#define unmark_cons(c) bitvector_set(consflags, cons_index(c), 0)

// collector ------------------------------------------------------------------

static value_t relocate(value_t v)
{
    value_t a, d, nc, first, *pcdr;

    if (!iscons(v))
        return v;
    // iterative implementation allows arbitrarily long cons chains
    pcdr = &first;
    do {
        if ((a=car_(v)) == UNBOUND) {
            *pcdr = cdr_(v);
            return first;
        }
        *pcdr = nc = mk_cons();
        d = cdr_(v);
        car_(v) = UNBOUND; cdr_(v) = nc;
        car_(nc) = relocate(a);
        pcdr = &cdr_(nc);
        v = d;
    } while (iscons(v));
    *pcdr = d;

    return first;
}

static void trace_globals(symbol_t *root)
{
    while (root != NULL) {
        root->binding = relocate(root->binding);
        trace_globals(root->left);
        root = root->right;
    }
}

void gc(int mustgrow)
{
    static int grew = 0;
    void *temp;
    u_int32_t i;
    readstate_t *rs;

    curheap = tospace;
    lim = curheap+heapsize-sizeof(cons_t);

    for (i=0; i < SP; i++)
        Stack[i] = relocate(Stack[i]);
    trace_globals(symtab);
    rs = readstate;
    while (rs) {
        for(i=0; i < rs->exprs.n; i++)
            rs->exprs.items[i] = relocate(rs->exprs.items[i]);
        rs = rs->prev;
    }
#ifdef VERBOSEGC
    printf("gc found %d/%d live conses\n",
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
            lerror("out of memory\n");
        tospace = temp;
        if (!grew) {
            heapsize*=2;
        }
        else {
            temp = bitvector_resize(consflags, heapsize/sizeof(cons_t));
            if (temp == NULL)
                lerror("out of memory\n");
            consflags = (u_int32_t*)temp;
        }
        grew = !grew;
    }
    if (curheap > lim)  // all data was live
        gc(0);
}

// read -----------------------------------------------------------------------

enum {
    TOK_NONE, TOK_OPEN, TOK_CLOSE, TOK_DOT, TOK_QUOTE, TOK_SYM, TOK_NUM,
    TOK_BQ, TOK_COMMA, TOK_COMMAAT, TOK_COMMADOT,
    TOK_SHARPDOT, TOK_LABEL, TOK_BACKREF, TOK_SHARPQUOTE
};

// defines which characters are ordinary symbol characters.
// the only exception is '.', which is an ordinary symbol character
// unless it is the only character in the symbol.
static int symchar(char c)
{
    static char *special = "()';`,\\|";
    return (!isspace(c) && !strchr(special, c));
}

static u_int32_t toktype = TOK_NONE;
static value_t tokval;
static char buf[256];

static char nextchar(FILE *f)
{
    int ch;
    char c;

    do {
        ch = fgetc(f);
        if (ch == EOF)
            return 0;
        c = (char)ch;
        if (c == ';') {
            // single-line comment
            do {
                ch = fgetc(f);
                if (ch == EOF)
                    return 0;
            } while ((char)ch != '\n');
            c = (char)ch;
        }
    } while (isspace(c));
    return c;
}

static void take(void)
{
    toktype = TOK_NONE;
}

static void accumchar(char c, int *pi)
{
    buf[(*pi)++] = c;
    if (*pi >= (int)(sizeof(buf)-1))
        lerror("read: error: token too long\n");
}

// return: 1 for dot token, 0 for symbol
static int read_token(FILE *f, char c, int digits)
{
    int i=0, ch, escaped=0, dot=(c=='.'), totread=0;

    ungetc(c, f);
    while (1) {
        ch = fgetc(f); totread++;
        if (ch == EOF)
            goto terminate;
        c = (char)ch;
        if (c == '|') {
            escaped = !escaped;
        }
        else if (c == '\\') {
            ch = fgetc(f);
            if (ch == EOF)
                goto terminate;
            accumchar((char)ch, &i);
        }
        else if (!escaped && !(symchar(c) && (!digits || isdigit(c)))) {
            break;
        }
        else {
            accumchar(c, &i);
        }
    }
    ungetc(c, f);
 terminate:
    buf[i++] = '\0';
    return (dot && (totread==2));
}

static u_int32_t peek(FILE *f)
{
    char c, *end;
    number_t x;
    int ch;

    if (toktype != TOK_NONE)
        return toktype;
    c = nextchar(f);
    if (feof(f)) return TOK_NONE;
    if (c == '(') {
        toktype = TOK_OPEN;
    }
    else if (c == ')') {
        toktype = TOK_CLOSE;
    }
    else if (c == '\'') {
        toktype = TOK_QUOTE;
    }
    else if (c == '`') {
        toktype = TOK_BQ;
    }
    else if (c == '#') {
        ch = fgetc(f);
        if (ch == EOF)
            lerror("read: error: invalid read macro\n");
        if ((char)ch == '.') {
            toktype = TOK_SHARPDOT;
        }
        else if ((char)ch == '\'') {
            toktype = TOK_SHARPQUOTE;
        }
        else if ((char)ch == '\\') {
            u_int32_t cval = u8_fgetc(f);
            toktype = TOK_NUM;
            tokval = number(cval);
        }
        else if (isdigit((char)ch)) {
            read_token(f, (char)ch, 1);
            c = (char)fgetc(f);
            if (c == '#')
                toktype = TOK_BACKREF;
            else if (c == '=')
                toktype = TOK_LABEL;
            else
                lerror("read: error: invalid label\n");
            x = strtol(buf, &end, 10);
            tokval = number(x);
        }
        else {
            lerror("read: error: unknown read macro\n");
        }
    }
    else if (c == ',') {
        toktype = TOK_COMMA;
        ch = fgetc(f);
        if (ch == EOF)
            return toktype;
        if ((char)ch == '@')
            toktype = TOK_COMMAAT;
        else if ((char)ch == '.')
            toktype = TOK_COMMADOT;
        else
            ungetc((char)ch, f);
    }
    else if (isdigit(c) || c=='-' || c=='+') {
        read_token(f, c, 0);
        x = strtol(buf, &end, 0);
        if (*end != '\0') {
            toktype = TOK_SYM;
            tokval = symbol(buf);
        }
        else {
            toktype = TOK_NUM;
            tokval = number(x);
        }
    }
    else {
        if (read_token(f, c, 0)) {
            toktype = TOK_DOT;
        }
        else {
            toktype = TOK_SYM;
            tokval = symbol(buf);
        }
    }
    return toktype;
}

static value_t do_read_sexpr(FILE *f, int fixup);

// build a list of conses. this is complicated by the fact that all conses
// can move whenever a new cons is allocated. we have to refer to every cons
// through a handle to a relocatable pointer (i.e. a pointer on the stack).
static void read_list(FILE *f, value_t *pval, int fixup)
{
    value_t c, *pc;
    u_int32_t t;

    PUSH(NIL);
    pc = &Stack[SP-1];  // to keep track of current cons cell
    t = peek(f);
    while (t != TOK_CLOSE) {
        if (feof(f))
            lerror("read: error: unexpected end of input\n");
        c = mk_cons(); car_(c) = cdr_(c) = NIL;
        if (iscons(*pc)) {
            cdr_(*pc) = c;
        }
        else {
            *pval = c;
            if (fixup != -1)
                readstate->exprs.items[fixup] = c;
        }
        *pc = c;
        c = do_read_sexpr(f,-1);  // must be on separate lines due to undefined
        car_(*pc) = c;            // evaluation order

        t = peek(f);
        if (t == TOK_DOT) {
            take();
            c = do_read_sexpr(f,-1);
            cdr_(*pc) = c;
            t = peek(f);
            if (feof(f))
                lerror("read: error: unexpected end of input\n");
            if (t != TOK_CLOSE)
                lerror("read: error: expected ')'\n");
        }
    }
    take();
    POP();
}

// fixup is the index of the label we'd like to fix up with this read
static value_t do_read_sexpr(FILE *f, int fixup)
{
    value_t v, *head;
    u_int32_t t, l;
    int i;

    t = peek(f);
    take();
    switch (t) {
    case TOK_CLOSE:
        lerror("read: error: unexpected ')'\n");
    case TOK_DOT:
        lerror("read: error: unexpected '.'\n");
    case TOK_SYM:
    case TOK_NUM:
        return tokval;
    case TOK_COMMA:
        head = &COMMA; goto listwith;
    case TOK_COMMAAT:
        head = &COMMAAT; goto listwith;
    case TOK_COMMADOT:
        head = &COMMADOT; goto listwith;
    case TOK_BQ:
        head = &BACKQUOTE; goto listwith;
    case TOK_QUOTE:
        head = &QUOTE;
    listwith:
        v = cons_reserve(2);
        car_(v) = *head;
        cdr_(v) = tagptr(((cons_t*)ptr(v))+1, TAG_CONS);
        car_(cdr_(v)) = cdr_(cdr_(v)) = NIL;
        PUSH(v);
        if (fixup != -1)
            readstate->exprs.items[fixup] = v;
        v = do_read_sexpr(f,-1);
        car_(cdr_(Stack[SP-1])) = v;
        return POP();
    case TOK_SHARPQUOTE:
        // femtoLisp doesn't need symbol-function, so #' does nothing
        return do_read_sexpr(f, fixup);
    case TOK_OPEN:
        PUSH(NIL);
        read_list(f, &Stack[SP-1], fixup);
        return POP();
    case TOK_SHARPDOT:
        // eval-when-read
        // evaluated expressions can refer to existing backreferences, but they
        // cannot see pending labels. in other words:
        // (... #2=#.#0# ... )    OK
        // (... #2=#.(#2#) ... )  DO NOT WANT
        v = do_read_sexpr(f,-1);
        return toplevel_eval(v);
    case TOK_LABEL:
        // create backreference label
        l = numval(tokval);
        if (ltable_lookup(&readstate->labels, l) != NOTFOUND)
            lerror("read: error: label %d redefined\n", l);
        ltable_insert(&readstate->labels, l);
        i = readstate->exprs.n;
        ltable_insert(&readstate->exprs, UNBOUND);
        v = do_read_sexpr(f,i);
        readstate->exprs.items[i] = v;
        return v;
    case TOK_BACKREF:
        // look up backreference
        l = numval(tokval);
        i = ltable_lookup(&readstate->labels, l);
        if (i == NOTFOUND || i >= (int)readstate->exprs.n ||
            readstate->exprs.items[i] == UNBOUND)
            lerror("read: error: undefined label %d\n", l);
        return readstate->exprs.items[i];
    }
    return NIL;
}

value_t read_sexpr(FILE *f)
{
    value_t v;
    readstate_t state;
    state.prev = readstate;
    ltable_init(&state.labels, 16);
    ltable_init(&state.exprs, 16);
    readstate = &state;

    v = do_read_sexpr(f, -1);

    readstate = state.prev;
    free(state.labels.items);
    free(state.exprs.items);
    return v;
}

// print ----------------------------------------------------------------------

static void print_traverse(value_t v)
{
    while (iscons(v)) {
        if (ismarked(v)) {
            ltable_adjoin(&printconses, v);
            return;
        }
        mark_cons(v);
        print_traverse(car_(v));
        v = cdr_(v);
    }
}

static void print_symbol(FILE *f, char *name)
{
    int i, escape=0, charescape=0;

    if (name[0] == '\0') {
        fprintf(f, "||");
        return;
    }
    if (name[0] == '.' && name[1] == '\0') {
        fprintf(f, "|.|");
        return;
    }
    if (name[0] == '#')
        escape = 1;
    i=0;
    while (name[i]) {
        if (!symchar(name[i])) {
            escape = 1;
            if (name[i]=='|' || name[i]=='\\') {
                charescape = 1;
                break;
            }
        }
        i++;
    }
    if (escape) {
        if (charescape) {
            fprintf(f, "|");
            i=0;
            while (name[i]) {
                if (name[i]=='|' || name[i]=='\\')
                    fprintf(f, "\\%c", name[i]);
                else
                    fprintf(f, "%c", name[i]);
                i++;
            }
            fprintf(f, "|");
        }
        else {
            fprintf(f, "|%s|", name);
        }
    }
    else {
        fprintf(f, "%s", name);
    }
}

static void do_print(FILE *f, value_t v, int princ)
{
    value_t cd;
    int label;
    char *name;

    switch (tag(v)) {
    case TAG_NUM: fprintf(f, "%d", numval(v)); break;
    case TAG_SYM:
        name = ((symbol_t*)ptr(v))->name;
        if (princ)
            fprintf(f, "%s", name);
        else
            print_symbol(f, name);
        break;
    case TAG_BUILTIN: fprintf(f, "#.%s", builtin_names[intval(v)]); break;
    case TAG_CONS:
        if ((label=ltable_lookup(&printconses,v)) != NOTFOUND) {
            if (!ismarked(v)) {
                fprintf(f, "#%d#", label);
                return;
            }
            fprintf(f, "#%d=", label);
        }
        fprintf(f, "(");
        while (1) {
            unmark_cons(v);
            do_print(f, car_(v), princ);
            cd = cdr_(v);
            if (!iscons(cd)) {
                if (cd != NIL) {
                    fprintf(f, " . ");
                    do_print(f, cd, princ);
                }
                fprintf(f, ")");
                break;
            }
            else {
                if ((label=ltable_lookup(&printconses,cd)) != NOTFOUND) {
                    fprintf(f, " . ");
                    do_print(f, cd, princ);
                    fprintf(f, ")");
                    break;
                }
            }
            fprintf(f, " ");
            v = cd;
        }
        break;
    }
}

void print(FILE *f, value_t v, int princ)
{
    ltable_clear(&printconses);
    print_traverse(v);
    do_print(f, v, princ);
}

// eval -----------------------------------------------------------------------

static inline void argcount(char *fname, int nargs, int c)
{
    if (nargs != c)
        lerror("%s: error: too %s arguments\n", fname, nargs<c ? "few":"many");
}

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

#define eval(e)         ((tag(e)<0x2) ? (e) : eval_sexpr((e),penv,0,envend))
#define topeval(e, env) ((tag(e)<0x2) ? (e) : eval_sexpr((e),env,1,SP))
#define tail_eval(xpr) do { SP = saveSP;  \
    if (tag(xpr)<0x2) { return (xpr); } \
    else { e=(xpr); goto eval_top; } } while (0)

/* stack setup on entry:
  n     n+1   ...
 +-----+-----+-----+-----+-----+-----+-----+-----+
 | SYM | VAL | SYM | VAL | CLO |     |     |     |
 +-----+-----+-----+-----+-----+-----+-----+-----+
  ^                             ^                      ^
  |                             |                      |
  penv                          envend                 SP (who knows where)

 sym is an argument name and val is its binding. CLO is a closed-up
 environment list (which can be empty, i.e. NIL).
 CLO is always there, but there might be zero SYM/VAL pairs.

 if tail==1, you are allowed (indeed encouraged) to overwrite this
 environment, otherwise you have to put any new environment on the top
 of the stack.
*/
value_t eval_sexpr(value_t e, value_t *penv, int tail, u_int32_t envend)
{
    value_t f, v, headsym, asym, *pv, *argsyms, *body, *lenv, *argenv;
    cons_t *c;
    symbol_t *sym;
    u_int32_t saveSP;
    int i, nargs, noeval=0;
    number_t s, n;

 eval_top:
    if (issymbol(e)) {
        sym = (symbol_t*)ptr(e);
        if (sym->constant != UNBOUND) return sym->constant;
        while (issymbol(*penv)) {   // 1. try lookup in argument env
            if (*penv == NIL)
                goto get_global;
            if (*penv == e)
                return penv[1];
            penv+=2;
        }
        if ((v=assoc(e,*penv)) != NIL)  // 2. closure env
            return cdr_(v);
    get_global:
        if ((v = sym->binding) == UNBOUND)   // 3. global env
            lerror("eval: error: variable %s has no value\n", sym->name);
        return v;
    }
    if ((unsigned)(char*)&nargs < (unsigned)stack_bottom || SP>=(N_STACK-100))
        lerror("eval: error: stack overflow\n");
    saveSP = SP;
    PUSH(e);
    v = car_(e);
    if (tag(v)<0x2) f = v;
    else if (issymbol(v) && (f=((symbol_t*)ptr(v))->constant)!=UNBOUND) ;
    else f = eval_sexpr(v, penv, 0, envend);
    if (isbuiltin(f)) {
        // handle builtin function
        if (!isspecial(f)) {
            // evaluate argument list, placing arguments on stack
            v = Stack[saveSP] = cdr_(Stack[saveSP]);
            while (iscons(v)) {
                v = eval(car_(v));
                PUSH(v);
                v = Stack[saveSP] = cdr_(Stack[saveSP]);
            }
        }
    apply_builtin:
        nargs = SP - saveSP - 1;
        switch (intval(f)) {
        // special forms
        case F_QUOTE:
            v = cdr_(Stack[saveSP]);
            if (!iscons(v)) lerror("quote: error: expected argument\n");
            v = car_(v);
            break;
        case F_MACRO:
        case F_LAMBDA:
            // build a closure (lambda args body . env)
            if (issymbol(*penv) && *penv != NIL) {
                // cons up and save temporary environment
                PUSH(Stack[envend-1]); // passed-in CLOENV
                // find out how many new conses we need
                nargs = ((int)(&Stack[envend] - penv - 1))>>1;
                if (nargs) {
                    lenv = penv;
                    Stack[SP-1] = cons_reserve(nargs*2);
                    c = (cons_t*)ptr(Stack[SP-1]);
                    while (1) {
                        c->car = tagptr(c+1, TAG_CONS);
                        (c+1)->car = penv[0];
                        (c+1)->cdr = penv[1];
                        nargs--;
                        if (nargs==0) break;
                        penv+=2;
                        c->cdr = tagptr(c+2, TAG_CONS);
                        c += 2;
                    }
                    // final cdr points to existing cloenv
                    c->cdr = Stack[envend-1];
                    // environment representation changed; install
                    // the new representation so everybody can see it
                    *lenv = Stack[SP-1];
                }
            }
            else {
                PUSH(*penv); // env has already been captured; share
            }
            v = cdr_(Stack[saveSP]);
            PUSH(car(v));
            PUSH(car(cdr_(v)));
            c = (cons_t*)ptr(v=cons_reserve(3));
            c->car = (intval(f)==F_LAMBDA ? LAMBDA : MACRO);
            c->cdr = tagptr(c+1, TAG_CONS); c++;
            c->car = Stack[SP-2]; //argsyms
            c->cdr = tagptr(c+1, TAG_CONS); c++;
            c->car = Stack[SP-1]; //body
            c->cdr = Stack[SP-3]; //env
            break;
        case F_LABEL:
            // the syntax of label is (label name (lambda args body))
            // nothing else is guaranteed to work
            v = cdr_(Stack[saveSP]);
            PUSH(car(v));
            PUSH(car(cdr_(v)));
            body = &Stack[SP-1];
            *body = eval(*body);  // evaluate lambda
            c = (cons_t*)ptr(cons_reserve(2));
            c->car = Stack[SP-2]; // name
            c->cdr = v = *body; c++;
            c->car = tagptr(c-1, TAG_CONS);
            f = cdr(cdr(v));
            c->cdr = cdr(f);
            // add (name . fn) to front of function's environment
            cdr_(f) = tagptr(c, TAG_CONS);
            break;
        case F_IF:
            v = car(cdr_(Stack[saveSP]));
            if (eval(v) != NIL)
                v = car(cdr_(cdr_(Stack[saveSP])));
            else
                v = car(cdr(cdr_(cdr_(Stack[saveSP]))));
            tail_eval(v);
            break;
        case F_COND:
            Stack[saveSP] = cdr_(Stack[saveSP]);
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
            Stack[saveSP] = cdr_(Stack[saveSP]);
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
            Stack[saveSP] = cdr_(Stack[saveSP]);
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
            PUSH(cdr(cdr_(Stack[saveSP])));
            body = &Stack[SP-1];
            PUSH(*body);
            Stack[saveSP] = car_(cdr_(Stack[saveSP]));
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
            Stack[saveSP] = cdr_(Stack[saveSP]);
            pv = &Stack[saveSP]; v = NIL;
            if (iscons(*pv)) {
                while (iscons(cdr_(*pv))) {
                    v = eval(car_(*pv));
                    *pv = cdr_(*pv);
                }
                tail_eval(car_(*pv));
            }
            break;

        // ordinary functions
        case F_SET:
            argcount("set", nargs, 2);
            e = Stack[SP-2];
            while (issymbol(*penv)) {
                if (*penv == NIL)
                    goto set_global;
                if (*penv == e) {
                    penv[1] = Stack[SP-1];
                    SP=saveSP; return penv[1];
                }
                penv+=2;
            }
            if ((v=assoc(e,*penv)) != NIL) {
                cdr_(v) = (e=Stack[SP-1]);
                SP=saveSP; return e;
            }
        set_global:
            tosymbol(e, "set")->binding = (v=Stack[SP-1]);
            break;
        case F_BOUNDP:
            argcount("boundp", nargs, 1);
            sym = tosymbol(Stack[SP-1], "boundp");
            if (sym->binding == UNBOUND && sym->constant == UNBOUND)
                v = NIL;
            else
                v = T;
            break;
        case F_EQ:
            argcount("eq", nargs, 2);
            v = ((Stack[SP-2] == Stack[SP-1]) ? T : NIL);
            break;
        case F_CONS:
            argcount("cons", nargs, 2);
            v = mk_cons();
            car_(v) = Stack[SP-2];
            cdr_(v) = Stack[SP-1];
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
            v = ((isnumber(Stack[SP-1])) ? T : NIL);
            break;
        case F_ADD:
            s = 0;
            for (i=saveSP+1; i < (int)SP; i++) {
                n = tonumber(Stack[i], "+");
                s += n;
            }
            v = number(s);
            break;
        case F_SUB:
            if (nargs < 1) lerror("-: error: too few arguments\n");
            i = saveSP+1;
            s = (nargs==1) ? 0 : tonumber(Stack[i++], "-");
            for (; i < (int)SP; i++) {
                n = tonumber(Stack[i], "-");
                s -= n;
            }
            v = number(s);
            break;
        case F_MUL:
            s = 1;
            for (i=saveSP+1; i < (int)SP; i++) {
                n = tonumber(Stack[i], "*");
                s *= n;
            }
            v = number(s);
            break;
        case F_DIV:
            if (nargs < 1) lerror("/: error: too few arguments\n");
            i = saveSP+1;
            s = (nargs==1) ? 1 : tonumber(Stack[i++], "/");
            for (; i < (int)SP; i++) {
                n = tonumber(Stack[i], "/");
                if (n == 0) lerror("/: error: division by zero\n");
                s /= n;
            }
            v = number(s);
            break;
        case F_LT:
            argcount("<", nargs, 2);
            // this implements generic comparison for all atoms
            // strange comparisons (for example with builtins) are resolved
            // arbitrarily but consistently.
            // ordering: number < builtin < symbol < cons
            if (tag(Stack[SP-2]) != tag(Stack[SP-1])) {
                v = (tag(Stack[SP-2]) < tag(Stack[SP-1]) ? T : NIL);
            }
            else {
                switch (tag(Stack[SP-2])) {
                case TAG_NUM:
                    v = (numval(Stack[SP-2]) < numval(Stack[SP-1])) ? T : NIL;
                    break;
                case TAG_SYM:
                    v = (strcmp(((symbol_t*)ptr(Stack[SP-2]))->name,
                                ((symbol_t*)ptr(Stack[SP-1]))->name) < 0) ?
                        T : NIL;
                    break;
                case TAG_BUILTIN:
                    v = (intval(Stack[SP-2]) < intval(Stack[SP-1])) ? T : NIL;
                    break;
                case TAG_CONS:
                    lerror("<: error: expected atom\n");
                }
            }
            break;
        case F_NOT:
            argcount("not", nargs, 1);
            v = ((Stack[SP-1] == NIL) ? T : NIL);
            break;
        case F_EVAL:
            argcount("eval", nargs, 1);
            v = Stack[SP-1];
            if (tag(v)<0x2) { SP=saveSP; return v; }
            if (tail) {
                *penv = NIL;
                envend = SP = (u_int32_t)(penv-&Stack[0]) + 1;
                e=v; goto eval_top;
            }
            else {
                PUSH(NIL);
                v = eval_sexpr(v, &Stack[SP-1], 1, SP);
            }
            break;
        case F_PRINT:
            for (i=saveSP+1; i < (int)SP; i++)
                print(stdout, v=Stack[i], 0);
            fprintf(stdout, "\n");
            break;
        case F_PRINC:
            for (i=saveSP+1; i < (int)SP; i++)
                print(stdout, v=Stack[i], 1);
            break;
        case F_READ:
            argcount("read", nargs, 0);
            v = read_sexpr(stdin);
            break;
        case F_LOAD:
            argcount("load", nargs, 1);
            v = load_file(tosymbol(Stack[SP-1], "load")->name);
            break;
        case F_EXIT:
            exit(0);
            break;
        case F_ERROR:
            for (i=saveSP+1; i < (int)SP; i++)
                print(stderr, Stack[i], 1);
            lerror("\n");
            break;
        case F_PROG1:
            // return first arg
            if (nargs < 1) lerror("prog1: error: too few arguments\n");
            v = Stack[saveSP+1];
            break;
        case F_ASSOC:
            argcount("assoc", nargs, 2);
            v = assoc(Stack[SP-2], Stack[SP-1]);
            break;
        case F_APPLY:
            argcount("apply", nargs, 2);
            v = Stack[saveSP] = Stack[SP-1];  // second arg is new arglist
            f = Stack[SP-2];            // first arg is new function
            POPN(2);                    // pop apply's args
            if (isbuiltin(f)) {
                if (isspecial(f))
                    lerror("apply: error: cannot apply special operator "
                           "%s\n", builtin_names[intval(f)]);
                // unpack arglist onto the stack
                while (iscons(v)) {
                    PUSH(car_(v));
                    v = cdr_(v);
                }
                goto apply_builtin;
            }
            noeval = 1;
            goto apply_lambda;
        }
        SP = saveSP;
        return v;
    }
    else {
        v = Stack[saveSP] = cdr_(Stack[saveSP]);
    }
 apply_lambda:
    if (iscons(f)) {
        headsym = car_(f);
        // apply lambda or macro expression
        PUSH(cdr(cdr_(f)));
        PUSH(car_(cdr_(f)));
        argsyms = &Stack[SP-1];
        argenv = &Stack[SP];  // argument environment starts now
        if (headsym == MACRO)
            noeval = 1;
        //else if (headsym != LAMBDA)
        //    lerror("apply: error: head must be lambda, macro, or label\n");
        // build a calling environment for the lambda
        // the environment is the argument binds on top of the captured
        // environment
        while (iscons(v)) {
            // bind args
            if (!iscons(*argsyms)) {
                if (*argsyms == NIL)
                    lerror("apply: error: too many arguments\n");
                break;
            }
            asym = car_(*argsyms);
            if (asym==NIL || iscons(asym))
                lerror("apply: error: invalid formal argument\n");
            v = car_(v);
            if (!noeval) {
                v = eval(v);
            }
            PUSH(asym);
            PUSH(v);
            *argsyms = cdr_(*argsyms);
            v = Stack[saveSP] = cdr_(Stack[saveSP]);
        }
        if (*argsyms != NIL) {
            if (issymbol(*argsyms)) {
                PUSH(*argsyms);
                if (noeval) {
                    PUSH(Stack[saveSP]);
                }
                else {
                    // this version uses collective allocation. about 7-10%
                    // faster for lists with > 2 elements, but uses more
                    // stack space
                    PUSH(NIL);
                    i = SP;
                    while (iscons(Stack[saveSP])) {
                        PUSH(eval(car_(Stack[saveSP])));
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
                        (c-1)->cdr = NIL;
                        POPN(nargs);
                    }
                }
            }
            else if (iscons(*argsyms)) {
                lerror("apply: error: too few arguments\n");
            }
        }
        noeval = 0;
        lenv = &Stack[saveSP+1];
        PUSH(cdr(*lenv)); // add cloenv to new environment
        e = car_(Stack[saveSP+1]);
        // macro: evaluate expansion in the calling environment
        if (headsym == MACRO) {
            if (tag(e)<0x2) ;
            else e = eval_sexpr(e, argenv, 1, SP);
            SP = saveSP;
            if (tag(e)<0x2) return(e);
            goto eval_top;
        }
        else {
            if (tag(e)<0x2) { SP=saveSP; return(e); }
            if (tail) {
                // ok to overwrite environment
                nargs = (int)(&Stack[SP] - argenv);
                for(i=0; i < nargs; i++)
                    penv[i] = argenv[i];
                envend = SP = (u_int32_t)((penv+nargs) - &Stack[0]);
                goto eval_top;
            }
            else {
                v = eval_sexpr(e, argenv, 1, SP);
                SP = saveSP;
                return v;
            }
        }
        // not reached
    }
    type_error("apply", "function", f);
    return NIL;
}

// repl -----------------------------------------------------------------------

static char *infile = NULL;

value_t toplevel_eval(value_t expr)
{
    value_t v;
    u_int32_t saveSP = SP;
    PUSH(NIL);
    v = topeval(expr, &Stack[SP-1]);
    SP = saveSP;
    return v;
}

value_t load_file(char *fname)
{
    value_t e, v=NIL;
    char *lastfile = infile;
    FILE *f = fopen(fname, "r");
    infile = fname;
    if (f == NULL) lerror("file not found\n");
    while (1) {
        e = read_sexpr(f);
        if (feof(f)) break;
        v = toplevel_eval(e);
    }
    infile = lastfile;
    fclose(f);
    return v;
}

int main(int argc, char* argv[])
{
    value_t v;

    stack_bottom = ((char*)&v) - PROCESS_STACK_SIZE;
    lisp_init();
    if (setjmp(toplevel)) {
        SP = 0;
        fprintf(stderr, "\n");
        if (infile) {
            fprintf(stderr, "error loading file \"%s\"\n", infile);
            infile = NULL;
        }
        goto repl;
    }
    load_file("system.lsp");
    if (argc > 1) { load_file(argv[1]); return 0; }
    printf(";  _                   \n");
    printf("; |_ _ _ |_ _ |  . _ _ 2\n");
    printf("; | (-||||_(_)|__|_)|_)\n");
    printf(";-------------------|----------------------------------------------------------\n\n");
 repl:
    while (1) {
        printf("> ");
        v = read_sexpr(stdin);
        if (feof(stdin)) break;
        print(stdout, v=toplevel_eval(v), 0);
        set(symbol("that"), v);
        printf("\n\n");
    }
    return 0;
}
