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

#ifdef __LP64__
typedef u_int64_t value_t;
typedef int64_t number_t;
#else
typedef u_int32_t value_t;
typedef int32_t number_t;
#endif

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
    F_APPLY, F_RPLACA, F_RPLACD, F_BOUNDP, N_BUILTINS
};
#define isspecial(v) (intval(v) <= (int)F_PROGN)

static char *builtin_names[] =
    { "quote", "cond", "if", "and", "or", "while", "lambda", "macro", "label",
      "progn", "eq", "atom", "cons", "car", "cdr", "read", "eval", "print",
      "set", "not", "load", "symbolp", "numberp", "+", "-", "*", "/", "<",
      "prog1", "apply", "rplaca", "rplacd", "boundp" };

static char *stack_bottom;
#define PROCESS_STACK_SIZE (2*1024*1024)
#define N_STACK 49152
static value_t Stack[N_STACK];
static u_int32_t SP = 0;
#define PUSH(v) (Stack[SP++] = (v))
#define POP()   (Stack[--SP])
#define POPN(n) (SP-=(n))

value_t NIL, T, LAMBDA, MACRO, LABEL, QUOTE;

value_t read_sexpr(FILE *f);
void print(FILE *f, value_t v);
value_t eval_sexpr(value_t e, value_t *penv);
value_t load_file(char *fname);

// error utilities ------------------------------------------------------------

jmp_buf toplevel;

void lerror(char *format, ...)
{
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    longjmp(toplevel, 1);
}

void type_error(char *fname, char *expected, value_t got)
{
    fprintf(stderr, "%s: error: expected %s, got ", fname, expected);
    print(stderr, got); lerror("\n");
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
static u_int32_t heapsize = 64*1024;//bytes

void lisp_init(void)
{
    int i;

    fromspace = malloc(heapsize);
    tospace   = malloc(heapsize);
    curheap = fromspace;
    lim = curheap+heapsize-sizeof(cons_t);

    NIL = symbol("nil"); setc(NIL, NIL);
    T   = symbol("t");   setc(T,   T);
    LAMBDA = symbol("lambda");
    MACRO = symbol("macro");
    LABEL = symbol("label");
    QUOTE = symbol("quote");
    for (i=0; i < (int)N_BUILTINS; i++)
        setc(symbol(builtin_names[i]), builtin(i));
    setc(symbol("princ"), builtin(F_PRINT));
}

// conses ---------------------------------------------------------------------

void gc(void);

static value_t mk_cons(void)
{
    cons_t *c;

    if (curheap > lim)
        gc();
    c = (cons_t*)curheap;
    curheap += sizeof(cons_t);
    return tagptr(c, TAG_CONS);
}

static value_t cons_(value_t *pcar, value_t *pcdr)
{
    value_t c = mk_cons();
    car_(c) = *pcar; cdr_(c) = *pcdr;
    return c;
}

value_t *cons(value_t *pcar, value_t *pcdr)
{
    value_t c = mk_cons();
    car_(c) = *pcar; cdr_(c) = *pcdr;
    PUSH(c);
    return &Stack[SP-1];
}

// collector ------------------------------------------------------------------

static value_t relocate(value_t v)
{
    value_t a, d, nc;

    if (!iscons(v))
        return v;
    if (car_(v) == UNBOUND)
        return cdr_(v);
    nc = mk_cons();
    a = car_(v);   d = cdr_(v);
    car_(v) = UNBOUND; cdr_(v) = nc;
    car_(nc) = relocate(a);
    cdr_(nc) = relocate(d);
    return nc;
}

static void trace_globals(symbol_t *root)
{
    while (root != NULL) {
        root->binding = relocate(root->binding);
        trace_globals(root->left);
        root = root->right;
    }
}

void gc(void)
{
    static int grew = 0;
    unsigned char *temp;
    u_int32_t i;

    curheap = tospace;
    lim = curheap+heapsize-sizeof(cons_t);

    for (i=0; i < SP; i++)
        Stack[i] = relocate(Stack[i]);
    trace_globals(symtab);
#ifdef VERBOSEGC
    printf("gc found %d/%d live conses\n", (curheap-tospace)/8, heapsize/8);
#endif
    temp = tospace;
    tospace = fromspace;
    fromspace = temp;

    // if we're using > 80% of the space, resize tospace so we have
    // more space to fill next time. if we grew tospace last time,
    // grow the other half of the heap this time to catch up.
    if (grew || ((lim-curheap) < (int)(heapsize/5))) {
        temp = realloc(tospace, grew ? heapsize : heapsize*2);
        if (temp == NULL)
            lerror("out of memory\n");
        tospace = temp;
        if (!grew)
            heapsize*=2;
        grew = !grew;
    }
    if (curheap > lim)  // all data was live
        gc();
}

// read -----------------------------------------------------------------------

enum {
    TOK_NONE, TOK_OPEN, TOK_CLOSE, TOK_DOT, TOK_QUOTE, TOK_SYM, TOK_NUM
};

static int symchar(char c)
{
    static char *special = "()';\\|";
    return (!isspace(c) && !strchr(special, c));
}

static u_int32_t toktype = TOK_NONE;
static value_t tokval;
static char buf[256];

static char nextchar(FILE *f)
{
    char c;
    int ch;

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
static int read_token(FILE *f, char c)
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
        else if (!escaped && !symchar(c)) {
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
    else if (isdigit(c) || c=='-' || c=='+') {
        read_token(f, c);
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
        if (read_token(f, c)) {
            toktype = TOK_DOT;
        }
        else {
            toktype = TOK_SYM;
            tokval = symbol(buf);
        }
    }
    return toktype;
}

// build a list of conses. this is complicated by the fact that all conses
// can move whenever a new cons is allocated. we have to refer to every cons
// through a handle to a relocatable pointer (i.e. a pointer on the stack).
static void read_list(FILE *f, value_t *pval)
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
        if (iscons(*pc))
            cdr_(*pc) = c;
        else
            *pval = c;
        *pc = c;
        c = read_sexpr(f);  // must be on separate lines due to undefined
        car_(*pc) = c;      // evaluation order

        t = peek(f);
        if (t == TOK_DOT) {
            take();
            c = read_sexpr(f);
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

value_t read_sexpr(FILE *f)
{
    value_t v;

    switch (peek(f)) {
    case TOK_CLOSE:
        take();
        lerror("read: error: unexpected ')'\n");
    case TOK_DOT:
        take();
        lerror("read: error: unexpected '.'\n");
    case TOK_SYM:
    case TOK_NUM:
        take();
        return tokval;
    case TOK_QUOTE:
        take();
        v = read_sexpr(f);
        PUSH(v);
        v = cons_(&QUOTE, cons(&Stack[SP-1], &NIL));
        POPN(2);
        return v;
    case TOK_OPEN:
        take();
        PUSH(NIL);
        read_list(f, &Stack[SP-1]);
        return POP();
    }
    return NIL;
}

// print ----------------------------------------------------------------------

void print(FILE *f, value_t v)
{
    value_t cd;

    switch (tag(v)) {
    case TAG_NUM: fprintf(f, "%ld", numval(v)); break;
    case TAG_SYM: fprintf(f, "%s", ((symbol_t*)ptr(v))->name); break;
    case TAG_BUILTIN: fprintf(f, "#<builtin %s>",
                              builtin_names[intval(v)]); break;
    case TAG_CONS:
        fprintf(f, "(");
        while (1) {
            print(f, car_(v));
            cd = cdr_(v);
            if (!iscons(cd)) {
                if (cd != NIL) {
                    fprintf(f, " . ");
                    print(f, cd);
                }
                fprintf(f, ")");
                break;
            }
            fprintf(f, " ");
            v = cd;
        }
        break;
    }
}

// eval -----------------------------------------------------------------------

static inline void argcount(char *fname, int nargs, int c)
{
    if (nargs != c)
        lerror("%s: error: too %s arguments\n", fname, nargs<c ? "few":"many");
}

#define eval(e, env) ((tag(e)<0x2) ? (e) : eval_sexpr((e),env))
#define tail_eval(xpr, env) do { SP = saveSP;  \
    if (tag(xpr)<0x2) { return (xpr); } \
    else { e=(xpr); *penv=(env); goto eval_top; } } while (0)

value_t eval_sexpr(value_t e, value_t *penv)
{
    value_t f, v, bind, headsym, asym, labl=0, *pv, *argsyms, *body, *lenv;
    value_t *rest;
    cons_t *c;
    symbol_t *sym;
    u_int32_t saveSP;
    int i, nargs, noeval=0;
    number_t s, n;

 eval_top:
    if (issymbol(e)) {
        sym = (symbol_t*)ptr(e);
        if (sym->constant != UNBOUND) return sym->constant;
        v = *penv;
        while (iscons(v)) {
            bind = car_(v);
            if (iscons(bind) && car_(bind) == e)
                return cdr_(bind);
            v = cdr_(v);
        }
        if ((v = sym->binding) == UNBOUND)
            lerror("eval: error: variable %s has no value\n", sym->name);
        return v;
    }
    if ((unsigned long)(char*)&nargs < (unsigned long)stack_bottom || SP>=(N_STACK-100))
        lerror("eval: error: stack overflow\n");
    saveSP = SP;
    PUSH(e);
    PUSH(*penv);
    f = eval(car_(e), penv);
    *penv = Stack[saveSP+1];
    if (isbuiltin(f)) {
        // handle builtin function
        if (!isspecial(f)) {
            // evaluate argument list, placing arguments on stack
            v = Stack[saveSP] = cdr_(Stack[saveSP]);
            while (iscons(v)) {
                v = eval(car_(v), penv);
                *penv = Stack[saveSP+1];
                PUSH(v);
                v = Stack[saveSP] = cdr_(Stack[saveSP]);
            }
        }
    apply_builtin:
        nargs = SP - saveSP - 2;
        switch (intval(f)) {
        // special forms
        case F_QUOTE:
            v = cdr_(Stack[saveSP]);
            if (!iscons(v))
                lerror("quote: error: expected argument\n");
            v = car_(v);
            break;
        case F_MACRO:
        case F_LAMBDA:
            v = Stack[saveSP];
            if (*penv != NIL) {
                // build a closure (lambda args body . env)
                v = cdr_(v);
                PUSH(car(v));
                argsyms = &Stack[SP-1];
                PUSH(car(cdr_(v)));
                body = &Stack[SP-1];
                v = cons_(intval(f)==F_LAMBDA ? &LAMBDA : &MACRO,
                          cons(argsyms, cons(body, penv)));
            }
            break;
        case F_LABEL:
            v = Stack[saveSP];
            if (*penv != NIL) {
                v = cdr_(v);
                PUSH(car(v));        // name
                pv = &Stack[SP-1];
                PUSH(car(cdr_(v)));  // function
                body = &Stack[SP-1];
                *body = eval(*body, penv);  // evaluate lambda
                v = cons_(&LABEL, cons(pv, cons(body, &NIL)));
            }
            break;
        case F_IF:
            v = car(cdr_(Stack[saveSP]));
            if (eval(v, penv) != NIL)
                v = car(cdr_(cdr_(Stack[saveSP])));
            else
                v = car(cdr(cdr_(cdr_(Stack[saveSP]))));
            tail_eval(v, Stack[saveSP+1]);
            break;
        case F_COND:
            Stack[saveSP] = cdr_(Stack[saveSP]);
            pv = &Stack[saveSP]; v = NIL;
            while (iscons(*pv)) {
                c = tocons(car_(*pv), "cond");
                v = eval(c->car, penv);
                *penv = Stack[saveSP+1];
                if (v != NIL) {
                    *pv = cdr_(car_(*pv));
                    // evaluate body forms
                    if (iscons(*pv)) {
                        while (iscons(cdr_(*pv))) {
                            v = eval(car_(*pv), penv);
                            *penv = Stack[saveSP+1];
                            *pv = cdr_(*pv);
                        }
                        tail_eval(car_(*pv), *penv);
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
                    if ((v=eval(car_(*pv), penv)) == NIL) {
                        SP = saveSP; return NIL;
                    }
                    *penv = Stack[saveSP+1];
                    *pv = cdr_(*pv);
                }
                tail_eval(car_(*pv), *penv);
            }
            break;
        case F_OR:
            Stack[saveSP] = cdr_(Stack[saveSP]);
            pv = &Stack[saveSP]; v = NIL;
            if (iscons(*pv)) {
                while (iscons(cdr_(*pv))) {
                    if ((v=eval(car_(*pv), penv)) != NIL) {
                        SP = saveSP; return v;
                    }
                    *penv = Stack[saveSP+1];
                    *pv = cdr_(*pv);
                }
                tail_eval(car_(*pv), *penv);
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
            while (eval(*cond, penv) != NIL) {
                *penv = Stack[saveSP+1];
                *body = Stack[SP-2];
                while (iscons(*body)) {
                    *pv = eval(car_(*body), penv);
                    *penv = Stack[saveSP+1];
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
                    v = eval(car_(*pv), penv);
                    *penv = Stack[saveSP+1];
                    *pv = cdr_(*pv);
                }
                tail_eval(car_(*pv), *penv);
            }
            break;

        // ordinary functions
        case F_SET:
            argcount("set", nargs, 2);
            e = Stack[SP-2];
            v = *penv;
            while (iscons(v)) {
                bind = car_(v);
                if (iscons(bind) && car_(bind) == e) {
                    cdr_(bind) = (v=Stack[SP-1]);
                    SP=saveSP; return v;
                }
                v = cdr_(v);
            }
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
            for (i=saveSP+2; i < (int)SP; i++) {
                n = tonumber(Stack[i], "+");
                s += n;
            }
            v = number(s);
            break;
        case F_SUB:
            if (nargs < 1)
                lerror("-: error: too few arguments\n");
            i = saveSP+2;
            s = (nargs==1) ? 0 : tonumber(Stack[i++], "-");
            for (; i < (int)SP; i++) {
                n = tonumber(Stack[i], "-");
                s -= n;
            }
            v = number(s);
            break;
        case F_MUL:
            s = 1;
            for (i=saveSP+2; i < (int)SP; i++) {
                n = tonumber(Stack[i], "*");
                s *= n;
            }
            v = number(s);
            break;
        case F_DIV:
            if (nargs < 1)
                lerror("/: error: too few arguments\n");
            i = saveSP+2;
            s = (nargs==1) ? 1 : tonumber(Stack[i++], "/");
            for (; i < (int)SP; i++) {
                n = tonumber(Stack[i], "/");
                if (n == 0)
                    lerror("/: error: division by zero\n");
                s /= n;
            }
            v = number(s);
            break;
        case F_LT:
            argcount("<", nargs, 2);
            if (tonumber(Stack[SP-2],"<") < tonumber(Stack[SP-1],"<"))
                v = T;
            else
                v = NIL;
            break;
        case F_NOT:
            argcount("not", nargs, 1);
            v = ((Stack[SP-1] == NIL) ? T : NIL);
            break;
        case F_EVAL:
            argcount("eval", nargs, 1);
            v = Stack[SP-1];
            tail_eval(v, NIL);
            break;
        case F_PRINT:
            for (i=saveSP+2; i < (int)SP; i++)
                print(stdout, v=Stack[i]);
            break;
        case F_READ:
            argcount("read", nargs, 0);
            v = read_sexpr(stdin);
            break;
        case F_LOAD:
            argcount("load", nargs, 1);
            v = load_file(tosymbol(Stack[SP-1], "load")->name);
            break;
        case F_PROG1:
            // return first arg
            if (nargs < 1)
                lerror("prog1: error: too few arguments\n");
            v = Stack[saveSP+2];
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
        if (headsym == LABEL) {
            // (label name (lambda ...)) behaves the same as the lambda
            // alone, except with name bound to the whole label expression
            labl = f;
            f = car(cdr(cdr_(labl)));
            headsym = car(f);
        }
        // apply lambda or macro expression
        PUSH(cdr(cdr(cdr_(f))));
        lenv = &Stack[SP-1];
        PUSH(car_(cdr_(f)));
        argsyms = &Stack[SP-1];
        PUSH(car_(cdr_(cdr_(f))));
        body = &Stack[SP-1];
        if (labl) {
            // add label binding to environment
            PUSH(labl);
            PUSH(car_(cdr_(labl)));
            *lenv = cons_(cons(&Stack[SP-1], &Stack[SP-2]), lenv);
            POPN(3);
            v = Stack[saveSP]; // refetch arglist
        }
        if (headsym == MACRO)
            noeval = 1;
        else if (headsym != LAMBDA)
            lerror("apply: error: head must be lambda, macro, or label\n");
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
            if (!issymbol(asym))
                lerror("apply: error: formal argument not a symbol\n");
            v = car_(v);
            if (!noeval) {
                v = eval(v, penv);
                *penv = Stack[saveSP+1];
            }
            PUSH(v);
            *lenv = cons_(cons(&asym, &Stack[SP-1]), lenv);
            POPN(2);
            *argsyms = cdr_(*argsyms);
            v = Stack[saveSP] = cdr_(Stack[saveSP]);
        }
        if (*argsyms != NIL) {
            if (issymbol(*argsyms)) {
                if (noeval) {
                    *lenv = cons_(cons(argsyms, &Stack[saveSP]), lenv);
                }
                else {
                    PUSH(NIL);
                    PUSH(NIL);
                    rest = &Stack[SP-1];
                    // build list of rest arguments
                    // we have to build it forwards, which is tricky
                    while (iscons(v)) {
                        v = eval(car_(v), penv);
                        *penv = Stack[saveSP+1];
                        PUSH(v);
                        v = cons_(&Stack[SP-1], &NIL);
                        POP();
                        if (iscons(*rest))
                            cdr_(*rest) = v;
                        else
                            Stack[SP-2] = v;
                        *rest = v;
                        v = Stack[saveSP] = cdr_(Stack[saveSP]);
                    }
                    *lenv = cons_(cons(argsyms, &Stack[SP-2]), lenv);
                }
            }
            else if (iscons(*argsyms)) {
                lerror("apply: error: too few arguments\n");
            }
        }
        noeval = 0;
        // macro: evaluate expansion in the calling environment
        if (headsym == MACRO) {
            SP = saveSP;
            PUSH(*lenv);
            lenv = &Stack[SP-1];
            v = eval(*body, lenv);
            tail_eval(v, *penv);
        }
        else {
            tail_eval(*body, *lenv);
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
    v = eval(expr, &Stack[SP-1]);
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
    printf("Welcome to femtoLisp ----------------------------------------------------------\n");
 repl:
    while (1) {
        printf("> ");
        v = read_sexpr(stdin);
        if (feof(stdin)) break;
        print(stdout, v=toplevel_eval(v));
        set(symbol("that"), v);
        printf("\n\n");
    }
    return 0;
}
