#ifndef _FLISP_H_
#define _FLISP_H_

typedef uptrint_t value_t;
typedef int_t fixnum_t;
#ifdef BITS64
#define T_FIXNUM T_INT64
#else
#define T_FIXNUM T_INT32
#endif

typedef struct {
    value_t car;
    value_t cdr;
} cons_t;

typedef struct _symbol_t {
    value_t syntax;    // syntax environment entry
    value_t binding;   // global value binding
    struct _fltype_t *type;
    uint32_t hash;
    void *dlcache;     // dlsym address
    // below fields are private
    struct _symbol_t *left;
    struct _symbol_t *right;
    union {
        char name[1];
        void *_pad;    // ensure field aligned to pointer size
    };
} symbol_t;

#define TAG_NUM      0x0
                   //0x1 unused
#define TAG_BUILTIN  0x2
#define TAG_VECTOR   0x3
#define TAG_NUM1     0x4
#define TAG_CVALUE   0x5
#define TAG_SYM      0x6
#define TAG_CONS     0x7
#define UNBOUND      ((value_t)0x1) // an invalid value
#define TAG_FWD      UNBOUND
#define TAG_CONST    ((value_t)-2)  // in sym->syntax for constants
#define tag(x) ((x)&0x7)
#define ptr(x) ((void*)((x)&(~(value_t)0x7)))
#define tagptr(p,t) (((value_t)(p)) | (t))
#define fixnum(x) ((value_t)((x)<<2))
#define numval(x)  (((fixnum_t)(x))>>2)
#ifdef BITS64
#define fits_fixnum(x) (((x)>>61) == 0 || (~((x)>>61)) == 0)
#else
#define fits_fixnum(x) (((x)>>29) == 0 || (~((x)>>29)) == 0)
#endif
#define fits_bits(x,b) (((x)>>(b-1)) == 0 || (~((x)>>(b-1))) == 0)
#define uintval(x)  (((unsigned int)(x))>>3)
#define builtin(n) tagptr((((int)n)<<3), TAG_BUILTIN)
#define iscons(x)    (tag(x) == TAG_CONS)
#define issymbol(x)  (tag(x) == TAG_SYM)
#define isfixnum(x)  (((x)&3) == TAG_NUM)
#define bothfixnums(x,y) ((((x)|(y))&3) == TAG_NUM)
#define isbuiltin(x) ((tag(x) == TAG_BUILTIN) && uintval(x) < N_BUILTINS)
#define isbuiltinish(x) (tag(x) == TAG_BUILTIN)
#define isvector(x) (tag(x) == TAG_VECTOR)
#define iscvalue(x) (tag(x) == TAG_CVALUE)
#define selfevaluating(x) (tag(x)<6)
// comparable with ==
#define eq_comparable(a,b) (!(((a)|(b))&1))
#define eq_comparablep(a) (!((a)&1))
// doesn't lead to other values
#define leafp(a) (((a)&3) != 3)

#define isforwarded(v) (((value_t*)ptr(v))[0] == TAG_FWD)
#define forwardloc(v)  (((value_t*)ptr(v))[1])
#define forward(v,to) do { (((value_t*)ptr(v))[0] = TAG_FWD); \
                           (((value_t*)ptr(v))[1] = to); } while (0)

#define vector_size(v) (((size_t*)ptr(v))[0]>>2)
#define vector_setsize(v,n) (((size_t*)ptr(v))[0] = ((n)<<2))
#define vector_elt(v,i) (((value_t*)ptr(v))[1+(i)])
#define vector_grow_amt(x) ((x)<8 ? 4 : 6*((x)>>3))
// functions ending in _ are unsafe, faster versions
#define car_(v) (((cons_t*)ptr(v))->car)
#define cdr_(v) (((cons_t*)ptr(v))->cdr)
#define car(v)  (tocons((v),"car")->car)
#define cdr(v)  (tocons((v),"cdr")->cdr)

#define set(s, v)  (((symbol_t*)ptr(s))->binding = (v))
#define setc(s, v) do { ((symbol_t*)ptr(s))->syntax = TAG_CONST; \
                        ((symbol_t*)ptr(s))->binding = (v); } while (0)
#define isconstant(s) (((symbol_t*)ptr(s))->syntax == TAG_CONST)
#define symbol_value(s) (((symbol_t*)ptr(s))->binding)
#define ismanaged(v) ((((unsigned char*)ptr(v)) >= fromspace) && \
                      (((unsigned char*)ptr(v)) < fromspace+heapsize))
#define isgensym(x)  (issymbol(x) && ismanaged(x))

extern value_t Stack[];
extern uint32_t SP;
#define PUSH(v) (Stack[SP++] = (v))
#define POP()   (Stack[--SP])
#define POPN(n) (SP-=(n))

enum {
    // special forms
    F_QUOTE=0, F_COND, F_IF, F_AND, F_OR, F_WHILE, F_LAMBDA,
    F_TRYCATCH, F_SPECIAL_APPLY, F_SETQ, F_PROGN,
    // functions
    F_EQ, F_ATOM, F_NOT, F_SYMBOLP, F_NUMBERP, F_BOUNDP, F_CONSP,
    F_BUILTINP, F_VECTORP, F_FIXNUMP, F_EQUAL,
    F_CONS, F_CAR, F_CDR, F_RPLACA, F_RPLACD,
    F_EVAL, F_EVALSTAR, F_APPLY, F_PROG1, F_RAISE,
    F_ADD, F_SUB, F_MUL, F_DIV, F_LT, F_BNOT, F_BAND, F_BOR, F_BXOR,
    F_VECTOR, F_AREF, F_ASET, F_LENGTH, F_ASSOC, F_COMPARE, F_FOR,
    N_BUILTINS
};
#define isspecial(v) (uintval(v) <= (unsigned int)F_PROGN)

extern value_t NIL, T;

/* read, eval, print main entry points */
value_t read_sexpr(ios_t *f);
void print(ios_t *f, value_t v, int princ);
value_t toplevel_eval(value_t expr);
value_t apply(value_t f, value_t l);
value_t load_file(char *fname);

/* object model manipulation */
value_t fl_cons(value_t a, value_t b);
value_t list2(value_t a, value_t b);
value_t listn(size_t n, ...);
value_t symbol(char *str);
value_t fl_gensym();
char *symbol_name(value_t v);
value_t alloc_vector(size_t n, int init);
size_t llength(value_t v);
value_t list_nth(value_t l, size_t n);
value_t compare(value_t a, value_t b);  // -1, 0, or 1
value_t equal(value_t a, value_t b);    // T or nil
int equal_lispvalue(value_t a, value_t b);
uptrint_t hash_lispvalue(value_t a);

/* safe casts */
cons_t *tocons(value_t v, char *fname);
symbol_t *tosymbol(value_t v, char *fname);
fixnum_t tofixnum(value_t v, char *fname);
char *tostring(value_t v, char *fname);

/* error handling */
void lerror(value_t e, char *format, ...) __attribute__ ((__noreturn__));
void raise(value_t e) __attribute__ ((__noreturn__));
void type_error(char *fname, char *expected, value_t got) __attribute__ ((__noreturn__));
void bounds_error(char *fname, value_t arr, value_t ind) __attribute__ ((__noreturn__));
extern value_t ArgError, IOError;
static inline void argcount(char *fname, int nargs, int c)
{
    if (nargs != c)
        lerror(ArgError,"%s: too %s arguments", fname, nargs<c ? "few":"many");
}

typedef struct {
    void (*print)(value_t self, ios_t *f, int princ);
    void (*relocate)(value_t oldv, value_t newv);
    void (*finalize)(value_t self);
    void (*print_traverse)(value_t self);
} cvtable_t;

/* functions needed to implement the value interface (cvtable_t) */
value_t relocate_lispvalue(value_t v);
void print_traverse(value_t v);
void fl_print_chr(char c, ios_t *f);
void fl_print_str(char *s, ios_t *f);
void fl_print_child(ios_t *f, value_t v, int princ);

typedef void (*cvinitfunc_t)(struct _fltype_t*, value_t, void*);

typedef struct _fltype_t {
    value_t type;
    numerictype_t numtype;
    size_t size;
    size_t elsz;
    cvtable_t *vtable;
    struct _fltype_t *eltype;  // for arrays
    struct _fltype_t *artype;  // (array this)
    int marked;
    cvinitfunc_t init;
} fltype_t;

typedef struct {
    fltype_t *type;
    void *data;
    size_t len;            // length of *data in bytes
    union {
        value_t parent;    // optional
        char _space[1];    // variable size
    };
} cvalue_t;

#define CVALUE_NWORDS 4

typedef struct {
    fltype_t *type;
    char _space[1];
} cprim_t;

#define CPRIM_NWORDS 2

#define CV_OWNED_BIT  0x1
#define CV_PARENT_BIT 0x2
#define owned(cv)      ((uptrint_t)(cv)->type & CV_OWNED_BIT)
#define hasparent(cv)  ((uptrint_t)(cv)->type & CV_PARENT_BIT)
#define isinlined(cv)  ((cv)->data == &(cv)->_space[0])
#define cv_class(cv)   ((fltype_t*)(((uptrint_t)(cv)->type)&~3))
#define cv_len(cv)     ((cv)->len)
#define cv_type(cv)    (cv_class(cv)->type)
#define cv_data(cv)    ((cv)->data)
#define cv_numtype(cv) (cv_class(cv)->numtype)
#define cv_isstr(cv)   (cv_class(cv)->eltype == chartype)

#define cvalue_data(v) cv_data((cvalue_t*)ptr(v))

#define valid_numtype(v) ((v) < N_NUMTYPES)

/* C type names corresponding to cvalues type names */
typedef unsigned long ulong;
typedef unsigned int  uint;
typedef unsigned char uchar;
typedef char char_t;
typedef long long_t;
typedef unsigned long ulong_t;
typedef double double_t;
typedef float float_t;

typedef value_t (*builtin_t)(value_t*, uint32_t);

extern value_t int8sym, uint8sym, int16sym, uint16sym, int32sym, uint32sym;
extern value_t int64sym, uint64sym;
extern value_t longsym, ulongsym, charsym, ucharsym, wcharsym;
extern value_t structsym, arraysym, enumsym, cfunctionsym, voidsym, pointersym;
extern value_t stringtypesym, wcstringtypesym, emptystringsym;
extern value_t unionsym, floatsym, doublesym, builtinsym;
extern fltype_t *chartype, *wchartype;
extern fltype_t *stringtype, *wcstringtype;
extern fltype_t *builtintype;

value_t cvalue(fltype_t *type, size_t sz);
size_t ctype_sizeof(value_t type, int *palign);
value_t cvalue_copy(value_t v);
value_t cvalue_from_data(fltype_t *type, void *data, size_t sz);
value_t cvalue_from_ref(fltype_t *type, void *ptr, size_t sz, value_t parent);
value_t cbuiltin(builtin_t f);
size_t cvalue_arraylen(value_t v);
value_t size_wrap(size_t sz);
size_t toulong(value_t n, char *fname);
value_t cvalue_string(size_t sz);
value_t cvalue_static_cstring(char *str);
value_t string_from_cstr(char *str);
int isstring(value_t v);
int isnumber(value_t v);
value_t cvalue_compare(value_t a, value_t b);

fltype_t *get_type(value_t t);
fltype_t *get_array_type(value_t eltype);
fltype_t *define_opaque_type(value_t sym, size_t sz, cvtable_t *vtab,
                             cvinitfunc_t init);

value_t mk_double(double_t n);
value_t mk_float(float_t n);
value_t mk_uint32(uint32_t n);
value_t mk_uint64(uint64_t n);
value_t return_from_uint64(uint64_t Uaccum);
value_t return_from_int64(int64_t Saccum);
value_t char_from_code(uint32_t code);

typedef struct {
    char *name;
    builtin_t fptr;
} builtinspec_t;

void assign_global_builtins(builtinspec_t *b);

/* builtins */
value_t fl_hash(value_t *args, u_int32_t nargs);
value_t cvalue_char(value_t *args, uint32_t nargs);
value_t cvalue_wchar(value_t *args, uint32_t nargs);

#endif
