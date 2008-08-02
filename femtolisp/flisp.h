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
    value_t binding;   // global value binding
    value_t syntax;    // syntax environment entry
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
#define TAG_BUILTIN  0x1
#define TAG_SYM      0x2
#define TAG_CONS     0x3
#define UNBOUND      ((value_t)TAG_SYM) // an invalid symbol pointer
#define TAG_CONST    ((value_t)-2)  // in sym->syntax for constants
#define tag(x) ((x)&0x3)
#define ptr(x) ((void*)((x)&(~(value_t)0x3)))
#define tagptr(p,t) (((value_t)(p)) | (t))
#define fixnum(x) ((value_t)((x)<<2))
#define numval(x)  (((fixnum_t)(x))>>2)
#define fits_fixnum(x) (((x)>>29) == 0 || (~((x)>>29)) == 0)
#define fits_bits(x,b) (((x)>>(b-1)) == 0 || (~((x)>>(b-1))) == 0)
#define uintval(x)  (((unsigned int)(x))>>2)
#define builtin(n) tagptr((((int)n)<<2), TAG_BUILTIN)
#define iscons(x)    (tag(x) == TAG_CONS)
#define issymbol(x)  (tag(x) == TAG_SYM)
#define isfixnum(x)  (tag(x) == TAG_NUM)
#define bothfixnums(x,y) (tag((x)|(y)) == TAG_NUM)
#define isbuiltin(x) ((tag(x) == TAG_BUILTIN) && uintval(x) < N_BUILTINS)
#define isvectorish(x) ((tag(x) == TAG_BUILTIN) && uintval(x) > N_BUILTINS)
#define isvector(x) (isvectorish(x) && !(((value_t*)ptr(x))[0] & 0x2))
#define iscvalue(x) (isvectorish(x) && (((value_t*)ptr(x))[0] & 0x2))
#define selfevaluating(x) (tag(x)<0x2)
// comparable with ==
#define eq_comparable(a,b) (!(((a)|(b))&0x1))
// distinguish a vector from a cvalue
#define discriminateAsVector(x) (!(((value_t*)ptr(x))[0] & 0x2))
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

extern value_t Stack[];
extern u_int32_t SP;
#define PUSH(v) (Stack[SP++] = (v))
#define POP()   (Stack[--SP])
#define POPN(n) (SP-=(n))

enum {
    // special forms
    F_QUOTE=0, F_COND, F_IF, F_AND, F_OR, F_WHILE, F_LAMBDA,
    F_TRYCATCH, F_SPECIAL_APPLY, F_PROGN,
    // functions
    F_EQ, F_ATOM, F_NOT, F_SYMBOLP, F_NUMBERP, F_BOUNDP, F_CONSP,
    F_BUILTINP, F_VECTORP, F_FIXNUMP, F_EQUAL,
    F_CONS, F_CAR, F_CDR, F_RPLACA, F_RPLACD,
    F_EVAL, F_APPLY, F_SET, F_PROG1, F_RAISE,
    F_ADD, F_SUB, F_MUL, F_DIV, F_LT, F_BNOT, F_BAND, F_BOR, F_BXOR,
    F_VECTOR, F_AREF, F_ASET, F_LENGTH, F_ASSOC, F_COMPARE, F_FOR,
    N_BUILTINS
};
#define isspecial(v) (uintval(v) <= (unsigned int)F_PROGN)

extern value_t NIL, T;

/* read, eval, print main entry points */
value_t read_sexpr(FILE *f);
void print(FILE *f, value_t v, int princ);
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
value_t compare(value_t a, value_t b);

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

/* c interface */
#define INL_SIZE_NBITS 16
typedef struct {
    unsigned two:2;
    unsigned moved:1;
    unsigned numtype:4;
    unsigned inllen:INL_SIZE_NBITS;
    unsigned cstring:1;
    unsigned unused:4;
    unsigned prim:1;
    unsigned inlined:1;
    unsigned islispfunction:1;
    unsigned autorelease:1;
#ifdef BITS64
    unsigned pad:32;
#endif
} cvflags_t;

// initial flags have two==0x2 (type tag) and numtype==0xf
#ifdef BITFIELD_BIG_ENDIAN
# ifdef BITS64
#  define INITIAL_FLAGS 0x9e00000000000000UL
# else
#  define INITIAL_FLAGS 0x9e000000
# endif
#else
# ifdef BITS64
#  define INITIAL_FLAGS 0x000000000000007aUL
# else
#  define INITIAL_FLAGS 0x0000007a
# endif
#endif

typedef struct {
    union {
        cvflags_t flags;
        unsigned long flagbits;
    };
    value_t type;
    value_t deps;
    // fields below are absent in inline-allocated values
    void *data;
    size_t len;      // length of *data in bytes
    //cvtable_t *vtable;
} cvalue_t;

typedef struct {
    union {
        cvflags_t flags;
        unsigned long flagbits;
    };
    value_t type;
    void *data;
} cprim_t;

#define cv_len(c)  ((c)->flags.inlined ? (c)->flags.inllen : (c)->len)
#define cv_type(c) ((c)->type)
#define cv_numtype(c) ((c)->flags.numtype)

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

typedef value_t (*guestfunc_t)(value_t*, u_int32_t);

extern value_t int8sym, uint8sym, int16sym, uint16sym, int32sym, uint32sym;
extern value_t int64sym, uint64sym, shortsym, ushortsym;
extern value_t intsym, uintsym, longsym, ulongsym, charsym, ucharsym, wcharsym;
extern value_t structsym, arraysym, enumsym, cfunctionsym, voidsym, pointersym;
extern value_t stringtypesym, wcstringtypesym, emptystringsym;
extern value_t unionsym, floatsym, doublesym, lispvaluesym;

value_t cvalue(value_t type, size_t sz);
size_t ctype_sizeof(value_t type, int *palign);
void *cvalue_data(value_t v);
void *cv_data(cvalue_t *cv);
value_t cvalue_copy(value_t v);
value_t cvalue_from_data(value_t type, void *data, size_t sz);
value_t cvalue_from_ref(value_t type, void *ptr, size_t sz, value_t parent);
value_t guestfunc(guestfunc_t f);
size_t cvalue_arraylen(value_t v);
value_t size_wrap(size_t sz);
size_t toulong(value_t n, char *fname);
value_t cvalue_string(size_t sz);
value_t cvalue_pinned_cstring(char *str);
int isstring(value_t v);
int isnumber(value_t v);
value_t cvalue_compare(value_t a, value_t b);
value_t cvalue_char(value_t *args, uint32_t nargs);

value_t mk_double(double_t n);
value_t mk_uint32(uint32_t n);
value_t mk_uint64(uint64_t n);
value_t return_from_uint64(uint64_t Uaccum);
value_t return_from_int64(int64_t Saccum);

#endif
