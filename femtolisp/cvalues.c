#define MAX_INL_SIZE 96
#ifdef BITS64
#define NWORDS(sz) (((sz)+7)>>3)
#else
#define NWORDS(sz) (((sz)+3)>>2)
#endif

static int struct_aligns[8] = {
    sizeof(struct { char a; int8_t i; }),
    sizeof(struct { char a; int16_t i; }),
    sizeof(struct { char a; char i[3]; }),
    sizeof(struct { char a; int32_t i; }),
    sizeof(struct { char a; char i[5]; }),
    sizeof(struct { char a; char i[6]; }),
    sizeof(struct { char a; char i[7]; }),
    sizeof(struct { char a; int64_t i; }) };
static int ALIGN2, ALIGN4, ALIGN8, ALIGNPTR;

typedef void (*cvinitfunc_t)(value_t, value_t, void*, void*);

value_t int8sym, uint8sym, int16sym, uint16sym, int32sym, uint32sym;
value_t int64sym, uint64sym;
value_t longsym, ulongsym, charsym, wcharsym;
value_t floatsym, doublesym;
value_t gftypesym, lispvaluesym, stringtypesym, wcstringtypesym;
value_t emptystringsym;

value_t structsym, arraysym, enumsym, cfunctionsym, voidsym, pointersym;
value_t unionsym;

value_t autoreleasesym, typeofsym, sizeofsym;

static void cvalue_init(value_t type, value_t v, void *dest);

void cvalue_print(ios_t *f, value_t v, int princ);
// exported guest functions
value_t cvalue_new(value_t *args, u_int32_t nargs);
value_t cvalue_sizeof(value_t *args, u_int32_t nargs);
value_t cvalue_typeof(value_t *args, u_int32_t nargs);

// compute the size of the metadata object for a cvalue
static size_t cv_nwords(cvalue_t *cv)
{
    if (cv->flags.prim) {
        if (cv->flags.inlined)
            return 2 + NWORDS(cv->flags.inllen);
        return 3;
    }
    if (cv->flags.inlined) {
        size_t s = 3 + NWORDS(cv->flags.inllen + cv->flags.cstring);
        return (s < 5) ? 5 : s;
    }
    return 5;
}

void *cv_data(cvalue_t *cv)
{
    if (cv->flags.prim) {
        if (cv->flags.inlined) {
            return &((cprim_t*)cv)->data;
        }
        return ((cprim_t*)cv)->data;
    }
    else if (cv->flags.inlined) {
        return &cv->data;
    }
    return cv->data;
}

void *cvalue_data(value_t v)
{
    return cv_data((cvalue_t*)ptr(v));
}

static void autorelease(cvalue_t *cv)
{
    cv->flags.autorelease = 1;
    // TODO: add to finalizer list
}

value_t cvalue(value_t type, size_t sz)
{
    cvalue_t *pcv;

    if (issymbol(type)) {
        cprim_t *pcp;
        pcp = (cprim_t*)alloc_words(2 + NWORDS(sz));
        pcp->flagbits = INITIAL_FLAGS;
        pcp->flags.inllen = sz;
        pcp->flags.inlined = 1;
        pcp->flags.prim = 1;
        pcp->type = type;
        return tagptr(pcp, TAG_CVALUE);
    }
    PUSH(type);
    if (sz <= MAX_INL_SIZE) {
        size_t nw = 3 + NWORDS(sz);
        pcv = (cvalue_t*)alloc_words((nw < 5) ? 5 : nw);
        pcv->flagbits = INITIAL_FLAGS;
        pcv->flags.inllen = sz;
        pcv->flags.inlined = 1;
    }
    else {
        pcv = (cvalue_t*)alloc_words(5);
        pcv->flagbits = INITIAL_FLAGS;
        pcv->flags.inlined = 0;
        pcv->data = malloc(sz);
        pcv->len = sz;
        autorelease(pcv);
    }
    pcv->deps = NIL;
    pcv->type = POP();
    return tagptr(pcv, TAG_CVALUE);
}

value_t cvalue_from_data(value_t type, void *data, size_t sz)
{
    cvalue_t *pcv;
    value_t cv;
    cv = cvalue(type, sz);
    pcv = (cvalue_t*)ptr(cv);
    memcpy(cv_data(pcv), data, sz);
    return cv;
}

// this effectively dereferences a pointer
// just like *p in C, it only removes a level of indirection from the type,
// it doesn't copy any data.
// this method of creating a cvalue only allocates metadata.
// ptr is user-managed; we don't autorelease it unless the
// user explicitly calls (autorelease ) on the result of this function.
// 'parent' is an optional cvalue that this pointer is known to point
// into; NIL if none.
value_t cvalue_from_ref(value_t type, void *ptr, size_t sz, value_t parent)
{
    cvalue_t *pcv;
    value_t cv;

    PUSH(parent);
    PUSH(type);
    pcv = (cvalue_t*)alloc_words(5);
    pcv->flagbits = INITIAL_FLAGS;
    pcv->flags.inlined = 0;
    pcv->data = ptr;
    pcv->len = sz;
    pcv->deps = NIL;
    pcv->type = POP();
    parent = POP();
    if (parent != NIL) {
        // TODO: add dependency
    }
    cv = tagptr(pcv, TAG_CVALUE);
    return cv;
}

value_t cvalue_string(size_t sz)
{
    value_t cv;
    char *data;
    cvalue_t *pcv;

    if (sz == 0)
        return symbol_value(emptystringsym);
    // secretly allocate space for 1 more byte, hide a NUL there so
    // any string will always be NUL terminated.
    cv = cvalue(symbol_value(stringtypesym), sz+1);
    pcv = (cvalue_t*)ptr(cv);
    data = cv_data(pcv);
    data[sz] = '\0';
    if (pcv->flags.inlined)
        pcv->flags.inllen = sz;
    else
        pcv->len = sz;
    pcv->flags.cstring = 1;
    return cv;
}

value_t cvalue_pinned_cstring(char *str)
{
    value_t v = cvalue_from_ref(symbol_value(stringtypesym), str, strlen(str),
                                NIL);
    ((cvalue_t*)ptr(v))->flags.cstring = 1;
    return v;
}

int isstring(value_t v)
{
    return (iscvalue(v) && ((cvalue_t*)ptr(v))->flags.cstring);
}

// convert to malloc representation (fixed address)
/*
static void cv_pin(cvalue_t *cv)
{
    if (!cv->flags.inlined)
        return;
    size_t sz = cv->flags.inllen;
    void *data = malloc(sz);
    cv->flags.inlined = 0;
    // TODO: handle flags.cstring
    if (cv->flags.prim) {
        memcpy(data, (void*)(&((cprim_t*)cv)->data), sz);
        ((cprim_t*)cv)->data = data;
    }
    else {
        memcpy(data, (void*)(&cv->data), sz);
        cv->data = data;
    }
    autorelease(cv);
}
*/

static int64_t strtoi64(char *str, char *fname)
{
    char *pend;
    int64_t i;
    errno = 0;
    i = strtoll(str, &pend, 0);
    if (*pend != '\0' || errno) lerror(ArgError, "%s: invalid string", fname);
    return i;
}
static uint64_t strtoui64(char *str, char *fname)
{
    char *pend;
    uint64_t i;
    errno = 0;
    i = strtoull(str, &pend, 0);
    if (*pend != '\0' || errno) lerror(ArgError, "%s: invalid string", fname);
    return i;
}
static double strtodouble(char *str, char *fname)
{
    char *pend;
    double d;
    errno = 0;
    d = strtod(str, &pend);
    if (*pend != '\0' || errno) lerror(ArgError, "%s: invalid string", fname);
    return d;
}

#define num_ctor(typenam, cnvt, tag, fromstr)                           \
static void cvalue_##typenam##_init(value_t type, value_t arg,          \
                                    void *dest, void *data)             \
{                                                                       \
    typenam##_t n=0;                                                    \
    (void)data; (void)type;                                             \
    if (isfixnum(arg)) {                                                \
        n = numval(arg);                                                \
    }                                                                   \
    else if (iscvalue(arg)) {                                           \
        cvalue_t *cv = (cvalue_t*)ptr(arg);                             \
        void *p = cv_data(cv);                                          \
        if (valid_numtype(cv_numtype(cv))) {                            \
            n = (typenam##_t)conv_to_##cnvt(p, cv_numtype(cv));         \
        }                                                               \
        else if (cv->flags.cstring) {                                   \
            n = fromstr(p, #typenam);                                   \
        }                                                               \
        else if (cv_len(cv) == sizeof(typenam##_t)) {                   \
            n = *(typenam##_t*)p;                                       \
        }                                                               \
        else {                                                          \
            goto cnvt_error;                                            \
        }                                                               \
    }                                                                   \
    else {                                                              \
        goto cnvt_error;                                                \
    }                                                                   \
    *((typenam##_t*)dest) = n;                                          \
    return;                                                             \
 cnvt_error:                                                            \
    type_error(#typenam, "number", arg);                                \
}                                                                       \
value_t cvalue_##typenam(value_t *args, u_int32_t nargs)                \
{                                                                       \
    if (nargs==0) { PUSH(fixnum(0)); args = &Stack[SP-1]; }             \
    value_t cv = cvalue(typenam##sym, sizeof(typenam##_t));             \
    ((cprim_t*)ptr(cv))->flags.numtype = tag;                           \
    cvalue_##typenam##_init(typenam##sym,                               \
                            args[0], &((cprim_t*)ptr(cv))->data, 0);    \
    return cv;                                                          \
}                                                                       \
value_t mk_##typenam(typenam##_t n)                                     \
{                                                                       \
    value_t cv = cvalue(typenam##sym, sizeof(typenam##_t));             \
    ((cprim_t*)ptr(cv))->flags.numtype = tag;                           \
    *(typenam##_t*)&((cprim_t*)ptr(cv))->data = n;                      \
    return cv;                                                          \
}

num_ctor(int8, int32, T_INT8, strtoi64)
num_ctor(uint8, uint32, T_UINT8, strtoui64)
num_ctor(int16, int32, T_INT16, strtoi64)
num_ctor(uint16, uint32, T_UINT16, strtoui64)
num_ctor(int32, int32, T_INT32, strtoi64)
num_ctor(uint32, uint32, T_UINT32, strtoui64)
num_ctor(int64, int64, T_INT64, strtoi64)
num_ctor(uint64, uint64, T_UINT64, strtoui64)
num_ctor(char, uint32, T_UINT8, strtoui64)
num_ctor(wchar, int32, T_INT32, strtoi64)
#ifdef BITS64
num_ctor(long, int64, T_INT64, strtoi64)
num_ctor(ulong, uint64, T_UINT64, strtoui64)
#else
num_ctor(long, int32, T_INT32, strtoi64)
num_ctor(ulong, uint32, T_UINT32, strtoui64)
#endif
num_ctor(float, double, T_FLOAT, strtodouble)
num_ctor(double, double, T_DOUBLE, strtodouble)

value_t size_wrap(size_t sz)
{
    if (fits_fixnum(sz))
        return fixnum(sz);
    assert(sizeof(void*) == sizeof(size_t));
    return mk_ulong(sz);
}

size_t toulong(value_t n, char *fname)
{
    if (isfixnum(n))
        return numval(n);
    if (iscvalue(n)) {
        cvalue_t *cv = (cvalue_t*)ptr(n);
        if (valid_numtype(cv_numtype(cv))) {
            return conv_to_ulong(cv_data(cv), cv_numtype(cv));
        }
    }
    type_error(fname, "number", n);
    return 0;
}

value_t char_from_code(uint32_t code)
{
    value_t ccode = fixnum(code);
    if (code > 0x7f)
        return cvalue_wchar(&ccode, 1);
    return cvalue_char(&ccode, 1);
}

static void cvalue_enum_init(value_t type, value_t arg, void *dest, void *data)
{
    int n=0;
    value_t syms;

    (void)data;
    syms = car(cdr(type));
    if (!iscons(syms))
        type_error("enum", "cons", syms);
    if (issymbol(arg)) {
        while (iscons(syms)) {
            if (car_(syms) == arg) {
                *(int*)dest = n;
                return;
            }
            n++;
            syms = cdr_(syms);
        }
        lerror(ArgError, "enum: invalid enum value");
    }
    if (isfixnum(arg)) {
        n = (int)numval(arg);
    }
    else if (iscvalue(arg)) {
        cvalue_t *cv = (cvalue_t*)ptr(arg);
        if (!valid_numtype(cv_numtype(cv)))
            type_error("enum", "number", arg);
        n = conv_to_int32(cv_data(cv), cv_numtype(cv));
    }
    if ((unsigned)n >= llength(syms))
        lerror(ArgError, "enum: value out of range");
    *(int*)dest = n;
}

value_t cvalue_enum(value_t *args, u_int32_t nargs)
{
    argcount("enum", nargs, 2);
    value_t cv = cvalue(list2(enumsym, args[0]), 4);
    ((cvalue_t*)ptr(cv))->flags.numtype = T_INT32;
    cvalue_enum_init(cv_type((cvalue_t*)ptr(cv)),
                     args[1], cv_data((cvalue_t*)ptr(cv)), NULL);
    return cv;
}

static void array_init_fromargs(char *dest, value_t *vals, size_t cnt,
                                value_t eltype, size_t elsize)
{
    size_t i;
    for(i=0; i < cnt; i++) {
        cvalue_init(eltype, vals[i], dest);
        dest += elsize;
    }
}

static int isarray(value_t v)
{
    if (!iscvalue(v)) return 0;
    value_t type = cv_type((cvalue_t*)ptr(v));
    return (iscons(type) && car_(type)==arraysym);
}

static size_t predict_arraylen(value_t arg)
{
    if (isvector(arg))
        return vector_size(arg);
    else if (iscons(arg))
        return llength(arg);
    else if (arg == NIL)
        return 0;
    if (isarray(arg))
        return cvalue_arraylen(arg);
    return 1;
}

static void cvalue_array_init(value_t type, value_t arg, void *dest, void *data)
{
    size_t elsize, i, cnt, sz;
    int junk;
    value_t eltype = car(cdr(type));

    if (data != 0)
        elsize = (size_t)data;  // already computed by constructor
    else
        elsize = ctype_sizeof(eltype, &junk);

    cnt = predict_arraylen(arg);

    if (iscons(cdr_(cdr_(type)))) {
        size_t tc = toulong(car_(cdr_(cdr_(type))), "array");
        if (tc != cnt)
            lerror(ArgError, "array: size mismatch");
    }

    sz = elsize * cnt;

    if (isvector(arg)) {
        array_init_fromargs((char*)dest, &vector_elt(arg,0), cnt,
                            eltype, elsize);
        return;
    }
    else if (iscons(arg) || arg==NIL) {
        i = 0;
        while (iscons(arg)) {
            if (SP >= N_STACK)
                break;
            PUSH(car_(arg));
            i++;
            arg = cdr_(arg);
        }
        if (i != cnt)
            lerror(ArgError, "array: size mismatch");
        array_init_fromargs((char*)dest, &Stack[SP-i], i, eltype, elsize);
        POPN(i);
        return;
    }
    else if (iscvalue(arg)) {
        cvalue_t *cv = (cvalue_t*)ptr(arg);
        if (isarray(arg)) {
            value_t aet = car(cdr(cv_type(cv)));
            if (aet == eltype) {
                if (cv_len(cv) == sz)
                    memcpy(dest, cv_data(cv), sz);
                else
                    lerror(ArgError, "array: size mismatch");
                return;
            }
            else {
                // TODO: initialize array from different type elements
                lerror(ArgError, "array: element type mismatch");
            }
        }
    }
    if (cnt == 1)
        cvalue_init(eltype, arg, dest);
    else
        type_error("array", "sequence", arg);
}

static value_t alloc_array(value_t type, size_t sz)
{
    value_t cv;
    if (car_(cdr_(type)) == charsym) {
        PUSH(type);
        cv = cvalue_string(sz);
        ((cvalue_t*)ptr(cv))->type = POP();
    }
    else {
        cv = cvalue(type, sz);
    }
    return cv;
}

value_t cvalue_array(value_t *args, u_int32_t nargs)
{
    size_t elsize, cnt, sz;
    int junk;

    if (nargs < 1)
        argcount("array", nargs, 1);

    cnt = nargs - 1;
    elsize = ctype_sizeof(args[0], &junk);
    sz = elsize * cnt;

    value_t cv = alloc_array(listn(3, arraysym, args[0], size_wrap(cnt)), sz);
    array_init_fromargs(cv_data((cvalue_t*)ptr(cv)), &args[1], cnt,
                        args[0], elsize);
    return cv;
}

// NOTE: v must be an array
size_t cvalue_arraylen(value_t v)
{
    cvalue_t *cv = (cvalue_t*)ptr(v);
    value_t type = cv_type(cv);

    if (iscons(cdr_(cdr_(type)))) {
        return toulong(car_(cdr_(cdr_(type))), "length");
    }
    // incomplete array type
    int junk;
    value_t eltype = car_(cdr_(type));
    size_t elsize = ctype_sizeof(eltype, &junk);
    return elsize ? cv_len(cv)/elsize : 0;
}

value_t cvalue_relocate(value_t v)
{
    size_t nw;
    cvalue_t *cv = (cvalue_t*)ptr(v);
    cvalue_t *nv;
    value_t ncv;

    if (cv->flags.moved)
        return cv->type;
    nw = cv_nwords(cv);
    if (!cv->flags.islispfunction) {
        nv = (cvalue_t*)alloc_words(nw);
        memcpy(nv, cv, nw*sizeof(value_t));
        ncv = tagptr(nv, TAG_CVALUE);
        cv->type = ncv;
        cv->flags.moved = 1;
    }
    else {
        // guestfunctions are permanent objects, unmanaged
        nv = cv;
        ncv = v;
    }
    nv->type = relocate(nv->type);
    return ncv;
}

size_t cvalue_struct_offs(value_t type, value_t field, int computeTotal,
                          int *palign)
{
    value_t fld = car(cdr_(type));
    size_t fsz, ssz = 0;
    int al;
    *palign = 0;

    while (iscons(fld)) {
        fsz = ctype_sizeof(car(cdr(car_(fld))), &al);

        ssz = ALIGN(ssz, al);
        if (al > *palign)
            *palign = al;

        if (!computeTotal && field==car_(car_(fld))) {
            // found target field
            return ssz;
        }

        ssz += fsz;
        fld = cdr_(fld);
    }
    return ALIGN(ssz, *palign);
}

static size_t cvalue_union_size(value_t type, int *palign)
{
    value_t fld = car(cdr_(type));
    size_t fsz, usz = 0;
    int al;
    *palign = 0;

    while (iscons(fld)) {
        fsz = ctype_sizeof(car(cdr(car_(fld))), &al);
        if (al > *palign) *palign = al;
        if (fsz > usz) usz = fsz;
        fld = cdr_(fld);
    }
    return ALIGN(usz, *palign);
}

// *palign is an output argument giving the alignment required by type
size_t ctype_sizeof(value_t type, int *palign)
{
    if (type == int8sym || type == uint8sym || type == charsym) {
        *palign = 1;
        return 1;
    }
    if (type == int16sym || type == uint16sym) {
        *palign = ALIGN2;
        return 2;
    }
    if (type == int32sym || type == uint32sym || type == wcharsym ||
        type == floatsym) {
        *palign = ALIGN4;
        return 4;
    }
    if (type == int64sym || type == uint64sym || type == doublesym) {
        *palign = ALIGN8;
        return 8;
    }
    if (type == longsym || type == ulongsym) {
#ifdef BITS64
        *palign = ALIGN8;
        return 8;
#else
        *palign = ALIGN4;
        return 4;
#endif
    }
    if (iscons(type)) {
        value_t hed = car_(type);
        if (hed == pointersym || hed == cfunctionsym || hed == lispvaluesym) {
            *palign = ALIGNPTR;
            return sizeof(void*);
        }
        if (hed == arraysym) {
            value_t t = car(cdr_(type));
            if (!iscons(cdr_(cdr_(type))))
                lerror(ArgError, "sizeof: incomplete type");
            value_t n = car_(cdr_(cdr_(type)));
            size_t sz = toulong(n, "sizeof");
            return sz * ctype_sizeof(t, palign);
        }
        else if (hed == structsym) {
            return cvalue_struct_offs(type, NIL, 1, palign);
        }
        else if (hed == unionsym) {
            return cvalue_union_size(type, palign);
        }
        else if (hed == enumsym) {
            *palign = ALIGN4;
            return 4;
        }
    }
    lerror(ArgError, "sizeof: invalid c type");
    return 0;
}

value_t cvalue_sizeof(value_t *args, u_int32_t nargs)
{
    cvalue_t *cv;
    argcount("sizeof", nargs, 1);
    if (iscvalue(args[0])) {
        cv = (cvalue_t*)ptr(args[0]);
        return size_wrap(cv_len(cv));
    }
    int a;
    return size_wrap(ctype_sizeof(args[0], &a));
}

value_t cvalue_typeof(value_t *args, u_int32_t nargs)
{
    argcount("typeof", nargs, 1);
    switch(tag(args[0])) {
    case TAG_CONS: return conssym;
    case TAG_NUM1:
    case TAG_NUM:  return fixnumsym;
    case TAG_SYM:  return symbolsym;
    case TAG_VECTOR: return vectorsym;
    case TAG_BUILTIN: return builtinsym;
    }
    return cv_type((cvalue_t*)ptr(args[0]));
}

value_t cvalue_copy(value_t v)
{
    assert(iscvalue(v));
    PUSH(v);
    cvalue_t *cv = (cvalue_t*)ptr(v);
    size_t nw = cv_nwords(cv);
    value_t *pnv = alloc_words(nw);
    v = POP(); cv = (cvalue_t*)ptr(v);
    memcpy(pnv, cv, nw * sizeof(value_t));
    if (!cv->flags.inlined) {
        size_t len = cv_len(cv);
        if (cv->flags.cstring) len++;
        void *data = malloc(len);
        memcpy(data, cv_data(cv), len);
        if (cv->flags.prim)
            ((cprim_t*)pnv)->data = data;
        else
            ((cvalue_t*)pnv)->data = data;
        autorelease((cvalue_t*)pnv);
    }

    return tagptr(pnv, TAG_CVALUE);
}

static void cvalue_init(value_t type, value_t v, void *dest)
{
    cvinitfunc_t f;

    if (issymbol(type)) {
        f = ((symbol_t*)ptr(type))->dlcache;
    }
    else if (iscons(type)) {
        value_t head = car_(type);
        f = ((symbol_t*)ptr(head))->dlcache;
    }
    else {
        f = NULL;
    }
    if (f == NULL)
        lerror(ArgError, "c-value: invalid c type");

    f(type, v, dest, NULL);
}

static numerictype_t sym_to_numtype(value_t type)
{
    if (type == int8sym)
        return T_INT8;
    else if (type == uint8sym || type == charsym)
        return T_UINT8;
    else if (type == int16sym)
        return T_INT16;
    else if (type == uint16sym)
        return T_UINT16;
#ifdef BITS64
    else if (type == int32sym || type == wcharsym)
#else
    else if (type == int32sym || type == wcharsym || type == longsym)
#endif
        return T_INT32;
#ifdef BITS64
    else if (type == uint32sym)
#else
    else if (type == uint32sym || type == ulongsym)
#endif
        return T_UINT32;
#ifdef BITS64
    else if (type == int64sym || type == longsym)
#else
    else if (type == int64sym)
#endif
        return T_INT64;
#ifdef BITS64
    else if (type == uint64sym || type == ulongsym)
#else
    else if (type == uint64sym)
#endif
        return T_UINT64;
    assert(false);
    return N_NUMTYPES;
}

// (new type . args)
// this provides (1) a way to allocate values with a shared type for
// efficiency, (2) a uniform interface for allocating cvalues of any
// type, including user-defined.
value_t cvalue_new(value_t *args, u_int32_t nargs)
{
    if (nargs < 1 || nargs > 2)
        argcount("c-value", nargs, 2);
    value_t type = args[0];
    value_t cv;
    if (iscons(type) && car_(type) == arraysym) {
        // special case to handle incomplete array types bla[]
        value_t eltype = car(cdr_(type));
        int junk;
        size_t elsz = ctype_sizeof(eltype, &junk);
        size_t cnt;
        if (iscons(cdr_(cdr_(type))))
            cnt = toulong(car_(cdr_(cdr_(type))), "array");
        else if (nargs == 2)
            cnt = predict_arraylen(args[1]);
        else
            cnt = 0;
        cv = alloc_array(type, elsz * cnt);
        if (nargs == 2)
            cvalue_array_init(type, args[1], cv_data((cvalue_t*)ptr(cv)),
                              (void*)elsz);
    }
    else {
        int junk;
        cv = cvalue(type, ctype_sizeof(type, &junk));
        if (issymbol(type)) {
            ((cvalue_t*)ptr(cv))->flags.numtype = sym_to_numtype(type);
        }
        if (nargs == 2)
            cvalue_init(type, args[1], cv_data((cvalue_t*)ptr(cv)));
    }
    return cv;
}

// NOTE: this only compares lexicographically; it ignores numeric formats
value_t cvalue_compare(value_t a, value_t b)
{
    cvalue_t *ca = (cvalue_t*)ptr(a);
    cvalue_t *cb = (cvalue_t*)ptr(b);
    char *adata = cv_data(ca);
    char *bdata = cv_data(cb);
    size_t asz = cv_len(ca);
    size_t bsz = cv_len(cb);
    size_t minsz = asz < bsz ? asz : bsz;
    int diff = memcmp(adata, bdata, minsz);
    if (diff == 0) {
        if (asz > bsz)
            return fixnum(1);
        else if (asz < bsz)
            return fixnum(-1);
    }
    return fixnum(diff);
}

static void check_addr_args(char *fname, size_t typesize, value_t *args,
                            void **data, ulong_t *index)
{
    size_t sz;
    if (!iscvalue(args[0]))
        type_error(fname, "cvalue", args[0]);
    *data = cv_data((cvalue_t*)ptr(args[0]));
    sz = cv_len((cvalue_t*)ptr(args[0]));
    cvalue_t *cv = (cvalue_t*)ptr(args[1]);
    if (isfixnum(args[1]))
        *index = numval(args[1]);
    else if (!iscvalue(args[1]) || !valid_numtype(cv_numtype(cv)))
        type_error(fname, "number", args[1]);
    else
        *index = conv_to_ulong(cv_data(cv), cv_numtype(cv));
    if (*index > sz - typesize)
        bounds_error(fname, args[0], args[1]);
}

value_t cvalue_get_int8(value_t *args, u_int32_t nargs)
{
    void *data; ulong_t index;
    argcount("get-int8", nargs, 2);
    check_addr_args("get-int8", sizeof(int8_t), args, &data, &index);
    return fixnum(((int8_t*)data)[index]);
}

value_t cvalue_set_int8(value_t *args, u_int32_t nargs)
{
    void *data; ulong_t index; int32_t val=0;
    argcount("set-int8", nargs, 3);
    check_addr_args("set-int8", sizeof(int8_t), args, &data, &index);
    cvalue_t *cv = (cvalue_t*)ptr(args[2]);
    if (isfixnum(args[2]))
        val = numval(args[2]);
    else if (!iscvalue(args[2]) || !valid_numtype(cv_numtype(cv)))
        type_error("set-int8", "number", args[2]);
    else
        val = conv_to_int32(cv_data(cv), cv_numtype(cv));
    ((int8_t*)data)[index] = val;
    return args[2];
}

value_t guestfunc(guestfunc_t f)
{
    value_t gf = cvalue(symbol_value(gftypesym), sizeof(void*));
    ((cvalue_t*)ptr(gf))->data = f;
    ((cvalue_t*)ptr(gf))->flags.islispfunction = 1;
    size_t nw = cv_nwords((cvalue_t*)ptr(gf));
    // directly-callable values are assumed not to move for
    // evaluator performance, so put guestfunction metadata on the
    // unmanaged heap
    cvalue_t *buf = malloc_aligned(nw * sizeof(value_t), 8);
    memcpy(buf, ptr(gf), nw*sizeof(value_t));
    return tagptr(buf, TAG_BUILTIN);
}

#define cv_intern(tok) tok##sym = symbol(#tok)
#define ctor_cv_intern(tok) cv_intern(tok); set(tok##sym, guestfunc(cvalue_##tok))
#define symbol_dlcache(s) (((symbol_t*)ptr(s))->dlcache)
#define cache_initfunc(tok) symbol_dlcache(tok##sym) = &cvalue_##tok##_init

void cvalues_init()
{
    int i;

    // compute struct field alignment required for primitives of sizes 1-8
    for(i=0; i < 8; i++)
        struct_aligns[i] -= (i+1);
    ALIGN2 = struct_aligns[1];
    ALIGN4 = struct_aligns[3];
    ALIGN8 = struct_aligns[7];
    ALIGNPTR = struct_aligns[sizeof(void*)-1];

    cv_intern(uint32);
    cv_intern(pointer);
    cfunctionsym = symbol("c-function");
    cv_intern(lispvalue);
    gftypesym = symbol("*guest-function-type*");
    setc(gftypesym, listn(3, cfunctionsym, lispvaluesym,
                          list2(list2(pointersym, lispvaluesym), uint32sym)));
    set(uint32sym, guestfunc(cvalue_uint32));

    ctor_cv_intern(int8);
    ctor_cv_intern(uint8);
    ctor_cv_intern(int16);
    ctor_cv_intern(uint16);
    ctor_cv_intern(int32);
    ctor_cv_intern(int64);
    ctor_cv_intern(uint64);
    ctor_cv_intern(char);
    ctor_cv_intern(wchar);
    ctor_cv_intern(long);
    ctor_cv_intern(ulong);
    ctor_cv_intern(float);
    ctor_cv_intern(double);

    ctor_cv_intern(array);
    ctor_cv_intern(enum);
    cv_intern(struct);
    cv_intern(union);
    cv_intern(void);
    set(symbol("c-value"), guestfunc(cvalue_new));
    set(symbol("get-int8"), guestfunc(cvalue_get_int8));
    set(symbol("set-int8"), guestfunc(cvalue_set_int8));

    cv_intern(autorelease);
    ctor_cv_intern(typeof);
    ctor_cv_intern(sizeof);

    // set up references to the init functions for each primitive type.
    // this is used for fast access in constructors for compound types
    // like arrays that need to initialize (but not allocate) elements.
    cache_initfunc(int8);
    cache_initfunc(uint8);
    cache_initfunc(int16);
    cache_initfunc(uint16);
    cache_initfunc(int32);
    cache_initfunc(uint32);
    cache_initfunc(int64);
    cache_initfunc(uint64);
    cache_initfunc(char);
    cache_initfunc(wchar);
    cache_initfunc(long);
    cache_initfunc(ulong);
    cache_initfunc(float);
    cache_initfunc(double);

    cache_initfunc(array);
    cache_initfunc(enum);

    stringtypesym = symbol("*string-type*");
    setc(stringtypesym, list2(arraysym, charsym));

    wcstringtypesym = symbol("*wcstring-type*");
    setc(wcstringtypesym, list2(arraysym, wcharsym));

    emptystringsym = symbol("*empty-string*");
    setc(emptystringsym, cvalue_pinned_cstring(""));
}

#define RETURN_NUM_AS(var, type) return(mk_##type((type##_t)var))

value_t return_from_uint64(uint64_t Uaccum)
{
    if (fits_fixnum(Uaccum)) {
        return fixnum((fixnum_t)Uaccum);
    }
    if (Uaccum > (uint64_t)S64_MAX) {
        RETURN_NUM_AS(Uaccum, uint64);
    }
    else if (Uaccum > (uint64_t)UINT_MAX) {
        RETURN_NUM_AS(Uaccum, int64);
    }
    else if (Uaccum > (uint64_t)INT_MAX) {
        RETURN_NUM_AS(Uaccum, uint32);
    }
    RETURN_NUM_AS(Uaccum, int32);
}

value_t return_from_int64(int64_t Saccum)
{
    if (fits_fixnum(Saccum)) {
        return fixnum((fixnum_t)Saccum);
    }
    if (Saccum > (int64_t)UINT_MAX || Saccum < (int64_t)INT_MIN) {
        RETURN_NUM_AS(Saccum, int64);
    }
    else if (Saccum > (int64_t)INT_MAX) {
        RETURN_NUM_AS(Saccum, uint32);
    }
    RETURN_NUM_AS(Saccum, int32);
}

value_t fl_add_any(value_t *args, u_int32_t nargs, fixnum_t carryIn)
{
    uint64_t Uaccum=0;
    int64_t Saccum = carryIn;
    double Faccum=0;
    uint32_t i;

    for(i=0; i < nargs; i++) {
        if (isfixnum(args[i])) {
            Saccum += numval(args[i]);
            continue;
        }
        else if (iscvalue(args[i])) {
            cvalue_t *cv = (cvalue_t*)ptr(args[i]);
            void *a = cv_data(cv);
            int64_t i64;
            switch(cv_numtype(cv)) {
            case T_INT8:   Saccum += *(int8_t*)a; break;
            case T_UINT8:  Saccum += *(uint8_t*)a; break;
            case T_INT16:  Saccum += *(int16_t*)a; break;
            case T_UINT16: Saccum += *(uint16_t*)a; break;
            case T_INT32:  Saccum += *(int32_t*)a; break;
            case T_UINT32: Saccum += *(uint32_t*)a; break;
            case T_INT64:
                i64 = *(int64_t*)a;
                if (i64 > 0)
                    Uaccum += (uint64_t)i64;
                else
                    Saccum += i64;
                break;
            case T_UINT64: Uaccum += *(uint64_t*)a; break;
            case T_FLOAT:  Faccum += *(float*)a; break;
            case T_DOUBLE: Faccum += *(double*)a; break;
            default:
                goto add_type_error;
            }
            continue;
        }
    add_type_error:
        type_error("+", "number", args[i]);
    }
    if (Faccum != 0) {
        Faccum += Uaccum;
        Faccum += Saccum;
        return mk_double(Faccum);
    }
    else if (Saccum < 0) {
        uint64_t negpart = (uint64_t)(-Saccum);
        if (negpart > Uaccum) {
            Saccum += (int64_t)Uaccum;
            // return value in Saccum
            if (Saccum >= INT_MIN) {
                if (fits_fixnum(Saccum)) {
                    return fixnum((fixnum_t)Saccum);
                }
                RETURN_NUM_AS(Saccum, int32);
            }
            RETURN_NUM_AS(Saccum, int64);
        }
        Uaccum -= negpart;
    }
    else {
        Uaccum += (uint64_t)Saccum;
    }
    // return value in Uaccum
    return return_from_uint64(Uaccum);
}

value_t fl_neg(value_t n)
{
    if (isfixnum(n)) {
        return fixnum(-numval(n));
    }
    else if (iscvalue(n)) {
        cvalue_t *cv = (cvalue_t*)ptr(n);
        void *a = cv_data(cv);
        uint32_t ui32;
        int32_t i32;
        int64_t i64;
        switch(cv_numtype(cv)) {
        case T_INT8:   return fixnum(-(int32_t)*(int8_t*)a);
        case T_UINT8:  return fixnum(-(int32_t)*(uint8_t*)a);
        case T_INT16:  return fixnum(-(int32_t)*(int16_t*)a);
        case T_UINT16: return fixnum(-(int32_t)*(uint16_t*)a);
        case T_INT32:
            i32 = *(int32_t*)a;
            if (i32 == (int32_t)BIT31)
                return mk_uint32((uint32_t)BIT31);
            return mk_int32(-i32);
        case T_UINT32:
            ui32 = *(uint32_t*)a;
            if (ui32 <= ((uint32_t)INT_MAX)+1) return mk_int32(-(int32_t)ui32);
            return mk_int64(-(int64_t)ui32);
        case T_INT64:
            i64 = *(int64_t*)a;
            if (i64 == (int64_t)BIT63)
                return mk_uint64((uint64_t)BIT63);
            return mk_int64(-i64);
        case T_UINT64: return mk_int64(-(int64_t)*(uint64_t*)a);
        case T_FLOAT:  return mk_float(-*(float*)a);
        case T_DOUBLE: return mk_double(-*(double*)a);
            break;
        }
    }
    type_error("-", "number", n);
}

value_t fl_mul_any(value_t *args, u_int32_t nargs, int64_t Saccum)
{
    uint64_t Uaccum=1;
    double Faccum=1;
    uint32_t i;

    for(i=0; i < nargs; i++) {
        if (isfixnum(args[i])) {
            Saccum *= numval(args[i]);
            continue;
        }
        else if (iscvalue(args[i])) {
            cvalue_t *cv = (cvalue_t*)ptr(args[i]);
            void *a = cv_data(cv);
            int64_t i64;
            switch(cv_numtype(cv)) {
            case T_INT8:   Saccum *= *(int8_t*)a; break;
            case T_UINT8:  Saccum *= *(uint8_t*)a; break;
            case T_INT16:  Saccum *= *(int16_t*)a; break;
            case T_UINT16: Saccum *= *(uint16_t*)a; break;
            case T_INT32:  Saccum *= *(int32_t*)a; break;
            case T_UINT32: Saccum *= *(uint32_t*)a; break;
            case T_INT64:
                i64 = *(int64_t*)a;
                if (i64 > 0)
                    Uaccum *= (uint64_t)i64;
                else
                    Saccum *= i64;
                break;
            case T_UINT64: Uaccum *= *(uint64_t*)a; break;
            case T_FLOAT:  Faccum *= *(float*)a; break;
            case T_DOUBLE: Faccum *= *(double*)a; break;
            default:
                goto mul_type_error;
            }
            continue;
        }
    mul_type_error:
        type_error("*", "number", args[i]);
    }
    if (Faccum != 1) {
        Faccum *= Uaccum;
        Faccum *= Saccum;
        return mk_double(Faccum);
    }
    else if (Saccum < 0) {
        Saccum *= (int64_t)Uaccum;
        if (Saccum >= INT_MIN) {
            if (fits_fixnum(Saccum)) {
                return fixnum((fixnum_t)Saccum);
            }
            RETURN_NUM_AS(Saccum, int32);
        }
        RETURN_NUM_AS(Saccum, int64);
    }
    else {
        Uaccum *= (uint64_t)Saccum;
    }
    return return_from_uint64(Uaccum);
}

value_t fl_div2(value_t a, value_t b)
{
    double da, db;
    int_t ai, bi;
    int ta, tb;
    void *aptr=NULL, *bptr=NULL;
    cvalue_t *cv;

    if (isfixnum(a)) {
        ai = numval(a);
        aptr = &ai;
        ta = T_FIXNUM;
    }
    else if (iscvalue(a)) {
        cv = (cvalue_t*)ptr(a);
        ta = cv_numtype(cv);
        if (ta <= T_DOUBLE)
            aptr = cv_data(cv);
    }
    if (aptr == NULL)
        type_error("/", "number", a);
    if (isfixnum(b)) {
        bi = numval(b);
        bptr = &bi;
        tb = T_FIXNUM;
    }
    else if (iscvalue(b)) {
        cv = (cvalue_t*)ptr(b);
        tb = cv_numtype(cv);
        if (tb <= T_DOUBLE)
            bptr = cv_data(cv);
    }
    if (bptr == NULL)
        type_error("/", "number", b);

    if (ta == T_FLOAT) {
        db = conv_to_double(bptr, tb);
        da = (double)*(float*)aptr / db;
        return mk_double(da);
    }
    if (ta == T_DOUBLE) {
        db = conv_to_double(bptr, tb);
        da = *(double*)aptr / db;
        return mk_double(da);
    }
    if (tb == T_FLOAT) {
        da = conv_to_double(aptr, ta);
        da /= (double)*(float*)bptr;
        return mk_double(da);
    }
    if (tb == T_DOUBLE) {
        da = conv_to_double(aptr, ta);
        da /= *(double*)bptr;
        return mk_double(da);
    }

    int64_t a64, b64;

    if (ta == T_UINT64) {
        if (tb == T_UINT64) {
            if (*(uint64_t*)bptr == 0) goto div_error;
            return return_from_uint64(*(uint64_t*)aptr / *(uint64_t*)bptr);
        }
        b64 = conv_to_int64(bptr, tb);
        if (b64 < 0) {
            return return_from_int64(-(int64_t)(*(uint64_t*)aptr /
                                                (uint64_t)(-b64)));
        }
        if (b64 == 0)
            goto div_error;
        return return_from_uint64(*(uint64_t*)aptr / (uint64_t)b64);
    }
    if (tb == T_UINT64) {
        if (*(uint64_t*)bptr == 0) goto div_error;
        a64 = conv_to_int64(aptr, ta);
        if (a64 < 0) {
            return return_from_int64(-((int64_t)((uint64_t)(-a64) /
                                                 *(uint64_t*)bptr)));
        }
        return return_from_uint64((uint64_t)a64 / *(uint64_t*)bptr);
    }

    b64 = conv_to_int64(bptr, tb);
    if (b64 == 0) goto div_error;

    return return_from_int64(conv_to_int64(aptr, ta) / b64);
 div_error:
    lerror(DivideError, "/: division by zero");
}

static void *int_data_ptr(value_t a, int *pnumtype, char *fname)
{
    cvalue_t *cv;
    if (iscvalue(a)) {
        cv = (cvalue_t*)ptr(a);
        *pnumtype = cv_numtype(cv);
        if (*pnumtype < T_FLOAT)
            return cv_data(cv);
    }
    type_error(fname, "integer", a);
    return NULL;
}

value_t fl_bitwise_not(value_t a)
{
    cvalue_t *cv;
    int ta;
    void *aptr;

    if (iscvalue(a)) {
        cv = (cvalue_t*)ptr(a);
        ta = cv_numtype(cv);
        aptr = cv_data(cv);
        switch (ta) {
        case T_INT8:   return mk_int8(~*(int8_t *)aptr);
        case T_UINT8:  return mk_uint8(~*(uint8_t *)aptr);
        case T_INT16:  return mk_int16(~*(int16_t *)aptr);
        case T_UINT16: return mk_uint16(~*(uint16_t*)aptr);
        case T_INT32:  return mk_int32(~*(int32_t *)aptr);
        case T_UINT32: return mk_uint32(~*(uint32_t*)aptr);
        case T_INT64:  return mk_int64(~*(int64_t *)aptr);
        case T_UINT64: return mk_uint64(~*(uint64_t*)aptr);
        }
    }
    type_error("~", "integer", a);
    return NIL;
}

#define BITSHIFT_OP(name, op)                                       \
value_t fl_##name(value_t a, int n)                                 \
{                                                                   \
    cvalue_t *cv;                                                   \
    int ta;                                                         \
    void *aptr;                                                     \
    if (iscvalue(a)) {                                              \
        cv = (cvalue_t*)ptr(a);                                     \
        ta = cv_numtype(cv);                                        \
        aptr = cv_data(cv);                                         \
        switch (ta) {                                               \
        case T_INT8:   return mk_int8((*(int8_t *)aptr) op n);      \
        case T_UINT8:  return mk_uint8((*(uint8_t *)aptr) op n);    \
        case T_INT16:  return mk_int16((*(int16_t *)aptr) op n);    \
        case T_UINT16: return mk_uint16((*(uint16_t*)aptr) op n);   \
        case T_INT32:  return mk_int32((*(int32_t *)aptr) op n);    \
        case T_UINT32: return mk_uint32((*(uint32_t*)aptr) op n);   \
        case T_INT64:  return mk_int64((*(int64_t *)aptr) op n);    \
        case T_UINT64: return mk_uint64((*(uint64_t*)aptr) op n);   \
        }                                                           \
    }                                                               \
    type_error(#op, "integer", a);                                  \
    return NIL;                                                     \
}
BITSHIFT_OP(shl,<<)
BITSHIFT_OP(shr,>>)

value_t fl_bitwise_op(value_t a, value_t b, int opcode, char *fname)
{
    int_t ai, bi;
    int ta, tb, itmp;
    void *aptr=NULL, *bptr=NULL, *ptmp;
    int64_t b64;

    if (isfixnum(a)) {
        ta = T_FIXNUM;
        ai = numval(a);
        aptr = &ai;
        bptr = int_data_ptr(b, &tb, fname);
    }
    else {
        aptr = int_data_ptr(a, &ta, fname);
        if (isfixnum(b)) {
            tb = T_FIXNUM;
            bi = numval(b);
            bptr = &bi;
        }
        else {
            bptr = int_data_ptr(b, &tb, fname);
        }
    }
    if (ta < tb) {
        itmp = ta; ta = tb; tb = itmp;
        ptmp = aptr; aptr = bptr; bptr = ptmp;
    }
    // now a's type is larger than or same as b's
    b64 = conv_to_int64(bptr, tb);
    switch (opcode) {
    case 0:
    switch (ta) {
    case T_INT8:   return mk_int8(  *(int8_t *)aptr  & (int8_t  )b64);
    case T_UINT8:  return mk_uint8( *(uint8_t *)aptr & (uint8_t )b64);
    case T_INT16:  return mk_int16( *(int16_t*)aptr  & (int16_t )b64);
    case T_UINT16: return mk_uint16(*(uint16_t*)aptr & (uint16_t)b64);
    case T_INT32:  return mk_int32( *(int32_t*)aptr  & (int32_t )b64);
    case T_UINT32: return mk_uint32(*(uint32_t*)aptr & (uint32_t)b64);
    case T_INT64:  return mk_int64( *(int64_t*)aptr  & (int64_t )b64);
    case T_UINT64: return mk_uint64(*(uint64_t*)aptr & (uint64_t)b64);
    }
    break;
    case 1:
    switch (ta) {
    case T_INT8:   return mk_int8(  *(int8_t *)aptr  | (int8_t  )b64);
    case T_UINT8:  return mk_uint8( *(uint8_t *)aptr | (uint8_t )b64);
    case T_INT16:  return mk_int16( *(int16_t*)aptr  | (int16_t )b64);
    case T_UINT16: return mk_uint16(*(uint16_t*)aptr | (uint16_t)b64);
    case T_INT32:  return mk_int32( *(int32_t*)aptr  | (int32_t )b64);
    case T_UINT32: return mk_uint32(*(uint32_t*)aptr | (uint32_t)b64);
    case T_INT64:  return mk_int64( *(int64_t*)aptr  | (int64_t )b64);
    case T_UINT64: return mk_uint64(*(uint64_t*)aptr | (uint64_t)b64);
    }
    break;
    case 2:
    switch (ta) {
    case T_INT8:   return mk_int8(  *(int8_t *)aptr  ^ (int8_t  )b64);
    case T_UINT8:  return mk_uint8( *(uint8_t *)aptr ^ (uint8_t )b64);
    case T_INT16:  return mk_int16( *(int16_t*)aptr  ^ (int16_t )b64);
    case T_UINT16: return mk_uint16(*(uint16_t*)aptr ^ (uint16_t)b64);
    case T_INT32:  return mk_int32( *(int32_t*)aptr  ^ (int32_t )b64);
    case T_UINT32: return mk_uint32(*(uint32_t*)aptr ^ (uint32_t)b64);
    case T_INT64:  return mk_int64( *(int64_t*)aptr  ^ (int64_t )b64);
    case T_UINT64: return mk_uint64(*(uint64_t*)aptr ^ (uint64_t)b64);
    }
    }
    assert(0);
    return NIL;
}
