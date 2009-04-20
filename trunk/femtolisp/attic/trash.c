value_t prim_types[32];
value_t *prim_sym_addrs[] = {
    &int8sym,  &uint8sym,  &int16sym, &uint16sym, &int32sym, &uint32sym,
    &int64sym, &uint64sym, &charsym,  &ucharsym,  &shortsym, &ushortsym,
    &intsym,   &uintsym,   &longsym,  &ulongsym,
    &lispvaluesym };
#define N_PRIMSYMS (sizeof(prim_sym_addrs) / sizeof(value_t*))

static value_t cv_type(cvalue_t *cv)
{
    if (cv->flags.prim) {
        return prim_types[cv->flags.primtype];
    }
    return cv->type;
}


    double t0,t1;
    int i;
    int32_t i32;
    char s8;
    ulong_t c8=3;
    t0 = clock();  //0.058125017
    set_secret_symtag(ulongsym,TAG_UINT32);
    set_secret_symtag(int8sym,TAG_INT8);
    for(i=0; i < 8000000; i++) {
        cnvt_to_int32(&i32, &s8, int8sym);
        c8+=c8;
        s8+=s8;
    }
    t1 = clock();
    printf("%d. that took %.16f\n", i32, t1-t0);


#define int_converter(type)                                         \
static int cnvt_to_##type(type##_t *i, void *data, value_t type)    \
{                                                                   \
         if (type==int32sym)  *i = *(int32_t*)data;                 \
    else if (type==charsym)   *i = *(char*)data;                    \
    else if (type==ulongsym)  *i = *(ulong*)data;                   \
    else if (type==uint32sym) *i = *(uint32_t*)data;                \
    else if (type==int8sym)   *i = *(int8_t*)data;                  \
    else if (type==uint8sym)  *i = *(uint8_t*)data;                 \
    else if (type==int64sym)  *i = *(int64_t*)data;                 \
    else if (type==uint64sym) *i = *(uint64_t*)data;                \
    else if (type==wcharsym)  *i = *(wchar_t*)data;                 \
    else if (type==longsym)   *i = *(long*)data;                    \
    else if (type==int16sym)  *i = *(int16_t*)data;                 \
    else if (type==uint16sym) *i = *(uint16_t*)data;                \
    else                                                            \
        return 1;                                                   \
    return 0;                                                       \
}
int_converter(int32)
int_converter(uint32)
int_converter(int64)
int_converter(uint64)

#ifdef BITS64
#define cnvt_to_ulong(i,d,t) cnvt_to_uint64(i,d,t)
#else
#define cnvt_to_ulong(i,d,t) cnvt_to_uint32(i,d,t)
#endif

long intabs(long n)
{
    long s = n>>(NBITS-1);   // either -1 or 0
    return (n^s) - s;
}

value_t fl_inv(value_t b)
{
    int_t bi;
    int tb;
    void *bptr=NULL;
    cvalue_t *cv;

    if (isfixnum(b)) {
        bi = numval(b);
        if (bi == 0)
            goto inv_error;
        else if (bi == 1)
            return fixnum(1);
        else if (bi == -1)
            return fixnum(-1);
        return fixnum(0);
    }
    else if (iscvalue(b)) {
        cv = (cvalue_t*)ptr(b);
        tb = cv_numtype(cv);
        if (tb <= T_DOUBLE)
            bptr = cv_data(cv);
    }
    if (bptr == NULL)
        type_error("/", "number", b);

    if (tb == T_FLOAT)
        return mk_double(1.0/(double)*(float*)bptr);
    if (tb == T_DOUBLE)
        return mk_double(1.0 / *(double*)bptr);

    if (tb == T_UINT64) {
        if (*(uint64_t*)bptr > 1)
            return fixnum(0);
        else if (*(uint64_t*)bptr == 1)
            return fixnum(1);
        goto inv_error;
    }
    int64_t b64  = conv_to_int64(bptr, tb);
    if (b64 == 0) goto inv_error;
    else if (b64 == 1) return fixnum(1);
    else if (b64 == -1) return fixnum(-1);

    return fixnum(0);
 inv_error:
    lerror(DivideError, "/: division by zero");
}

static void printstack(value_t *penv, uint32_t envsz)
{
    int i;
    printf("env=%d, size=%d\n", penv - &Stack[0], envsz);
    for(i=0; i < SP; i++) {
        printf("%d: ", i);
        print(stdout, Stack[i], 0);
        printf("\n");
    }
    printf("\n");
}

// unordered comparison
// not any faster than ordered comparison

// a is a fixnum, b is a cvalue
static value_t equal_num_cvalue(value_t a, value_t b)
{
    cvalue_t *bcv = (cvalue_t*)ptr(b);
    numerictype_t bt;
    if (valid_numtype(bt=cv_numtype(bcv))) {
        fixnum_t ia = numval(a);
        void *bptr = cv_data(bcv);
        if (cmp_eq(&ia, T_FIXNUM, bptr, bt))
            return fixnum(0);
    }
    return fixnum(1);
}

static value_t bounded_equal(value_t a, value_t b, int bound);
static value_t cyc_equal(value_t a, value_t b, ptrhash_t *table);

static value_t bounded_vector_equal(value_t a, value_t b, int bound)
{
    size_t la = vector_size(a);
    size_t lb = vector_size(b);
    if (la != lb) return fixnum(1);
    size_t i;
    for (i = 0; i < la; i++) {
        value_t d = bounded_equal(vector_elt(a,i), vector_elt(b,i), bound-1);
        if (d==NIL || numval(d)!=0) return d;
    }
    return fixnum(0);
}

static value_t bounded_equal(value_t a, value_t b, int bound)
{
    value_t d;

 compare_top:
    if (a == b) return fixnum(0);
    if (bound <= 0)
        return NIL;
    int taga = tag(a);
    int tagb = cmptag(b);
    switch (taga) {
    case TAG_NUM :
    case TAG_NUM1:
        if (isfixnum(b)) {
            return fixnum(1);
        }
        if (iscvalue(b)) {
            return equal_num_cvalue(a, b);
        }
        return fixnum(1);
    case TAG_SYM:
        return fixnum(1);
    case TAG_VECTOR:
        if (isvector(b))
            return bounded_vector_equal(a, b, bound);
        break;
    case TAG_CVALUE:
        if (iscvalue(b)) {
            cvalue_t *acv=(cvalue_t*)ptr(a), *bcv=(cvalue_t*)ptr(b);
            numerictype_t at, bt;
            if (valid_numtype(at=cv_numtype(acv)) &&
                valid_numtype(bt=cv_numtype(bcv))) {
                void *aptr = cv_data(acv);
                void *bptr = cv_data(bcv);
                if (cmp_eq(aptr, at, bptr, bt))
                    return fixnum(0);
                return fixnum(1);
            }
            return cvalue_compare(a, b);
        }
        else if (isfixnum(b)) {
            return equal_num_cvalue(b, a);
        }
        break;
    case TAG_BUILTIN:
        return fixnum(1);
    case TAG_CONS:
        if (tagb != TAG_CONS) return fixnum(1);
        d = bounded_equal(car_(a), car_(b), bound-1);
        if (d==NIL || numval(d) != 0) return d;
        a = cdr_(a); b = cdr_(b);
        bound--;
        goto compare_top;
    }
    return fixnum(1);
}

static value_t cyc_vector_equal(value_t a, value_t b, ptrhash_t *table)
{
    size_t la = vector_size(a);
    size_t lb = vector_size(b);
    size_t i;
    value_t d, xa, xb, ca, cb;
    if (la != lb) return fixnum(1);

    // first try to prove them different with no recursion
    for (i = 0; i < la; i++) {
        xa = vector_elt(a,i);
        xb = vector_elt(b,i);
        if (leafp(xa) || leafp(xb)) {
            d = bounded_equal(xa, xb, 1);
            if (numval(d)!=0) return d;
        }
        else if (cmptag(xa) != cmptag(xb)) {
            return fixnum(1);
        }
    }

    ca = eq_class(table, a);
    cb = eq_class(table, b);
    if (ca!=NIL && ca==cb)
        return fixnum(0);

    eq_union(table, a, b, ca, cb);

    for (i = 0; i < la; i++) {
        xa = vector_elt(a,i);
        xb = vector_elt(b,i);
        if (!leafp(xa) && !leafp(xb)) {
            d = cyc_equal(xa, xb, table);
            if (numval(d)!=0) return d;
        }
    }

    return fixnum(0);
}

static value_t cyc_equal(value_t a, value_t b, ptrhash_t *table)
{
    if (a==b)
        return fixnum(0);
    if (iscons(a)) {
        if (iscons(b)) {
            value_t aa = car_(a); value_t da = cdr_(a);
            value_t ab = car_(b); value_t db = cdr_(b);
            int tagaa = cmptag(aa); int tagda = cmptag(da);
            int tagab = cmptag(ab); int tagdb = cmptag(db);
            value_t d, ca, cb;
            if (leafp(aa) || leafp(ab)) {
                d = bounded_equal(aa, ab, 1);
                if (numval(d)!=0) return d;
            }
            else if (tagaa != tagab)
                return fixnum(1);
            if (leafp(da) || leafp(db)) {
                d = bounded_equal(da, db, 1);
                if (numval(d)!=0) return d;
            }
            else if (tagda != tagdb)
                return fixnum(1);

            ca = eq_class(table, a);
            cb = eq_class(table, b);
            if (ca!=NIL && ca==cb)
                return fixnum(0);

            eq_union(table, a, b, ca, cb);
            d = cyc_equal(aa, ab, table);
            if (numval(d)!=0) return d;
            return cyc_equal(da, db, table);
        }
        else {
            return fixnum(1);
        }
    }
    else if (isvector(a) && isvector(b)) {
        return cyc_vector_equal(a, b, table);
    }
    return bounded_equal(a, b, 1);
}
