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
