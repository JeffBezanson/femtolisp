#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include "llt.h"
#include "flisp.h"

// is it a leaf? (i.e. does not lead to other values)
static inline int leafp(value_t a)
{
    return (!iscons(a) && !isvector(a));
}

static value_t eq_class(ptrhash_t *table, value_t key)
{
    value_t c = (value_t)ptrhash_get(table, (void*)key);
    if (c == (value_t)PH_NOTFOUND)
        return NIL;
    if (c == key)
        return c;
    return eq_class(table, c);
}

static void eq_union(ptrhash_t *table, value_t a, value_t b,
                     value_t c, value_t cb)
{
    value_t ca = (c==NIL ? a : c);
    if (cb != NIL)
        ptrhash_put(table, (void*)cb, (void*)ca);
    ptrhash_put(table, (void*)a, (void*)ca);
    ptrhash_put(table, (void*)b, (void*)ca);
}

// a is a fixnum, b is a cvalue
static int compare_num_cvalue(value_t a, value_t b)
{
    cvalue_t *bcv = (cvalue_t*)ptr(b);
    numerictype_t bt;
    if (valid_numtype(bt=cv_numtype(bcv))) {
        fixnum_t ia = numval(a);
        void *bptr = cv_data(bcv);
        if (cmp_eq(&ia, T_FIXNUM, bptr, bt))
            return 0;
        if (cmp_lt(&ia, T_FIXNUM, bptr, bt))
            return -1;
    }
    else {
        return -1;
    }
    return 1;
}

static value_t bounded_compare(value_t a, value_t b, int bound);
static value_t cyc_compare(value_t a, value_t b, ptrhash_t *table);

static value_t bounded_vector_compare(value_t a, value_t b, int bound)
{
    size_t la = vector_size(a);
    size_t lb = vector_size(b);
    size_t m, i;
    m = la < lb ? la : lb;
    for (i = 0; i < m; i++) {
        value_t d = bounded_compare(vector_elt(a,i), vector_elt(b,i), bound-1);
        if (d==NIL || numval(d)!=0) return d;
    }
    if (la < lb) return fixnum(-1);
    if (la > lb) return fixnum(1);
    return fixnum(0);
}

// strange comparisons are resolved arbitrarily but consistently.
// ordering: number < builtin < cvalue < vector < symbol < cons
static value_t bounded_compare(value_t a, value_t b, int bound)
{
    value_t d;

 compare_top:
    if (a == b) return fixnum(0);
    if (bound <= 0)
        return NIL;
    switch (tag(a)) {
    case TAG_NUM:
        if (isfixnum(b)) {
            return (numval(a) < numval(b)) ? fixnum(-1) : fixnum(1);
        }
        if (iscvalue(b)) {
            return fixnum(compare_num_cvalue(a, b));
        }
        return fixnum(-1);
    case TAG_SYM:
        if (tag(b) < TAG_SYM) return fixnum(1);
        if (tag(b) > TAG_SYM) return fixnum(-1);
        return fixnum(strcmp(symbol_name(a), symbol_name(b)));
    case TAG_BUILTIN:
        if (tag(b) > TAG_BUILTIN) return fixnum(-1);
        if (tag(b) == TAG_BUILTIN) {
            if (uintval(a) < N_BUILTINS || uintval(b) < N_BUILTINS) {
                return (uintval(a) < uintval(b)) ? fixnum(-1) : fixnum(1);
            }
            if (discriminateAsVector(a)) {
                if (discriminateAsVector(b))
                    return bounded_vector_compare(a, b, bound);
                return fixnum(1);
            }
            if (discriminateAsVector(b))
                return fixnum(-1);
            assert(iscvalue(a));
            assert(iscvalue(b));
            cvalue_t *acv=(cvalue_t*)ptr(a), *bcv=(cvalue_t*)ptr(b);
            numerictype_t at, bt;
            if (valid_numtype(at=cv_numtype(acv)) &&
                valid_numtype(bt=cv_numtype(bcv))) {
                void *aptr = cv_data(acv);
                void *bptr = cv_data(bcv);
                if (cmp_eq(aptr, at, bptr, bt))
                    return fixnum(0);
                if (cmp_lt(aptr, at, bptr, bt))
                    return fixnum(-1);
                return fixnum(1);
            }
            return cvalue_compare(a, b);
        }
        assert(isfixnum(b));
        return fixnum(-compare_num_cvalue(b, a));
    case TAG_CONS:
        if (tag(b) < TAG_CONS) return fixnum(1);
        d = bounded_compare(car_(a), car_(b), bound-1);
        if (numval(d) != 0) return d;
        a = cdr_(a); b = cdr_(b);
        bound--;
        goto compare_top;
    }
    return NIL;
}

static value_t cyc_vector_compare(value_t a, value_t b, ptrhash_t *table)
{
    size_t la = vector_size(a);
    size_t lb = vector_size(b);
    size_t m, i;
    value_t d, xa, xb, ca, cb;

    // first try to prove them different with no recursion
    m = la < lb ? la : lb;
    for (i = 0; i < m; i++) {
        xa = vector_elt(a,i);
        xb = vector_elt(b,i);
        if (leafp(xa) || leafp(xb)) {
            d = bounded_compare(xa, xb, 1);
            if (numval(d)!=0) return d;
        }
        else if (tag(xa) < tag(xb)) {
            return fixnum(-1);
        }
        else if (tag(xa) > tag(xb)) {
            return fixnum(1);
        }
    }

    ca = eq_class(table, a);
    cb = eq_class(table, b);
    if (ca!=NIL && ca==cb)
        return fixnum(0);

    eq_union(table, a, b, ca, cb);

    for (i = 0; i < m; i++) {
        xa = vector_elt(a,i);
        xb = vector_elt(b,i);
        if (!leafp(xa) && !leafp(xb)) {
            d = cyc_compare(xa, xb, table);
            if (numval(d)!=0)
                return d;
        }
    }

    if (la < lb) return fixnum(-1);
    if (la > lb) return fixnum(1);
    return fixnum(0);
}

static value_t cyc_compare(value_t a, value_t b, ptrhash_t *table)
{
    if (a==b)
        return fixnum(0);
    if (iscons(a)) {
        if (iscons(b)) {
            value_t aa = car_(a); value_t da = cdr_(a);
            value_t ab = car_(b); value_t db = cdr_(b);
            value_t d, ca, cb;
            if (leafp(aa) || leafp(ab)) {
                d = bounded_compare(aa, ab, 1);
                if (numval(d)!=0) return d;
            }
            else if (tag(aa) < tag(ab))
                return fixnum(-1);
            else if (tag(aa) > tag(ab))
                return fixnum(1);
            if (leafp(da) || leafp(db)) {
                d = bounded_compare(da, db, 1);
                if (numval(d)!=0) return d;
            }
            else if (tag(da) < tag(db))
                return fixnum(-1);
            else if (tag(da) > tag(db))
                return fixnum(1);

            ca = eq_class(table, a);
            cb = eq_class(table, b);
            if (ca!=NIL && ca==cb)
                return fixnum(0);

            eq_union(table, a, b, ca, cb);
            d = cyc_compare(aa, ab, table);
            if (numval(d)!=0) return d;
            return cyc_compare(da, db, table);
        }
        else {
            return fixnum(1);
        }
    }
    else if (isvector(a) && isvector(b)) {
        return cyc_vector_compare(a, b, table);
    }
    return bounded_compare(a, b, 1);
}

value_t compare(value_t a, value_t b)
{
    ptrhash_t h;
    value_t guess = bounded_compare(a, b, 2048);
    if (guess != NIL)
        return guess;

    ptrhash_new(&h, 512);
    guess = cyc_compare(a, b, &h);
    ptrhash_free(&h);
    return guess;
}

/*
  optimizations:
  - use hash updates instead of calling lookup then insert. i.e. get the
    bp once and use it twice.
  - preallocate hash table and call reset() instead of new/free
  - specialized version for equal (unordered comparison)
  - less redundant tag checking, 3-bit tags
*/
