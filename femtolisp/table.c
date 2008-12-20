#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include "llt.h"
#include "flisp.h"

static value_t tablesym;
static fltype_t *tabletype;

/*
  there are 2 kinds of hash tables (eq and equal), each with some
  optimized special cases. here are the building blocks:

  hash/compare function: (h1) eq (ptrhash) and (h2) equal (deep hash)
  relocate: (r1) no relocate, (r2) relocate but no rehash, (r3) rehash

  eq hash:
  keys all eq_comparable, no gensyms: h1, r1
  anything else: h1, r3

  equal hash:
  keys all eq_comparable, no gensyms: h1, r1
  with gensyms: h1, r2
  anything else: h2, r2
*/

typedef struct {
    void *(*get)(void *t, void *key);
    void (*remove)(void *t, void *key);
    void **(*bp)(void *t, void *key);
} table_interface_t;

typedef struct {
    table_interface_t *ti;
    ulong_t nkeys;
    htable_t ht;
} fltable_t;

void print_htable(value_t v, ios_t *f, int princ)
{
    fltable_t *pt = (fltable_t*)cv_data((cvalue_t*)ptr(v));
    htable_t *h = &pt->ht;
    size_t i;
    int first=1;
    fl_print_str("#table(", f);
    for(i=0; i < h->size; i+=2) {
        if (h->table[i+1] != HT_NOTFOUND) {
            if (!first) fl_print_str("  ", f);
            fl_print_child(f, (value_t)h->table[i], princ);
            fl_print_chr(' ', f);
            fl_print_child(f, (value_t)h->table[i+1], princ);
            first = 0;
        }
    }
    fl_print_chr(')', f);
}

void free_htable(value_t self)
{
    fltable_t *pt = (fltable_t*)cv_data((cvalue_t*)ptr(self));
    htable_free(&pt->ht);
}

void relocate_htable(value_t oldv, value_t newv)
{
    fltable_t *pt = (fltable_t*)cv_data((cvalue_t*)ptr(newv));
    htable_t *h = &pt->ht;
    size_t i;
    for(i=0; i < h->size; i++) {
        if (h->table[i] != HT_NOTFOUND)
            h->table[i] = (void*)relocate_lispvalue((value_t)h->table[i]);
    }
}

void print_traverse_htable(value_t self)
{
    fltable_t *pt = (fltable_t*)cv_data((cvalue_t*)ptr(self));
    htable_t *h = &pt->ht;
    size_t i;
    for(i=0; i < h->size; i++) {
        if (h->table[i] != HT_NOTFOUND)
            print_traverse((value_t)h->table[i]);
    }
}

void rehash_htable(value_t oldv, value_t newv)
{
}

cvtable_t h_r1_vtable = { print_htable, NULL, free_htable,
                          print_traverse_htable };
cvtable_t h_r2_vtable = { print_htable, relocate_htable, free_htable,
                          print_traverse_htable };
cvtable_t h_r3_vtable = { print_htable, rehash_htable, free_htable,
                          print_traverse_htable };

int ishashtable(value_t v)
{
    return iscvalue(v) && cv_class((cvalue_t*)ptr(v)) == tabletype;
}

value_t fl_hashtablep(value_t *args, u_int32_t nargs)
{
    argcount("hashtablep", nargs, 1);
    return ishashtable(args[0]) ? T : NIL;
}

value_t fl_table(value_t *args, u_int32_t nargs)
{
    if (nargs & 1)
        lerror(ArgError, "table: arguments must come in pairs");
    value_t nt = cvalue(tabletype, sizeof(fltable_t));
    fltable_t *h = (fltable_t*)cv_data((cvalue_t*)ptr(nt));
    htable_new(&h->ht, 8);
    int i;
    for(i=0; i < nargs; i+=2)
        equalhash_put(&h->ht, args[i], args[i+1]);
    return nt;
}

// (put table key value)
value_t fl_hash_put(value_t *args, u_int32_t nargs)
{
    argcount("put", nargs, 3);
    return NIL;
}

// (get table key [default])
value_t fl_hash_get(value_t *args, u_int32_t nargs)
{
    argcount("get", nargs, 2);
    return NIL;
}

// (has table key)
value_t fl_hash_has(value_t *args, u_int32_t nargs)
{
    argcount("has", nargs, 2);
    return NIL;
}

// (del table key)
value_t fl_hash_delete(value_t *args, u_int32_t nargs)
{
    argcount("del", nargs, 2);
    return NIL;
}

static builtinspec_t tablefunc_info[] = {
    { "table", fl_table },
    { NULL, NULL }
};

void table_init()
{
    tablesym = symbol("table");
    tabletype = define_opaque_type(tablesym, sizeof(fltable_t),
                                   &h_r2_vtable, NULL);
    assign_global_builtins(tablefunc_info);
}
