#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include "llt.h"
#include "flisp.h"
#include "equalhash.h"

static value_t tablesym;
static fltype_t *tabletype;

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

void print_traverse_htable(value_t self)
{
    fltable_t *pt = (fltable_t*)cv_data((cvalue_t*)ptr(self));
    htable_t *h = &pt->ht;
    size_t i;
    for(i=0; i < h->size; i+=2) {
        if (h->table[i+1] != HT_NOTFOUND) {
            print_traverse((value_t)h->table[i]);
            print_traverse((value_t)h->table[i+1]);
        }
    }
}

void free_htable(value_t self)
{
    fltable_t *pt = (fltable_t*)cv_data((cvalue_t*)ptr(self));
    htable_free(&pt->ht);
}

void relocate_htable(value_t oldv, value_t newv)
{
    (void)oldv;
    fltable_t *pt = (fltable_t*)cv_data((cvalue_t*)ptr(newv));
    htable_t *h = &pt->ht;
    size_t i;
    for(i=0; i < h->size; i++) {
        if (h->table[i] != HT_NOTFOUND)
            h->table[i] = (void*)relocate_lispvalue((value_t)h->table[i]);
    }
}

cvtable_t table_vtable = { print_htable, relocate_htable, free_htable,
                           print_traverse_htable };

int ishashtable(value_t v)
{
    return iscvalue(v) && cv_class((cvalue_t*)ptr(v)) == tabletype;
}

value_t fl_hashtablep(value_t *args, uint32_t nargs)
{
    argcount("hashtablep", nargs, 1);
    return ishashtable(args[0]) ? T : NIL;
}

static fltable_t *totable(value_t v, char *fname)
{
    if (ishashtable(v))
        return (fltable_t*)cv_data((cvalue_t*)ptr(v));
    type_error(fname, "table", v);
    return NULL;
}

value_t fl_table(value_t *args, uint32_t nargs)
{
    if (nargs & 1)
        lerror(ArgError, "table: arguments must come in pairs");
    value_t nt = cvalue(tabletype, sizeof(fltable_t));
    fltable_t *h = (fltable_t*)cv_data((cvalue_t*)ptr(nt));
    htable_new(&h->ht, 8);
    uint32_t i;
    for(i=0; i < nargs; i+=2)
        equalhash_put(&h->ht, (void*)args[i], (void*)args[i+1]);
    return nt;
}

// (put table key value)
value_t fl_table_put(value_t *args, uint32_t nargs)
{
    argcount("put", nargs, 3);
    fltable_t *pt = totable(args[0], "put");
    equalhash_put(&pt->ht, (void*)args[1], (void*)args[2]);
    return args[0];
}

// (get table key [default])
value_t fl_table_get(value_t *args, uint32_t nargs)
{
    if (nargs != 3)
        argcount("get", nargs, 2);
    fltable_t *pt = totable(args[0], "get");
    value_t v = (value_t)equalhash_get(&pt->ht, (void*)args[1]);
    if (v == (value_t)HT_NOTFOUND) {
        if (nargs == 3)
            return args[2];
        lerror(KeyError, "get: key not found");
    }
    return v;
}

// (has table key)
value_t fl_table_has(value_t *args, uint32_t nargs)
{
    argcount("has", nargs, 2);
    fltable_t *pt = totable(args[0], "has");
    return equalhash_has(&pt->ht, (void*)args[1]) ? T : NIL;
}

// (del table key)
value_t fl_table_del(value_t *args, uint32_t nargs)
{
    argcount("del", nargs, 2);
    fltable_t *pt = totable(args[0], "del");
    if (!equalhash_remove(&pt->ht, (void*)args[1]))
        lerror(KeyError, "del: key not found");
    return args[0];
}

value_t fl_table_foldl(value_t *args, uint32_t nargs)
{
    argcount("table.foldl", nargs, 3);
    PUSH(listn(3, NIL, NIL, NIL));
    fltable_t *pt = totable(args[2], "table.foldl");
    size_t i, n = pt->ht.size;
    void **table = pt->ht.table;
    value_t c;
    for(i=0; i < n; i+=2) {
        if (table[i+1] != HT_NOTFOUND) {
            c = Stack[SP-1];
            car_(c) = (value_t)table[i];
            car_(cdr_(c)) = (value_t)table[i+1];
            car_(cdr_(cdr_(c))) = args[1];
            args[1] = apply(args[0], c);
            // reload pointer
            table = ((fltable_t*)cv_data((cvalue_t*)ptr(args[2])))->ht.table;
        }
    }
    (void)POP();
    return args[1];
}

static builtinspec_t tablefunc_info[] = {
    { "table", fl_table },
    { "put", fl_table_put },
    { "get", fl_table_get },
    { "has", fl_table_has },
    { "del", fl_table_del },
    { "table.foldl", fl_table_foldl },
    { NULL, NULL }
};

void table_init()
{
    tablesym = symbol("table");
    tabletype = define_opaque_type(tablesym, sizeof(fltable_t),
                                   &table_vtable, NULL);
    assign_global_builtins(tablefunc_info);
}
