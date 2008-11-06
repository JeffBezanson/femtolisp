/*
  pointer hash table
  optimized for storing info about particular values
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

#include "dtypes.h"
#include "ptrhash.h"
#include "hashing.h"

#define ptrhash_size(h) ((h)->size/2)

ptrhash_t *ptrhash_new(ptrhash_t *h, size_t size)
{
    size = nextipow2(size);
    size *= 2;  // 2 pointers per key/value pair
    size *= 2;  // aim for 50% occupancy
    h->size = size;
    h->table = (void**)malloc(size*sizeof(void*));
    if (h->table == NULL) return NULL;
    size_t i;
    for(i=0; i < size; i++)
        h->table[i] = PH_NOTFOUND;
    return h;
}

void ptrhash_free(ptrhash_t *h)
{
    free(h->table);
}

// empty and reduce size
void ptrhash_reset(ptrhash_t *h, size_t sz)
{
    if (h->size > sz*4) {
        size_t newsz = sz*4;
        void **newtab = (void**)realloc(h->table, newsz*sizeof(void*));
        if (newtab == NULL)
            return;
        h->size = newsz;
        h->table = newtab;
    }
    size_t i, hsz=h->size;
    for(i=0; i < hsz; i++)
        h->table[i] = PH_NOTFOUND;
}

// compute empirical max-probe for a given size
#define ph_max_probe(size) ((size)>>5)

static void **ptrhash_lookup_bp(ptrhash_t *h, void *key)
{
    uint_t hv;
    size_t i, orig, index, iter;
    size_t newsz, sz = ptrhash_size(h);
    size_t maxprobe = ph_max_probe(sz);
    void **tab = h->table;
    void **ol;

    hv = inthash((uptrint_t)key);
 retry_bp:
    iter = 0;
    index = (index_t)(hv & (sz-1)) * 2;
    sz *= 2;
    orig = index;

    do {
        if (tab[index+1] == PH_NOTFOUND) {
            tab[index] = key;
            return &tab[index+1];
        }

        if (key == tab[index])
            return &tab[index+1];

        index = (index+2) & (sz-1);
        iter++;
        if (iter > maxprobe)
            break;
    } while (index != orig);

    // table full
    // quadruple size, rehash, retry the insert
    // it's important to grow the table really fast; otherwise we waste
    // lots of time rehashing all the keys over and over.
    sz = h->size;
    ol = h->table;
    if (sz >= (1<<19))
        newsz = sz<<1;
    else
        newsz = sz<<2;
    //printf("trying to allocate %d words.\n", newsz); fflush(stdout);
    tab = (void**)malloc(newsz*sizeof(void*));
    if (tab == NULL)
        return NULL;
    for(i=0; i < newsz; i++)
        tab[i] = PH_NOTFOUND;
    h->table = tab;
    h->size = newsz;
    for(i=0; i < sz; i+=2) {
        if (ol[i] != PH_NOTFOUND && ol[i+1] != PH_NOTFOUND) {
            (*ptrhash_lookup_bp(h, ol[i])) = ol[i+1];
            /*
            // this condition is not really possible
            if (bp == NULL) {
                free(h->table);
                h->table = ol;
                h->size = sz;
                // another thing we could do in this situation
                // is newsz<<=1 and go back to the malloc, retrying with
                // a bigger buffer on this level of recursion.
                return NULL;
            }
            */
        }
    }
    free(ol);

    sz = ptrhash_size(h);
    maxprobe = ph_max_probe(sz);

    goto retry_bp;

    return NULL;
}

void ptrhash_put(ptrhash_t *h, void *key, void *val)
{
    void **bp = ptrhash_lookup_bp(h, key);

    *bp = val;
}

void **ptrhash_bp(ptrhash_t *h, void *key)
{
    return ptrhash_lookup_bp(h, key);
}

// returns bp if key is in hash, otherwise NULL
static void **ptrhash_peek_bp(ptrhash_t *h, void *key)
{
    size_t sz = ptrhash_size(h);
    size_t maxprobe = ph_max_probe(sz);
    void **tab = h->table;
    size_t index = (index_t)(inthash((uptrint_t)key) & (sz-1)) * 2;
    sz *= 2;
    size_t orig = index;
    size_t iter = 0;

    do {
        if (tab[index] == PH_NOTFOUND)
            return NULL;
        if (key == tab[index] && tab[index+1] != PH_NOTFOUND)
            return &tab[index+1];

        index = (index+2) & (sz-1);
        iter++;
        if (iter > maxprobe)
            break;
    } while (index != orig);

    return NULL;
}

void *ptrhash_get(ptrhash_t *h, void *key)
{
    void **bp = ptrhash_peek_bp(h, key);
    if (bp == NULL)
        return PH_NOTFOUND;
    return *bp;
}

int ptrhash_has(ptrhash_t *h, void *key)
{
    return (ptrhash_get(h,key) != PH_NOTFOUND);
}

void ptrhash_remove(ptrhash_t *h, void *key)
{
    void **bp = ptrhash_peek_bp(h, key);
    if (bp != NULL)
        *bp = PH_NOTFOUND;
}

void ptrhash_adjoin(ptrhash_t *h, void *key, void *val)
{
    void **bp = ptrhash_lookup_bp(h, key);
    if (*bp == PH_NOTFOUND)
        *bp = val;
}
