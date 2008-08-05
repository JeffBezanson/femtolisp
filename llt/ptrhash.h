#ifndef __PTRHASH_H_
#define __PTRHASH_H_

typedef struct _ptrhash_t {
    size_t size;
    void **table;
} ptrhash_t;

// define this to be an invalid key/value
#define PH_NOTFOUND ((void*)1)

// initialize and free
ptrhash_t *ptrhash_new(ptrhash_t *h, size_t size);
void ptrhash_free(ptrhash_t *h);

// clear and (possibly) change size
void ptrhash_reset(ptrhash_t *h, size_t sz);

// return value, or PH_NOTFOUND if key not found
void *ptrhash_get(ptrhash_t *h, void *key);

// add key/value binding
void ptrhash_put(ptrhash_t *h, void *key, void *val);

// add binding iff key is unbound
void ptrhash_adjoin(ptrhash_t *h, void *key, void *val);

// does key exist?
int ptrhash_has(ptrhash_t *h, void *key);

// logically remove key
void ptrhash_remove(ptrhash_t *h, void *key);

// get a pointer to the location of the value for the given key.
// creates the location if it doesn't exist. only returns NULL
// if memory allocation fails.
// this should be used for updates, for example:
//     void **bp = ptrhash_bp(h, key);
//     *bp = f(*bp);
// do not reuse bp if there might be intervening calls to ptrhash_put,
// ptrhash_bp, ptrhash_reset, or ptrhash_free.
void **ptrhash_bp(ptrhash_t *h, void *key);

#endif
