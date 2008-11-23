#ifndef __HTABLE_H_
#define __HTABLE_H_

typedef struct {
    size_t size;
    void **table;
} htable_t;

// define this to be an invalid key/value
#define HT_NOTFOUND ((void*)1)

// initialize and free
htable_t *htable_new(htable_t *h, size_t size);
void htable_free(htable_t *h);

// clear and (possibly) change size
void htable_reset(htable_t *h, size_t sz);

#endif
