typedef struct {
    size_t n, maxsize;
    unsigned long *items;
} ltable_t;

void ltable_init(ltable_t *t, size_t n)
{
    t->n = 0;
    t->maxsize = n;
    t->items = (unsigned long*)malloc(n * sizeof(unsigned long));
}

void ltable_clear(ltable_t *t)
{
    t->n = 0;
}

void ltable_insert(ltable_t *t, unsigned long item)
{
    unsigned long *p;

    if (t->n == t->maxsize) {
        p = realloc(t->items, (t->maxsize*2)*sizeof(unsigned long));
        if (p == NULL) return;
        t->items = p;
        t->maxsize *= 2;
    }
    t->items[t->n++] = item;
}

#define LT_NOTFOUND ((int)-1)

int ltable_lookup(ltable_t *t, unsigned long item)
{
    int i;
    for(i=0; i < (int)t->n; i++)
        if (t->items[i] == item)
            return i;
    return LT_NOTFOUND;
}

void ltable_adjoin(ltable_t *t, unsigned long item)
{
    if (ltable_lookup(t, item) == LT_NOTFOUND)
        ltable_insert(t, item);
}

char *snprintf_gensym_id(char *nbuf, size_t n, u_int32_t g)
{
    size_t i=n-1;

    nbuf[i--] = '\0';
    do {
        nbuf[i--] = '0' + g%10;
        g/=10;
    } while (g && i);
    nbuf[i] = 'g';
    return &nbuf[i];
}
