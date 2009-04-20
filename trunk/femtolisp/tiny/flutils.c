u_int32_t *bitvector_resize(u_int32_t *b, size_t n)
{
    u_int32_t *p;
    size_t sz = ((n+31)>>5) * 4;
    p = realloc(b, sz);
    if (p == NULL) return NULL;
    memset(p, 0, sz);
    return p;
}

u_int32_t *mk_bitvector(size_t n)
{
    return bitvector_resize(NULL, n);
}

void bitvector_set(u_int32_t *b, u_int32_t n, u_int32_t c)
{
    if (c)
        b[n>>5] |= (1<<(n&31));
    else
        b[n>>5] &= ~(1<<(n&31));
}

u_int32_t bitvector_get(u_int32_t *b, u_int32_t n)
{
    return b[n>>5] & (1<<(n&31));
}

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

#define NOTFOUND ((int)-1)

int ltable_lookup(ltable_t *t, unsigned long item)
{
    int i;
    for(i=0; i < (int)t->n; i++)
        if (t->items[i] == item)
            return i;
    return NOTFOUND;
}

void ltable_adjoin(ltable_t *t, unsigned long item)
{
    if (ltable_lookup(t, item) == NOTFOUND)
        ltable_insert(t, item);
}

static const u_int32_t offsetsFromUTF8[6] = {
    0x00000000UL, 0x00003080UL, 0x000E2080UL,
    0x03C82080UL, 0xFA082080UL, 0x82082080UL
};

static const char trailingBytesForUTF8[256] = {
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5
};

int u8_seqlen(const char c)
{
    return trailingBytesForUTF8[(unsigned int)(unsigned char)c] + 1;
}

#define UEOF ((u_int32_t)EOF)

u_int32_t u8_fgetc(FILE *f)
{
    int amt=0, sz, c;
    u_int32_t ch=0;

    c = fgetc(f);
    if (c == EOF)
        return UEOF;
    ch = (u_int32_t)c;
    amt = sz = u8_seqlen(ch);
    while (--amt) {
        ch <<= 6;
        c = fgetc(f);
        if (c == EOF)
            return UEOF;
        ch += (u_int32_t)c;
    }
    ch -= offsetsFromUTF8[sz-1];

    return ch;
}
