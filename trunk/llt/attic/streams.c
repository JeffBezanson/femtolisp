#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <limits.h>

#ifdef WIN32
#include <malloc.h>
#include <io.h>
#include <fcntl.h>
#include <errno.h>
#define fileno _fileno
#else
#include <unistd.h>
#include <sys/time.h>
#include <sys/select.h>
#endif

#include "dtypes.h"
#include "utils.h"
#include "utf8.h"
#include "streams.h"

unsigned int type_sizes[] = {
    sizeof(int32_t), sizeof(int8_t), sizeof(int16_t),
    sizeof(int32_t), sizeof(float),  sizeof(double), sizeof(int64_t), 0,

    /* unsigned */
    0, sizeof(u_int8_t), sizeof(u_int16_t),
    sizeof(u_int32_t), 0, 0, sizeof(u_int64_t), 0,

    /* complex */
    2*sizeof(int32_t), 2*sizeof(int8_t), 2*sizeof(int16_t),
    2*sizeof(int32_t), 2*sizeof(float),  2*sizeof(double), 2*sizeof(int64_t),0,

    /* complex unsigned */
    0, 2*sizeof(u_int8_t), 2*sizeof(u_int16_t),
    2*sizeof(u_int32_t), 0, 0, 2*sizeof(u_int64_t), 0
};

bool_t valid_type(u_int32_t type)
{
    u_int32_t sz;

    /* irrelevant bits set */
    if (type & ~T_TYPEMASK)
        return false;
    sz = type & T_TYPESIZE;
    if (sz > T_INT64)
        return false;
    if (type == T_CPLX|T_BOOL)
        return false;
    /* no unsigned float or complex unsigned */
    if (type & T_UNSIGNED) {
        if ((sz > T_INT && sz != T_INT64) || sz == T_BOOL || type&T_CPLX)
            return false;
    }
    return true;
}

/* an important function: some stream routines need to call this at
   various points to ensure proper coexistence with bitwise I/O */
static void stream_bit_flush(stream_t *s)
{
    if (s->bitpos > 0) {
        stream_write(s, &s->bitbuf, 1);
        s->bitpos = 0;
    }
}

void stream_flush(stream_t *s)
{
    stream_bit_flush(s);
    s->funcs->flush(s);
}

void stream_free(stream_t *s)
{
    stream_flush(s);
    s->funcs->free_func(s);
}

int stream_readfd(stream_t *s)
{
    if (is_filestream(s)) {
        return fileno(s->fptr);
    }
    else if (is_pipestream(s)) {
        return s->rd;
    }
    return -1;
}

int stream_writefd(stream_t *s)
{
    if (is_filestream(s)) {
        return fileno(s->fptr);
    }
    else if (is_pipestream(s)) {
        return s->wd;
    }
    return -1;
}


/* file stream */

off_t fs_seek(struct _stream *s, off_t where)
{
    FILE *f = s->fptr;

    stream_bit_flush(s);

    if (fseek(f, where, SEEK_SET) == -1)
        return -1;
    s->pos = ftell(f);
    return s->pos;
}

off_t fs_skip(struct _stream *s, off_t offs)
{
    FILE *f = s->fptr;

    stream_bit_flush(s);

    // a successful fseek always resets the end-of-file condition, even if
    // the offset is zero. we change this so that moving by 0 bytes when you're
    // at eof means you stay at eof.
    if (offs == 0)
        return s->pos;

    if (fseek(f, offs, SEEK_CUR) == -1)
        return -1;
    s->pos += offs;
    return s->pos;
}

off_t fs_seek_end(struct _stream *s)
{
    FILE *f = s->fptr;

    stream_bit_flush(s);

    if (fseek(f, 0, SEEK_END) == -1)
        return -1;
    s->pos = ftell(f);
    return s->pos;
}

size_t fs_read(struct _stream *s, char *dest, size_t size)
{
    FILE *f = s->fptr;
    size_t c;

    c = fread(dest, 1, size, f); /* read single-byte entities */
    s->pos += c;
    return c;
}

size_t fs_write(struct _stream *s, char *data, size_t size)
{
    FILE *f = s->fptr;
    size_t c;

    c = fwrite(data, 1, size, f);
    s->pos += c;
    return c;
}

size_t fs_trunc(struct _stream *s, size_t size)
{
    FILE *f = s->fptr;

#ifndef WIN32
    if (ftruncate(fileno(f), s->pos) == -1)
        return -1; // TODO this should be unsigned!
#else
    if (_chsize(_fileno(f), s->pos) == -1)
        return -1;
#endif
    return s->pos;
}

void fs_flush(struct _stream *s)
{
    FILE *f = s->fptr;

    (void)fflush(f);
}

bool_t fs_eof(struct _stream *s)
{
    return (bool_t)feof(s->fptr);
}

void free_fs_stream(stream_t *s)
{
    fclose(s->fptr);
    free(s->name);
}

void stream_close(stream_t *s)
{
    /*
    if (!is_filestream(s))
        return;
    */
    stream_flush(s);
    // so far file streams are not designed to exist in the closed state
    //    fclose(s->fptr);
}

stream_interface_t fs_funcs =
    {free_fs_stream, fs_seek, fs_seek_end, fs_skip,
     fs_read, fs_write, fs_trunc, fs_eof, fs_flush};

stream_t *filestream_new(stream_t *s,
                         char *fName, bool_t create, bool_t rewrite)
{
    FILE *f;
    size_t sz;

    if (create) {
        if (rewrite) {
            f = fopen(fName, "w+b");
        }
        else {
            f = fopen(fName, "a+b");
            if (f)
                fseek(f, 0, SEEK_SET);
        }
    }
    else {
        f = fopen(fName, "r+b");
    }
    if (f == NULL) {
        /* try readonly */
        f = fopen(fName, "rb");
        if (f == NULL)
            return NULL;
    }

    s->fptr = f;
    s->name = strdup(fName);

    s->funcs = &fs_funcs;

    s->pos = 0;
    s->bitpos = s->bitbuf = 0;
    s->byteswap = false;
    return s;
}

stream_t *stream_fromfile(stream_t *s, FILE *f, char *name)
{
    s->fptr = f;
    s->name = strdup(name);

    s->funcs = &fs_funcs;

    s->pos = 0;
    s->bitpos = s->bitbuf = 0;
    s->byteswap = false;
    return s;
}

/* memory stream */

off_t ms_seek(struct _stream *s, off_t where)
{
    if ((size_t)where > s->size)
        return -1;

    stream_bit_flush(s);
    s->pos = where;

    return s->pos;
}

off_t ms_skip(struct _stream *s, off_t offs)
{
    if (s->pos+offs < 0 || s->pos+offs > s->size)
        return -1;

    stream_bit_flush(s);

    s->pos += offs;
    return s->pos;
}

off_t ms_seek_end(struct _stream *s)
{
    stream_bit_flush(s);
    s->pos = s->size;
    return s->pos;
}

size_t ms_read(struct _stream *s, char *dest, size_t size)
{
    size_t amt = size;

    assert(s->pos <= s->size);

    if (size == 0)
        return 0;

    if (s->size - s->pos < size)
        amt = s->size - s->pos;

    if (amt > 0) {
        memcpy(dest, &s->data[s->pos], amt);
    }

    s->pos += amt;
    return amt;
}

static char *ms_realloc(stream_t *s, size_t size)
{
    char *temp;

    if (size <= s->maxsize)
        return s->data;

    /* UNOFFICIALLY, we put a '\0' at the end of every memory stream for
       compatability with C library string functions, which are convenient.
       You are not allowed to depend on this behavior; it may eventually
       change.

       We implement this by telling everybody that maxsize is one less than
       the actual size of the buffer. They think the last byte is at index
       maxsize-1, but it is actually at index maxsize.

       data[s->size] and data[s->maxsize] are kept at '\0'. */

    if (size <= N_STREAM_LOCAL-1) {
        /* TO DO: if we want to allow shrinking, see if the buffer shrank
           down to this size, in which case we need to copy. */
        s->data = &s->local[0];
        s->maxsize = N_STREAM_LOCAL-1;
        s->data[s->maxsize] = '\0';
        return s->data;
    }
    if (s->data == &s->local[0]) {
        temp = malloc(size+1);
        // TODO nullcheck
        memcpy(temp, s->data, s->size);
    }
    else {
        /* here we rely on the realloc() behavior of
           doing a malloc() when passed a NULL pointer. */
        temp = realloc(s->data, size+1);
        if (temp == NULL)
            return NULL;
    }
    s->data = temp;
    s->maxsize = size;
    s->data[s->maxsize] = '\0';
    return s->data;
}

size_t ms_trunc(struct _stream *s, size_t size)
{
    if (size == s->size)
        return size;

    if (size < s->size) {
        // TODO: if big shrink, release space
        if (s->pos > size)
            s->pos = size;
    }
    else {
        if (ms_realloc(s, size) == NULL)
            return s->size;
    }

    s->size = size;
    s->data[size] = '\0';
    return size;
}

size_t ms_write(struct _stream *s, char *data, size_t size)
{
    size_t amt;
    size_t newsize;

    if (size == 0)
        return 0;

    if (s->pos + size > s->size) {
        if (s->pos + size > s->maxsize) {
            /* TO DO: here you might want to add a mechanism for limiting
               the growth of the stream. */
            newsize = s->maxsize * 2;
            while (s->pos + size > newsize)
                newsize *= 2;
            if (ms_realloc(s, newsize) == NULL) {
                /* no more space; write as much as we can */
                amt = s->maxsize - s->pos;
                if (amt > 0) {
                    memcpy(&s->data[s->pos], data, amt);
                }
                s->pos += amt;
                s->size = s->maxsize;
                /* in this case we've written up to the end of the buffer,
                   so we know the next char is \0 since ms_realloc sets that
                   up every time it allocates. */
                return amt;
            }
        }
        s->size = s->pos + size;

        /* always valid since secretly we know the buffer is 1 bigger than
           maxsize */
        s->data[s->size] = '\0';
    }
    memcpy(&s->data[s->pos], data, size);
    s->pos += size;

    return size;
}

void ms_flush(struct _stream *s)
{
}

bool_t ms_eof(struct _stream *s)
{
    assert(s->pos <= s->size);
    return (bool_t)(s->pos == s->size);
}

void free_ms_stream(stream_t *s)
{
    if (s->data != NULL && s->data != &s->local[0])
        free(s->data);
}
/*
stream_t *memstream_copy(stream_t *s)
{
    stream_t *ns = memstream_new(s->size);

    ms_write(ns, s->s, s->size);
    stream_seek(ns, 0);
    return ns;
}
*/
stream_interface_t ms_funcs =
    {free_ms_stream, ms_seek, ms_seek_end, ms_skip,
     ms_read, ms_write, ms_trunc, ms_eof, ms_flush};

stream_t *memstream_new(stream_t *s, size_t initsize)
{
    s->pos = 0;
    s->size = 0;
    s->data = &s->local[0];
    s->maxsize = N_STREAM_LOCAL-1;
    s->data[s->maxsize] = '\0';

    if (ms_realloc(s, initsize) == NULL)
        julia_outofmemory();

    s->data[initsize] = '\0';
    s->data[s->pos] = '\0';
    s->size = initsize;

    s->funcs = &ms_funcs;

    s->bitpos = s->bitbuf = 0;
    s->byteswap = false;

    return s;
}

#if 0
string_t *string_new(int len)
{
    string_t *str;

    str = memstream_new(len);
    str->len = len;

    return str;
}

/* this function makes string streams from C strings (NUL-term) */
string_t *string_fromc(char *s)
{
    string_t *str;
    int len = strlen(s);

    str = string_new(len);
    stream_write(str, s, len);
    stream_seek(str, 0);

    return str;
}
#endif

/* pipe */
off_t ps_seek(struct _stream *s, off_t where)
{
    return -1;
}

size_t ps_read(struct _stream *s, char *dest, size_t size);

off_t ps_skip(struct _stream *s, off_t offs)
{
    char buf[CHUNK_SIZE];
    int rd = s->rd;
    size_t c, amt;

    if (offs < 0)
        return -1;
    while (offs > 0) {
        amt = offs > CHUNK_SIZE ? CHUNK_SIZE : offs;
        c = ps_read(s, buf, amt);
        if (c < amt)
            return 0;
        offs -= c;
    }
    return 0;
}

off_t ps_seek_end(struct _stream *s)
{
    return -1;
}

size_t ps_read(struct _stream *s, char *dest, size_t size)
{
    if (ps_eof(s))
        return 0;
#ifdef WIN32
    int c = _read(s->rd, dest, size);
#else
    ssize_t c = read(s->rd, dest, size);
#endif
    if (c < 0)
        return 0;
    return (size_t)c;
}

size_t ps_write(struct _stream *s, char *data, size_t size)
{
#ifdef WIN32
    int c = _write(s->wd, data, size);
#else
    ssize_t c = write(s->wd, data, size);
#endif
    if (c < 0)
        return 0;
    return c;
}

size_t ps_trunc(struct _stream *s, size_t size)
{
    return 0;
}

void ps_flush(struct _stream *s)
{
}

bool_t ps_eof(struct _stream *s)
{
#ifndef WIN32
    fd_set set;
    struct timeval tv = {0, 0};

    FD_ZERO(&set);
    FD_SET(s->rd, &set);
    return (select(s->rd+1, &set, NULL, NULL, &tv)==0);
#else
    return 0;
#endif
}

void free_ps_stream(stream_t *s)
{
    close(s->rd);
    close(s->wd);
}

stream_interface_t ps_funcs =
    {free_ps_stream, ps_seek, ps_seek_end, ps_skip,
     ps_read, ps_write, ps_trunc, ps_eof, ps_flush};

stream_t *pipestream_new(stream_t *s, int flags)
{
    int fds[2];

    s->funcs = &ps_funcs;

#ifdef WIN32
    _pipe(&fds[0], 32768, _O_BINARY | flags);
#else
    pipe(&fds[0]);
#endif
    s->rd = fds[0]; s->wd = fds[1];

    s->byteswap = false;
    s->bitpos = s->bitbuf = 0;
    s->pos = 0;

    return s;
}


/* high-level stream functions */

/* type is a "T_" code as defined in streams.h */
int stream_put_num(stream_t *stream, char *d, u_int32_t type)
{
    int c, sz, i;

    assert(valid_type(type));
    sz = type_sizes[type];

    if (!stream->byteswap) {
        c = stream_write(stream, d, sz);
    }
    else if (sz >= 4) {
        char temp[32];
        if (type & T_CPLX) {
            bswap_to(temp,      (byte_t*)d,      sz/2);
            bswap_to(temp+sz/2, (byte_t*)d+sz/2, sz/2);
        }
        else {
            bswap_to(temp, (byte_t*)d, sz);
        }
        c = stream_write(stream, temp, sz);
    }
    else {
        assert(sz == 2 || sz == 1);
        if (type & T_CPLX) {
            c = stream_write(stream, &d[0], 2);
            /*
            for(i=sz/2-1; i >= 0; i--) {
                c += stream_write(stream, &d[i], 1);
            }
            for(i=sz-1; i >= sz/2; i--) {
                c += stream_write(stream, &d[i], 1);
            }
            */
        }
        else {
            c = 0;
            if (sz == 2)
                c += stream_write(stream, &d[1], 1);
            c += stream_write(stream, &d[0], 1);
        }
    }

    return c;
}

int stream_get_num(stream_t *s, char *data, u_int32_t type)
{
    int c, sz;

    assert(valid_type(type));
    sz = type_sizes[type];

    c = stream_read(s, data, sz);

    if (s->byteswap && c == sz) {
        if (type & T_CPLX) {
            bswap((byte_t*)data, sz/2);
            bswap((byte_t*)data+sz/2, sz/2);
        }
        else {
            bswap((byte_t*)data, sz);
        }
    }
    if (c < sz)
        return -1;
    return c;
}

int stream_put_int(stream_t *s, int n)
{
    return stream_put_num(s, (char*)&n, T_INT);
}

int stream_put_char(stream_t *s, u_int32_t wc)
{
    char buf[8];
    int amt;

    amt = u8_wc_toutf8(buf, wc);
    return stream_write(s, buf, amt);
}

int stream_put_stringz(stream_t *s, char *str, bool_t nulterm)
{
    int c, l = strlen(str);

    c = stream_write(s, str, l);
    if (nulterm)
        c += stream_write(s, &str[l], 1);
    return c;
}

int stream_get_char(stream_t *s, u_int32_t *pwc)
{
    char buf[8];
    int amt;
    unsigned int i;

    amt = stream_read(s, buf, 1);
    if (amt == 0)
        return 0;
    amt = u8_seqlen(buf) - 1;  // find out how many more bytes in this seq
    if (amt) {
        stream_read(s, &buf[1], amt);
    }
    amt++;
    buf[amt] = '\0';
    i=0;
    *pwc = u8_nextmemchar(buf, &i);
    return i;
}

int stream_nextchar(stream_t *s)
{
    u_int32_t wc;

    if (is_memstream(s)) {
        if (stream_eof(s))
            return -1;
        stream_bit_flush(s);
        s->pos++;
        while (!stream_eof(s) && !isutf(s->s[s->pos]))
            s->pos++;
        return s->pos;
    }

    if (stream_get_char(s, &wc) == 0)
        return -1;
    return s->pos;
}

int stream_prevchar(stream_t *s)
{
    char c;

    if (is_memstream(s)) {
        if (s->pos == 0)
            return -1;
        stream_bit_flush(s);
        s->pos--;
        while (s->pos > 0 && !isutf(s->s[s->pos]))
            s->pos--;
        return s->pos;
    }

    do {
        if (stream_skip(s, -1) == -1)
            return -1;
        stream_read(s, &c, 1);
        stream_skip(s, -1);
    } while(!isutf(c));
    return s->pos;
}

void stream_put_bit(stream_t *s, int bit)
{
    byte_t mask = 0x1;

    if (s->bitpos == 0) {
        if (!stream_read(s, &s->bitbuf, 1)) {
            s->bitbuf = 0;
        }
        else {
            stream_skip(s, -1);
        }
    }

    mask <<= s->bitpos;
    if (bit)
        s->bitbuf |= mask;
    else
        s->bitbuf &= ~mask;
    s->dirty = 1;

    s->bitpos++;
    if (s->bitpos > 7) {
        s->bitpos = 0;
        stream_write(s, &s->bitbuf, 1);
    }
}

int stream_get_bit(stream_t *s, int *pbit)
{
    byte_t mask = 0x1;

    if (s->bitpos == 0) {
        if (!stream_read(s, &s->bitbuf, 1)) {
            return 0;
        }
        else {
            stream_skip(s, -1);
        }
        s->dirty = 0;
    }

    mask <<= s->bitpos;
    *pbit = (s->bitbuf & mask) ? 1 : 0;

    s->bitpos++;

    if (s->bitpos > 7) {
        s->bitpos = 0;
        if (s->dirty) {
            stream_write(s, &s->bitbuf, 1);
        }
        else {
            stream_skip(s, 1);
        }
    }
    return 1;
}

/* warning: DO NOT write a trivial wrapper for this function; it allows
   easily crashing the interpreter using unfriendly format strings.

   also, this function is designed to print small things like messages. it
   cannot print arbitrarily large data. to do that, use stream_write,
   or repeated calls to this function.

   TODO: this doesn't handle UTF-8 properly:

printf("%*s|\n%*s|\n", -4, "X", -4, "a")
printf("%-4s|\n%-4s|\n", "X", "a")
X  |
a   |

Where X is a 2-byte character.
*/
int stream_printf(stream_t *s, char *format, ...)
{
    int c;
    va_list args;
    char buf[512];

    va_start(args, format);
    c = vsnprintf(buf, sizeof(buf), format, args);
    va_end(args);

    if (c < 0)
        return 0;

    if ((unsigned)c > sizeof(buf))
        c = sizeof(buf);

    return stream_write(s, buf, c);
}

char *stream_take_buffer(stream_t *s, size_t *size)
{
    char *buf;

    if (!is_memstream(s) || s->data == NULL)
        return NULL;

    stream_flush(s);

    if (s->data == &s->local[0]) {
        buf = malloc(s->size+1);
        if (buf == NULL)
            return NULL;
        memcpy(buf, s->data, s->size+1);
        buf[s->size] = '\0';
    }
    else {
        buf = s->data;
    }

    *size = s->size+1; /* buffer is actually 1 bigger for terminating NUL */

    /* empty stream and reinitialize */
    s->data = &s->local[0];
    s->maxsize = N_STREAM_LOCAL-1;
    s->data[s->maxsize] = '\0';
    stream_trunc(s, 0);

    return buf;
}

/* Chunk size for reading lines: if too small, then we make too many low-level
   calls. if too large, then we waste time reading bytes beyond the end of the
   current line. this is effectively a guess as to how big a line is.
   this value should be < N_STREAM_LOCAL, allowing at least some lines to
   fit without extra allocation. */
#define LINE_CHUNK_SIZE (N_STREAM_LOCAL-1)

int stream_readline(stream_t *dest, stream_t *s, char delim)
{
    char chunk[LINE_CHUNK_SIZE];
    int i, cnt, total=0;

    if (stream_eof(s))
        return 0;

    do {
        cnt = stream_read(s, chunk, LINE_CHUNK_SIZE);
        for(i=0; i < cnt; i++) {
            if (chunk[i] == delim)
                break;
        }
        if (i < cnt) {
            total += stream_write(dest, chunk, i+1);
            stream_skip(s, -(cnt-(i+1)));
            break;
        }
        total += stream_write(dest, chunk, cnt);
    } while (!stream_eof(s));

    return total;
}

/* extract one line of text from a stream, into a memory buffer.
   a pointer to the buffer is returned in *pbuf, the size of the buffer
   in *psz, and the number of characters in the line (including newline) is
   returned. the line may contain NULs, but there will also be a NUL
   terminator as the last character in the buffer.

   This routine's behavior is not exactly the same as GNU getline. In
   particular, it always allocates a buffer.
*/
int stream_getline(stream_t *s, char **pbuf, size_t *psz)
{
    stream_t buf;

    memstream_new(&buf, 0);

    stream_readline(&buf, s, '\n');

    *pbuf = stream_take_buffer(&buf, psz);

    return *psz-1;
}

int stream_get_stringz(stream_t *dest, stream_t *src)
{
    return stream_readline(dest, src, '\0');
}

/* get n UTF-8 characters */
int stream_get_stringn(stream_t *dest, stream_t *src, size_t c)
{
    u_int32_t wc;
    size_t cnt=0;

    while (c > 0) {
        if (stream_get_char(src, &wc) == 0 ||
            stream_put_char(dest, wc) == 0)
            break;
        c--;
        cnt++;
    }
    return cnt;
}

/* TODO: change API to allow passing a heap-allocated page-aligned
   chunk to reuse on each copy. if it's NULL we alloc our own. */
int stream_copy(stream_t *to, stream_t *from, size_t nbytes, bool_t all)
{
    int c=0, cnt, wc, rdc;
    char buf[CHUNK_SIZE];
    int remain = all ? CHUNK_SIZE : nbytes;

    if (is_memstream(to)) {
        // avoid extra copy, read directly into memstream
        while (!stream_eof(from)) {
            if (all) {
                rdc = CHUNK_SIZE;
            }
            else {
                /* if !all, only 1 call to stream_read needed */
                rdc = nbytes;
            }

            if (ms_realloc(to, to->pos + rdc) == NULL) {
                rdc = to->maxsize - to->pos;
                if (rdc == 0)
                    break;
            }
            cnt = stream_read(from, &to->s[to->pos], rdc);
            wc = cnt;
            to->pos += wc;
            if (to->pos > to->size) {
                to->size = to->pos;
                to->s[to->size] = '\0';
            }
            c += wc;

            if (!all)
                remain -= wc;
            if (wc < rdc || remain == 0)
                break;
        }
    }
    else if (is_memstream(from)) {
        while (1) {
            if (all) {
                rdc = CHUNK_SIZE;
            }
            else {
                /* if !all, only 1 call to stream_read needed */
                rdc = nbytes;
            }
            // check for source out of data
            if (from->size - from->pos < rdc) {
                remain = rdc = from->size - from->pos;
            }
            cnt = stream_write(to, &from->s[from->pos], rdc);
            wc = cnt;
            from->pos += wc;
            c += wc;
            if (!all)
                remain -= wc;
            if (wc < rdc || remain == 0)
                break;
        }
    }
    else {
        while (!stream_eof(from)) {
            rdc = remain>CHUNK_SIZE ? CHUNK_SIZE : remain;

            cnt = stream_read(from, buf, rdc);
            wc = stream_write(to, buf, cnt);
            c += wc;

            if (!all)
                remain -= wc;
            if (wc < rdc || remain == 0)
                break;
        }
    }
    return c;
}


/* serialization functions */

/* nbytes is either 4 or 8 */
size_t stream_get_offset(stream_t *s, off_t *po, int nbytes)
{
    size_t c;
    int64_t off64;
    int32_t off32;

    if (nbytes == 4) {
        c = stream_read(s, (char*)&off32, nbytes);
        if (c < nbytes)
            return c;
        if (s->byteswap)
            off32 = bswap_32(off32);
        /* OK on either system since off_t is >= off32 in size */
        *po = (off_t)off32;
    }
    else {
        c = stream_read(s, (char*)&off64, nbytes);
        if (c < nbytes)
            return c;
        if (s->byteswap)
            off64 = bswap_64(off64);
        if (sizeof(off_t) == 8) {
            *po = (off_t)off64;
        }
        else {
            if (off64 <= INT_MAX && off64 >= INT_MIN) {
                // downcast safe
                *po = (off_t)off64;
            }
            else {
                cerror("I/O error: 64-bit offset truncated on input.\n");
            }
        }
    }

    return c;
}

size_t stream_put_offset(stream_t *s, off_t o, int nbytes)
{
    int64_t off64;
    int32_t off32;

    if (nbytes == 4) {
        if (sizeof(off_t) == 8 && (o > INT_MAX || o < INT_MIN)) {
            cerror("I/O error: 64-bit offset truncated on output.\n");
        }
        off32 = (int32_t)o;
        if (s->byteswap)
            off32 = bswap_32(off32);
        return stream_write(s, (char*)&off32, 4);
    }
    off64 = (int64_t)o;
    if (s->byteswap)
        off64 = bswap_64(off64);
    return stream_write(s, (char*)&off64, 8);
}
