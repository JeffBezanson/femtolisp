#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <errno.h>
#include <wchar.h>

#ifdef WIN32
#include <malloc.h>
#include <io.h>
#include <fcntl.h>
#define fileno _fileno
#else
#include <unistd.h>
#include <sys/time.h>
#include <sys/select.h>
#endif

#include "dtypes.h"
#include "utils.h"
#include "utf8.h"
#include "ios.h"
#include "socket.h"
#include "timefuncs.h"

/* OS-level primitive wrappers */

static int _fd_available(long fd)
{
#ifndef WIN32
    fd_set set;
    struct timeval tv = {0, 0};

    FD_ZERO(&set);
    FD_SET(fd, &set);
    return (select(fd+1, &set, NULL, NULL, &tv)!=0);
#else
    return 0;
#endif
}

// poll for read, unless forwrite!=0
static void _fd_poll(long fd, int forwrite)
{
#ifndef WIN32
    fd_set set;

    FD_ZERO(&set);
    FD_SET(fd, &set);
    if (forwrite)
        select(fd+1, NULL, &set, NULL, NULL);
    else
        select(fd+1, &set, NULL, NULL, NULL);
#else
#endif
}

static int _enonfatal(int err)
{
    return (err == EAGAIN || err == EINPROGRESS || err == EINTR ||
            err == EWOULDBLOCK);
}

#define SLEEP_TIME 5//ms

// return error code, #bytes read in *nread
// these wrappers retry operations until success or a fatal error
static int _os_read(long fd, void *buf, size_t n, size_t *nread)
{
    ssize_t r;

    while (1) {
        r = read((int)fd, buf, n);
        if (r > -1) {
            *nread = (size_t)r;
            return 0;
        }
        if (!_enonfatal(errno)) {
            *nread = 0;
            return errno;
        }
        sleep_ms(SLEEP_TIME);
    }
    return 0;
}

static int _os_read_all(long fd, void *buf, size_t n, size_t *nread)
{
    size_t got;

    *nread = 0;

    while (n>0) {
        int err = _os_read(fd, buf, n, &got);
        n -= got;
        *nread += got;
        buf += got;
        if (err)
            return err;
        if (got == 0)
            _fd_poll(fd, 0);
    }
    return 0;
}

static int _os_write(long fd, void *buf, size_t n, size_t *nwritten)
{
    ssize_t r;

    while (1) {
        r = write((int)fd, buf, n);
        if (r > -1) {
            *nwritten = (size_t)r;
            return 0;
        }
        if (!_enonfatal(errno)) {
            *nwritten = 0;
            return errno;
        }
        sleep_ms(5);
    }
    return 0;
}

static int _os_write_all(long fd, void *buf, size_t n, size_t *nwritten)
{
    size_t wrote;

    *nwritten = 0;

    while (n>0) {
        int err = _os_write(fd, buf, n, &wrote);
        n -= wrote;
        *nwritten += wrote;
        buf += wrote;
        if (err)
            return err;
        if (wrote == 0)
            _fd_poll(fd, 1);
    }
    return 0;
}


/* internal utility functions */

static char *_buf_realloc(ios_t *s, size_t sz)
{
    char *temp;

    if (sz <= s->maxsize)
        return s->buf;

    if ((s->buf==NULL || s->buf==&s->local[0]) && (sz <= IOS_INLSIZE)) {
        /* TODO: if we want to allow shrinking, see if the buffer shrank
           down to this size, in which case we need to copy. */
        s->buf = &s->local[0];
        s->maxsize = IOS_INLSIZE;
        s->ownbuf = 1;
        return s->buf;
    }
    else if (s->ownbuf && s->buf != &s->local[0]) {
        // if we own the buffer we're free to resize it
        // always allocate 1 bigger in case user wants to add a NUL
        // terminator after taking over the buffer
        temp = realloc(s->buf, sz+1);
        if (temp == NULL)
            return NULL;
    }
    else {
        temp = malloc(sz+1);
        if (temp == NULL)
            return NULL;
        s->ownbuf = 1;
        if (s->size > 0)
            memcpy(temp, s->buf, s->size);
    }

    s->buf = temp;
    s->maxsize = sz;
    return s->buf;
}

// write a block of data into the buffer at the current position, resizing
// if necessary. returns # written.
static size_t _writebuf_force(ios_t *s, char *data, size_t n)
{
    size_t amt;
    size_t newsize;

    if (n == 0)
        return 0;

    if (s->bpos + n > s->size) {
        if (s->bpos + n > s->maxsize) {
            /* TODO: here you might want to add a mechanism for limiting
               the growth of the stream. */
            newsize = s->maxsize * 2;
            while (s->bpos + n > newsize)
                newsize *= 2;
            if (_buf_realloc(s, newsize) == NULL) {
                /* no more space; write as much as we can */
                amt = s->maxsize - s->bpos;
                if (amt > 0) {
                    memcpy(&s->buf[s->bpos], data, amt);
                }
                s->bpos += amt;
                s->size = s->maxsize;
                return amt;
            }
        }
        s->size = s->bpos + n;
    }
    memcpy(&s->buf[s->bpos], data, n);
    s->bpos += n;

    return n;
}


/* interface functions, low level */

size_t ios_read(ios_t *s, char *dest, size_t n, int all)
{
    size_t tot = 0;
    size_t got, avail;
    int result;

    while (n > 0) {
        avail = s->size - s->bpos;
        
        if (avail >= n) {
            memcpy(dest, s->buf + s->bpos, n);
            s->bpos += n;
            return tot+n;
        }
        
        if (avail > 0) {
            memcpy(dest, s->buf + s->bpos, avail);
        }
        if (s->bm == bm_mem || s->fd == -1) {
            // can't get any more data
            s->bpos += avail;
            return avail;
        }
        else {
            dest += avail;
            n -= avail;
            tot += avail;

            ios_flush(s);
            s->bpos = s->size = 0;
            s->state = bst_rd;
        }
        
        if (n > (s->maxsize - (s->maxsize>>4))) {
            // doesn't fit comfortably in buffer; go direct
            if (all)
                result = _os_read_all(s->fd, dest, n, &got);
            else
                result = _os_read(s->fd, dest, n, &got);
            return tot+got;
        }
        else {
            // refill buffer
            if (_os_read(s->fd, s->buf, s->maxsize, &got)) {
                return tot;
            }
            if (got == 0) {
                if (all)
                    _fd_poll(s->fd, 0);
                else
                    return tot;
            }
            s->size = got;
        }
    }

    return tot;
}

size_t ios_write(ios_t *s, char *data, size_t n)
{
}

off_t ios_seek(ios_t *s, off_t pos)
{
}

off_t ios_seek_end(ios_t *s)
{
}

off_t ios_skip(ios_t *s, off_t offs)
{
}

off_t ios_pos(ios_t *s)
{
    if (s->bm == bm_mem)
        return (off_t)s->bpos;

    off_t fdpos = lseek(s->fd, 0, SEEK_CUR);
    if (fdpos == (off_t)-1)
        return fdpos;

    if (s->state == bst_wr)
        fdpos += s->bpos;
    else if (s->state == bst_rd)
        fdpos -= (s->size - s->bpos);
    return fdpos;
}

size_t ios_trunc(ios_t *s, size_t size)
{
}

int ios_eof(ios_t *s)
{
    if (s->bm == bm_mem)
        return (s->bpos >= s->size);
    if (s->fd == -1)
        return 1;
    // todo
}

static void _discard_partial_buffer(ios_t *s)
{
    // this function preserves the invariant that data to write
    // begins at the beginning of the buffer, and s->size refers
    // to how much valid file data is stored in the buffer.

    // this needs to be called when normal operation is interrupted in
    // the middle of the buffer. "normal operation" is reading or
    // writing to the end of the buffer. this happens e.g. when flushing.
    size_t delta = 0;
    if (s->ndirty && s->size > s->ndirty) {
        delta = s->size - s->ndirty;
        memmove(s->buf, s->buf + s->ndirty, delta);
    }
    s->size -= s->ndirty;
    s->bpos -= s->ndirty;
}

int ios_flush(ios_t *s)
{
    if (s->ndirty == 0 || s->bm == bm_mem || s->buf == NULL)
        return 0;
    if (s->fd == -1)
        return -1;

    if (s->state == bst_rd) {
        if (lseek(s->fd, -(off_t)s->size, SEEK_CUR) == (off_t)-1) {
        }
    }

    size_t nw, ntowrite=s->ndirty;
    // todo: this should use sendall
    int err = _os_write(s->fd, s->buf, ntowrite, &nw);
    // todo: try recovering from some kinds of errors (e.g. retry)

    if (s->state == bst_rd) {
        if (lseek(s->fd, s->size - nw, SEEK_CUR) == (off_t)-1) {
        }
    }

    if (s->ndirty <= s->bpos) {
        // in this case assume we're done with the first part of the buffer
        _discard_partial_buffer(s);
    }
    s->ndirty = 0;

    if (err)
        return err;
    if (nw < ntowrite)
        return -1;
    return 0;
}

void ios_close(ios_t *s)
{
    ios_flush(s);
    if (s->fd != -1 && s->ownfd)
        close(s->fd);
    s->fd = -1;
}

char *ios_takebuf(ios_t *s, size_t *psize)
{
    char *buf;

    ios_flush(s);

    if (s->buf == &s->local[0]) {
        buf = malloc(s->size+1);
        if (buf == NULL)
            return NULL;
        if (s->size)
            memcpy(buf, s->buf, s->size);
        buf[s->size] = '\0';
    }
    else {
        buf = s->buf;
    }

    *psize = s->size+1;  // buffer is actually 1 bigger for terminating NUL

    /* empty stream and reinitialize */
    if (s->bm == bm_mem || s->bm == bm_none) {
        s->buf = &s->local[0];
        s->maxsize = IOS_INLSIZE;
    }
    else {
        s->buf = NULL;
        _buf_realloc(s, IOS_BUFSIZE);
    }
    s->size = s->bpos = 0;

    return buf;
}

int ios_setbuf(ios_t *s, char *buf, size_t size, int own)
{
    ios_flush(s);
    size_t nvalid=0;

    nvalid = (size < s->size) ? size : s->size;
    memcpy(buf, s->buf, nvalid);
    if (s->bpos > nvalid) {
        // truncated
        s->bpos = nvalid;
    }
    s->size = nvalid;

    if (s->buf!=NULL && s->ownbuf && s->buf!=&s->local[0])
        free(s->buf);
    s->buf = buf;
    s->maxsize = size;
    s->ownbuf = own;
    return 0;
}

int ios_bufmode(ios_t *s, bufmode_t mode)
{
    // no fd; can only do mem-only buffering
    if (s->fd == -1 && mode != bm_mem)
        return -1;
    s->bm = mode;
    return 0;
}

void ios_bswap(ios_t *s, int bswap)
{
    s->byteswap = !!bswap;
}

int ios_copy(ios_t *to, ios_t *from, size_t nbytes, bool_t all)
{
}


/* stream object initializers. we do no allocation. */

ios_t *ios_file(ios_t *s, char *fname, int create, int rewrite)
{
}

ios_t *ios_mem(ios_t *s, size_t initsize)
{
}

ios_t *ios_fd(ios_t *s, long fd)
{
}


/* higher level interface */

