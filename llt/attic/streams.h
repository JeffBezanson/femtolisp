#ifndef __STREAMS_H_
#define __STREAMS_H_

struct _stream;

// numeric type codes
#define T_BOOL   0x000
#define T_BYTE   0x001
#define T_SHORT  0x002
#define T_INT    0x003
#define T_FLOAT  0x004
#define T_DOUBLE 0x005
#define T_INT64   0x006
//#define T_LDOUBLE 0x007
#define T_UNSIGNED 0x008
#define T_CPLX     0x010

#define T_TYPEMASK 0x1f  /* bits related to numeric type */
#define T_TYPESIZE 0x07  /* bits related to type size */

#define is_type(a, t) (((a) & T_TYPESIZE) == (t))
// type_bitseq tells whether 2 types are bit-representation compatible
#define type_bitseq(a, b) (((a) & ~T_UNSIGNED) == ((b) & ~T_UNSIGNED))

extern unsigned int type_sizes[];

typedef struct {
    void (*free_func)(struct _stream *s);
    /* these return -1 on error, or new position if successful */
    /* set absolute position */
    off_t (*seek)(struct _stream *s, off_t where);
    /* seek to end (past last byte) */
    off_t (*seek_end)(struct _stream *s);
    /* move relative to current position */
    off_t (*skip)(struct _stream *s, off_t offs);

    /* these return # of bytes read/written, 0 on error */
    size_t (*read)(struct _stream *s, char *dest, size_t size);
    size_t (*write)(struct _stream *s, char *data, size_t size);

    /* truncate a stream to the given length */
    size_t (*trunc)(struct _stream *s, size_t size);

    /* no data left? */
    bool_t (*eof)(struct _stream *s);

    /* sync bit buffer, and sync to hardware if applicable */
    void (*flush)(struct _stream *s);
    /* could add fsync() call for durable writes */
} stream_interface_t;

extern stream_interface_t fs_funcs;
extern stream_interface_t ms_funcs;
extern stream_interface_t ps_funcs;

#define is_memstream(s)  (((stream_t*)s)->funcs==&ms_funcs)
#define is_filestream(s) (((stream_t*)s)->funcs==&fs_funcs)
#define is_pipestream(s) (((stream_t*)s)->funcs==&ps_funcs)

/* general i/o chunk size */
#define CHUNK_SIZE 4096

/* this should be a multiple of 4; otherwise the struct gets padded to
   the next multiple of 4 anyway, wasting space */
#define N_STREAM_LOCAL 40

typedef struct _stream {
    /* unfortunately, it seems that pos needs to be a signed type to be
       compatible with OS functions. */
    off_t pos;
    byte_t bitpos;     /* offset within a byte, for writing individual bits */
    byte_t bitbuf;     /* a copy of the byte at the current stream position */
    char ungotc;
    struct {
        /* does bit buffer need to be written? */
        unsigned char dirty:1;
        unsigned char byteswap:1;
        unsigned char readonly:1;
        unsigned char ungot:1;
    };

    stream_interface_t *funcs;

    /* stream-specific data */
    union {
        char *name;
        size_t maxsize;
        int rd; // pipe read descriptor
    };
    union {
        FILE *fptr;
        char *data;
        char *s;
        int wd; // pipe write descriptor
    };
    union {
        /* this is always a size in BYTES */
        size_t size;
        size_t len;
    };
    char local[N_STREAM_LOCAL];
} stream_t;

#include <stdio.h>

stream_t *filestream_new(stream_t *s, char *fName,
                         bool_t create, bool_t rewrite);
stream_t *memstream_new(stream_t *s, size_t initsize);
stream_t *pipestream_new(stream_t *s, int flags);
stream_t *stream_fromfile(stream_t *s, FILE *f, char *name);
//string_t *string_new(int len);
//string_t *string_fromc(char *s);
stream_t *memstream_copy(stream_t *s);
void stream_free(stream_t *s);
void stream_flush(stream_t *s);


/* high level stream functions */

/* 'all' means copy to end of stream */
int stream_copy(stream_t *to, stream_t *from, size_t nbytes, bool_t all);

int stream_put_num(stream_t *s, char *data, u_int32_t type);
int stream_put_int(stream_t *s, int n);
int stream_put_char(stream_t *s, u_int32_t wc);
int stream_put_stringz(stream_t *s, char *str, bool_t nulterm);

/* single-bit I/O - even works for mixed reads and writes.
   mixing bit-level I/O with normal byte stream I/O has undefined effects and
   will almost certainly destroy your file. however, it is safe to switch
   between bit and byte I/O if you call stream_flush in between. */
void stream_put_bit(stream_t *s, int bit);

/* warning: this uses a fixed-size buffer. it is intended only for printing
   normal things like "a= %d". if you might be printing a large buffer, you
   should use stream i/o functions directly. */
int stream_printf(stream_t *s, char *format, ...);


/* high level stream functions - input */

int stream_get_num(stream_t *s, char *data, u_int32_t type);
int stream_get_char(stream_t *s, u_int32_t *pwc);
int stream_get_stringz(stream_t *dest, stream_t *src);
int stream_get_stringn(stream_t *dest, stream_t *src, size_t c);
int stream_readline(stream_t *dest, stream_t *s, char delim);
int stream_getline(stream_t *s, char **pbuf, size_t *psz);
/* returns # of bits read (0 or 1) */
int stream_get_bit(stream_t *s, int *pbit);

int stream_nextchar(stream_t *s);
int stream_prevchar(stream_t *s);

void stream_close(stream_t *s);

/* TODO */
// stream_fgetc
// stream_ungetc

/* get underlying file descriptors, -1 if none */
int stream_readfd(stream_t *s);
int stream_writefd(stream_t *s);

/*
  low level stream functions

  Streams are intended to provide a uniform function interface to various
  kinds of byte streams.

  The eight low-level stream functions (below) are intended to be light weight.
  It must be easy to implement reasonably efficient higher-level stream
  functions, therefore any complexity required to make (for example) single
  byte reads and writes efficient must be implemented by the stream.

  Note that you can implement file streams using fread(), fwrite(), etc.
  because buffering is already implemented in every standard C library. These
  calls do not make system calls in general, and are perfectly fine to use.
*/

#define stream_seek(s, w)      (s)->funcs->seek(s, w)
#define stream_seek_end(s)     (s)->funcs->seek_end(s)
#define stream_skip(s, o)      (s)->funcs->skip(s, o)
#define stream_read(s, d, sz)  (s)->funcs->read(s, (char*)d, sz)
#define stream_write(s, d, sz) (s)->funcs->write(s, (char*)d, sz)
#define stream_trunc(s, sz)    (s)->funcs->trunc(s, sz)
#define stream_eof(s)          (s)->funcs->eof(s)


STATIC_INLINE size_t stream_put_byte(stream_t *s, byte_t b)
{
    return stream_write(s, (char*)&b, 1);
}

STATIC_INLINE size_t stream_get_byte(stream_t *s, byte_t *pb)
{
    return stream_read(s, (char*)pb, 1);
}

#define stream_puts(s, str) stream_write(s, str, strlen(str))

/*
  stream_take_buffer

  This lets you get the data of a stream without having to copy it. In order
  not to either lose the buffer or free it twice, this operation effectively
  empties the stream. In other words, size goes to 0, data becomes NULL.
  Whoever called the function takes full responsibility for the buffer.
  You must free it eventually.
  "size" gets set to the size of the data in the buffer.
*/
char *stream_take_buffer(stream_t *s, size_t *size);

#endif
