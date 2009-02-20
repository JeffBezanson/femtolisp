#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include "llt.h"
#include "flisp.h"

static value_t iostreamsym, rdsym, wrsym, apsym, crsym, truncsym, instrsym;
static fltype_t *iostreamtype;

void print_iostream(value_t v, ios_t *f, int princ)
{
    (void)v;
    (void)princ;
    fl_print_str("#<io stream>", f);
}

void free_iostream(value_t self)
{
    ios_t *s = value2c(ios_t*, self);
    ios_close(s);
}

void relocate_iostream(value_t oldv, value_t newv)
{
    ios_t *olds = value2c(ios_t*, oldv);
    ios_t *news = value2c(ios_t*, newv);
    cvalue_t *cv = (cvalue_t*)ptr(oldv);
    if (isinlined(cv)) {
        if (olds->buf == &olds->local[0]) {
            news->buf = &news->local[0];
        }
    }
}

cvtable_t iostream_vtable = { print_iostream, relocate_iostream,
                              free_iostream, NULL };

int isiostream(value_t v)
{
    return iscvalue(v) && cv_class((cvalue_t*)ptr(v)) == iostreamtype;
}

value_t fl_iostreamp(value_t *args, uint32_t nargs)
{
    argcount("iostream?", nargs, 1);
    return isiostream(args[0]) ? FL_T : FL_F;
}

static ios_t *toiostream(value_t v, char *fname)
{
    if (!isiostream(v))
        type_error(fname, "iostream", v);
    return value2c(ios_t*, v);
}

value_t fl_file(value_t *args, uint32_t nargs)
{
    if (nargs < 1)
        argcount("file", nargs, 1);
    int i, r=1, w=0, c=0, t=0, a=0;
    for(i=1; i < (int)nargs; i++) {
        if      (args[i] == wrsym)    w = 1;
        else if (args[i] == apsym)    a = 1;
        else if (args[i] == crsym)    c = 1;
        else if (args[i] == truncsym) t = 1;
    }
    value_t f = cvalue(iostreamtype, sizeof(ios_t));
    char *fname = tostring(args[0], "file");
    ios_t *s = value2c(ios_t*, f);
    if (ios_file(s, fname, r, w, c, t) == NULL)
        lerror(IOError, "file: could not open \"%s\"", fname);
    if (a) ios_seek_end(s);
    return f;
}

value_t fl_read(value_t *args, u_int32_t nargs)
{
    if (nargs > 1) {
        argcount("read", nargs, 1);
    }
    else if (nargs == 0) {
        PUSH(symbol_value(instrsym));
        args = &Stack[SP-1];
    }
    ios_t *s = toiostream(args[0], "read");
    // temporarily pin the stream while reading
    ios_t temp = *s;
    if (s->buf == &s->local[0])
        temp.buf = &temp.local[0];
    value_t v = read_sexpr(&temp);
    s = value2c(ios_t*, args[0]);
    *s = temp;
    return v;
}

value_t fl_iogetc(value_t *args, u_int32_t nargs)
{
    argcount("io.getc", nargs, 1);
    ios_t *s = toiostream(args[0], "io.getc");
    uint32_t wc;
    if (ios_getutf8(s, &wc) == IOS_EOF)
        lerror(IOError, "io.getc: end of file reached");
    return mk_wchar(wc);
}

value_t fl_ioflush(value_t *args, u_int32_t nargs)
{
    argcount("io.flush", nargs, 1);
    ios_t *s = toiostream(args[0], "io.flush");
    if (ios_flush(s) != 0)
        return FL_F;
    return FL_T;
}

value_t fl_ioclose(value_t *args, u_int32_t nargs)
{
    argcount("io.close", nargs, 1);
    ios_t *s = toiostream(args[0], "io.close");
    ios_close(s);
    return FL_T;
}

value_t fl_iopurge(value_t *args, u_int32_t nargs)
{
    argcount("io.discardbuffer", nargs, 1);
    ios_t *s = toiostream(args[0], "io.discardbuffer");
    ios_purge(s);
    return FL_T;
}

value_t fl_ioeof(value_t *args, u_int32_t nargs)
{
    argcount("io.eof?", nargs, 1);
    ios_t *s = toiostream(args[0], "io.eof?");
    return (ios_eof(s) ? FL_T : FL_F);
}

static void do_ioprint(value_t *args, u_int32_t nargs, int princ, char *fname)
{
    if (nargs < 2)
        argcount(fname, nargs, 2);
    ios_t *s = toiostream(args[0], fname);
    unsigned i;
    for (i=1; i < nargs; i++) {
        print(s, args[i], princ);
    }
}
value_t fl_ioprint(value_t *args, u_int32_t nargs)
{
    do_ioprint(args, nargs, 0, "io.print");
    return args[nargs-1];
}
value_t fl_ioprinc(value_t *args, u_int32_t nargs)
{
    do_ioprint(args, nargs, 1, "io.princ");
    return args[nargs-1];
}

static builtinspec_t iostreamfunc_info[] = {
    { "iostream?", fl_iostreamp },
    { "file", fl_file },
    { "read", fl_read },
    { "io.print", fl_ioprint },
    { "io.princ", fl_ioprinc },
    { "io.flush", fl_ioflush },
    { "io.close", fl_ioclose },
    { "io.eof?" , fl_ioeof },
    { "io.getc" , fl_iogetc },
    { "io.discardbuffer", fl_iopurge },
    { NULL, NULL }
};

void iostream_init()
{
    iostreamsym = symbol("iostream");
    rdsym = symbol(":read");
    wrsym = symbol(":write");
    apsym = symbol(":append");
    crsym = symbol(":create");
    truncsym = symbol(":truncate");
    instrsym = symbol("*input-stream*");
    iostreamtype = define_opaque_type(iostreamsym, sizeof(ios_t),
                                      &iostream_vtable, NULL);
    assign_global_builtins(iostreamfunc_info);

    setc(symbol("*stdout*"), cvalue_from_ref(iostreamtype, ios_stdout,
                                             sizeof(ios_t), NIL));
    setc(symbol("*stderr*"), cvalue_from_ref(iostreamtype, ios_stderr,
                                             sizeof(ios_t), NIL));
    setc(symbol("*stdin*" ), cvalue_from_ref(iostreamtype, ios_stdin,
                                             sizeof(ios_t), NIL));
}
