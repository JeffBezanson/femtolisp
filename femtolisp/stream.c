#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include "llt.h"
#include "flisp.h"

static value_t streamsym;
static fltype_t *streamtype;

void print_stream(value_t v, ios_t *f, int princ)
{
}

void free_stream(value_t self)
{
}

void relocate_stream(value_t oldv, value_t newv)
{
}

cvtable_t stream_vtable = { print_stream, relocate_stream, free_stream, NULL };

int isstream(value_t v)
{
    return iscvalue(v) && cv_class((cvalue_t*)ptr(v)) == streamtype;
}

value_t fl_streamp(value_t *args, uint32_t nargs)
{
    argcount("stream?", nargs, 1);
    return isstream(args[0]) ? FL_T : FL_F;
}

static ios_t *tostream(value_t v, char *fname)
{
    if (!isstream(v))
        type_error(fname, "stream", v);
    return (ios_t*)cv_data((cvalue_t*)ptr(v));
}

static builtinspec_t streamfunc_info[] = {
    { "stream?", fl_streamp },
    { NULL, NULL }
};

void stream_init()
{
    streamsym = symbol("stream");
    streamtype = define_opaque_type(streamsym, sizeof(ios_t),
                                    &stream_vtable, NULL);
    assign_global_builtins(streamfunc_info);
}
