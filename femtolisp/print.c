static htable_t printconses;
static u_int32_t printlabel;
static int print_pretty;
static int SCR_WIDTH = 80;

static int HPOS, VPOS;
static void outc(char c, ios_t *f)
{
    ios_putc(c, f);
    HPOS++;
}
static void outs(char *s, ios_t *f)
{
    ios_puts(s, f);
    HPOS += u8_strwidth(s);
}
static int outindent(int n, ios_t *f)
{
    // move back to left margin if we get too indented
    if (n > SCR_WIDTH-12)
        n = 2;
    int n0 = n;
    ios_putc('\n', f);
    VPOS++;
    HPOS = n;
    while (n >= 8) {
        ios_putc('\t', f);
        n -= 8;
    }
    while (n) {
        ios_putc(' ', f);
        n--;
    }
    return n0;
}

void fl_print_chr(char c, ios_t *f)
{
    outc(c, f);
}

void fl_print_str(char *s, ios_t *f)
{
    outs(s, f);
}

void print_traverse(value_t v)
{
    value_t *bp;
    while (iscons(v)) {
        if (ismarked(v)) {
            bp = (value_t*)ptrhash_bp(&printconses, (void*)v);
            if (*bp == (value_t)HT_NOTFOUND)
                *bp = fixnum(printlabel++);
            return;
        }
        mark_cons(v);
        print_traverse(car_(v));
        v = cdr_(v);
    }
    if (!ismanaged(v) || issymbol(v))
        return;
    if (ismarked(v)) {
        bp = (value_t*)ptrhash_bp(&printconses, (void*)v);
        if (*bp == (value_t)HT_NOTFOUND)
            *bp = fixnum(printlabel++);
        return;
    }
    if (isvector(v)) {
        mark_cons(v);
        unsigned int i;
        for(i=0; i < vector_size(v); i++)
            print_traverse(vector_elt(v,i));
    }
    else if (iscprim(v)) {
        mark_cons(v);
    }
    else {
        assert(iscvalue(v));
        cvalue_t *cv = (cvalue_t*)ptr(v);
        // don't consider shared references to ""
        if (!cv_isstr(cv) || cv_len(cv)!=0)
            mark_cons(v);
        fltype_t *t = cv_class(cv);
        if (t->vtable != NULL && t->vtable->print_traverse != NULL)
            t->vtable->print_traverse(v);
    }
}

static void print_symbol_name(ios_t *f, char *name)
{
    int i, escape=0, charescape=0;

    if ((name[0] == '\0') ||
        (name[0] == '.' && name[1] == '\0') ||
        (name[0] == '#') ||
        isnumtok(name, NULL))
        escape = 1;
    i=0;
    while (name[i]) {
        if (!symchar(name[i])) {
            escape = 1;
            if (name[i]=='|' || name[i]=='\\') {
                charescape = 1;
                break;
            }
        }
        i++;
    }
    if (escape) {
        if (charescape) {
            outc('|', f);
            i=0;
            while (name[i]) {
                if (name[i]=='|' || name[i]=='\\')
                    outc('\\', f);
                outc(name[i], f);
                i++;
            }
            outc('|', f);
        }
        else {
            outc('|', f);
            outs(name, f);
            outc('|', f);
        }
    }
    else {
        outs(name, f);
    }
}

/*
  The following implements a simple pretty-printing algorithm. This is
  an unlimited-width approach that doesn't require an extra pass.
  It uses some heuristics to guess whether an expression is "small",
  and avoids wrapping symbols across lines. The result is high
  performance and nice output for typical code. Quality is poor for
  pathological or deeply-nested expressions, but those are difficult
  to print anyway.
*/
#define SMALL_STR_LEN 20
static inline int tinyp(value_t v)
{
    if (issymbol(v))
        return (u8_strwidth(symbol_name(v)) < SMALL_STR_LEN);
    if (isstring(v))
        return (cv_len((cvalue_t*)ptr(v)) < SMALL_STR_LEN);
    return (isfixnum(v) || isbuiltinish(v));
}

static int smallp(value_t v)
{
    if (tinyp(v)) return 1;
    if (isnumber(v)) return 1;
    if (iscons(v)) {
        if (tinyp(car_(v)) && (tinyp(cdr_(v)) ||
                               (iscons(cdr_(v)) && tinyp(car_(cdr_(v))) &&
                                cdr_(cdr_(v))==NIL)))
            return 1;
        return 0;
    }
    if (isvector(v)) {
        size_t s = vector_size(v);
        return (s == 0 || (tinyp(vector_elt(v,0)) &&
                           (s == 1 || (s == 2 &&
                                       tinyp(vector_elt(v,1))))));
    }
    return 0;
}

static int specialindent(value_t head)
{
    // indent these forms 2 spaces, not lined up with the first argument
    if (head == LAMBDA || head == TRYCATCH || head == definesym ||
        head == defmacrosym || head == forsym || head == labelsym)
        return 2;
    return -1;
}

static int lengthestimate(value_t v)
{
    // get the width of an expression if we can do so cheaply
    if (issymbol(v))
        return u8_strwidth(symbol_name(v));
    return -1;
}

static int allsmallp(value_t v)
{
    int n = 1;
    while (iscons(v)) {
        if (!smallp(car_(v)))
            return 0;
        v = cdr_(v);
        n++;
        if (n > 25)
            return n;
    }
    return n;
}

static int indentafter3(value_t head, value_t v)
{
    // for certain X always indent (X a b c) after b
    return ((head == forsym) && !allsmallp(cdr_(v)));
}

static int indentafter2(value_t head, value_t v)
{
    // for certain X always indent (X a b) after a
    return ((head == definesym || head == defmacrosym) &&
            !allsmallp(cdr_(v)));
}

static int indentevery(value_t v)
{
    // indent before every subform of a special form, unless every
    // subform is "small"
    value_t c = car_(v);
    if (c == LAMBDA || c == labelsym || c == setqsym)
        return 0;
    value_t f;
    if (issymbol(c) && (f=((symbol_t*)ptr(c))->syntax) && isspecial(f))
        return !allsmallp(cdr_(v));
    return 0;
}

static int blockindent(value_t v)
{
    // in this case we switch to block indent mode, where the head
    // is no longer considered special:
    // (a b c d e
    //  f g h i j)
    return (allsmallp(v) > 9);
}

static void print_pair(ios_t *f, value_t v, int princ)
{
    value_t cd;
    char *op = NULL;
    if (iscons(cdr_(v)) && cdr_(cdr_(v)) == NIL &&
        !ptrhash_has(&printconses, (void*)cdr_(v)) &&
        (((car_(v) == QUOTE)     && (op = "'"))  ||
         ((car_(v) == BACKQUOTE) && (op = "`"))  ||
         ((car_(v) == COMMA)     && (op = ","))  ||
         ((car_(v) == COMMAAT)   && (op = ",@")) ||
         ((car_(v) == COMMADOT)  && (op = ",.")))) {
        // special prefix syntax
        unmark_cons(v);
        unmark_cons(cdr_(v));
        outs(op, f);
        fl_print_child(f, car_(cdr_(v)), princ);
        return;
    }
    int startpos = HPOS;
    outc('(', f);
    int newindent=HPOS, blk=blockindent(v);
    int lastv, n=0, si, ind=0, est, always=0, nextsmall, thistiny;
    if (!blk) always = indentevery(v);
    value_t head = car_(v);
    int after3 = indentafter3(head, v);
    int after2 = indentafter2(head, v);
    int n_unindented = 1;
    while (1) {
        lastv = VPOS;
        unmark_cons(v);
        fl_print_child(f, car_(v), princ);
        cd = cdr_(v);
        if (!iscons(cd) || ptrhash_has(&printconses, (void*)cd)) {
            if (cd != NIL) {
                outs(" . ", f);
                fl_print_child(f, cd, princ);
            }
            outc(')', f);
            break;
        }

        if (princ || !print_pretty ||
            ((head == LAMBDA || head == labelsym) && n == 0)) {
            // never break line before lambda-list or in princ
            ind = 0;
        }
        else {
            est = lengthestimate(car_(cd));
            nextsmall = smallp(car_(cd));
            thistiny = tinyp(car_(v));
            ind = (((VPOS > lastv) ||
                    (HPOS>SCR_WIDTH/2 && !nextsmall && !thistiny && n>0)) ||
                   
                   (HPOS > SCR_WIDTH-4) ||
                   
                   (est!=-1 && (HPOS+est > SCR_WIDTH-2)) ||
                   
                   ((head == LAMBDA || head == labelsym) && !nextsmall) ||
                   
                   (n > 0 && always) ||
                   
                   (n == 2 && after3) ||
                   (n == 1 && after2) ||

                   (n_unindented >= 3 && !nextsmall) ||
                   
                   (n == 0 && !smallp(head)));
        }

        if (ind) {
            newindent = outindent(newindent, f);
            n_unindented = 1;
        }
        else {
            n_unindented++;
            outc(' ', f);
            if (n==0) {
                // set indent level after printing head
                si = specialindent(head);
                if (si != -1)
                    newindent = startpos + si;
                else if (!blk)
                    newindent = HPOS;
            }
        }
        n++;
        v = cd;
    }
}

static void cvalue_print(ios_t *f, value_t v, int princ);

void fl_print_child(ios_t *f, value_t v, int princ)
{
    value_t label;
    char *name;

    switch (tag(v)) {
    case TAG_NUM :
    case TAG_NUM1: HPOS+=ios_printf(f, "%ld", numval(v)); break;
    case TAG_SYM:
        name = symbol_name(v);
        if (princ)
            outs(name, f);
        else if (ismanaged(v)) {
            outs("#:", f);
            outs(name, f);
        }
        else
            print_symbol_name(f, name);
        break;
    case TAG_BUILTIN:
        if (v == FL_T) {
            outs("#t", f);
            break;
        }
        if (v == FL_F) {
            outs("#f", f);
            break;
        }
        if (v == NIL) {
            outs("()", f);
            break;
        }
        if (isbuiltin(v)) {
            if (!princ)
                outs("#.", f);
            outs(builtin_names[uintval(v)], f);
            break;
        }
        label = (value_t)ptrhash_get(&reverse_dlsym_lookup_table, ptr(v));
        if (label == (value_t)HT_NOTFOUND) {
            HPOS += ios_printf(f, "#<builtin @0x%08lx>",
                               (unsigned long)(builtin_t)ptr(v));
        }
        else {
            if (princ)
                outs(symbol_name(label), f);
            else
                HPOS += ios_printf(f, "#builtin(%s)", symbol_name(label));
        }
        break;
    case TAG_CVALUE:
    case TAG_CPRIM:
      if (v == UNBOUND) { outs("#<undefined>", f); break; }
    case TAG_VECTOR:
    case TAG_CONS:
        if ((label=(value_t)ptrhash_get(&printconses, (void*)v)) !=
            (value_t)HT_NOTFOUND) {
            if (!ismarked(v)) {
                HPOS+=ios_printf(f, "#%ld#", numval(label));
                return;
            }
            HPOS+=ios_printf(f, "#%ld=", numval(label));
        }
        if (isvector(v)) {
            outc('[', f);
            int newindent = HPOS, est;
            unmark_cons(v);
            int i, sz = vector_size(v);
            for(i=0; i < sz; i++) {
                fl_print_child(f, vector_elt(v,i), princ);
                if (i < sz-1) {
                    if (princ) {
                        outc(' ', f);
                    }
                    else {
                        est = lengthestimate(vector_elt(v,i+1));
                        if (HPOS > SCR_WIDTH-4 ||
                            (est!=-1 && (HPOS+est > SCR_WIDTH-2)) ||
                            (HPOS > SCR_WIDTH/2 &&
                             !smallp(vector_elt(v,i+1)) &&
                             !tinyp(vector_elt(v,i))))
                            newindent = outindent(newindent, f);
                        else
                            outc(' ', f);
                    }
                }
            }
            outc(']', f);
            break;
        }
        if (iscvalue(v) || iscprim(v)) {
            unmark_cons(v);
            cvalue_print(f, v, princ);
            break;
        }
        print_pair(f, v, princ);
        break;
    }
}

static void print_string(ios_t *f, char *str, size_t sz)
{
    char buf[512];
    size_t i = 0;

    outc('"', f);
    while (i < sz) {
        u8_escape(buf, sizeof(buf), str, &i, sz, 1, 0);
        outs(buf, f);
    }
    outc('"', f);
}

static numerictype_t sym_to_numtype(value_t type);

// 'weak' means we don't need to accurately reproduce the type, so
// for example #int32(0) can be printed as just 0. this is used
// printing in a context where a type is already implied, e.g. inside
// an array.
static void cvalue_printdata(ios_t *f, void *data, size_t len, value_t type,
                             int princ, int weak)
{
    int64_t tmp=0;

    if (type == bytesym) {
        unsigned char ch = *(unsigned char*)data;
        if (princ)
            outc(ch, f);
        else if (weak)
            HPOS+=ios_printf(f, "0x%hhx", ch);
        else
            HPOS+=ios_printf(f, "#byte(0x%hhx)", ch);
    }
    else if (type == wcharsym) {
        uint32_t wc = *(uint32_t*)data;
        char seq[8];
        if (princ || iswprint(wc)) {
            size_t nb = u8_toutf8(seq, sizeof(seq), &wc, 1);
            seq[nb] = '\0';
            // TODO: better multibyte handling
            if (!princ) outs("#\\", f);
            outs(seq, f);
        }
        else if (weak) {
            HPOS+=ios_printf(f, "%d", (int)wc);
        }
        else {
            HPOS+=ios_printf(f, "#%s(%d)", symbol_name(type), (int)wc);
        }
    }
    else if (type == int64sym
#ifdef BITS64
             || type == longsym
#endif
             ) {
        int64_t i64 = *(int64_t*)data;
        if (fits_fixnum(i64) || princ) {
            if (weak || princ)
                HPOS+=ios_printf(f, "%lld", i64);
            else
                HPOS+=ios_printf(f, "#%s(%lld)", symbol_name(type), i64);
        }
        else
            HPOS+=ios_printf(f, "#%s(0x%08x%08x)", symbol_name(type),
                             (uint32_t)(i64>>32),
                             (uint32_t)(i64));
    }
    else if (type == uint64sym
#ifdef BITS64
             || type == ulongsym
#endif
             ) {
        uint64_t ui64 = *(uint64_t*)data;
        if (fits_fixnum(ui64) || princ) {
            if (weak || princ)
                HPOS+=ios_printf(f, "%llu", ui64);
            else
                HPOS+=ios_printf(f, "#%s(%llu)", symbol_name(type), ui64);
        }
        else
            HPOS+=ios_printf(f, "#%s(0x%08x%08x)", symbol_name(type),
                             (uint32_t)(ui64>>32),
                             (uint32_t)(ui64));
    }
    else if (type == floatsym || type == doublesym) {
        char buf[64];
        double d;
        int ndec;
        if (type == floatsym) { d = (double)*(float*)data; ndec = 8; }
        else { d = *(double*)data; ndec = 16; }
        if (!DFINITE(d)) {
            char *rep;
            if (isnan(d))
                rep = sign_bit(d) ? "-NaN" : "+NaN";
            else
                rep = sign_bit(d) ? "-Inf" : "+Inf";
            if (type == floatsym && !princ && !weak)
                HPOS+=ios_printf(f, "#%s(%s)", symbol_name(type), rep);
            else
                outs(rep, f);
        }
        else if (d == 0) {
            if (1/d < 0)
                outs("-0.0", f);
            else
                outs("0.0", f);
            if (type == floatsym && !princ && !weak)
                outc('f', f);
        }
        else {
            snprint_real(buf, sizeof(buf), d, 0, ndec, 3, 10);
            int hasdec = (strpbrk(buf, ".eE") != NULL);
            outs(buf, f);
            if (!hasdec) outs(".0", f);
            if (type == floatsym && !princ && !weak)
                outc('f', f);
        }
    }
    else if (issymbol(type)) {
        // handle other integer prims. we know it's smaller than 64 bits
        // at this point, so int64 is big enough to capture everything.
        tmp = conv_to_int64(data, sym_to_numtype(type));
        if (fits_fixnum(tmp) || princ) {
            if (weak || princ)
                HPOS+=ios_printf(f, "%lld", tmp);
            else
                HPOS+=ios_printf(f, "#%s(%lld)", symbol_name(type), tmp);
        }
        else
            HPOS+=ios_printf(f, "#%s(0x%08x)", symbol_name(type),
                             (uint32_t)(tmp&0xffffffff));
    }
    else if (iscons(type)) {
        if (car_(type) == arraysym) {
            value_t eltype = car(cdr_(type));
            size_t cnt, elsize;
            if (iscons(cdr_(cdr_(type)))) {
                cnt = toulong(car_(cdr_(cdr_(type))), "length");
                elsize = cnt ? len/cnt : 0;
            }
            else {
                // incomplete array type
                int junk;
                elsize = ctype_sizeof(eltype, &junk);
                cnt = elsize ? len/elsize : 0;
            }
            if (eltype == bytesym) {
                if (princ) {
                    ios_write(f, data, len);
                }
                else {
                    print_string(f, (char*)data, len);
                }
                return;
            }
            else if (eltype == wcharsym) {
                // TODO wchar
            }
            else {
            }
            size_t i;
            if (!weak) {
                outs("#array(", f);
                fl_print_child(f, eltype, princ);
                if (cnt > 0)
                    outc(' ', f);
            }
            else {
                outc('[', f);
            }
            for(i=0; i < cnt; i++) {
                if (i > 0)
                    outc(' ', f);
                cvalue_printdata(f, data, elsize, eltype, princ, 1);
                data += elsize;
            }
            if (!weak)
                outc(')', f);
            else
                outc(']', f);
        }
        else if (car_(type) == enumsym) {
            int n = *(int*)data;
            value_t syms = car(cdr_(type));
            assert(isvector(syms));
            if (!weak) {
                outs("#enum(", f);
                fl_print_child(f, syms, princ);
                outc(' ', f);
            }
            if (n >= (int)vector_size(syms)) {
                cvalue_printdata(f, data, len, int32sym, princ, 1);
            }
            else {
                fl_print_child(f, vector_elt(syms, n), princ);
            }
            if (!weak)
                outc(')', f);
        }
    }
}

static void cvalue_print(ios_t *f, value_t v, int princ)
{
    cvalue_t *cv = (cvalue_t*)ptr(v);
    void *data = cptr(v);

    if (cv_class(cv) == builtintype) {
        HPOS+=ios_printf(f, "#<builtin @0x%08lx>",
                         (unsigned long)(builtin_t)data);
    }
    else if (cv_class(cv)->vtable != NULL &&
             cv_class(cv)->vtable->print != NULL) {
        cv_class(cv)->vtable->print(v, f, princ);
    }
    else {
        value_t type = cv_type(cv);
        size_t len = iscprim(v) ? cv_class(cv)->size : cv_len(cv);
        cvalue_printdata(f, data, len, type, princ, 0);
    }
}

static void set_print_width()
{
    value_t pw = symbol_value(printwidthsym);
    if (!isfixnum(pw)) return;
    SCR_WIDTH = numval(pw);
}

void print(ios_t *f, value_t v, int princ)
{
    print_pretty = (symbol_value(printprettysym) != FL_F);
    if (print_pretty)
        set_print_width();
    printlabel = 0;
    print_traverse(v);
    HPOS = VPOS = 0;

    fl_print_child(f, v, princ);

    htable_reset(&printconses, 32);
}
