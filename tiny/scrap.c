// code to relocate cons chains iteratively
    pcdr = &cdr_(nc);
    while (iscons(d)) {
        if (car_(d) == FWD) {
            *pcdr = cdr_(d);
            return first;
        }
        *pcdr = nc = mk_cons();
        a = car_(d);   v = cdr_(d);
        car_(d) = FWD; cdr_(d) = nc;
        car_(nc) = relocate(a);
        pcdr = &cdr_(nc);
        d = v;
    }
    *pcdr = d;

/*
  f = *rest;
  *rest = NIL;
  while (iscons(f)) {   // nreverse!
      v = cdr_(f);
      cdr_(f) = *rest;
      *rest = f;
      f = v;
  }*/

int favailable(FILE *f)
{
    fd_set set;
    struct timeval tv = {0, 0};
    int fd = fileno(f);

    FD_ZERO(&set);
    FD_SET(fd, &set);
    return (select(fd+1, &set, NULL, NULL, &tv)!=0);
}

static void print_env(value_t *penv)
{
    printf("<[ ");
    while (issymbol(*penv) && *penv!=NIL) {
        print(stdout, *penv, 0);
        printf(" ");
        penv++;
        print(stdout, *penv, 0);
        printf(" ");
        penv++;
    }
    printf("] ");
    print(stdout, *penv, 0);
    printf(">\n");
}

#else
                    PUSH(NIL);
                    PUSH(NIL);
                    value_t *rest = &Stack[SP-1];
                    // build list of rest arguments
                    // we have to build it forwards, which is tricky
                    while (iscons(v)) {
                        v = eval(car_(v));
                        PUSH(v);
                        v = cons_(&Stack[SP-1], &NIL);
                        POP();
                        if (iscons(*rest))
                            cdr_(*rest) = v;
                        else
                            Stack[SP-2] = v;
                        *rest = v;
                        v = Stack[saveSP] = cdr_(Stack[saveSP]);
                    }
                    POP();
#endif
                    // this version uses collective allocation. about 7-10%
                    // faster for lists with > 2 elements, but uses more
                    // stack space
                    i = SP;
                    while (iscons(v)) {
                        v = eval(car_(v));
                        PUSH(v);
                        v = Stack[saveSP] = cdr_(Stack[saveSP]);
                    }
                    if ((int)SP==i) {
                        PUSH(NIL);
                    }
                    else {
                        e = v = cons_reserve(nargs=(SP-i));
                        for(; i < (int)SP; i++) {
                            car_(v) = Stack[i];
                            v = cdr_(v);
                        }
                        POPN(nargs);
                        PUSH(e);
                    }

value_t list_to_vector(value_t l)
{
    value_t v;
    size_t n = llength(l), i=0;
    v = alloc_vector(n, 0);
    while (iscons(l)) {
        vector_elt(v,i) = car_(l);
        i++;
        l = cdr_(l);
    }
    return v;
}
