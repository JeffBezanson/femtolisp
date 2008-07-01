#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <wchar.h>
#include "llt.h"

int main()
{
    llt_init();

    test_dblprint();
    test_operators();

    /*
    char *buf = malloc(20000);
    char *buf2 = malloc(20000);
    FILE *f = fopen("textread.m","rb");
    int i=0;
    while (!feof(f))
      buf[i++] = fgetc(f);
    buf[i-1] = '\0';
    int len = i-1;
    double t0 = clock_now();
    int j=0;
    for(i=0; i < 20000; i++) {
        //j+=u8_charnum(buf,len);
        u8_reverse(buf2, buf, len);
    }
    printf("textread took %.4f sec (%d)\n", clock_now()-t0, j);

    FILE *f2 = fopen("u8.txt","rb");
    i=0;
    while (!feof(f2))
      buf[i++] = fgetc(f2);
    buf[i-1] = '\0';
    len = i-1;
    t0 = clock_now();
    j=0;
    for(i=0; i < 20000; i++) {
        //j+=u8_charnum(buf,len);
        u8_reverse(buf2, buf, len);
    }
    printf("u8 took %.4f sec (%d)\n\n", clock_now()-t0, j);
    */

    return 0;
}

static void prettycplx(double r, double i)
{
    char str[64];
    snprint_cplx(str, sizeof(str), r, i, 0, 16, 3, 10, 1);
    fputs(str, stdout);
    fputc('\n', stdout);
}

static void prettyreal(double r)
{
    char str[64];
    snprint_real(str, sizeof(str), r, 0, 16, 3, 10);
    fputs(str, stdout);
    fputc('\n', stdout);
}

void test_dblprint()
{
    char str[64];

    dbl_tolerance(1e-12);

    prettycplx(0,0);
    prettycplx(1,0);
    prettycplx(0,1);
    prettycplx(1,1);
    prettycplx(-1,0);
    prettycplx(0,-1);
    prettycplx(1,-1);
    prettycplx(-1,1);
    prettycplx(-1,-1);
    prettycplx(2,0);
    prettycplx(0,2);
    prettycplx(2,2);
    prettycplx(-2,0);
    prettycplx(0,-2);
    prettycplx(2,-2);
    prettycplx(-2,2);
    prettycplx(-2,-2);

    prettyreal(1.5);
    prettyreal(1.1);
    prettyreal(1.1e-100);
    prettyreal(1.1e20);
    prettyreal(123456789);
    prettyreal(1234567890);
    prettyreal(12345678901);
    prettyreal(-12345678901);
    prettyreal(12345678901223);
    prettyreal(-12345678901223);
    prettyreal(.02);
    prettyreal(.002);
    prettyreal(.0002);
    prettyreal(-.0002);
    prettyreal(.00002);
    prettyreal(-.00002);

    prettyreal(1.0/0);
    prettyreal(-1.0/0);
    prettyreal(strtod("nan",NULL));
    prettyreal(0.0/0);
    prettyreal(-0.0/0);

    prettyreal(DBL_EPSILON);
}
