#include <stdlib.h>
#include "dtypes.h"

char *int2str(char *dest, size_t n, long num, uint32_t base)
{
    int i = n-1;
    int b = (int)base, neg = 0;
    char ch;
    if (num < 0) {
        num = -num;
        neg = 1;
    }
    dest[i--] = '\0';
    while (i >= 0) {
        ch = (char)(num % b);
        if (ch < 10)
            ch += '0';
        else
            ch = ch-10+'a';
        dest[i--] = ch;
        num /= b;
        if (num == 0)
            break;
    }
    if (i >= 0 && neg)
        dest[i--] = '-';
    return &dest[i+1];
}
