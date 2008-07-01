#include <stdio.h>

struct _b {
    char a;
    short b:9;
};

struct _bb {
    char a;
    int :0;
    int b:10;
    int :0;
    int b0:10;
    int :0;
    int b1:10;
    int :0;
    int b2:10;
    int :0;
    int b4:30;
    char c;
};

union _cc {
    struct {
        char a;
        int b:1;   // bit 8
        int b1:1;  // bit 9
        int b2:24; // bits 32..55
        char c;
    };
    unsigned long long ull;
};

union _cc2 {
    struct {
        char a;
        int b:24;   // bit 8
        int b1:1;
        int b2:1;
        char c;
    };
    unsigned long long ull;
};

union _dd {
    struct {
        int a0:10;
        int a1:10;
        int a2:10;
        int a3:10;
        int a4:10;
    };
    struct {
        unsigned long long ull;
    };
};

struct _ee {
    short s:9;
    short j:9;
    char c;
};

typedef long long int int64_t;
typedef unsigned long long int uint64_t;
typedef int int32_t;
typedef unsigned int uint32_t;
typedef short int16_t;
typedef unsigned short uint16_t;
typedef char int8_t;
typedef unsigned char uint8_t;

#define lomask(type,n) (type)((((type)1)<<(n))-1)

uint64_t get_u_bitfield(char *ptr, int typesz, int boffs, int blen)
{
    uint64_t i8;
    uint32_t i4;
    uint16_t i2;
    uint8_t i1;

    switch (typesz) {
    case 8:
        i8 = *(uint64_t*)ptr;
        return (i8>>boffs) & lomask(uint64_t,blen);
    case 4:
        i4 = *(uint32_t*)ptr;
        return (i4>>boffs) & lomask(uint32_t,blen);
    case 2:
        i2 = *(uint16_t*)ptr;
        return (i2>>boffs) & lomask(uint16_t,blen);
    case 1:
        i1 = *(uint8_t*)ptr;
        return (i1>>boffs) & lomask(uint8_t,blen);
    }
    //error
    return 0;
}

int64_t get_s_bitfield(char *ptr, int typesz, int boffs, int blen)
{
    int64_t i8;
    int32_t i4;
    int16_t i2;
    int8_t i1;

    switch (typesz) {
    case 8:
        i8 = *(int64_t*)ptr;
        return (i8<<(64-boffs-blen))>>(64-blen);
    case 4:
        i4 = *(int32_t*)ptr;
        return (i4<<(32-boffs-blen))>>(32-blen);
    case 2:
        i2 = *(int16_t*)ptr;
        return (i2<<(16-boffs-blen))>>(16-blen);
    case 1:
        i1 = *(int8_t*)ptr;
        return (i1<<(8-boffs-blen))>>(8-blen);
    }
    //error
    return 0;
}

void set_bitfield(char *ptr, int typesz, int boffs, int blen, uint64_t v)
{
    uint64_t i8, m8;
    uint32_t i4, m4;
    uint16_t i2, m2;
    uint8_t i1, m1;

    switch (typesz) {
    case 8:
        m8 = lomask(uint64_t,blen)<<boffs;
        i8 = *(uint64_t*)ptr;
        *(uint64_t*)ptr = (i8&~m8) | ((v<<boffs)&m8);
        break;
    case 4:
        m4 = lomask(uint32_t,blen)<<boffs;
        i4 = *(uint32_t*)ptr;
        *(uint32_t*)ptr = (i4&~m4) | ((v<<boffs)&m4);
        break;
    case 2:
        m2 = lomask(uint16_t,blen)<<boffs;
        i2 = *(uint16_t*)ptr;
        *(uint16_t*)ptr = (i2&~m2) | ((v<<boffs)&m2);
        break;
    case 1:
        m1 = lomask(uint8_t,blen)<<boffs;
        i1 = *(uint8_t*)ptr;
        *(uint8_t*)ptr = (i1&~m1) | ((v<<boffs)&m1);
        break;
    }
}

int main()
{
    union _cc2 c;
    union _dd d;
    printf("%d\n", sizeof(struct _b));

    printf("%d\n", sizeof(d));
    //printf("%d\n\n", sizeof(struct _bb));

    //printf("%d\n", (char*)&b.b - (char*)&b);
    //printf("%d\n", (char*)&b.c - (char*)&b);
    //printf("%d\n", (char*)&b.e - (char*)&b);

    c.ull = 0;
    d.ull = 0;
    //d.ull2 = 0;

    d.a0 = d.a1 = d.a2 = d.a3 = d.a4 = 1;
    printf("0x%016llx\n", d.ull);
    unsigned long long m = 1;
    int bn = 0;
    while (m) {
        if (d.ull & m)
            printf("bit %d set\n", bn);
        bn++;
        m<<=1;
    }
    //printf("%016x\n", d.ull2);


    c.a = 1;
    c.b = 1;
    c.c = 1;
    printf("0x%016llx\n", c.ull);
    bn=0;m=1;
    while (m) {
        if (c.ull & m)
            printf("bit %d set\n", bn);
        bn++;
        m<<=1;
    }

    return 0;
}

/*
  offset/alignment rules for bit fields:

  - alignment for whole struct is still the most strict of any of the
    named types, regardless of bit fields. (i.e. just take the bit field
    widths away and compute struct alignment normally)

  - a bit field cannot cross a word boundary of its declared type

  - otherwise pack bit fields as tightly as possible

 */
