#ifndef __HASHING_H_
#define __HASHING_H_

uint_t nextipow2(uint_t i);
u_int32_t int32hash(u_int32_t a);
u_int64_t int64hash(u_int64_t key);
u_int32_t int64to32hash(u_int64_t key);
#ifdef BITS64
#define inthash int64hash
#else
#define inthash int32hash
#endif
u_int64_t memhash(char* buf, size_t n);
#define random() genrand_int32()
#define srandom(n) init_genrand(n)
double rand_double();
float rand_float();
void randn(double *pre, double *pim);
u_int64_t i64time();
void randomize();
unsigned long genrand_int32();
void init_genrand(unsigned long s);

#endif
