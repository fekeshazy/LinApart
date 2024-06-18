#ifndef LINAPART_H
#define LINAPART_H

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <gmp.h>

#define BUF_SIZE 10485760/500

size_t buf(int disk, FILE *fptr, char* str, char const* fmt, ...);
long double a(int i);
void fact(mpz_t *result, long long n);
char* convert_to_string(mpz_t value);
void binomial(mpz_t *result, int n, int m);
void ReplaceRemainedStructure(int n, int m, int i, char **a, FILE *fptr, char *prod);
void genTerms(int i, int *partition, int *m, int n, int mi, char **a, int remainedNumPower, FILE *fptr, char *prod);
void generatePartitions(int ii, int nOrig, int n, int mi, char **a, int *partition, int *m, int index, int remainedNumPower, FILE *fptr, char *prod);
void run(FILE *fptr, int N, char **a, int *m);

#endif
