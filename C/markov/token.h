#ifndef H_TOKEN_H
#define H_TOKEN_H

#include <stdio.h>

typedef char token_t;

int tokencmp(const token_t a, const token_t b);
unsigned hash_string(const token_t *string, int length, unsigned modulo);
int flex(token_t *dst, FILE *fp);
int lex(token_t *dst, const char **string);

#endif // H_TOKEN_H
