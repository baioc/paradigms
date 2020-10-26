#ifndef H_TOKEN_H
#define H_TOKEN_H

#include <stdio.h>


#define TOKEN_CHARACTER 0
#define TOKEN_WORD      1


#if TOKEN_CHARACTER

	typedef char token_t;

#elif TOKEN_WORD

	#define MAX_WORD_LEN 31

	struct word {
		char chars[MAX_WORD_LEN + 1];
	};

	typedef struct word token_t;

#endif


int tokencmp(const token_t a, const token_t b);
unsigned hash_string(const token_t *string, int length, unsigned modulo);
int flex(token_t *dst, FILE *fp);
int lex(token_t *dst, const char **string);

#endif // H_TOKEN_H
