#ifndef H_TOKEN
#define H_TOKEN

#include <stdio.h>

#ifndef MAX_WORD_LEN
#	define MAX_WORD_LEN 31
#endif

#if TOKEN_CHARACTER
	typedef char token_t;
#elif TOKEN_WORD
	struct word {
		char chars[MAX_WORD_LEN+1];
	};
	typedef struct word token_t;
#else
#	error "N-Gram token type is undefined, use either TOKEN_CHARACTER or TOKEN_WORD"
#endif

int tokencmp(token_t a, token_t b);
unsigned hash_string(const token_t *string, int length, unsigned modulo);
int flex(token_t *dst, FILE *fp);
int lex(token_t *dst, const char **string);
int snprintok(char *dest, int max_len, token_t tok);

#endif // H_TOKEN
