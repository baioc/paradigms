#include "token.h"

#include <string.h>
#include <stdio.h>
#include <assert.h>

int tokencmp(const token_t a, const token_t b)
{
	return a - b;
}

unsigned hash_string(const token_t *string, int length, unsigned modulo)
{
	assert(0 < length);
	assert(0 < modulo);
	unsigned long long a = 31415, b = 27183;
	unsigned hash = string[0] % (modulo - 1);
	for (int i = 0; i < length; ++i) {
		a = a * b;
		hash += (a * string[i]) % (modulo - 1);
	}
	return hash % modulo;
}

int lex(token_t *dst, const char **string)
{
	const char c = (*string)[0];
	*string += 1;
	switch (c) {
	case '\0':
		return 0;
	case '\n': case '\r':
		return -1;
	default:
		*dst = c;
		return 1;
	}
}

int flex(token_t *dst, FILE *fp)
{
	const char c = fgetc(fp);
	switch (c) {
	case EOF:
		return 0;
	case '\n': case '\r':
		return -1;
	default:
		*dst = c;
		return 1;
	}
}
