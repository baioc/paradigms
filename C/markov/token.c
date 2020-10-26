#include "token.h"

#include <string.h>
#include <stdio.h>
#include <assert.h>


#if TOKEN_CHARACTER

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

#elif TOKEN_WORD

	int tokencmp(const token_t a, const token_t b)
	{
		return strcmp(a.chars, b.chars);
	}

	unsigned hash_string(const token_t *string, int length, unsigned modulo)
	{
		assert(0 < length);
		assert(0 < modulo);
		unsigned long long a = 31415, b = 27183;
		unsigned hash = string[0].chars[0] % (modulo - 1);
		for (int i = 0; i < length; ++i) {
			for (const char *s = string[i].chars; *s != '\0'; ++s) {
				const char c = *s;
				a = a * b;
				hash += (a * c) % (modulo - 1);
			}
		}
		return hash % modulo;
	}

	static char skip_whitespace(void *src, char (*get)(void *s))
	{
		for (char c;;) {
			c = get(src);
			switch (c) {
			case '\n': case '\r': case '\t': case ' ': continue;
			default: return c;
			}
		}
	}

	static int lex_from(token_t *dst, void *src, char (*get)(void *s), void (*unget)(char c, void *s))
	{
		char c = skip_whitespace(src, get);
		switch (c) {
		case EOF: case '\0':
			return 0;

		// single character tokens
		case ',': case ':':
		case '(': case ')': case '\'': case '\"':
		case '-': case '&': case '/':
		case '$': case '%':
		case '#':
			dst->chars[0] = c;
			return 1;

		// terminators
		case '.': case '!': case '?': case ';':
			return -1;

		default:
			break;
		}

		int count;
		for (count = 0; count < MAX_WORD_LEN; c = get(src)) {
			switch (c) {
			case '\n': case '\r': case '\t': case ' ':
			case EOF: case '\0':
			case ',': case ':':
			case '(': case ')': case '\'': case '\"':
			case '-': case '&': case '/':
			case '$': case '%':
			case '#':
			case '.': case '!': case '?': case ';':
				unget(c, src);
				return count;

			default:
				dst->chars[count++] = c;
				continue;
			}
		}
		return count;
	}

	static char get_string(void *arg)
	{
		const char **sp = (const char **)arg;
		const char *str = *sp;
		const char c = *str++;
		*sp = str;
		return c;
	}

	static void unget_string(char c, void *arg)
	{
		const char **sp = (const char **)arg;
		const char *str = *sp;
		str -= 1;
		*sp = str;
	}

	int lex(token_t *dst, const char **string)
	{
		const int n = lex_from(dst, (void *)string, get_string, unget_string);
		if (n > 0) dst->chars[n] = '\0';
		return n;
	}

	int flex(token_t *dst, FILE *fp)
	{
		const int n = lex_from(dst, (void *)fp, (char (*)(void *))fgetc, (void (*)(char, void *))ungetc);
		if (n > 0) dst->chars[n] = '\0';
		return n;
	}

#endif
