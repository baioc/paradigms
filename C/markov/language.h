#ifndef H_LANGUAGE
#define H_LANGUAGE

#include <stdio.h>

#include "ngrams.h"

#ifndef GRAMSIZE
#	define GRAMSIZE 3
#endif

struct language {
	ngrams_t ngrams;
	unsigned long long total_tokens;
	int lexicon_size;
};

void language_init(struct language *model);
void language_destroy(struct language *model);
void language_train(struct language *model, FILE *fp);
double language_logp(const struct language *model, const char *string);
void language_generate(const struct language *model, const char *seed,
                       char **results, int n, int max_len);

#endif // H_LANGUAGE
