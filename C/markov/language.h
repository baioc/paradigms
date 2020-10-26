#ifndef H_LANGUAGE
#define H_LANGUAGE

#include <stdio.h>

#include "ngrams.h"

struct language {
	ngrams_t ngrams;
	unsigned long long total_tokens;
	int lexicon_size;
};

void language_init(struct language *model);
void language_destroy(struct language *model);
void language_train(struct language *model, FILE *fp);
double language_logp(struct language *model, const char *string);

#endif // H_LANGUAGE
