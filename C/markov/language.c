#include "language.h"

#include <string.h>
#include <math.h>
#include <float.h>
#include <assert.h>

#include "ngrams.h"
#include "token.h"

#define GRAMSIZE 3
#define BUCKETS 1000


void language_init(struct language *model)
{
	model->ngrams = make_ngrams(GRAMSIZE, BUCKETS);
	model->total_tokens = 0;
	model->lexicon_size = -1;
}

void language_destroy(struct language *model)
{
	free_ngrams(model->ngrams);
}

static inline void shift_buffer_left(token_t *buf, int size)
{
	for (int i = 1; i < size; ++i)
		buf[i - 1] = buf[i];
}

void language_train(struct language *model, FILE *fp)
{
	token_t buffer[GRAMSIZE] = { 0 };

	for (;;) {
		const int r = flex(buffer + GRAMSIZE - 1, fp);
		if (r == 0) {
			break;
		} else if (r < 0) {
			// start a new chain on negative return code
			memset(buffer, 0, GRAMSIZE * sizeof(token_t));
		} else /* if (r > 0) */ {
			model->total_tokens++;

			// increment the frequency of this n-gram
			// and that of each of its previous inferior order subgrams
			for (int n = 0; n < GRAMSIZE; ++n)
				ngrams_add(model->ngrams, buffer + n, GRAMSIZE - n);

			// shift buffer, freeing up rightmost space for next token
			shift_buffer_left(buffer, GRAMSIZE);
		}
	}

	// invalidate cached unigram count
	model->lexicon_size = -1;
}

static void count_unigrams(const token_t *ngram, int n, void *fwd)
{
	int *count = (int *)fwd;
	if (n == 1) *count += 1;
}

static int lexicon_size(struct language *model)
{
	// if not invalidated, return the cached value count
	if (model->lexicon_size > 0) return model->lexicon_size;

	// otherwise, re-compute and update it
	model->lexicon_size = 0;
	ngrams_for_each(model->ngrams, count_unigrams, &model->lexicon_size);
	assert(model->lexicon_size > 0);
	return model->lexicon_size;
}

static double ngram_logp(struct language *model, const token_t *ngram)
{
	// get the frequency of this ngram ...
	const long freq = ngrams_frequency(model->ngrams, ngram, GRAMSIZE);

	// ... and that of its conditional subgram
	token_t subgram[GRAMSIZE - 1];
	memmove(subgram, ngram, (GRAMSIZE - 1) * sizeof(token_t));
	const long sub_freq = ngrams_frequency(model->ngrams, subgram, GRAMSIZE - 1);

	// @NOTE: we apply Laplace add-one additive smoothing
	return log(freq + 1.0) - log(sub_freq + lexicon_size(model));
}

double language_logp(struct language *model, const char *string)
{
	double prob = 0;
	token_t buffer[GRAMSIZE] = { 0 };

	for (;;) {
		const int r = lex(buffer + GRAMSIZE - 1, &string);
		if (r == 0) {
			break;
		} else if (r < 0) {
			memset(buffer, 0, GRAMSIZE * sizeof(token_t));
		} else /* if (r > 0) */ {
			prob += ngram_logp(model, buffer);
			shift_buffer_left(buffer, GRAMSIZE);
		}
	}

	return prob != 0 ? prob : -FLT_MAX;
}
