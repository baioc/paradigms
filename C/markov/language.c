#include "language.h"

#include <string.h>
#include <math.h>
#include <float.h>
#include <assert.h>
#include <stdlib.h>

#include "ngrams.h"
#include "token.h"

#define BUCKETS (1000 / GRAMSIZE)


void language_init(struct language *model)
{
	model->ngrams = make_ngrams(GRAMSIZE, BUCKETS);
	assert(model->ngrams != NULL);
	model->total_tokens = 0;
	model->lexicon_size = 0;
}

void language_destroy(struct language *model)
{
	free_ngrams(model->ngrams);
}

static inline void shift_buffer_left(void *buf, unsigned n, unsigned size)
{
	unsigned char *b = (unsigned char *)buf;
	for (unsigned i = 1; i < n; ++i)
		memcpy(b + (i-1)*size, b + i*size, size);
}

void language_train(struct language *model, FILE *fp)
{
	token_t buffer[GRAMSIZE] = { 0 };
	token_t sink;
	for (;;) {
		const int r = flex(&sink, fp);
		if (r == 0) {
			break;

		} else if (r < 0) {
			// start a new chain on negative return code
			memset(buffer, 0, sizeof(buffer));

		} else /* if (r > 0) */ {
			// shift buffer, freeing up rightmost space for next token
			shift_buffer_left(buffer, GRAMSIZE, sizeof(token_t));
			buffer[GRAMSIZE - 1] = sink;
			model->total_tokens++;

			// increment the frequency of this n-gram
			// and that of each of its previous inferior order subgrams
			for (int n = 0; n < GRAMSIZE; ++n) {
				const int f = ngrams_add(model->ngrams, buffer + n, GRAMSIZE - n);

				// increase unique unigram count, if thats the case
				if (GRAMSIZE - n == 1 && f == 0)
					model->lexicon_size += 1;
			}
		}
	}
}

static double ngram_logp(const struct language *model, const token_t *ngram, int n)
{
	// get the frequency of this ngram ...
	assert(1 < n);
	const long freq = ngrams_frequency(model->ngrams, ngram, n);

	// ... and that of its conditional subgram
	token_t subgram[GRAMSIZE - 1];
	memcpy(subgram, ngram, (n - 1)*sizeof(token_t));
	const long sub_freq = ngrams_frequency(model->ngrams, subgram, n - 1);

	// @NOTE: we apply Laplace add-one additive smoothing
	return log(freq + 1.0) - log(sub_freq + model->lexicon_size);
}

double language_logp(const struct language *model, const char *string)
{
	double prob = 0;
	token_t buffer[GRAMSIZE] = { 0 };

	for (;;) {
		const int r = lex(buffer + GRAMSIZE - 1, &string);
		if (r == 0) {
			break;
		} else if (r < 0) {
			memset(buffer, 0, sizeof(buffer));
		} else /* if (r > 0) */ {
			prob += ngram_logp(model, buffer, GRAMSIZE);
			shift_buffer_left(buffer, GRAMSIZE, sizeof(token_t));
		}
	}

	return prob != 0 ? prob : -FLT_MAX;
}

#define CANDIDATES GRAMSIZE

struct gen {
	const struct language *model;
	token_t *buffer;
	struct candidate {
		token_t token;
		double prob;
	} candidates[CANDIDATES];
};

static void find_most_likely(const token_t *ngram, void *arg)
{
	struct gen *g = (struct gen *)arg;
	for (int i = 0; i < GRAMSIZE - 1; ++i) {
		if (tokencmp(ngram[i], g->buffer[i]) != 0) return;
	}

	// maintain candidate list sorted (ascending probability)
	const double p = ngram_logp(g->model, ngram, GRAMSIZE);
	for (int j = CANDIDATES - 1; j >= 0; --j) {
		if (p > g->candidates[j].prob) {
			shift_buffer_left(g->candidates, j + 1, sizeof(struct candidate));
			g->candidates[j].token = ngram[GRAMSIZE - 1];
			g->candidates[j].prob = p;
			break;
		}
	}
}

void language_generate(const struct language *model, const char *seed,
                       char **results, int n, int max_len)
{
	assert(0 < n);
	token_t buffer[GRAMSIZE] = { 0 };

	// lex until end of given seed
	for (token_t sink;;) {
		const int r = lex(&sink, &seed);
		if (r == 0) {
			break;
		} else if (r < 0) {
			memset(buffer, 0, sizeof(buffer));
		} else /* if (r > 0) */ {
			shift_buffer_left(buffer, GRAMSIZE, sizeof(token_t));
			buffer[GRAMSIZE - 1] = sink;
		}
	}
	shift_buffer_left(buffer, GRAMSIZE, sizeof(token_t));

	// generate n tokens
	struct gen gen = { .model = model, .buffer = buffer };
	for (int i = 0; i < n; ++i) {
		// find candidates with best probability
		for (int j = 0; j < CANDIDATES; ++j) {
			memset(&gen.candidates[j].token, 0, sizeof(token_t));
			gen.candidates[j].prob = -FLT_MAX;
		}
		ngrams_for_each(model->ngrams, GRAMSIZE, find_most_likely, &gen);

		// make a weighted random choice between candidates
		int choice = CANDIDATES - 1;
		double total = 0;
		for (int j = 0; j < CANDIDATES; ++j) {
			total += gen.candidates[j].prob;
		}
		double limit = ((double)rand() / RAND_MAX) * total;
		for (int j = CANDIDATES - 1; j >= 0; --j) {
			limit -= gen.candidates[j].prob;
			if (limit * total <= 0) {
				choice = j;
				break;
			}
		}

		// fill in n-gram, print result and
		buffer[GRAMSIZE - 1] = gen.candidates[choice].token;
		snprintok(results[i], max_len, gen.candidates[choice].token);
		shift_buffer_left(buffer, GRAMSIZE, sizeof(token_t));
	}
}
