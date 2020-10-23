#include "language.h"

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#include "ngrams.h"

#define GRAMSIZE 3
#define BUCKETS 1000
#define EMPTYCH ' '

void language_init(struct language *model)
{
	model->ngrams = make_ngrams(GRAMSIZE, BUCKETS);
	model->total_chars = 0;
	model->alphabet_size = -1;
}

void language_destroy(struct language *model)
{
	free_ngrams(model->ngrams);
}

static void shift_bytes_left(char *bytes, unsigned size, char last)
{
	for (unsigned i = 1; i < size; ++i) {
		bytes[i - 1] = bytes[i];
	}
	bytes[size - 1] = last;
}

void language_train(struct language *model, FILE *fp)
{
	char buffer[GRAMSIZE + 1];
	memset(buffer, EMPTYCH, GRAMSIZE);
	buffer[GRAMSIZE] = '\0';

	char c;
	while ((c = fgetc(fp)) != EOF) {
		model->total_chars++;

		if (c == '\n')
			memset(buffer, EMPTYCH, GRAMSIZE);
		else
			shift_bytes_left(buffer, GRAMSIZE, c);

		// increment the frequency of this n-gram
		// and that of each of its previous inferior order subgrams
		for (int n = 0; n < GRAMSIZE; ++n)
			ngrams_add(model->ngrams, buffer + n);
	}

	// invalidate cached unigram count
	model->alphabet_size = -1;
}

static void count_unigrams(const char *ngram, void *fwd)
{
	int *count = (int *)fwd;
	if (ngram[1] == '\0') *count += 1;
}

static int alphabet_size(struct language *model)
{
	// if not invalidated, return the cached value count
	if (model->alphabet_size > 0) return model->alphabet_size;

	// otherwise, re-compute and update it
	model->alphabet_size = 0;
	ngrams_for_each(model->ngrams, count_unigrams, &model->alphabet_size);
	assert(model->alphabet_size > 0);
	return model->alphabet_size;
}

static double ngram_logp(struct language *model, const char *ngram)
{
	// get the frequency of this ngram ...
	const long freq = ngrams_frequency(model->ngrams, ngram);

	// ... and that of its conditional subgram
	char subgram[GRAMSIZE];
	strncpy(subgram, ngram, GRAMSIZE - 1);
	subgram[GRAMSIZE - 1] = '\0';
	const long sub_freq = ngrams_frequency(model->ngrams, subgram);

	// @NOTE: we apply Laplace add-one additive smoothing
	return log(freq + 1.0) - log(sub_freq + alphabet_size(model));
}

double language_logp(struct language *model, const char *string)
{
	double prob = 0;

	char buffer[GRAMSIZE + 1];
	memset(buffer, EMPTYCH, GRAMSIZE);
	buffer[GRAMSIZE] = '\0';

	char c;
	while ((c = *string++) != '\0') {
		if (c == '\n') {
			memset(buffer, EMPTYCH, GRAMSIZE);
		} else {
			shift_bytes_left(buffer, GRAMSIZE, c);
			prob += ngram_logp(model, buffer);
		}
	}

	return prob;
}
