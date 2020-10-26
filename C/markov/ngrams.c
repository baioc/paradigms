#include "ngrams.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "token.h"


struct ngram {
	long frequency;
	struct ngram *next;
	int length;
	token_t tokens[];
};

static struct ngram *make_ngram(const token_t *string, int n)
{
	assert(0 < n);
	struct ngram *ng = malloc(sizeof(struct ngram) + n * sizeof(token_t));
	if (ng != NULL) {
		ng->frequency = 0;
		ng->next = NULL;
		ng->length = n;
		memmove(ng->tokens, string, n * sizeof(token_t));
	}
	return ng;
}


typedef struct ngram *list_t;

static list_t list_append(list_t list, struct ngram *ngram)
{
	ngram->next = list;
	return ngram;
}

static void list_free(list_t node)
{
	if (node == NULL) return;
	struct ngram *next = node->next;
	free(node);
	list_free(next);
}

static struct ngram *list_find(const list_t node, const token_t *string, int length)
{
	if (node == NULL) return NULL;

	if (length == node->length) {
		for (int i = 0; i < length; ++i) {
			if (tokencmp(string[i], node->tokens[i]) != 0) goto RECURSE;
		}
		return node;
	}

RECURSE:
	return list_find(node->next, string, length);
}


struct ngrams {
	int gramsize;
	unsigned bucket_number;
	list_t buckets[];
};

ngrams_t make_ngrams(int gramsize, int buckets)
{
	assert(0 < gramsize);
	assert(0 < buckets);
	struct ngrams *ngs = calloc(1, sizeof(struct ngrams) + buckets * sizeof(list_t));
	if (ngs != NULL) {
		ngs->bucket_number = buckets;
		ngs->gramsize = gramsize;
	}
	return ngs;
}

void free_ngrams(ngrams_t table)
{
	for (unsigned i = 0; i < table->bucket_number; ++i) {
		list_free(table->buckets[i]);
	}
	free(table);
}

void ngrams_add(ngrams_t table, const token_t *string, int length)
{
	assert(length <= table->gramsize);
	const int idx = hash_string(string, length, table->bucket_number);
	struct ngram *found = list_find(table->buckets[idx], string, length);
	if (found != NULL) {
		found->frequency++;
	} else {
		struct ngram *ng = make_ngram(string, length);
		assert(ng != NULL);
		ng->frequency = 1;
		table->buckets[idx] = list_append(table->buckets[idx], ng);
	}
}

long ngrams_frequency(const ngrams_t table, const token_t *string, int length)
{
	assert(length <= table->gramsize);
	const int idx = hash_string(string, length, table->bucket_number);
	struct ngram *found = list_find(table->buckets[idx], string, length);
	return found != NULL ? found->frequency : 0;
}

void ngrams_for_each(const ngrams_t table,
                     void (*proc)(const token_t *str, int n, void *fwd),
                     void *forward)
{
	for (unsigned i = 0; i < table->bucket_number; ++i) {
		for (const struct ngram *node = table->buckets[i]; node != NULL; node = node->next) {
			proc(node->tokens, node->length, forward);
		}
	}
}
