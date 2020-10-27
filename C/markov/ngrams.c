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
	struct ngram *ng = malloc(sizeof(struct ngram) + n * sizeof(token_t));
	if (ng != NULL) {
		ng->frequency = 0;
		ng->next = NULL;
		ng->length = n;
		memcpy(ng->tokens, string, n * sizeof(token_t));
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
	if (gramsize <= 0 || buckets <= 0) return NULL;
	struct ngrams *ngs = calloc(1, sizeof(struct ngrams) + gramsize * buckets * sizeof(list_t));
	if (ngs != NULL) {
		ngs->bucket_number = buckets;
		ngs->gramsize = gramsize;
	}
	return ngs;
}

static inline int get_index(const ngrams_t table, const token_t *string, int length)
{
	const int i = length - 1;
	const int j = hash_string(string, length, table->bucket_number);
	return i*table->bucket_number + j;
}

void free_ngrams(ngrams_t table)
{
	for (int i = 0; i < table->gramsize; ++i) {
		for (unsigned j = 0; j < table->bucket_number; ++j) {
			list_free(table->buckets[i*table->bucket_number + j]);
		}
	}
	free(table);
}

int ngrams_add(ngrams_t table, const token_t *string, int length)
{
	const int idx = get_index(table, string, length);
	struct ngram *found = list_find(table->buckets[idx], string, length);
	if (found != NULL) {
		const int n = found->frequency;
		found->frequency++;
		return n;
	} else {
		struct ngram *ng = make_ngram(string, length);
		assert(ng != NULL);
		ng->frequency = 1;
		table->buckets[idx] = list_append(table->buckets[idx], ng);
		return 0;
	}
}

long ngrams_frequency(const ngrams_t table, const token_t *string, int length)
{
	const int idx = get_index(table, string, length);
	struct ngram *found = list_find(table->buckets[idx], string, length);
	return found != NULL ? found->frequency : 0;
}

void ngrams_for_each(const ngrams_t table, int n,
                     void (*proc)(const token_t *str, void *fwd), void *forward)
{
	if (n < 0 || table->gramsize < n) return;
	const int i = n - 1;
	for (unsigned j = 0; j < table->bucket_number; ++j) {
		const struct ngram *node = table->buckets[i*table->bucket_number + j];
		while (node != NULL) {
			proc(node->tokens, forward);
			node = node->next;
		}
	}
}
