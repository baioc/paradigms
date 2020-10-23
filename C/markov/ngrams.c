#include "ngrams.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>


struct ngram {
	long frequency;
	struct ngram *next;
	char chars[];
};

static struct ngram *make_ngram(const char *chars, int n)
{
	assert(0 < n);
	struct ngram *ng = calloc(1, sizeof(struct ngram) + (n + 1) * sizeof(char));
	if (ng != NULL) {
		strncpy(ng->chars, chars, n);
		ng->chars[n] = '\0';
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

static struct ngram *list_find(const list_t node, const char *ngram)
{
	if (node == NULL) return NULL;
	else if (strcmp(ngram, node->chars) == 0) return node;
	else return list_find(node->next, ngram);
}


struct ngrams {
	unsigned bucket_number;
	unsigned gramsize;
	list_t buckets[];
};

ngrams_t make_ngrams(int gramsize, int buckets)
{
	assert(0 < gramsize);
	assert(0 < buckets);
	struct ngrams *ngs = calloc(1, sizeof(struct ngrams) + buckets * sizeof(struct ngram *));
	if (ngs != NULL) {
		ngs->bucket_number = buckets;
		ngs->gramsize = gramsize;
	}
	return ngs;
}

void free_ngrams(ngrams_t table)
{
	for (int i = 0; i < table->bucket_number; ++i) {
		list_free(table->buckets[i]);
	}
	free(table);
}

static unsigned hash(const char *string, unsigned hash_max)
{
	unsigned long long a = 31415, b = 27183;
	unsigned hash = string[0] % (hash_max - 1);
	char c;
	while ((c = *string++) != '\0') {
		a = a * b;
		hash += (a * c) % (hash_max - 1);
	}
	return hash % hash_max;
}

void ngrams_add(ngrams_t table, const char *ngram)
{
	const int idx = hash(ngram, table->bucket_number);
	struct ngram *found = list_find(table->buckets[idx], ngram);
	if (found != NULL) {
		found->frequency++;
	} else {
		struct ngram *ng = make_ngram(ngram, table->gramsize);
		assert(ng != NULL);
		ng->frequency = 1;
		table->buckets[idx] = list_append(table->buckets[idx], ng);
	}
}

long ngrams_frequency(const ngrams_t table, const char *ngram)
{
	const int idx = hash(ngram, table->bucket_number);
	const struct ngram *found = list_find(table->buckets[idx], ngram);
	return found != NULL ? found->frequency : 0;
}

void ngrams_for_each(const ngrams_t table,
                     void (*proc)(const char *ngram, void *fwd),
                     void *forward)
{
	for (int i = 0; i < table->bucket_number; ++i) {
		for (const struct ngram *node = table->buckets[i]; node != NULL; node = node->next) {
			proc(node->chars, forward);
		}
	}
}
