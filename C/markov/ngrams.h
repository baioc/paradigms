#ifndef H_NGRAMS
#define H_NGRAMS

#include "token.h"

typedef struct ngrams *ngrams_t;

ngrams_t make_ngrams(int gramsize, int buckets);
void free_ngrams(ngrams_t table);
void ngrams_add(ngrams_t table, const token_t *string, int length);
long ngrams_frequency(const ngrams_t table, const token_t *string, int length);
void ngrams_for_each(const ngrams_t table,
                     void (*proc)(const token_t *str, int n, void *fwd),
                     void *forward);

#endif // H_NGRAMS
