#ifndef H_NGRAMS
#define H_NGRAMS

typedef struct ngrams *ngrams_t;

ngrams_t make_ngrams(int gramsize, int buckets);
void free_ngrams(ngrams_t table);
void ngrams_add(ngrams_t table, const char *ngram);
long ngrams_frequency(const ngrams_t table, const char *ngram);
void ngrams_for_each(const ngrams_t table,
                     void (*proc)(const char *ngram, void *fwd),
                     void *forward);

#endif // H_NGRAMS
