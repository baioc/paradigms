#ifndef H_GENERIC
#define H_GENERIC

void swap(void *a, void *b, int size);

void *lsearch(const void *key, const void *base, int length, int type_size, int (*cmpfn)(const void *, const void *));
void *bsearch(const void *key, const void *base, int length, int type_size, int (*cmpfn)(const void *, const void *));

#endif
