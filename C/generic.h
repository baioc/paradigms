#ifndef H_GENERIC
#define H_GENERIC

void swap(void *a, void *b, int size);

void *lsearch(const void *key, const void *base, int length,
			  int type_size, int (*cmpfn)(const void *, const void *));

void *my_bsearch(const void *key, const void *base, int length,
				 int type_size, int (*cmpfn)(const void *, const void *));

#endif	// H_GENERIC
