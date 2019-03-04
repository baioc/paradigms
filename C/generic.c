#include "generic.h"

#include <inttypes.h>
#include <string.h>


void swap(void *a, void *b, int size)
{
	uint8_t tmp[size];
	memcpy(tmp, a, size);
	memcpy(a, b, size);
	memcpy(b, tmp, size);
}

void *lsearch(const void *key, const void *base, int length,
			  int type_size, int (*cmpfn)(const void *, const void *))
{
	for (int i = 0; i < length; ++i) {
		void *addr = (uint8_t *)base + i*type_size;
		if (cmpfn(key, addr) == 0)
			return addr;
	}

	return NULL;
}

void *my_bsearch(const void *key, const void *base, int length,
				 int type_size, int (*cmpfn)(const void *, const void *))
{
	int low = 0;
	int high = length - 1;
	int mid;

	while (low <= high) {
		mid = (low + high) / 2;
		void *addr = (uint8_t *)base + mid*type_size;
		int diff = cmpfn(key, addr);
		if (diff == 0)
			return addr;
		else if (diff > 0)
			low = mid + 1;
		else
			high = mid - 1;
	}

	return NULL;
}
