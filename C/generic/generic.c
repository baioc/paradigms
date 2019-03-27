#include "generic.h"

#include <stddef.h>	// size_t
#include <string.h>	// memcpy


void swap(void *a, void *b, int size)
{
	char tmp[size];
	memcpy(tmp, a, size);
	memcpy(a, b, size);
	memcpy(b, tmp, size);
}

void *lsearch(const void *key, const void *base, int length,
              size_t type_size, int (*cmpfn)(const void *, const void *))
{
	for (int i = 0; i < length; ++i) {
		void *addr = (char*) base + i*type_size;
		if (cmpfn(key, addr) == 0)
			return addr;
	}

	return NULL;
}

void *my_bsearch(const void *key, const void *base, int length,
                 size_t type_size, int (*cmpfn)(const void *, const void *))
{
	int low = 0;
	int high = length - 1;
	int mid;

	while (low <= high) {
		mid = (low + high) / 2;
		void *addr = (char*) base + mid*type_size;
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


#include <stdio.h>  	// printf
#include <assert.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

void test_swap(void)
{
	int x = 5, y = 7;
	swap(&x, &y, sizeof(x));
	assert(x == 7 && y == 5);

	const char *s1 = "Hello, generic", *s2 = "World!";
	swap(&s1, &s2, sizeof(s1));
	assert(strcmp(s1, "World!") == 0 && strcmp(s2, "Hello, generic") == 0);
	#if DEBUG
		printf("%s %s\n", s2, s1);
	#endif // DEBUG
}

int intcmp(const void *i1, const void *i2)
{
	const int *p1 = (int *)i1;
	const int *p2 = (int *)i2;
	return *p1 - *p2;
}

int my_strcmp(const void *s1, const void *s2)
{
	const char *c1 = *(char **)s1;
	const char *c2 = *(char **)s2;
	return strcmp(c1, c2);
}

void test_search(void)
{
	{
		int array[] = {-6, 0, 2, 3, 6, 7, 11};
		int number = 6;
		int *found = my_bsearch(&number, array, ARRAY_SIZE(array),
		                        sizeof(number), intcmp);
		assert(found != NULL);
		#if DEBUG
			printf("Found: %d\n", *found);
		#endif // DEBUG
	}

	{
		const char *notes[] = {"Ab", "B", "D", "F#", "Gb"};
		const char *fav_note = "B";
		char **found = lsearch(&fav_note, notes, ARRAY_SIZE(notes),
							   sizeof(fav_note), my_strcmp);
		assert(found != NULL);
		#if DEBUG
			printf("Found: %s\n", *found);
		#endif // DEBUG
	}
}

int main(int argc, char const *argv[])
{
	test_swap();
	test_search();
	return 0;
}
