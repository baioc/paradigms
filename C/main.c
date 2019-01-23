#include "generic.h"

#include <stdio.h>
#include <string.h>
#include <assert.h>


int intcmp(const void *i1, const void *i2)
{
	const int *p1 = (int *)i1;
	const int *p2 = (int *)i2;
	return *p1 - *p2;
}

int mystrcmp(const void *s1, const void *s2)
{
	const char *c1 = *(char **)s1;
	const char *c2 = *(char **)s2;
	return strcmp(c1, c2);
}

void test_swap(void)
{
	int x = 5, y = 7;
	swap(&x, &y, sizeof(x));
	assert(x == 7 && y == 5);

	char *s1 = "Hello, generic ", *s2 = "World!";
	swap(&s1, &s2, sizeof(s1));
	assert(strcmp(s1, "World!") == 0 && strcmp(s2, "Hello, generic ") == 0);
}

void test_search(void)
{
	{
		int array[] = {-6, 0, 2, 3, 6, 7, 11};
		int number = 6;
		int *found = bsearch(&number, array, 7, sizeof(number), intcmp);
		assert(found != NULL);
		#if DEBUG
			printf("Found: %d\n", *found);
		#endif // DEBUG
	}

	{
		char *notes[] = {"Ab", "B", "D", "F#", "Gb"};
		char *fav_note = "B";
		char **found = lsearch(&fav_note, notes, 5, sizeof(fav_note), mystrcmp);
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
