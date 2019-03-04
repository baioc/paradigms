#include "generic.h"
#include "stack.h"

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>


#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))


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

int fibonacci(int n)
{
	int current = 0;
	int next = 1;
	int result = 1;

	for (int i = 0; i < n; ++i) {
		result = current + next;
		current = next;
		next = result;
	}

	return result;
}

void test_fib(void)
{
	{
		int n = 16;
		for (int i = 0; i <= n; ++i) {
			#if DEBUG
				printf("fib(%d) = %d\n", i, fibonacci(i));
			#endif // DEBUG
		}
	}

	{
		int f = 0, i = 0, n = 32768;
		while (f <= n) {
			f = fibonacci(i++);
			#if DEBUG
				printf("fib(%d) = %d <= %d\n", i, f, n);
			#endif // DEBUG
		}
	}
}

void test_swap(void)
{
	int x = 5, y = 7;
	swap(&x, &y, sizeof(x));
	assert(x == 7 && y == 5);

	char *s1 = "Hello, generic", *s2 = "World!";
	swap(&s1, &s2, sizeof(s1));
	assert(strcmp(s1, "World!") == 0 && strcmp(s2, "Hello, generic") == 0);
	#if DEBUG
		printf("%s %s\n", s2, s1);
	#endif // DEBUG
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
		char *notes[] = {"Ab", "B", "D", "F#", "Gb"};
		char *fav_note = "B";
		char **found = lsearch(&fav_note, notes, ARRAY_SIZE(notes),
							   sizeof(fav_note), my_strcmp);
		assert(found != NULL);
		#if DEBUG
			printf("Found: %s\n", *found);
		#endif // DEBUG
	}
}

void strfree(void *str)
{
	free(*(char **)str);
}

void test_stack(void)
{
	{
		int array[] = {-6, 0, 2, 3, 6, 7, 11};
		struct stack s;
		stack_new(&s, 4, sizeof(array[0]), NULL);

		for (int i = 0; i < ARRAY_SIZE(array); ++i)
			stack_push(&s, &array[i]);

		for (int i = 0; i < ARRAY_SIZE(array); ++i) {
			int temp;
			stack_pop(&s, &temp);
			#if DEBUG
				printf("%d (%d/%d)\n", temp, s.index+1, s.depth);
			#endif // DEBUG
		}

		stack_free(&s);
	}

	{
		const char *names[] = {"Al", "Bob", "Carl"};
		struct stack s;
		stack_new(&s, 2, sizeof(char *), strfree);

		for (int i = 0; i < ARRAY_SIZE(names); ++i) {
			char *copy = (char*) malloc(sizeof(char*));	// no strdup
			strcpy(copy, names[i]);
			stack_push(&s, &copy);
		}

		char *name;
		stack_pop(&s, &name);
		#if DEBUG
			printf("%s\n", name);
		#endif // DEBUG
		free(name);

		stack_free(&s);
	}
}

int main(int argc, char const *argv[])
{
	test_fib();
	test_swap();
	test_search();
	test_stack();
	return 0;
}
