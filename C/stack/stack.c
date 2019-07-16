#include "stack.h"

#include <stddef.h> 	// size_t
#include <stdbool.h>
#include <stdlib.h> 	// malloc, realloc, free
#include <assert.h>
#include <string.h> 	// memcpy
#include <errno.h>  	// error codes


#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))


void stack_init(struct stack *s, int depth,
                size_t type_size, void (*freefn)(void *))
{
	assert(depth > 0);
	s->elements = malloc(depth * type_size);
	assert(s->elements != NULL);

	s->current_size = 0;
	s->allocated_size = depth;
	s->type_size = type_size;
	s->freefn = freefn;
}

void stack_destroy(struct stack *s)
{
	if (s->freefn != NULL) {
		for (int i = 0; i < s->current_size; ++i)
			s->freefn((char *) s->elements + i * s->type_size);
	}
	free(s->elements);
}

static void stack_grow(struct stack *s)
{
	assert(s->allocated_size > 0);
	s->allocated_size *= 2;

	void *mem = realloc(s->elements, s->allocated_size * s->type_size);
	if (mem == NULL) {
		free(s->elements);
		exit(ENOMEM);
	}

	s->elements = mem;
}

void stack_push(struct stack *s, void *source)
{
	if (s->current_size >= s->allocated_size)
		stack_grow(s);

	void *target = (char *) s->elements + s->current_size * s->type_size;
	memcpy(target, source, s->type_size);
	s->current_size++;
}

void stack_pop(struct stack *s, void *sink)
{
	assert(s->current_size > 0);
	s->current_size--;

	void *source = (char *) s->elements + s->current_size * s->type_size;
	memcpy(sink, source, s->type_size);
}

void *stack_top(const struct stack *s)
{
	assert(s->current_size > 0);
	return (char *) s->elements + (s->current_size-1) * s->type_size;
}

bool stack_empty(const struct stack *s)
{
	return s->current_size <= 0;
}


#include <stdio.h>  	// printf

void strfree(void *str)
{
	free(*(char **)str);
}

bool balanced(const char *string)
{
	struct stack brackets;
	stack_init(&brackets, 2, sizeof(char), NULL);

	size_t length = strlen(string);
	for (int i = 0; i < length; ++i) {
		char c = string[i];

		if (c == '(' || c == '[' || c == '{') {
			stack_push(&brackets, &c);
		}
		else if (c == ')' || c == ']' || c == '}') {
			if (stack_empty(&brackets)) {
				goto FAIL;
			}
			else {
				char last_open;
				stack_pop(&brackets, &last_open);

				if (   (last_open == '(' && c != ')')
				    || (last_open == '[' && c != ']')
				    || (last_open == '{' && c != '}')
				   ) {
					goto FAIL;
				}
			}
		}
	}

	if (stack_empty(&brackets)) {
		stack_destroy(&brackets);
		return true;
	}

	FAIL: stack_destroy(&brackets);
	return false;
}

int main(int argc, char const *argv[])
{
	{
		int array[] = {-6, 0, 2, 3, 6, 7, 11};
		struct stack s;
		stack_init(&s, 4, sizeof(array[0]), NULL);

		for (int i = 0; i < ARRAY_SIZE(array); ++i)
			stack_push(&s, &array[i]);

		for (int i = 0; i < ARRAY_SIZE(array); ++i) {
			int temp;
			stack_pop(&s, &temp);
			#if DEBUG
				printf("%d (%d/%ld)\n", temp, s.current_size+1, s.allocated_size);
			#endif // DEBUG
		}

		stack_destroy(&s);
	}

	{
		const char *names[] = {"Al", "Bob", "Carl"};
		struct stack s;
		stack_init(&s, 1, sizeof(char *), strfree);

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

		name = *((char **)stack_top(&s));
		#if DEBUG
			printf("%s\n", name);
		#endif // DEBUG

		stack_destroy(&s);

		// stack top was freed, this pointer is now dangling
		#if DEBUG
			printf("%s\n", name);
		#endif // DEBUG
	}

	{
		const char* cases[] = {
			"()",
			")",
			"([[]]{[]}{()}).",
			"([)()(])({}{)(})",
			"[[](){](()",
		};

		for (int i = 0; i < ARRAY_SIZE(cases); ++i) {
			bool b = balanced(cases[i]);
			#if DEBUG
				printf("Case %d is %s.\n", i, b ? "balanced" : "not balanced");
			#endif // DEBUG
		}
	}

	return 0;
}
