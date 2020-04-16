#include "stack.h"

#include <assert.h>
#include <stdlib.h> 	// malloc, realloc, free
#include <errno.h>
#include <string.h> 	// memcpy

void stack_init(stack_t *s, int depth, size_t type_size)
{
	assert(depth > 0);
	s->elements = malloc(depth * type_size);
	assert(s->elements != NULL);

	s->current_size = 0;
	s->allocated_size = depth;
	s->type_size = type_size;
}

void stack_destroy(stack_t *s, void (*freefn)(void *))
{
	if (freefn != NULL) {
		for (int i = 0; i < s->current_size; ++i)
			freefn((char *) s->elements + i * s->type_size);
	}
	free(s->elements);
}

static void stack_grow(stack_t *s)
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

void stack_push(stack_t *s, void *source)
{
	if (s->current_size >= s->allocated_size)
		stack_grow(s);

	void *target = (char *) s->elements + s->current_size * s->type_size;
	memcpy(target, source, s->type_size);
	s->current_size++;
}

void stack_pop(stack_t *s, void *sink)
{
	assert(s->current_size > 0);
	s->current_size--;

	void *source = (char *) s->elements + s->current_size * s->type_size;
	memcpy(sink, source, s->type_size);
}

void *stack_top(const stack_t *s)
{
	assert(s->current_size > 0);
	return (char *) s->elements + (s->current_size-1) * s->type_size;
}

bool stack_empty(const stack_t *s)
{
	return s->current_size <= 0;
}
