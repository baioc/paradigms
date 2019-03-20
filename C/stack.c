#include "stack.h"

#include <stddef.h> 	// size_t
#include <stdbool.h>
#include <stdlib.h> 	// malloc, realloc, free
#include <assert.h>
#include <string.h> 	// memcpy


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
	s->elements = realloc(s->elements, s->allocated_size * s->type_size);
	assert(s->elements != NULL);
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
