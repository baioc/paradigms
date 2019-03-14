#include "stack.h"

#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>


void stack_new(struct stack *s, int size,
               size_t size_type, void (*freefn)(void *))
{
	assert(size > 0);

	s->current_size = 0;
	s->allocated_size = size;
	s->size_type = size_type;
	s->freefn = freefn;

	s->elements = malloc(s->allocated_size * s->size_type);
	assert(s->elements != NULL);
}

void stack_free(struct stack *s)
{
	if (s->freefn != NULL) {
		for (int i = 0; i < s->current_size; ++i)
			s->freefn((char *) s->elements + i * s->size_type);
	}
	free(s->elements);
}

static void stack_grow(struct stack *s)
{
	s->allocated_size *= 2;
	s->elements = realloc(s->elements, s->allocated_size * s->size_type);
	assert(s->elements != NULL);
}

void stack_push(struct stack *s, void *source)
{
	if (s->current_size >= s->allocated_size)
		stack_grow(s);

	void *target = (char *) s->elements + s->current_size * s->size_type;
	memcpy(target, source, s->size_type);
	s->current_size++;
}

void stack_pop(struct stack *s, void *sink)
{
	assert(s->current_size > 0);
	s->current_size--;

	void *source = (char *) s->elements + s->current_size * s->size_type;
	memcpy(sink, source, s->size_type);
}

void *stack_top(const struct stack *s)
{
	assert(s->current_size > 0);
	return (char *) s->elements + (s->current_size-1) * s->size_type;
}

bool stack_empty(const struct stack *s)
{
	return s->current_size <= 0;
}
