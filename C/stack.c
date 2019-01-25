#include "stack.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>


void stack_new(struct stack *s, int depth,
			   int type_size, void (*freefn)(void *))
{
	assert(depth > 0);
	assert(type_size > 0);

	s->depth = depth;
	s->index = 0;
	s->type_size = type_size;
	s->freefn = freefn;

	s->elements = malloc(s->depth * s->type_size);
	assert(s->elements != NULL);
}

void stack_free(struct stack *s)
{
	if (s->freefn != NULL) {
		for (int i = 0; i < s->index; ++i)
			s->freefn((char *) s->elements + i * s->type_size);
	}
	free(s->elements);
}

static void stack_grow(struct stack *s) {
	s->depth *= 2;
	s->elements = realloc(s->elements, s->depth * s->type_size);
}

void stack_push(struct stack *s, void *elem_addr)
{
	if (s->index == s->depth)
		stack_grow(s);

	void *target = (char *) s->elements + s->index * s->type_size;
	memcpy(target, elem_addr, s->type_size);
	s->index++;
}

void stack_pop(struct stack *s, void *elem_addr)
{
	s->index--;
	void *source = (char *) s->elements + s->index * s->type_size;
	memcpy(elem_addr, source, s->type_size);
}
