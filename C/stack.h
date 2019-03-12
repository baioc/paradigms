#ifndef H_STACK
#define H_STACK

#include <stddef.h>
#include <stdbool.h>

struct stack {
	void *elements;
	int current_size;
	int allocated_size;
	size_t size_type;
	void (*freefn)(void *);
};

void stack_new(struct stack *s, int initial_size,
               size_t size_type, void (*freefn)(void *));

// frees the stack and all its elements applying freefn on each one
void stack_free(struct stack *s);

void stack_push(struct stack *s, void *source);

// hands back the responsibility over the popped element
void stack_pop(struct stack *s, void *sink);

// use with caution, returns a pointer to internal dinamically allocated memory
void *stack_top(const struct stack *s);

bool stack_empty(const struct stack *s);

#endif	// H_STACK
