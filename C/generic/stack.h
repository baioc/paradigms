/*
 * Copyright (c) 2019 Gabriel B. Sant'Anna <baiocchi.gabriel@gmail.com>
 * @License Apache <https://gitlab.com/baioc/paradigms>
 */

#ifndef H_STACK
#define H_STACK

#include <stddef.h> 	// size_t
#include <stdbool.h>

typedef struct stack stack_t;

void stack_init(stack_t *s, int depth, size_t type_size);

// frees the stack and all its elements applying freefn on each one
void stack_destroy(stack_t *s, void (*freefn)(void *));

void stack_push(stack_t *s, void *source);

// hands back the responsibility over the popped element
void stack_pop(stack_t *s, void *sink);

// use with caution, returns a pointer to internal dinamically allocated memory
void *stack_top(const stack_t *s);

bool stack_empty(const stack_t *s);

#endif	// H_STACK
