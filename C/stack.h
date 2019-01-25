#ifndef H_STACK
#define H_STACK

struct stack {
	void *elements;
	int index;
	int depth;
	int type_size;
	void (*freefn)(void *);
};

void stack_new(struct stack *s, int depth,
			   int type_size, void (*freefn)(void *));

void stack_free(struct stack *s);

void stack_push(struct stack *s, void *elem_addr);

void stack_pop(struct stack *s, void *elem_addr);

#endif	// H_STACK
