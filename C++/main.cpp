#include "stack.hpp"

#include <stdio.h>


int main(int argc, char const *argv[])
{
	structures::Stack<int> stack = structures::Stack<int>(4);
	structures::Stack<int> *s = &stack;

	s->push(1);
	s->push(2);
	s->push(3);
	s->push(4);
	// s->push(5);	// overflows

	printf("%d\n", s->pop());
	printf("%d\n", s->pop());
	printf("%d\n", s->pop());
	printf("%d\n", s->pop());
	printf("%d\n", s->pop());	// underflows

	return 0;
}
