#include "stack.hpp"

#include <iostream>


int main(int argc, char const *argv[])
{
	structures::Stack<int> stack = structures::Stack<int>(4);
	structures::Stack<int> *s = &stack;

	s->push(1);
	s->push(2);
	s->push(3);
	s->push(4);
	// s->push(5);	// overflows

	std::cout << s->pop() << "\n";
	std::cout << s->pop() << "\n";
	std::cout << s->pop() << "\n";
	std::cout << s->pop() << std::endl;
	// std::cout << s->pop() << "\n"; // underflows

	return 0;
}
