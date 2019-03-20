#include "array_stack.hpp"

#include <iostream>


int main(int argc, char const *argv[])
{
	using namespace structures;

	constexpr auto n = 4u;

	// ArrayStack<int> *stack = new ArrayStack<int>(n); // unnecessary dynamic allocation
	// ArrayStack<int> s = *stack;

	// ArrayStack<int> s = ArrayStack<int>(n); // static initialization
	// auto s = ArrayStack<int>(n); // less verbose
	ArrayStack<int> s(n); // better static construction

	int i = 0;
	while (!s.full()) {
		std::cout << "push > " << i << std::endl;
		s.push(i++);
	}

	s.pop();
	s.pick(1);
	std::cout << "top is " << s.top() << std::endl;

	auto t = std::move(s); // move
	std::cout << "after move, s has size " << s.size() << std::endl;

	while (!t.empty())
		std::cout << t.pop() << " < popped" << std::endl;

	ArrayStack<int> u(n*2);

	s = u; // copy
	std::cout << "after copy, s has depth " << s.max_size() << std::endl;

	// delete stack; // explicit destruction only needed if dynamically alloc'ed

	return 0;
}
