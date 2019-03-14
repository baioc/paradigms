#include "stack.hpp"

#include <iostream>


int main(int argc, char const *argv[])
{
	using namespace data_structures;

	// Stack<int> *stack = new Stack<int>(4); // unnecessary dynamic allocation
	// Stack<int> s = *stack;

	// Stack<int> s = Stack<int>(4); // static initialization
	// auto s = Stack<int>(4); // less verbose
	Stack<int> s(4); // better static construction

	for (int i = 0; i < s.size(); ++i) {
		std::cout << "push > " << i << std::endl;
		s.push(i);
	}

	while (!s.empty())
		std::cout << s.pop() << " < popped" << std::endl;

	// delete stack; // explicit destruction only needed if dynamically alloc'ed

	return 0;
}
