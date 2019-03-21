#include "array_stack.hpp"

#include <iostream>


int main(int argc, char const *argv[])
{
	using namespace structures;

	constexpr auto n = 18u;

	// Stack<int> *stack = new Stack<int>(n); // unnecessary dynamic allocation
	// Stack<int> s = *stack;

	// Stack<int> s = Stack<int>(n); // static initialization
	// auto s = Stack<int>(n); // less verbose
	Stack<int> s(n); // better static construction

	// fibonacci seed
	s.push(1);
	s.push(1);

	// cached fibonacci
	while (s.size() < n-1) {
		s.pick(1);
		s.pick(1);
		auto i = s.pop() + s.pop();
		std::cout << "push > " << i << std::endl;
		s.push(i);
	}

	auto t = std::move(s); // move
	std::cout << "after move, s has size " << s.size() << std::endl;

	while (!t.empty())
		std::cout << t.pop() << " < popped" << std::endl;

	Stack<int> u(n*2);
	u.push(n-2);

	s = u; // copy
	std::cout << "after copy, s has size " << s.size() << std::endl;
	std::cout << "ending fib of " << s.top() << std::endl;

	// delete stack; // explicit destruction only needed if dynamically alloc'ed

	return 0;
}
