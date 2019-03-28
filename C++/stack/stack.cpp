#include "array_stack.hpp"

#include <iostream>


template <typename T>
void hanoi(int size, structures::Stack<T>& from,
           structures::Stack<T>& to, structures::Stack<T>& aux)
{
	if (size > 1) {
		hanoi(size - 1, from, aux, to);
		hanoi(1, from, to, aux);
		hanoi(size - 1, aux, to, from);
	} else if (size == 1) {
		to.push(from.pop());
	} else {
		return;
	}
}

template <typename T>
void hanoi(structures::Stack<T>& from, structures::Stack<T>& to)
{
	int n = from.size();
	structures::Stack<T> aux(n);
	hanoi(n, from, to, aux);
}

int main(int argc, char const *argv[])
{
	using namespace structures;

	constexpr auto n = 16u;

	// Stack<int> *stack = new Stack<int>(n+1); // unnecessary dynamic allocation
	// Stack<int> s = *stack;

	// Stack<int> s = Stack<int>(n+1); // static initialization
	// auto s = Stack<int>(n+1); // less verbose
	Stack<int> s(n+1); // better static construction

	// fibonacci seed
	s.push(1);
	s.push(1);

	// cached fibonacci
	while (s.size() < n+1) {
		auto current = s.top();
		s.pick(1);
		auto next = current + s.pop();
		std::cout << "push > " << next << '\n';
		s.push(next);
	}

	std::cout << "before move, s has size " << s.size() << '\n';

	// move with class methods
	// auto t = std::move(s);

	// move with hanoi
	Stack<int> t(s.size());
	hanoi(s, t);

	std::cout << "after move, s has size " << s.size() << '\n';

	while (!t.empty())
		std::cout << t.pop() << " < popped" << '\n';

	Stack<int> u(n*2);
	u.push(n);

	std::cout << "before copy, s has size " << s.size() << '\n';
	s = u; // copy
	std::cout << "after copy, s has size " << s.size() << '\n';
	std::cout << "ending fib of " << s.top() << '\n';

	// delete stack; // explicit destruction only needed if dynamically alloc'ed

	return 0;
}
