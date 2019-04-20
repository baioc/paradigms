#include "list.hpp"

#include <iostream>

#include "array_list.hpp"
#include "linked_list.hpp"


using baioc::List;

template <typename T>
void print(const List<T>& list)
{
	std::cout << "(";
	for (int i = 0; i < list.size(); ++i)
		std::cout << (i == 0 ? "" : " ") << list[i];
	std::cout << ")\n\n";
}

int main(int argc, char const *argv[])
{
	using TestedList = baioc::LinkedList<int>;

	TestedList l = {1, 0, 1, 1, 2, 3};
	print(l);

	l.push_front(0);

	l.front() -= 1;
	l.back() -= 2;
	print(l);

	l.pop_back();
	print(l);

	l.pop_front();
	print(l);

	l.insert(3, 24);
	print(l);

	std::cout << l.pop(3) << " was removed from index 3\n";
	print(l);

	std::cout << l.remove(2) << " was the index where the first 2 was\n";
	print(l);

	std::cout << l.remove(1) << " was the index where the first 1 was\n";
	print(l);

	l.push_back(-3);
	l.push_front(16);
	print(l);

	l.insert(-2);
	print(l);

	std::cout << "1 repeats " << l.count(1) << " times\n";

	l.sort();
	print(l);

	std::cout << "5 was inserted in index " << l.insert(5) <<'\n';
	print(l);

	TestedList m = {4, -1, 8};
	l += m;
	std::cout << "size: " << l.size() << '\n';
	print(l);

	auto n = std::move(l);
	std::cout << "size after move: " << l.size() << '\n';
	print(l);

	auto o(n);
	std::cout << "size after copy: " << n.size() << '\n';
	print(n);

	auto p = o + n + m + l;
	print(p);

	p.sort();
	print(p);

	std::cout << "unique (requires sorted):\n";
	for (int i = 0; i < p.size(); ++i) {
		auto e = p[i];
		while (p.count(e) > 1)
			p.remove(e);
	}
	print(p);
}
