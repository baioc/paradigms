#include "array_list.hpp"

#include <iostream>


#define PRINTL(list) do {\
		std::cout << "(";\
		for (auto i = 0u; i < (list).size(); ++i)\
			std::cout << (i == 0 ? "" : " ") << (list)[i];\
		std::cout << ")\n\n";\
	} while (0)

int main(int argc, char const *argv[])
{
	using structures::ArrayList;

	ArrayList<int> l = {-1, 1, 0, 1, 1, 2, 3};
	PRINTL(l);

	l.front() -= 1;
	l.back() -= 2;
	PRINTL(l);

	l.pop_back();
	PRINTL(l);

	l.pop_front();
	PRINTL(l);

	l.insert(3, 24);
	PRINTL(l);

	std::cout << l.pop(3) << " was removed from index 3\n";
	PRINTL(l);

	std::cout << l.remove(2) << " was the index where the first 2 was\n";
	PRINTL(l);

	std::cout << l.remove(1) << " was the index where the first 1 was\n";
	PRINTL(l);

	l.push_back(-3);
	l.push_front(16);
	PRINTL(l);

	l.insert(-2);
	PRINTL(l);

	std::cout << "1 repeats " << l.count(1) << " times\n";

	l.sort();
	PRINTL(l);

	std::cout << "5 was inserted in index " << l.insert(5) <<'\n';
	PRINTL(l);

	ArrayList<int> m = {4, -1, 8};
	l += m;
	std::cout << "size: " << l.size() << '\n';
	PRINTL(l);

	auto n = std::move(l);
	std::cout << "size after move: " << l.size() << '\n';
	PRINTL(l);

	auto o(n);
	std::cout << "size after copy: " << n.size() << '\n';
	PRINTL(n);

	auto p = o + n + m + l;
	PRINTL(p);

	p.sort();
	PRINTL(p);

	std::cout << "unique (requires sorted):\n";
	for (auto i = 0u; i < p.size(); ++i) {
		auto e = p[i];
		while (p.count(e) > 1)
			p.remove(e);
	}
	PRINTL(p);

	return 0;
}
