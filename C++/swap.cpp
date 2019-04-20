#include <iostream>
#include <cassert>

template <typename T>
	// requires MoveConstructible<T> && MoveAssignable<T>
void swap(T& a, T& b) {
	T tmp(std::move(a));
	a = std::move(b);
	b = std::move(tmp);
}

int main(int argc, char const *argv[]) {
	int x = 5, y = 7;
	swap(x, y);
	assert(x == 7 && y == 5);

	std::string s1 = "Hello, generic", s2 = "World!";
	swap(s1, s2);
	std::cout << s2 << " " << s1 << "\n";

	return 0;
}
