#include <iostream>
#include <cassert>


template <typename T>
void swap(T& a, T& b)
{
	T tmp(std::move(a));
	a = std::move(b);
	b = std::move(tmp);
}

void test_swap(void)
{
	int x = 5, y = 7;
	swap(x, y);
	assert(x == 7 && y == 5);

	std::string s1 = "Hello, generic", s2 = "World!";
	swap(s1, s2);
	std::cout << s2 << " " << s1 << "\n";
}

int main(int argc, char const *argv[])
{
	test_swap();
	return 0;
}
