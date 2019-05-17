#include <iostream>

class C {
public:
    C(int) {}

    C(C&& x)
    {
        std::cout << "rvalue\n";
    }

    C(const C& x)
    {
        std::cout << "lvalue\n";
    }
};

int main()
{
    C x = 3;
    C y = 4;

    C{x};
    C{std::move(y)};
}
