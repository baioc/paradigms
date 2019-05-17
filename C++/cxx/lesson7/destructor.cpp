#include <iostream>
#include <memory>


class C {
public:
    C(int i):
        i{i}
    {
        std::cout << "Created " << i << '\n';
    }

    ~C()
    {
        std::cout << "Destroyed " << i << '\n';
    }

private:
    int i;
};



int main()
{
    try {
        auto c1 = std::make_unique<C>(1);
        auto c2 = std::make_unique<C>(2);
        auto c3 = std::make_unique<C>(3);
        throw std::runtime_error{""};
    } catch (...) {
        std::cout << "Error!";
    }
}
