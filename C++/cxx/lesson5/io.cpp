#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <stdexcept>

#include "person.hpp"


using namespace std::string_literals;


int _main(std::istream& in)
{
    auto p = from_stream(in);

    std::cout << "Hello, " << p.name()
              << "! I see you were born in " << p.birth_year() << '\n';

    return 0;
}


int main(int argc, char** argv)
{
    if (argc == 1) {
        return _main(std::cin);
    }

    auto file = std::ifstream{argv[1]};

    return _main(file);
}
