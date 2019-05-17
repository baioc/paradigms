#include "person.hpp"

#include <iostream>
#include <sstream>
#include <string>
#include <stdexcept>


using namespace std::string_literals;


class InvalidInput: public std::runtime_error {
    using std::runtime_error::runtime_error;
};


int try_get_int(std::istream& in)
{
    auto age = 0;
    auto age_str = ""s;

    std::getline(in, age_str);

    auto ss = std::stringstream{age_str};

    ss >> age;

    if (ss.fail()) {
        throw InvalidInput{"Not a valid integer number."};
    }

    return age;
}


Person from_stream(std::istream& in)
{
    auto name = ""s;
    std::getline(in, name);

    auto birth_year = try_get_int(in);

    return Person(std::move(name), birth_year);
}

std::ostream& operator<<(std::ostream& o, const Person& p)
{
    o << "Person{\"" << p.name() << "\", " << p.birth_year() << "}";

    return o;
}
