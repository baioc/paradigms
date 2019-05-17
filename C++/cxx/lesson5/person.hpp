#ifndef PERSON_H
#define PERSON_H

#include <iosfwd>
#include <string>


class Person {
public:
    Person(std::string name, int birth_year):
        name_{std::move(name)},
        birth_year_{birth_year}
    {}

    const std::string& name() const
    {
        return name_;
    }

    int birth_year() const
    {
        return birth_year_;
    }

private:
    std::string name_;
    int birth_year_;
};

Person from_stream(std::istream&);
std::ostream& operator<<(std::ostream& o, const Person& p);

#endif
