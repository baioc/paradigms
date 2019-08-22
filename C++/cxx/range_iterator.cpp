#ifndef UTIL_RANGE_HPP
#define UTIL_RANGE_HPP

namespace util {

class Range {
	class RangeIterator {
	 public:
		RangeIterator(const Range& range, int value): range_(range), value_(value) {}
		int operator*() { return value_; }
		RangeIterator operator++() { value_ += range_.step_; return *this; }
		bool operator!=(const RangeIterator& other) { return value_ not_eq other.value_; }

	 private:
		int value_;
		const Range& range_;
	};

 public:
	Range(int begin, int end, int step=1): begin_(begin), end_(end), step_(step) {}
	RangeIterator begin() const { return RangeIterator(*this, begin_); }
	RangeIterator end() const { return RangeIterator(*this, end_); }

 private:
	int begin_, end_, step_;
};

} // namespace util

#endif // UTIL_RANGE_HPP


#include <iostream>

int main(int argc, char const *argv[])
{
	using namespace std;
	using namespace util;

	{
		const auto beg = atoi(argv[1]);
		const auto end = atoi(argv[2]);

		for (auto i : Range(beg, end))
			cout << i << '\n';
	}

	{
		const auto beg = atoi(argv[1]);
		const auto end = atoi(argv[2]);

		for (int i = beg; i < end; ++i)
			cout << i << '\n';
	}
}
