#include <algorithm>
#include <string>
#include <future>
#include <thread>
#include <chrono>
#include <iterator>

#include "library.hpp"
#include "algorithm.hpp"


using namespace std;

class my_class {
	int bla{0};
};

void draw(const my_class& x, ostream& out, size_t position)
{
	out << string(position, ' ') << "my_class" << endl;
}


int main()
{
	{
		using namespace string_literals;
		using namespace lib;

		history doc(1);

		current(doc).emplace_back(0);
		current(doc).emplace_back("Hello!"s);
		draw(current(doc), cout, 0);
		cout << "--------------------" << endl;

		commit(doc);
		current(doc)[0] = 42.5;

		auto saving = async([document = current(doc)](){
			this_thread::sleep_for(chrono::seconds(3));
			draw(document, cout, 0);
			cout << "------ 'save' ------" << endl;
		});

		current(doc)[1] = "World";
		current(doc).emplace_back(current(doc));
		current(doc).emplace_back(my_class());
		draw(current(doc), cout, 0);
		cout << "--------------------" << endl;

		undo(doc);
		draw(current(doc), cout, 0);
		cout << "--------------------" << endl;
	}


	{
		using namespace alg;

		int  a[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
		bool b[] = {0, 1, 0, 1, 0, 1, 0, 1, 0};

		auto p = stable_partition_position(begin(a), end(a), [&](auto i) {
			return *(begin(b) + (i - begin(a)));
		});

		for (auto i = begin(a); i != end(a); ++i)
			cout << *i << " ";
		cout << '\n';
	}
}
