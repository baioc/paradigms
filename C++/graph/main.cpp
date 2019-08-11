#include "graph.hpp"

#include <iostream>
#include <cmath> // HUGE_VAL


template <typename T>
std::ostream& operator<<(std::ostream&  out, const structures::Graph<T>& g) {
	out << "Graph has a total of "
	    << g.vertice_number() << " vertices" << " and "
	    << g.edge_number() << " edges" << '\n';

	for (auto a : g.vertices()) {
		out << "Degree of " << a << " is " << g.degree(a) << '\n';
		for (auto b : g.neighbours(a)) {
			out << "  |-> " << b << " (w: " << g.edges(a)[b] << ")" << '\n'; // g.edges(a)[b] === g.weight(a,b)
		}
	}

	return out;
}

int main(int argc, char const *argv[])
{
	using namespace structures;

	Graph<char> g(5);
	std::cout << g << '\n';

	// explicity inserting each node is optional
	g.insert('a');
	g.insert('b');
	g.insert('c');
	g.insert('d');
	g.insert('e');
	std::cout << g << '\n';

	g.link('a', 'b');
	g.link('b', 'c', 2);
	g.link('c', 'd', -3.25);
	g.link('d', 'b', 3.75);
	g.link('d', 'e');
	g.link('e', 'c', 0.0);
	std::cout << g << '\n';

	// next two lines should be equivalent
	g.link('b', 'a', HUGE_VAL);
	// g.unlink('b', 'a');
	std::cout << g.weight('a', 'b') << '\n';
	std::cout << g << '\n';

	g.erase('d');
	std::cout << g;// << '\n';

	return 0;
}
