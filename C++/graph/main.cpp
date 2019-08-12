#include "graph.hpp"

#include <iostream>
#include <cmath> // HUGE_VAL


template <typename L, typename W, bool d>
std::ostream& operator<<(std::ostream&  out, const structures::Graph<L,W,d>& g) {
	out << "Graph has a total of "
	    << g.vertice_number() << " vertices" << " and "
	    << g.edge_number() << " edges" << '\n';

	for (auto a : g.vertices()) {
		out << "Degree of " << a << " is " << g.degree(a)
		    << " (out: " << g.degree_out(a) << ")"
		    << " (in: " << g.degree_in(a) << ")\n";
		for (auto b : g.neighbours(a))
			out << "  |-> " << b
			    << " (w: " << g.edges(a)[b] << ")" << '\n'; // g.edges(a)[b] === g.weight(a,b)
	}

	return out;
}

int main(int argc, char const *argv[])
{
	using Graph = structures::Graph<char,float,true>;

	Graph g(5);
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
	g.link('a', 'b', HUGE_VALF);
	// g.unlink('a', 'b');
	std::cout << g.weight('a', 'b') << '\n';
	std::cout << g << '\n';

	g.erase('d');
	std::cout << g;// << '\n';

	return 0;
}