#include "graph.hpp"

#include <iostream>
#include <cmath> // HUGE_VAL


template <typename L, typename W, bool d>
std::ostream& operator<<(std::ostream&  out, const structures::Graph<L,W,d>& g) {
	out << "Graph has a total of "
	    << g.node_number() << " nodes" << " and "
	    << g.edge_number() << " edges" << '\n';

	for (auto a : g.nodes()) {
		auto u = a.first;
		out << "Degree of " << u << " is " << g.degree(u)
		    << " (out: " << g.degree_out(u) << ")"
		    << " (in: " << g.degree_in(u) << ")\n";
		for (auto b : g.neighbours(u)) {
			auto v = b.first;
			out << "  |-> " << v
			    << " (w: " << g.weight(u,v) << ")" << '\n';
		}
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

	g.link('a', 'a'); // should do nothing
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
