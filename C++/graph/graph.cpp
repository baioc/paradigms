#include "graph.hpp"

#include <iostream>


int main(int argc, char const *argv[])
{
	using namespace structures;

	Graph<char> g(1);


	// explicity inserting each node is optional
	// std::cout << "Graph has vertex a? " << g.contains('a') << std::endl;
	// g.insert('a');
	// std::cout << "Graph has vertex a? " << g.contains('a') << std::endl;

	// std::cout << "Graph has vertex b? " << g.contains('b') << std::endl;
	// g.insert('b');
	// std::cout << "Graph has vertex b? " << g.contains('b') << std::endl;

	// std::cout << "Graph has vertex c? " << g.contains('c') << std::endl;
	// g.insert('c');
	// std::cout << "Graph has vertex c? " << g.contains('c') << std::endl;

	// std::cout << "Graph has vertex d? " << g.contains('d') << std::endl;
	// g.insert('d');
	// std::cout << "Graph has vertex d? " << g.contains('d') << std::endl;

	// std::cout << "Graph has vertex e? " << g.contains('e') << std::endl;
	// g.insert('e');
	// std::cout << "Graph has vertex e? " << g.contains('e') << std::endl;

	// std::cout << std::endl;


	std::cout << "Graph has edge (a,b)? " << g.contains('a', 'b') << std::endl;
	g.link('a', 'b');
	std::cout << "Graph has edge (a,b)? " << g.contains('a', 'b') << std::endl;

	std::cout << "Graph has edge (b,c)? " << g.weight('b', 'c') << std::endl;
	g.link('b', 'c', 2);
	std::cout << "Graph has edge (b,c)? " << g.weight('b', 'c') << std::endl;

	std::cout << "Graph has edge (c,d)? " << g.weight('c', 'd') << std::endl;
	g.link('c', 'd', 3.25);
	std::cout << "Graph has edge (c,d)? " << g.weight('c', 'd') << std::endl;

	std::cout << "Graph has edge (d,b)? " << g.weight('d', 'b') << std::endl;
	g.link('d', 'b', 3.75);
	std::cout << "Graph has edge (d,b)? " << g.weight('d', 'b') << std::endl;

	std::cout << "Graph has edge (d,e)? " << g.weight('d', 'e') << std::endl;
	g.link('d', 'e', 2);
	std::cout << "Graph has edge (d,e)? " << g.weight('d', 'e') << std::endl;

	std::cout << "Graph has edge (e,c)? " << g.weight('e', 'c') << std::endl;
	g.link('e', 'c', 1);
	std::cout << "Graph has edge (e,c)? " << g.weight('e', 'c') << std::endl;

	std::cout << std::endl;


	std::cout << "Graph has a total of " << g.vertice_number() << " vertices." << std::endl;
	std::cout << "Graph has a total of " << g.edge_number() << " edges." << std::endl;
	std::cout << std::endl;


	for (auto ch : {'a', 'b', 'c', 'd', 'e'}) {
		std::cout << "Degree of " << ch << " is " << g.degree(ch) << ":" << std::endl;
		for (auto adj : g.neighbours(ch)) {
			std::cout << "|-> " << adj << std::endl;
		}
	}
	std::cout << std::endl;


	g.unlink('b', 'a');

	std::cout << "Graph has a total of " << g.vertice_number() << " vertices." << std::endl;
	std::cout << "Graph has a total of " << g.edge_number() << " edges." << std::endl;
	for (auto ch : g.vertices()) {
		std::cout << "Degree of " << ch << " is " << g.degree(ch) << ":" << std::endl;
		for (auto adj : g.neighbours(ch)) {
			std::cout << "|-> " << adj << std::endl;
		}
	}

	std::cout << std::endl;


	g.erase('d');

	std::cout << "Graph has a total of " << g.vertice_number() << " vertices." << std::endl;
	std::cout << "Graph has a total of " << g.edge_number() << " edges." << std::endl;
	for (auto ch : g.vertices()) {
		std::cout << "Degree of " << ch << " is " << g.degree(ch) << ":" << std::endl;
		for (auto adj : g.neighbours(ch)) {
			std::cout << "|-> " << adj << std::endl;
		}
	}

	// std::cout << std::endl;


	return 0;
}
