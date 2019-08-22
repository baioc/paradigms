// SWIG interface definition
%module graphs
%{
	// preprocessor directives directly included into wrapper code
	#include "graph.hpp"
%}

// wrap standard headers
%include "std_string.i"
%include "std_unordered_map.i"

// ignores
//

// parse files to generate wrappers
%include "graph.hpp"

// explicit template instantiation
%template(Graph) structures::Graph<std::string,double>;
%template(GraphEdges) std::unordered_map<std::string,double>;
%template(GraphNodes) std::unordered_map<std::string,std::unordered_map<std::string,double>>;
%template(Digraph) structures::Graph<std::string,double,true>;

// type mapping
//
