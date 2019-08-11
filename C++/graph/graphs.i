// SWIG interface definition
%module graphs
%{
	// preprocessor directives
	//
	// headers included into wrapper code
	#include "graph.hpp"
%}

// parse files to generate wrappers
%include "graph.hpp"
%include "std_string.i"
%include "std_vector.i"
%include "std_unordered_map.i"

// explicit template instantiation
%template(Graph) structures::Graph<std::string>;
%template(VectorString) std::vector<std::string>;
%template(UnorderedMapStringDouble) std::unordered_map<std::string,double>;

// type mapping
//
