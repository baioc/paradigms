// SWIG interface definition
%module graphs
%{
	// preprocessor directives
	//
	// headers directly included into wrapper code
	#include "graph.hpp"
%}

// wrap standard headers
%include "std_string.i"
%include "std_vector.i"
%include "std_unordered_map.i"

// ignores
// @note: method templates that take generic iterators as arguments are not
// instantiated, thus not included in the wrapper
//

// parse files to generate wrappers
%include "graph.hpp"

// explicit template instantiation
%template(Graph) structures::Graph<std::string,double>;
%template(Digraph) structures::Graph<std::string,double,true>;
%template(VectorString) std::vector<std::string>;
%template(UnorderedMapStringDouble) std::unordered_map<std::string,double>;

// type mapping
//
