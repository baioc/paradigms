// SWIG interface definition
%module graphs
%{
	// preprocessor directives
	//
	// headers directly included into wrapper code
	#include "graph.hpp"
%}

// parse files to generate wrappers
%include "graph.hpp"

// ignores
// @note: method templates that take generic iterators as arguments are not
// instantiated, thus not included in the wrapper
//

// wrap standard headers
%include "std_string.i"
%include "std_vector.i"
%include "std_unordered_map.i"

// explicit template instantiation
%template(Graph) structures::Graph<std::string,double>;
%template(Digraph) structures::Graph<std::string,double,true>;
%template(VectorString) std::vector<std::string>;
%template(UnorderedMapStringDouble) std::unordered_map<std::string,double>;

// type mapping
//
