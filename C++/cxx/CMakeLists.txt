cmake_minimum_required( VERSION 3.0 )
project( magic CXX )

add_executable( magic client.cpp )

target_sources(magic
	PRIVATE
		client.cpp
		library.hpp
		unique_ptr.hpp
		algorithm.hpp
)

find_package ( Threads REQUIRED )

target_link_libraries ( magic ${CMAKE_THREAD_LIBS_INIT} )
