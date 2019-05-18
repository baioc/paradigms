cmake_minimum_required(VERSION 3.0)
project(renderer CXX)

add_executable(evil_renderer
	inheritance.cpp
)

target_sources(evil_renderer
	PRIVATE
		inheritance.cpp
		renderer.h
		renderer.cpp
		unique_ptr.hpp
)

target_compile_features(evil_renderer
	PRIVATE
		cxx_std_17
)


add_executable(nice_renderer
	polymorphism.cpp
)

target_sources(nice_renderer
	PRIVATE
		polymorphism.cpp
		renderer.h
		renderer.cpp
		unique_ptr.hpp
)

target_compile_features(nice_renderer
	PRIVATE
		cxx_std_17
)
