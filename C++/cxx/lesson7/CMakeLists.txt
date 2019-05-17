cmake_minimum_required(VERSION 3.0)
project(renderer CXX)

add_executable(renderer)

target_sources(renderer
    PRIVATE
        main.cpp
        renderer.h
        renderer.cpp
        point.h
        point.cpp
)

target_compile_features(renderer
    PRIVATE
        cxx_std_17
)
