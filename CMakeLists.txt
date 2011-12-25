project(apl CXX)
cmake_minimum_required(VERSION 2.8)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
include_directories(SYSTEM ${CMAKE_SOURCE_DIR}/include)

add_subdirectory(src)
