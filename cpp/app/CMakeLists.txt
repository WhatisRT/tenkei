cmake_minimum_required(VERSION 2.8)
project(tenkei-cpp-test)

set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -L../../tenkei-build")

add_executable(test-exe main.cpp)

find_library(TEST_LIBRARY test-library HINTS ../../tenkei-build)
target_link_libraries(test-exe cbor ${TEST_LIBRARY})
