cmake_minimum_required(VERSION 2.8)
project(test-library)

add_library(test-library SHARED test_library_tenkei.c)

target_link_libraries(test-library cbor)
