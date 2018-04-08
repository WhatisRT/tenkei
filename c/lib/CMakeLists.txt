cmake_minimum_required(VERSION 2.8)
project(test-library)

file(GLOB SOURCES "*.c")

add_library(test-library SHARED ${SOURCES})

target_link_libraries(test-library cbor)
