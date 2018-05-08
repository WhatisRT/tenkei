cmake_minimum_required(VERSION 2.8)
project(tenkei-c)

file(GLOB SOURCES "*.cpp")

add_library(tenkei-cpp SHARED ${SOURCES})

target_link_libraries(tenkei-cpp cbor)
