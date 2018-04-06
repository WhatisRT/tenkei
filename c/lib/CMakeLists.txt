cmake_minimum_required(VERSION 2.8)
project(tenkei-c)

file(GLOB SOURCES "*.c")

add_library(tenkei-c SHARED ${SOURCES})

target_link_libraries(tenkei-c cbor)
