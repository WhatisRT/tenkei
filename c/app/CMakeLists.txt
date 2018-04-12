cmake_minimum_required(VERSION 2.8)

project(tenkei-c-test)

add_executable(test main.c)

set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -L../../tenkei-build")

find_library(TEST test-library HINTS ../../tenkei-build)

if(HASKELL_LIB)
  add_definitions(-DHASKELL_LIBRARY)

  find_library(HSRTS HSrts-ghc8.2.2 HINTS ${HASKELL_PATH}/rts)

  target_link_libraries(test cbor ${HSRTS} ${TEST})
else()
  target_link_libraries(test cbor ${TEST})
endif()
