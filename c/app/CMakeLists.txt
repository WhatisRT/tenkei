cmake_minimum_required(VERSION 2.8)

project(tenkei-c-test)

add_executable(test main.c)

set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -L.")

#find_library(HSRTS HSrts HINTS ${HASKELL_PATH}/rts)
#find_library(HSGHC HSghc-prim-0.5.1.1 HINTS ${HASKELL_PATH}/ghc-prim-0.5.1.1)
#find_library(HSBASE HSbase-4.10.1.0 HINTS ${HASKELL_PATH}/base-4.10.1.0)
#find_library(HSINT_GMP HSinteger-gmp-1.0.1.0 HINTS ${HASKELL_PATH}/integer-gmp-1.0.1.0)
#target_link_libraries(test cbor ffi iconv ${HSRTS} ${HSGHC} ${HSBASE} ${HSINT_GMP} test-library)

find_library(TEST test-library HINTS ../lib)
target_link_libraries(test cbor ${TEST})
