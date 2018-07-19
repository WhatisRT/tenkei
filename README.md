# Build instructions

Compile build.hs (e.g. with `stack ghc`), and execute it with two parameters. The first parameter is the language of the executable, the second parameter the language of the library. Make sure the directory tenkei-build exists. To run the program, use `env LD_LIBRARY_PATH=. ./test-exe` in the tenkei-build directory.
