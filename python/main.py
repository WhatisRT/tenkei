from cffi import FFI
ffibuilder = FFI()

ffibuilder.set_source("_example",
   r"""
    """,
                      libraries=['HShaskell-test-lib-0.1.0.0 ', 'HSrts'],
                      library_dirs=['./', '/Library/Frameworks/GHC.framework/Versions/Current/usr/lib/ghc-8.2.2/rts/'],
                      extra_compile_args=['-arch x86_64', '-Wl,-rpath=/Library/Frameworks/GHC.framework/Versions/Current/usr/lib/ghc-8.2.2/rts/'])

ffibuilder.cdef("""
        void tenkei_modify_array(
            uint8_t *input,
            size_t input_len,
            uint8_t **output,
            size_t *output_len
        );
""")

if __name__ == "__main__":
    ffibuilder.compile(verbose=True)
