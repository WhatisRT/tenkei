from cffi import FFI
ffibuilder = FFI()

ffibuilder.set_source("my_plugin",
   r"""
    """,
                      libraries=['test-library'],
                      library_dirs=['../../tenkei-build'])
                      #library_dirs=['./', '/Library/Frameworks/GHC.framework/Versions/Current/usr/lib/ghc-8.2.2/rts/'],
                      #extra_compile_args=['-arch x86_64', '-Wl,-rpath=/Library/Frameworks/GHC.framework/Versions/Current/usr/lib/ghc-8.2.2/rts/'])

ffibuilder.cdef("""
        void tenkei_library_language(
            uint8_t *input,
            size_t input_len,
            uint8_t **output,
            size_t *output_len
        );
        
        void tenkei_binary_or(
            uint8_t *input,
            size_t input_len,
            uint8_t **output,
            size_t *output_len
        );
        
        void tenkei_modify_array(
            uint8_t *input,
            size_t input_len,
            uint8_t **output,
            size_t *output_len
        );
        
        void tenkei_exponentiate(
            uint8_t *input,
            size_t input_len,
            uint8_t **output,
            size_t *output_len
        );
        
        void tenkei_identity(
            uint8_t *input,
            size_t input_len,
            uint8_t **output,
            size_t *output_len
        );
        
        void tenkei_choose_left(
            uint8_t *input,
            size_t input_len,
            uint8_t **output,
            size_t *output_len
        );
        
        void tenkei_reverse_list(
            uint8_t *input,
            size_t input_len,
            uint8_t **output,
            size_t *output_len
        );
        
        void tenkei_apply_function(
            uint8_t *input,
            size_t input_len,
            uint8_t **output,
            size_t *output_len
        );
        
""")

if __name__ == "__main__":
    ffibuilder.compile(verbose=True)
