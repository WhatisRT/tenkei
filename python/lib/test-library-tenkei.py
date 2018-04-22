import cffi

def read_file(filename):
    with open(filename) as f:
        return f.read()

def main():
    ffibuilder = cffi.FFI()
    ffibuilder.embedding_api("""
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
        
        void tenkei_free(
            uint8_t *buffer,
            size_t buffer_len
        );
    """)
    ffibuilder.set_source("my_plugin", "")
    ffibuilder.embedding_init_code(read_file("../libtenkei-python/ffi_wrappers.py") + "\n\n" +
                                   """
from collections import Counter

DONT_FORGET=Counter()

@ffi.def_extern()
def tenkei_free(buffer, buffer_len):
    DONT_FORGET[buffer] -= 1
    if DONT_FORGET[buffer] == 0:
        del DONT_FORGET[buffer]

@ffi.def_extern()
def tenkei_library_language(*args):
    return offer_with_conversion(library_language, [])(*args)

@ffi.def_extern()
def tenkei_binary_or(*args):
    return offer_with_conversion(binary_or, [])(*args)

@ffi.def_extern()
def tenkei_modify_array(*args):
    return offer_with_conversion(modify_array, [])(*args)

@ffi.def_extern()
def tenkei_exponentiate(*args):
    return offer_with_conversion(exponentiate, [])(*args)

@ffi.def_extern()
def tenkei_identity(*args):
    return offer_with_conversion(identity, [])(*args)

@ffi.def_extern()
def tenkei_choose_left(*args):
    return offer_with_conversion(choose_left, [])(*args)

@ffi.def_extern()
def tenkei_reverse_list(*args):
    return offer_with_conversion(reverse_list, [])(*args)

@ffi.def_extern()
def tenkei_apply_function(*args):
    return offer_with_conversion(apply_function, [0])(*args)

""" +
                                   read_file("test-library.py"))
    ffibuilder.compile(target="libtest-library.*")

if __name__ == '__main__':
    main()
