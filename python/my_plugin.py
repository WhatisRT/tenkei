import cffi

def read_file(filename):
    with open(filename) as f:
        return f.read()

def main():
    ffibuilder = cffi.FFI()
    ffibuilder.embedding_api("""
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

        void tenkei_free(
            uint8_t *buffer,
            size_t buffer_len
        );
    """)
    ffibuilder.set_source("my_plugin", "")
    ffibuilder.embedding_init_code(read_file("plugin.py"))
    ffibuilder.compile(target="libmy_plugin.*")

if __name__ == '__main__':
    main()
