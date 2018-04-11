import cbor2
from my_plugin import ffi

def offer(f):
    def offered(input, input_len, output, output_len):
        result = cbor2.dumps(f(*cbor2.loads(bytes(ffi.buffer(input, input_len)))))
        result_ptr = ffi.cast("uint8_t *", ffi.from_buffer(result))
        output[0] = result_ptr
        output_len[0] = len(result)
        DONT_FORGET[result_ptr] += 1
    return offered

def call(f):
    def called(*args):
        args_bin = cbor2.dumps(args)
        result_ptr = ffi.new("uint8_t **")
        result_len = ffi.new("size_t *")
        f(ffi.from_buffer(args_bin), len(args_bin), result_ptr, result_len)
        result = cbor2.loads(bytes(ffi.buffer(result_ptr, result_len)))
        return result
    return called
