import cbor2
from my_plugin import ffi
from collections import Counter

DONT_FORGET=Counter()

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
        result = cbor2.loads(bytes(ffi.buffer(result_ptr[0], result_len[0])))
        return result
    return called

def call_fun_ptr(f):
    callback = ffi.cast("void (*)(void *, uint8_t *, size_t, uint8_t **, size_t *)", f[0])
    free = f[1]
    data = f[2]

    def called(*args):
        args_bin = cbor2.dumps(args)
        result_ptr = ffi.new("uint8_t **")
        result_len = ffi.new("size_t *")
        callback(ffi.cast("void *", data), ffi.from_buffer(args_bin), len(args_bin), result_ptr, result_len)
        result = cbor2.loads(bytes(ffi.buffer(result_ptr[0], result_len[0])))
        return result
    return called

def offer_with_conversion(f, i):
    def offered(input, input_len, output, output_len):
        args = cbor2.loads(bytes(ffi.buffer(input, input_len)))
        convert_args(args, i)
        result = cbor2.dumps(f(*args))
        result_ptr = ffi.cast("uint8_t *", ffi.from_buffer(result))
        output[0] = result_ptr
        output_len[0] = len(result)
        DONT_FORGET[result_ptr] += 1
    return offered

def convert_args(arg_list, indices):
    for i in indices:
        arg_list[i] = call_fun_ptr(arg_list[i])
