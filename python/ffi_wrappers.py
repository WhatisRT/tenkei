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
