from collections import Counter
from my_plugin import ffi
import cbor2

def quintuple_impl(x):
    return x * 5

DONT_FORGET=Counter()

@ffi.def_extern()
def quintuple(input, input_len, output, output_len):
    result = cbor2.dumps(quintuple_impl(cbor2.loads(bytes(ffi.buffer(input, input_len)))))
    result_ptr = ffi.cast("uint8_t *", ffi.from_buffer(result))
    output[0] = result_ptr
    output_len[0] = len(result)
    DONT_FORGET[result_ptr] += 1

@ffi.def_extern()
def tenkei_free(buffer, buffer_len):
    DONT_FORGET[buffer] -= 1
    if DONT_FORGET[buffer] == 0:
        del DONT_FORGET[buffer]
