from collections import Counter
from my_plugin import ffi
import cbor2

def modify_array(x):
    for i in range(0, len(x)):
        x[i] = x[i] * (i + 1)
    return x

def exponentiate(x, y):
    return x ** y

DONT_FORGET=Counter()

def offer(f):
    def offered(input, input_len, output, output_len):
        result = cbor2.dumps(f(*cbor2.loads(bytes(ffi.buffer(input, input_len)))))
        result_ptr = ffi.cast("uint8_t *", ffi.from_buffer(result))
        output[0] = result_ptr
        output_len[0] = len(result)
        DONT_FORGET[result_ptr] += 1
    return offered

@ffi.def_extern()
def tenkei_modify_array(*args):
    return offer(modify_array)(*args)

@ffi.def_extern()
def tenkei_exponentiate(*args):
    return offer(exponentiate)(*args)

@ffi.def_extern()
def tenkei_free(buffer, buffer_len):
    DONT_FORGET[buffer] -= 1
    if DONT_FORGET[buffer] == 0:
        del DONT_FORGET[buffer]
