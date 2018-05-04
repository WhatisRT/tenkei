from my_plugin import ffi, lib
import sys
sys.path.append("../python/libtenkei-python")
from ffi_wrappers import call, offer

FUN_POINTERS=[]
callback_address = int(ffi.cast("uintptr_t", lib.tenkei_callback))

def fun_to_fun_ptr(f):
    # handle = ffi.new_handle(f)
    # FUN_POINTERS.append(handle)
    FUN_POINTERS.append(f)

    return [callback_address, 0, len(FUN_POINTERS) - 1]

def call_with_conversion(f, indices, arg_list):
    for i in indices:
        arg_list[i] = fun_to_fun_ptr(arg_list[i])

    return call(f)(*arg_list)

@ffi.def_extern()
def tenkei_callback(data, *args):
    f = FUN_POINTERS[int(ffi.cast("uintptr_t", data))]
    return offer(f)(*args)

def library_language(*args):
    return call_with_conversion(lib.tenkei_library_language, [], list(args))

def binary_or(*args):
    return call_with_conversion(lib.tenkei_binary_or, [], list(args))

def modify_array(*args):
    return call_with_conversion(lib.tenkei_modify_array, [], list(args))

def exponentiate(*args):
    return call_with_conversion(lib.tenkei_exponentiate, [], list(args))

def identity(*args):
    return call_with_conversion(lib.tenkei_identity, [], list(args))

def choose_left(*args):
    return call_with_conversion(lib.tenkei_choose_left, [], list(args))

def reverse_list(*args):
    return call_with_conversion(lib.tenkei_reverse_list, [], list(args))

def apply_function(*args):
    return call_with_conversion(lib.tenkei_apply_function, [0], list(args))

