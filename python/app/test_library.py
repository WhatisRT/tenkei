from my_plugin import ffi, lib
import sys
sys.path.append("../python/libtenkei-python")
from ffi_wrappers import call, offer
import re

def library_language(*args):
    return call(lib.tenkei_library_language)(*args)

def binary_or(*args):
    return call(lib.tenkei_binary_or)(*args)

def modify_array(*args):
    return call(lib.tenkei_modify_array)(*args)

def exponentiate(*args):
    return call(lib.tenkei_exponentiate)(*args)

def identity(*args):
    return call(lib.tenkei_identity)(*args)

def choose_left(*args):
    return call(lib.tenkei_choose_left)(*args)

def reverse_list(*args):
    return call(lib.tenkei_reverse_list)(*args)

def apply_function(f, *args):
    data = fun_to_fun_ptr(f)
    return call(lib.tenkei_apply_function)(data, *args)


FUN_POINTERS=[]
callback_address = int(ffi.cast("uintptr_t", lib.tenkei_callback))

def fun_to_fun_ptr(f):
    # handle = ffi.new_handle(f)
    # FUN_POINTERS.append(handle)
    FUN_POINTERS.append(f)

    return [callback_address, 0, len(FUN_POINTERS) - 1]

@ffi.def_extern()
def tenkei_callback(data, *args):
    f = FUN_POINTERS[int(ffi.cast("uintptr_t", data))]
    return offer(f)(*args)
