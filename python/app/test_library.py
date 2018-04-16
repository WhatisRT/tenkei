from _example import ffi, lib
import sys
sys.path.append("../python/libtenkei-python")
from ffi_wrappers import call

# we have to call hs_init somehow

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
