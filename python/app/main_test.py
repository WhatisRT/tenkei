from _example import ffi, lib
import sys
sys.path.append("../libtenkei-python")
from ffi_wrappers import call

# we have to call hs_init somehow

print(call(lib.tenkei_modify_array)([1,2,3]))
