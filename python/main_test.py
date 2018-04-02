from _example import ffi, lib

# we have to call hs_init somehow

print(call(lib.tenkei_modify_array)([1,2,3]))
