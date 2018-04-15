import test_library

test_list = [1,2,3,4,5,6,7,8,9,10]

def print_bool(b):
    if b:
        print("True")
    else:
        print("False")

def print_list(l):
    print("[" + (''.join(str(c) + "," for c in l)[:-1] + "]"))

print("python")
print(''.join(str(chr(e)) for e in test_library.library_language()))
print_bool(test_library.binary_or(1, 0))
print_list(test_library.modify_array(test_list))
print_list(test_library.exponentiate(2,x) for x in test_list)
print_list(test_library.identity(test_list))
print(test_library.choose_left(1,2))
print_list(test_library.reverse_list(test_list))
