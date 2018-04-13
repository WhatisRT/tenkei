import test_library

test_list = [1,2,3,4,5,6,7,8,9,10]

print("python")
print(''.join(str(chr(e)) for e in test_library.library_language()))
print(test_library.modify_array(test_list))
print(test_library.exponentiate(2,3))
print(test_library.identity(test_list))
print(test_library.choose_left(1,2))
print(test_library.reverse_list(test_list))
