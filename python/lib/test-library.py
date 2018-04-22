def library_language():
    return [112,121,116,104,111,110]

def binary_or(x, y):
    return x or y

def modify_array(x):
    for i in range(0, len(x)):
        x[i] = x[i] * (i + 1)
    return x

def exponentiate(x, y):
    return x ** y

def identity(x):
    return x

def choose_left(x, y):
    return x

def reverse_list(l):
    return list(reversed(l))

def apply_function(f, x):
    return f(x)
