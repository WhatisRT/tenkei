# Build instructions

Compile build.hs (e.g. with stack ghc), and execute it with two parameters. The first parameter is the language of the executable, the second parameter the language of the library. Make sure the directory tenkei-build exists. To run the program, use env LD_LIBRARY_PATH=. ./test-exe in the tenkei-build directory.

# Tenkei Specification

All types are serialized using CBOR. See eg. wikipedia for details.

## Primitive types
Type     | Major types | Additional information | Notes
---------|-------------|------------------------|-----------------------------
unit     | ?           | ?                      | Not implemented
bool     | 0           | 0/1                    | Additional information contains the value
int8     | 0/1         | 0-24                   | Major type contains the sign
int16    | 0/1         | 0-25                   | See above
int32    | 0/1         | 0-26                   | See above
int64    | 0/1         | 0-27                   | See above
uint8    | 0           | 0-24                   |
uint16   | 0           | 0-25                   |
uint32   | 0           | 0-26                   |
uint64   | 0           | 0-27                   |
float16  | 7           | 25                     |
float32  | 7           | 26                     |
float64  | 7           | 27                     |
bigint   | 6           | 2                      | Followed by bytestring
bytestring | 2         | 0-31                   |
codepoint_unicode | 0  | 0-26                   |
string_utf8 | 3        | 0-27                   |
list     | 4           | 0-31                   |

### Automatic conversions

The types string_utf8 and list(codepoint_unicode) will be automatically converted to eachother when passed as function arguments. This means 

## Composite types
* Sums: A_1 + ... + A_n is encoded as a 2-element list: the first element is some index i, the second element is the serialized value of some element of type A_i
* Products: A_1 x ... x A_n is encoded as an n-element list

## Type variables
Types may leave variables in their definition (the canonical example being list). There are two strategies for serializing such types:
* Serialize everything
* Replace the type that was substituted into a variable with a pointer type, and serialize the pointer (as a regular unsigned integer)
The second strategy has the advantage of needing less work, but a function might require more information than just a pointer to be computed. Note: to be able to implement the second strategy, the type has to be a (traversable) functor on the type system.

## Function calls
Tenkei functions take a bytestring of CBOR values as arguments and return such a bytestring. All arguments have to be bundled together in a CBOR array, where the order of the arguments determines which CBOR value corresponds to which argument. Note that a one argument function still requires to be packaged into an array, which is an overhead of one byte.
The bytestrings are passed by specifying pointers to memory locations and giving the lenghts of the bytestrings. On the level of C libraries, a tenkei function f has the following signature:
`void tenkei_f(uint8_t *, size_t, uint8_t **, size_t *)`
The first two arguments correspond to the input bytestring, the second two to the output bytestrings.

## Functions & Type variables
Let S and T be two types with a type variable and A be a type without a type variable. A function of type A -> S a or S a -> A will in general need to serialize everything for the type S a to be computed, or to return the result. On the other hand, a function S a -> T a what works for all types a can never use any sepecific information about a, so it can instead be viewed as a function S ptr -> T ptr where the serializers only serialize the data up to the pointers. Thus, in the presence of type variables, the correct serialization strategy has to be chosen.

## Function pointers
Tenkei functions can accept function pointers (but the type of these function pointers must not contain type variables for the time being, see issue #2). These are passed around just like regular pointers. These functions must have the same (C) signature as other tenkei functions. Note: there is currently no mechanism to pass the correct free function required with function pointers.

## Next version of the specification
* Decide how to implement the primitives unit 
* Make arguments of the tenkei functions constant
* Tenkei functions should return int instead of void, to communicate basic errors
* Add a way of passing free functions together with function pointers
* Function pointers with type variables
* Bytestring type: use pointer and length instead of cbor serialization
