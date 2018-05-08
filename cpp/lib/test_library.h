#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <vector>

#include "test_library.cpp"

std::vector<uint32_t> library_language();

bool binary_or(bool, bool);

std::vector<int32_t> modify_array(std::vector<int32_t>);

int32_t exponentiate(int32_t, int32_t);

struct tenkei_value identity(struct tenkei_value);

struct tenkei_value choose_left(struct tenkei_value, struct tenkei_value);

std::vector<tenkei_value> reverse_list(std::vector<tenkei_value>);
