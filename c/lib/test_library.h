#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

#include "test_library.c"

void tenkei_free(uint8_t *, size_t);

struct list_uint32_t library_language();

bool binary_or(bool, bool);

struct list_int32_t modify_array(struct list_int32_t);

int32_t exponentiate(int32_t, int32_t);

struct tenkei_value identity(struct tenkei_value );

struct tenkei_value choose_left(struct tenkei_value , struct tenkei_value );

struct list_tenkei_value reverse_list(struct list_tenkei_value);
