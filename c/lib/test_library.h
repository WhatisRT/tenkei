#include <stdlib.h>

struct list_int32_t {
  int32_t *start;
  unsigned int length;
};

struct list_char {
  char *start;
  unsigned int length;
};

struct list_tenkei_ptr {
  void * *start;
  unsigned int length;
};

void tenkei_free(uint8_t *, size_t);

struct list_int32_t library_language();

struct list_int32_t modify_array(struct list_int32_t);

int32_t exponentiate(int32_t, int32_t);

void *identity(void *);

void *choose_left(void *, void *);

struct list_tenkei_ptr reverse_list(struct list_tenkei_ptr);
