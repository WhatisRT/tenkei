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

struct list_int32_t modify_array(struct list_int32_t l);

int32_t exponentiate(int32_t x, int32_t y);

void *identity(void *x);

void *choose_left(void *l, void *r);

struct list_tenkei_ptr reverse_list(struct list_tenkei_ptr l);
