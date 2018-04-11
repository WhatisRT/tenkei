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

struct list_int32_t library_language()
{
  int32_t *list_ = malloc(sizeof(int32_t) * 1);
  struct list_int32_t res = {list_, 1};
  list_[0] = 99;

  return res;
}

struct list_int32_t modify_array(struct list_int32_t l)
{
  for(int i = 0; i < l.length; i++)
  {
    l.start[i] *= (i + 1);
  }

  return l;
}

int32_t exponentiate(int32_t x, int32_t y)
{
  int32_t acc = 1;
  for(int i = 0; i < y; i++)
    acc *= x;

  return acc;
}

void *identity(void *x)
{
  return x;
}

void *choose_left(void *l, void *r)
{
  return l;
}

struct list_tenkei_ptr reverse_list(struct list_tenkei_ptr l)
{
  void **list_ = malloc(sizeof(void *) * l.length);
  struct list_tenkei_ptr res = {list_, l.length};

  for(int i = 0; i < l.length; i++)
    list_[i] = l.start[l.length - i - 1];

  return res;
}
