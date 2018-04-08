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

struct list_int32_t modify_array(struct list_int32_t l)
{
  for(int i = 0; i < l.length; i++)
  {
    l.start[i] *= i;
  }

  return l;
}

int32_t exponentiate(int32_t x, int32_t y)
{
  return x ^ y;
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
  {
    list_[i] = l.start[l.length - i];
  }

  return res;
}
