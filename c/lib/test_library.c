#include <stdlib.h>
#include <stdbool.h>

#include "../libtenkei-c/serializers.h"
//#include "../libtenkei-c/ffi_wrappers.h"
#include "../common/list_types.h"

static struct list_int32_t library_language(void)
{
  int32_t *list_ = malloc(sizeof(int32_t) * 1);
  struct list_int32_t res = {list_, 1};
  list_[0] = 99;

  return res;
}

static bool binary_or(bool a, bool b)
{
  return a || b;
}

static struct list_int32_t modify_array(struct list_int32_t l)
{
  for(int i = 0; i < l.length; i++)
  {
    l.start[i] *= (i + 1);
  }

  return l;
}

static int32_t exponentiate(int32_t x, int32_t y)
{
  int32_t acc = 1;
  for(int i = 0; i < y; i++)
    acc *= x;

  return acc;
}

static struct tenkei_value identity(struct tenkei_value x)
{
  return x;
}

static struct tenkei_value choose_left(struct tenkei_value l, struct tenkei_value r)
{
  return l;
}

static struct list_tenkei_value reverse_list(struct list_tenkei_value l)
{
  struct tenkei_value *list_ = malloc(sizeof(struct tenkei_value) * l.length);
  struct list_tenkei_value res = {list_, l.length};

  for(int i = 0; i < l.length; i++)
    list_[i] = l.start[l.length - i - 1];

  return res;
}

static struct tenkei_value apply_function(void (*f)(uint8_t *, size_t, uint8_t **, size_t *), struct tenkei_value x)
{
  cbor_item_t *arg = cbor_new_definite_array(1);
  cbor_array_push(arg, x.contents);
  struct tenkei_value res = {call_cbor(f, arg)};
  return res;
}
