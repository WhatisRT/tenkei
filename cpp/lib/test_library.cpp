#include <stdlib.h>
#include <stdbool.h>
#include <vector>

#include "../libtenkei-cpp/serializers.h"
//#include "../libtenkei-c/ffi_wrappers.h"

static std::vector<uint32_t> library_language(void)
{
  std::vector<uint32_t> res {99, 43, 43};

  return res;
}

static bool binary_or(bool a, bool b)
{
  return a || b;
}

static std::vector<int32_t> modify_array(std::vector<int32_t> l)
{
  for(int i = 0; i < l.size(); i++)
  {
    l[i] *= (i + 1);
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

static std::vector<tenkei_value> reverse_list(std::vector<tenkei_value> l)
{
  std::vector<tenkei_value> res(l.size());

  for(int i = 0; i < l.size(); i++)
    res[i] = l[l.size() - i - 1];

  return res;
}

static struct tenkei_value apply_function(struct tenkei_fun_ptr f, struct tenkei_value x)
{
  cbor_item_t *arg = cbor_new_definite_array(1);
  cbor_array_push(arg, x.contents);
  struct tenkei_value res = {call_fun_ptr_cbor(f, arg)};
  return res;
}
