#include <stdlib.h>
#include <stdbool.h>
#include <vector>

#include "../libtenkei-cpp/serializers.h"
//#include "../libtenkei-c/ffi_wrappers.h"

static std::vector<uint32_t> library_language(void)
{
  std::vector<uint32_t> res {99, 112, 112};

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

template <typename T> static T identity(T x)
{
  return x;
}

template <typename T, typename U> static T choose_left(T l, U r)
{
  return l;
}

template <typename T> static std::vector<T> reverse_list(std::vector<T> l)
{
  std::vector<T> res(l.size());

  for(int i = 0; i < l.size(); i++)
    res[i] = l[l.size() - i - 1];

  return res;
}

template <typename T, typename U> static struct tenkei_value apply_function(struct tenkei_fun_ptr f, struct tenkei_value x)
{
  cbor_item_t *arg = cbor_new_definite_array(1);
  cbor_array_push(arg, x.contents);
  struct tenkei_value res = {call_fun_ptr_cbor(f, arg)};
  return res;
}
