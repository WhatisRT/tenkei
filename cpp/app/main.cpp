#include "cbor.h"
#include <stdio.h>
#include "test_library_tenkei.cpp"
#include <vector>
#include <algorithm>

void print_list(const std::vector<int32_t> l)
{
  printf("[");
  for(int i = 0; i < l.size()-1; i++)
    printf("%d,", l[i]);

  printf("%d]\n", l[l.size()-1]);

  fflush(stdout);
}

void print_list_char(const std::vector<uint32_t> l)
{
  for(int i = 0; i < l.size(); i++)
    printf("%c", (char)l[i]);

  printf("\n");
}

template <typename T> tenkei_value to_tenkei_value(T x)
{
  return {serialize<T>(x)};
}

template <typename T> T from_tenkei_value(tenkei_value x)
{
  return deserialize<T>(x.contents);
}

template <typename T> std::vector<tenkei_value> list_to_list_tenkei_value(std::vector<T> l)
{
  std::vector<tenkei_value> result(l.size());

  std::transform(l.begin(), l.end(), result.begin(), to_tenkei_value<T>);

  return result;
}

template <typename T> std::vector<T> list_tenkei_value_to_list(const std::vector<tenkei_value> l)
{
  std::vector<T> result(l.size());

  std::transform(l.begin(), l.end(), result.begin(), from_tenkei_value<T>);

  return result;
}

template <typename T> std::vector<T> map(T (*f)(T), const std::vector<T> l)
{
  std::vector<T> result(l.size());

  std::transform(l.begin(), l.end(), result.begin(), f);

  return result;
}

int32_t exp_2(int32_t e)
{
  return exponentiate(2,e);
}

template <typename T> T head(std::vector<T> l)
{
  return l[0];
}

cbor_item_t *cbor_head(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  std::vector<tenkei_value> arg0 = deserialize<std::vector<tenkei_value>>(arg_list[0]);
  struct tenkei_value res = head(arg0);
  cbor_item_t *result = serialize<tenkei_value>(res);
  return result;
}

int main(int argc, char *argv[])
{
  printf("c++\n");

  print_list_char(library_language());

  printf(binary_or(true, false) ? "True\n" : "False\n");

  std::vector<int32_t> list({1,2,3,4,5,6,7,8,9,10});

  auto res = modify_array(list);

  print_list(res);

  print_list(map(exp_2, list));

  struct tenkei_value arg = {serialize(list)};

  std::vector<int32_t> res3 = deserialize<std::vector<int32_t>>(identity(arg).contents);
  print_list(res3);

  struct tenkei_value i1 = {serialize<int32_t>(1)};
  struct tenkei_value i2 = {serialize<int32_t>(2)};

  printf("%d\n", deserialize<int32_t>(choose_left(i1, i2).contents));

  std::vector<tenkei_value> list3 = list_to_list_tenkei_value(list);

  std::vector<tenkei_value> res4 = reverse_list(list3);

  std::vector<int32_t> res5 = list_tenkei_value_to_list<int32_t>(res4);

  print_list(res5);

  std::vector<int32_t> list4_1({1, 2});
  std::vector<int32_t> list4_2({3, 4});
  std::vector<std::vector<int32_t>> list4({list4_1, list4_2});

  struct tenkei_value val = {serialize(list4)};

  struct tenkei_value res6_ = apply_function(offer_fun_ptr_cbor(&cbor_head), val);

  std::vector<int32_t> res6 = deserialize<std::vector<int32_t>>(res6_.contents);

  print_list(res6);

  fflush(stdout);
}
