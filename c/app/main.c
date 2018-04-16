#include "cbor.h"
#include <stdio.h>
#include "test_library_tenkei.c"
#include "../common/list_serializers.h"

void print_list(const struct list_int32_t l)
{
  printf("[");
  for(int i = 0; i < l.length-1; i++)
    printf("%d,", l.start[i]);

  printf("%d]\n", l.start[l.length-1]);

  fflush(stdout);
}

void print_list_char(const struct list_int32_t l)
{
  for(int i = 0; i < l.length; i++)
    printf("%c", (char)l.start[i]);

  printf("\n");
}

struct list_tenkei_value list_int32_t_to_list_tenkei_value(const struct list_int32_t l)
{
  struct tenkei_value *list_ = malloc(sizeof(struct tenkei_value) * l.length);

  for(int i = 0; i < l.length; i++)
  {
    struct tenkei_value temp = {serialize_int32_t(l.start[i])};
    list_[i] = temp;
  }

  struct list_tenkei_value res = {list_, l.length};

  return res;
}

struct list_int32_t list_tenkei_value_to_list_int32_t(const struct list_tenkei_value l)
{
  int32_t *list_ = malloc(sizeof(int32_t) * l.length);

  for(int i = 0; i < l.length; i++)
    list_[i] = (deserialize_int32_t(l.start[i].contents));

  struct list_int32_t res = {list_, l.length};

  return res;
}

struct list_int32_t map_int32_t(int32_t (*f)(int32_t), const struct list_int32_t l)
{
  int32_t *list_ = malloc(sizeof(int32_t) * l.length);

  for(int i = 0; i < l.length; i++)
    list_[i] = f(l.start[i]);

  struct list_int32_t res = {list_, l.length};
  return res;
}

int32_t exp_2(int32_t e)
{
  return exponentiate(2,e);
}

struct tenkei_value head(struct list_tenkei_value l)
{
  return l.start[0];
}

cbor_item_t *cbor_head(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  struct list_tenkei_value arg0 = deserialize_list_tenkei_value(arg_list[0]);
  struct tenkei_value res = head(arg0);
  cbor_item_t *result = serialize_tenkei_value(res);
  return result;
}

void tenkei_head(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(&cbor_head, input, input_len, output, output_len);
}

int main(int argc, char *argv[])
{
#ifdef HASKELL_LIBRARY
  hs_init(&argc, &argv);
#endif

  printf("c\n");

  print_list_char(library_language());

  printf(binary_or(true, false) ? "True\n" : "False\n");

  int list_[] = {1,2,3,4,5,6,7,8,9,10};
  struct list_int32_t list = {list_, 10};

  struct list_int32_t res = modify_array(list);

  print_list(res);

  print_list(map_int32_t(exp_2, list));

  struct tenkei_value arg = {serialize_list_int32_t(list)};

  struct list_int32_t res3 = deserialize_list_int32_t(identity(arg).contents);
  print_list(res3);

  struct tenkei_value i1 = {serialize_int32_t(1)};
  struct tenkei_value i2 = {serialize_int32_t(2)};

  printf("%d\n", deserialize_int32_t(choose_left(i1, i2).contents));

  struct list_tenkei_value list3 = list_int32_t_to_list_tenkei_value(list);

  struct list_tenkei_value res4 = reverse_list(list3);

  struct list_int32_t res5 = list_tenkei_value_to_list_int32_t(res4);

  print_list(res5);

  struct tenkei_value list4 = {serialize_list_int32_t(list)};

  struct tenkei_value res6_ = apply_function(&tenkei_head, list4);

  int32_t res6 = deserialize_int32_t(res6_.contents);

  //printf("%d\n", res6);

  fflush(stdout);

#ifdef HASKELL_LIBRARY
  hs_exit();
#endif
}
