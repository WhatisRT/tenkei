#include "test_library.h"
#include "../app/serializers.c"
#include "../libtenkei-c/ffi_wrappers.c"

void tenkei_free(uint8_t *arg, size_t len)
{
  return;
}

cbor_item_t *cbor_library_language(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  struct list_int32_t res = library_language();
  cbor_item_t *result = serialize_list_int32_t(res);
  return result;
};

void tenkei_library_language(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_library_language, input, input_len, output, output_len);
}

cbor_item_t *cbor_modify_array(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  struct list_int32_t arg0 = deserialize_list_int32_t(arg_list[0]);
  struct list_int32_t res = modify_array(arg0);
  cbor_item_t *result = serialize_list_int32_t(res);
  return result;
};

void tenkei_modify_array(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_modify_array, input, input_len, output, output_len);
}

cbor_item_t *cbor_exponentiate(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  int32_t arg0 = deserialize_int32_t(arg_list[0]);
  int32_t arg1 = deserialize_int32_t(arg_list[1]);
  int32_t res = exponentiate(arg0, arg1);
  cbor_item_t *result = serialize_int32_t(res);
  return result;
};

void tenkei_exponentiate(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_exponentiate, input, input_len, output, output_len);
}

cbor_item_t *cbor_identity(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  void* arg0 = deserialize_tenkei_ptr(arg_list[0]);
  void* res = identity(arg0);
  cbor_item_t *result = serialize_tenkei_ptr(res);
  return result;
};

void tenkei_identity(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_identity, input, input_len, output, output_len);
}

cbor_item_t *cbor_choose_left(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  void* arg0 = deserialize_tenkei_ptr(arg_list[0]);
  void* arg1 = deserialize_tenkei_ptr(arg_list[1]);
  void* res = choose_left(arg0, arg1);
  cbor_item_t *result = serialize_tenkei_ptr(res);
  return result;
};

void tenkei_choose_left(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_choose_left, input, input_len, output, output_len);
}

cbor_item_t *cbor_reverse_list(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  struct list_tenkei_ptr arg0 = deserialize_list_tenkei_ptr(arg_list[0]);
  struct list_tenkei_ptr res = reverse_list(arg0);
  cbor_item_t *result = serialize_list_tenkei_ptr(res);
  return result;
};

void tenkei_reverse_list(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_reverse_list, input, input_len, output, output_len);
}

