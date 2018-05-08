#include "../libtenkei-cpp/ffi_wrappers.h"
#include "test_library.h"
#include "../libtenkei-cpp/serializers.h"

extern "C" void tenkei_free(uint8_t *arg, size_t len)
{
  return;
}

cbor_item_t *cbor_library_language(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  std::vector<uint32_t> res = library_language();
  cbor_item_t *result = serialize(res);
  return result;
};

extern "C" void tenkei_library_language(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_library_language, input, input_len, output, output_len);
}

cbor_item_t *cbor_binary_or(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  bool arg0 = deserialize<bool>(arg_list[0]);
  bool arg1 = deserialize<bool>(arg_list[1]);
  bool res = binary_or(arg0, arg1);
  cbor_item_t *result = serialize(res);
  return result;
};

extern "C" void tenkei_binary_or(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_binary_or, input, input_len, output, output_len);
}

cbor_item_t *cbor_modify_array(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  std::vector<int32_t> arg0 = deserialize<std::vector<int32_t>>(arg_list[0]);
  std::vector<int32_t> res = modify_array(arg0);
  cbor_item_t *result = serialize(res);
  return result;
};

extern "C" void tenkei_modify_array(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_modify_array, input, input_len, output, output_len);
}

cbor_item_t *cbor_exponentiate(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  int32_t arg0 = deserialize<int32_t>(arg_list[0]);
  int32_t arg1 = deserialize<int32_t>(arg_list[1]);
  int32_t res = exponentiate(arg0, arg1);
  cbor_item_t *result = serialize(res);
  return result;
};

extern "C" void tenkei_exponentiate(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_exponentiate, input, input_len, output, output_len);
}

cbor_item_t *cbor_identity(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  struct tenkei_value arg0 = deserialize<tenkei_value>(arg_list[0]);
  struct tenkei_value res = identity<tenkei_value>(arg0);
  cbor_item_t *result = serialize(res);
  return result;
};

extern "C" void tenkei_identity(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_identity, input, input_len, output, output_len);
}

cbor_item_t *cbor_choose_left(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  struct tenkei_value arg0 = deserialize<tenkei_value>(arg_list[0]);
  struct tenkei_value arg1 = deserialize<tenkei_value>(arg_list[1]);
  struct tenkei_value res = choose_left<tenkei_value, tenkei_value>(arg0, arg1);
  cbor_item_t *result = serialize(res);
  return result;
};

extern "C" void tenkei_choose_left(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_choose_left, input, input_len, output, output_len);
}

cbor_item_t *cbor_reverse_list(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  std::vector<tenkei_value> arg0 = deserialize<std::vector<tenkei_value>>(arg_list[0]);
  std::vector<tenkei_value> res = reverse_list<tenkei_value>(arg0);
  cbor_item_t *result = serialize(res);
  return result;
};

extern "C" void tenkei_reverse_list(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_reverse_list, input, input_len, output, output_len);
}

cbor_item_t *cbor_apply_function(cbor_item_t *args)
{
  cbor_item_t **arg_list = cbor_array_handle(args);
  struct tenkei_fun_ptr arg0 = deserialize<tenkei_fun_ptr>(arg_list[0]);
  struct tenkei_value arg1 = deserialize<tenkei_value>(arg_list[1]);
  struct tenkei_value res = apply_function<tenkei_value, tenkei_value>(arg0, arg1);
  cbor_item_t *result = serialize(res);
  return result;
};

extern "C" void tenkei_apply_function(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)
{
  offer_cbor(cbor_apply_function, input, input_len, output, output_len);
}

