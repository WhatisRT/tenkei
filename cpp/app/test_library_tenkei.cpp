#include "../libtenkei-cpp/ffi_wrappers.h"

void tenkei_free(uint8_t *buffer, size_t buffer_len);

extern "C" void tenkei_library_language(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
extern "C" void tenkei_binary_or(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
extern "C" void tenkei_modify_array(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
extern "C" void tenkei_exponentiate(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
extern "C" void tenkei_identity(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
extern "C" void tenkei_choose_left(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
extern "C" void tenkei_reverse_list(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
extern "C" void tenkei_apply_function(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);

std::vector<uint32_t> library_language()
{
  cbor_item_t *args = cbor_new_definite_array(0);
  cbor_item_t *res = call_cbor(tenkei_library_language, args);
  std::vector<uint32_t> result = deserialize<std::vector<uint32_t>>(res);
  cbor_decref(&args);
  return result;
}

bool binary_or(bool arg_0, bool arg_1)
{
  cbor_item_t *args = cbor_new_definite_array(2);
  cbor_item_t *arg1 = serialize<bool>(arg_0);
  cbor_array_push(args, arg1);
  cbor_item_t *arg2 = serialize<bool>(arg_1);
  cbor_array_push(args, arg2);
  cbor_item_t *res = call_cbor(tenkei_binary_or, args);
  bool result = deserialize<bool>(res);
  cbor_decref(&args);
  return result;
}

std::vector<int32_t> modify_array(std::vector<int32_t> arg_0)
{
  cbor_item_t *args = cbor_new_definite_array(1);
  cbor_item_t *arg1 = serialize<std::vector<int32_t>>(arg_0);
  cbor_array_push(args, arg1);
  cbor_item_t *res = call_cbor(tenkei_modify_array, args);
  std::vector<int32_t> result = deserialize<std::vector<int32_t>>(res);
  cbor_decref(&args);
  return result;
}

int32_t exponentiate(int32_t arg_0, int32_t arg_1)
{
  cbor_item_t *args = cbor_new_definite_array(2);
  cbor_item_t *arg1 = serialize<int32_t>(arg_0);
  cbor_array_push(args, arg1);
  cbor_item_t *arg2 = serialize<int32_t>(arg_1);
  cbor_array_push(args, arg2);
  cbor_item_t *res = call_cbor(tenkei_exponentiate, args);
  int32_t result = deserialize<int32_t>(res);
  cbor_decref(&args);
  return result;
}

struct tenkei_value identity(struct tenkei_value arg_0)
{
  cbor_item_t *args = cbor_new_definite_array(1);
  cbor_item_t *arg1 = serialize<tenkei_value>(arg_0);
  cbor_array_push(args, arg1);
  cbor_item_t *res = call_cbor(tenkei_identity, args);
  struct tenkei_value result = deserialize<tenkei_value>(res);
  cbor_decref(&args);
  return result;
}

struct tenkei_value choose_left(struct tenkei_value arg_0, struct tenkei_value arg_1)
{
  cbor_item_t *args = cbor_new_definite_array(2);
  cbor_item_t *arg1 = serialize<tenkei_value>(arg_0);
  cbor_array_push(args, arg1);
  cbor_item_t *arg2 = serialize<tenkei_value>(arg_1);
  cbor_array_push(args, arg2);
  cbor_item_t *res = call_cbor(tenkei_choose_left, args);
  struct tenkei_value result = deserialize<tenkei_value>(res);
  cbor_decref(&args);
  return result;
}

std::vector<tenkei_value> reverse_list(std::vector<tenkei_value> arg_0)
{
  cbor_item_t *args = cbor_new_definite_array(1);
  cbor_item_t *arg1 = serialize<std::vector<tenkei_value>>(arg_0);
  cbor_array_push(args, arg1);
  cbor_item_t *res = call_cbor(tenkei_reverse_list, args);
  std::vector<tenkei_value> result = deserialize<std::vector<tenkei_value>>(res);
  cbor_decref(&args);
  return result;
}

struct tenkei_value apply_function(struct tenkei_fun_ptr arg_0, struct tenkei_value arg_1)
{
  cbor_item_t *args = cbor_new_definite_array(2);
  cbor_item_t *arg1 = serialize<tenkei_fun_ptr>(arg_0);
  cbor_array_push(args, arg1);
  cbor_item_t *arg2 = serialize<tenkei_value>(arg_1);
  cbor_array_push(args, arg2);
  cbor_item_t *res = call_cbor(tenkei_apply_function, args);
  struct tenkei_value result = deserialize<tenkei_value>(res);
  cbor_decref(&args);
  return result;
}

