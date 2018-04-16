#include "../libtenkei-c/ffi_wrappers.h"
#include "../common/list_serializers.h"

#ifdef __cplusplus
extern "C" {
#endif
  extern void tenkei_free(uint8_t *buffer, size_t buffer_len);
#ifdef HASKELL_LIBRARY
  extern void hs_init(int* argc, char** argv[]);
  extern void hs_exit();
#endif

  extern void tenkei_library_language(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
  extern void tenkei_binary_or(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
  extern void tenkei_modify_array(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
  extern void tenkei_exponentiate(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
  extern void tenkei_identity(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
  extern void tenkei_choose_left(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
  extern void tenkei_reverse_list(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
  extern void tenkei_apply_function(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
#ifdef __cplusplus
}
#endif

struct list_int32_t library_language()
{
  cbor_item_t *args = cbor_new_definite_array(0);
  cbor_item_t *res = call_cbor(tenkei_library_language, args);
  struct list_int32_t result = deserialize_list_int32_t(res);
  cbor_decref(&args);
  return result;
}

bool binary_or(bool param0, bool param1)
{
  cbor_item_t *args = cbor_new_definite_array(2);
  cbor_item_t *arg1 = serialize_bool(param0);
  cbor_array_push(args, arg1);
  cbor_item_t *arg2 = serialize_bool(param1);
  cbor_array_push(args, arg2);
  cbor_item_t *res = call_cbor(tenkei_binary_or, args);
  bool result = deserialize_bool(res);
  cbor_decref(&args);
  return result;
}

struct list_int32_t modify_array(struct list_int32_t param)
{
  cbor_item_t *args = cbor_new_definite_array(1);
  cbor_item_t *arg1 = serialize_list_int32_t(param);
  cbor_array_push(args, arg1);
  cbor_item_t *res = call_cbor(tenkei_modify_array, args);
  struct list_int32_t result = deserialize_list_int32_t(res);
  cbor_decref(&args);
  return result;
}

int32_t exponentiate(int32_t param0, int32_t param1)
{
  cbor_item_t *args = cbor_new_definite_array(2);
  cbor_item_t *arg1 = serialize_int32_t(param0);
  cbor_array_push(args, arg1);
  cbor_item_t *arg2 = serialize_int32_t(param1);
  cbor_array_push(args, arg2);
  cbor_item_t *res = call_cbor(tenkei_exponentiate, args);
  int32_t result = deserialize_int32_t(res);
  cbor_decref(&args);
  return result;
}

struct tenkei_value identity(struct tenkei_value param)
{
  cbor_item_t *args = cbor_new_definite_array(1);
  cbor_item_t *arg1 = serialize_tenkei_value(param);
  cbor_array_push(args, arg1);
  cbor_item_t *res = call_cbor(tenkei_identity, args);
  struct tenkei_value result = deserialize_tenkei_value(res);
  cbor_decref(&args);
  return result;
}

struct tenkei_value choose_left(struct tenkei_value param0, struct tenkei_value param1)
{
  cbor_item_t *args = cbor_new_definite_array(2);
  cbor_item_t *arg1 = serialize_tenkei_value(param0);
  cbor_array_push(args, arg1);
  cbor_item_t *arg2 = serialize_tenkei_value(param1);
  cbor_array_push(args, arg2);
  cbor_item_t *res = call_cbor(tenkei_choose_left, args);
  struct tenkei_value result = deserialize_tenkei_value(res);
  cbor_decref(&args);
  return result;
}

struct list_tenkei_value reverse_list(struct list_tenkei_value param)
{
  cbor_item_t *args = cbor_new_definite_array(1);
  cbor_item_t *arg1 = serialize_list_tenkei_value(param);
  cbor_array_push(args, arg1);
  cbor_item_t *res = call_cbor(tenkei_reverse_list, args);
  struct list_tenkei_value result = deserialize_list_tenkei_value(res);
  cbor_decref(&args);
  return result;
}

struct tenkei_value apply_function(void (*param0)(uint8_t *, size_t, uint8_t **, size_t *), struct tenkei_value param1)
{
  cbor_item_t *args = cbor_new_definite_array(2);
  cbor_item_t *arg1 = serialize_fun_ptr(param0);
  cbor_array_push(args, arg1);
  cbor_item_t *arg2 = serialize_tenkei_value(param1);
  cbor_array_push(args, arg2);
  cbor_item_t *res = call_cbor(tenkei_apply_function, args);
  struct tenkei_value result = deserialize_tenkei_value(res);
  cbor_decref(&args);
  return result;
}

