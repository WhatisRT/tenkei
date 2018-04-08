#include "../lib/ffi_wrappers.c"

#ifdef __cplusplus
extern "C" {
#endif
  extern void tenkei_free(uint8_t *buffer, size_t buffer_len);
  extern void hs_init(int* argc, char** argv[]);
  extern void hs_exit();

  extern void tenkei_modify_array(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
  extern void tenkei_invert_string_case(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
  extern void tenkei_exponentiate(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
  extern void tenkei_identity(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
  extern void tenkei_choose_left(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
  extern void tenkei_reverse_list(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
#ifdef __cplusplus
}
#endif

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

#include "serializers.c"

struct list_int32_t modify_array(struct list_int32_t param)
{
  cbor_item_t *args = cbor_new_definite_array(1);
  cbor_item_t *arg1 = serialize_list_int32_t(param);
  cbor_array_push(args, arg1);
  cbor_item_t *res = call_cbor(tenkei_modify_array, args);
  struct list_int32_t result = deserialize_list_int32_t(res);
  cbor_decref(&args);
  cbor_decref(&res);
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
  cbor_decref(&res);
  return result;
}

void* identity(void* param)
{
  cbor_item_t *args = cbor_new_definite_array(1);
  cbor_item_t *arg1 = serialize_tenkei_ptr(param);
  cbor_array_push(args, arg1);
  cbor_item_t *res = call_cbor(tenkei_identity, args);
  void* result = deserialize_tenkei_ptr(res);
  cbor_decref(&args);
  cbor_decref(&res);
  return result;
}

void* choose_left(void* param0, void* param1)
{
  cbor_item_t *args = cbor_new_definite_array(2);
  cbor_item_t *arg1 = serialize_tenkei_ptr(param0);
  cbor_array_push(args, arg1);
  cbor_item_t *arg2 = serialize_tenkei_ptr(param1);
  cbor_array_push(args, arg2);
  cbor_item_t *res = call_cbor(tenkei_choose_left, args);
  void* result = deserialize_tenkei_ptr(res);
  cbor_decref(&args);
  cbor_decref(&res);
  return result;
}

struct list_tenkei_ptr reverse_list(struct list_tenkei_ptr param)
{
  cbor_item_t *args = cbor_new_definite_array(1);
  cbor_item_t *arg1 = serialize_list_tenkei_ptr(param);
  cbor_array_push(args, arg1);
  cbor_item_t *res = call_cbor(tenkei_reverse_list, args);
  struct list_tenkei_ptr result = deserialize_list_tenkei_ptr(res);
  cbor_decref(&args);
  cbor_decref(&res);
  return result;
}
