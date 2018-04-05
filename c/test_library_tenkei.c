#ifdef __cplusplus
extern "C" {
#endif
  extern void tenkei_free(uint8_t *buffer, size_t buffer_len);
  extern void hs_init(int* argc, char** argv[]);
  extern void hs_exit();

  extern void tenkei_modify_array(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
#ifdef __cplusplus
}
#endif

#include "helpers.c"
#include "serializers.c"

struct list_int modify_array(struct list_int param)
{
  cbor_item_t *args = cbor_new_definite_array(1);
  cbor_item_t *arg1 = serialize_list_int(param);
  cbor_array_push(args, arg1);
  cbor_item_t *res = call_cbor(tenkei_modify_array, args);
  struct list_int result = deserialize_list_int(res);
  cbor_decref(&args);
  cbor_decref(&res);
  return result;
}
