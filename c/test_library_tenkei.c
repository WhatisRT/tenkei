#ifdef __cplusplus
extern "C" {
#endif
  extern void tenkei_modify_array(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);
  extern void tenkei_free(uint8_t *buffer, size_t buffer_len);
  extern void hs_init(int* argc, char** argv[]);
  extern void hs_exit();

#ifdef __cplusplus
}
#endif

struct list_int {
  int *start;
  unsigned int length;
};

cbor_item_t *serialize(const struct list_int l)
{
  cbor_item_t *result = cbor_new_definite_array(l.length);

  for(int i = 0; i < l.length; i++)
    cbor_array_push(result, cbor_build_uint8(l.start[i]));

  return result;
}

struct list_int deserialize(const cbor_item_t *cbor)
{
  cbor_item_t **res_list = cbor_array_handle(cbor);
  size_t res_len = cbor_array_size(cbor);

  int *res_ = malloc(sizeof(int) * res_len);

  for(int i = 0; i < res_len; i++)
    res_[i] = cbor_get_uint8(res_list[i]);

  struct list_int result = {res_, res_len};
  return result;
}

struct list_int modify_array(const struct list_int l)
{
  cbor_item_t *args = cbor_new_definite_array(1);
  cbor_item_t *arg = serialize(l);

  cbor_array_push(args, arg);

  cbor_item_t *result = call_cbor(tenkei_modify_array, args);

  struct list_int res = deserialize(result);

  cbor_decref(&result);
  cbor_decref(&args);
  cbor_decref(&arg);

  return res;
}
