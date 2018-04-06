cbor_item_t *serialize_int32_t(const int32_t i)
{
  return cbor_build_uint32(i);
}

int32_t deserialize_int32_t(const cbor_item_t *cbor)
{
  return cbor_get_uint32(cbor);
}

cbor_item_t *serialize_tenkei_ptr(const void *i)
{
  return cbor_build_uint64((size_t) i);
}

void *deserialize_tenkei_ptr(const cbor_item_t *cbor)
{
  return (void *)cbor_get_uint64(cbor);
}

cbor_item_t *serialize_list_int32_t(const struct list_int32_t l)
{
  cbor_item_t *result = cbor_new_definite_array(l.length);

  for(int i = 0; i < l.length; i++)
    cbor_array_push(result, cbor_build_uint8(l.start[i]));

  return result;
}

struct list_int32_t deserialize_list_int32_t(const cbor_item_t *cbor)
{
  cbor_item_t **res_list = cbor_array_handle(cbor);
  size_t res_len = cbor_array_size(cbor);

  int *res_ = malloc(sizeof(int) * res_len);

  for(int i = 0; i < res_len; i++)
    res_[i] = cbor_get_uint8(res_list[i]);

  struct list_int32_t result = {res_, res_len};
  return result;
}

cbor_item_t *serialize_list_tenkei_ptr(const struct list_tenkei_ptr l)
{
  cbor_item_t *result = cbor_new_definite_array(l.length);

  for(int i = 0; i < l.length; i++)
    cbor_array_push(result, serialize_tenkei_ptr(l.start[i]));

  return result;
}

struct list_tenkei_ptr deserialize_list_tenkei_ptr(const cbor_item_t *cbor)
{
  cbor_item_t **res_list = cbor_array_handle(cbor);
  size_t res_len = cbor_array_size(cbor);

  void **res_ = malloc(sizeof(void *) * res_len);

  for(int i = 0; i < res_len; i++)
    res_[i] = deserialize_tenkei_ptr(res_list[i]);

  struct list_tenkei_ptr result = {res_, res_len};
  return result;
}

cbor_item_t *serialize_list_char(const struct list_char l)
{
  cbor_item_t *result = cbor_new_definite_array(l.length);

  for(int i = 0; i < l.length; i++)
    cbor_array_push(result, cbor_build_uint8(l.start[i]));

  return result;
}

struct list_char deserialize_list_char(const cbor_item_t *cbor)
{
  cbor_item_t **res_list = cbor_array_handle(cbor);
  size_t res_len = cbor_array_size(cbor);

  char *res_ = malloc(sizeof(char) * res_len);

  for(int i = 0; i < res_len; i++)
    res_[i] = cbor_get_uint8(res_list[i]);

  struct list_char result = {res_, res_len};
  return result;
}
