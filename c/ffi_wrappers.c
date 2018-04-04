cbor_item_t *call_cbor(void (*f)(uint8_t *, size_t, uint8_t **, size_t *), cbor_item_t *args)
{
  uint8_t *input;
  size_t input_len, length = cbor_serialize_alloc(args, &input, &input_len);

  uint8_t *res;
  size_t res_len;

  f(input, input_len, &res, &res_len);

  struct cbor_load_result result;

  // TODO: Do something with the result ...
  cbor_item_t *result_cbor = cbor_load(res, res_len, &result);

  free(input);
  free(res);

  return result_cbor;
}
