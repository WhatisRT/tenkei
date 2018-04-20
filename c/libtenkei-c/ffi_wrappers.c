#include "stdint.h"
#include "stdio.h"
#include "cbor.h"

#include "serializers.h"

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

  return result_cbor;
}

void offer_cbor(cbor_item_t *(*f)(cbor_item_t *), uint8_t *arg, size_t arg_len, uint8_t **res, size_t *res_len)
{
  struct cbor_load_result result;

  cbor_item_t *input = cbor_load(arg, arg_len, &result);

  // TODO: Do something with the result ...

  cbor_item_t *res_ = f(input);

  size_t length = cbor_serialize_alloc(res_, res, res_len);
}

/* void (void *data, uint8_t *arg, size_t arg_len, uint8_t **res, size_t *res_len) */
/* { */
  
/* } */

cbor_item_t *call_fun_ptr_cbor(struct tenkei_fun_ptr f, cbor_item_t *args)
{
  uint8_t *input;
  size_t input_len, length = cbor_serialize_alloc(args, &input, &input_len);

  uint8_t *res;
  size_t res_len;

  f.fun(f.data, input, input_len, &res, &res_len);

  struct cbor_load_result result;

  // TODO: Do something with the result ...
  cbor_item_t *result_cbor = cbor_load(res, res_len, &result);

  free(input);

  // TODO: Free the result with f.free_fun

  return result_cbor;
}

struct tenkei_fun_ptr offer_fun_ptr_cbor(cbor_item_t *(*f)(cbor_item_t *))
{
  struct tenkei_fun_ptr result = { &offer_cbor, 0, f };

  return result;
}
