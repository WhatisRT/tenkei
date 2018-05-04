#include "cbor.h"

struct tenkei_value {
  cbor_item_t *contents;
};

typedef void (*tenkei_fun_ptr_)(void *, uint8_t *, size_t, uint8_t **, size_t *);
typedef void (*tenkei_free_ptr)(uint8_t *, size_t);

struct tenkei_fun_ptr {
  tenkei_fun_ptr_ fun;
  tenkei_free_ptr free_fun;
  void *data;
};

uint64_t tenkei_fun_ptr_to_ptr(tenkei_fun_ptr_ f)
{
  return *(uint64_t*)(&f);
}

tenkei_fun_ptr_ ptr_to_tenkei_fun_ptr(const uint64_t ptr)
{
  return *(tenkei_fun_ptr_*)&ptr;
}

uint64_t tenkei_free_ptr_to_ptr(tenkei_free_ptr f)
{
  return *(uint64_t*)(&f);
}

void (*ptr_to_tenkei_free_ptr(const uint64_t ptr))(uint8_t *, size_t)
{
  return *(tenkei_free_ptr*)&ptr;
}

cbor_item_t *serialize_bool(const bool i)
{
  return cbor_build_uint8(i);
}

bool deserialize_bool(const cbor_item_t *cbor)
{
  return (bool)cbor_get_int(cbor);
}

cbor_item_t *serialize_int32_t(const int32_t i)
{
  return cbor_build_uint32(i);
}

int32_t deserialize_int32_t(const cbor_item_t *cbor)
{
  return (int32_t)cbor_get_int(cbor);
}

cbor_item_t *serialize_uint32_t(const uint32_t i)
{
  return cbor_build_uint32(i);
}

uint32_t deserialize_uint32_t(const cbor_item_t *cbor)
{
  return (uint32_t)cbor_get_int(cbor);
}

cbor_item_t *serialize_tenkei_ptr(const void *i)
{
  return cbor_build_uint64((size_t) i);
}

void *deserialize_tenkei_ptr(const cbor_item_t *cbor)
{
  return (void *)cbor_get_uint64(cbor);
}

cbor_item_t *serialize_tenkei_value(const struct tenkei_value i)
{
  return i.contents;
}

struct tenkei_value deserialize_tenkei_value(cbor_item_t *i)
{
  struct tenkei_value res = {i};
  return res;
}

cbor_item_t *serialize_fun_ptr(struct tenkei_fun_ptr f)
{
  cbor_item_t *result = cbor_new_definite_array(3);

  cbor_array_push(result, cbor_build_uint64(tenkei_fun_ptr_to_ptr(f.fun)));
  cbor_array_push(result, cbor_build_uint64(tenkei_free_ptr_to_ptr(f.free_fun)));
  cbor_array_push(result, cbor_build_uint64((uint64_t)f.data));

  return result;
}

struct tenkei_fun_ptr deserialize_fun_ptr(cbor_item_t *cbor)
{
  cbor_item_t **res_list = cbor_array_handle(cbor);

  struct tenkei_fun_ptr result = {
    ptr_to_tenkei_fun_ptr(cbor_get_int(res_list[0])),
    ptr_to_tenkei_free_ptr(cbor_get_int(res_list[1])),
    (void *)cbor_get_int(res_list[2])
  };

  return result;
}
