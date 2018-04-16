#include "cbor.h"

struct tenkei_value {
  cbor_item_t *contents;
};

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

typedef void (*tenkei_fun_ptr)(uint8_t *, size_t, uint8_t **, size_t *);

cbor_item_t *serialize_fun_ptr(tenkei_fun_ptr f)
{
  return cbor_build_uint64(*(uint64_t*)(&f));
}

void (*deserialize_fun_ptr(const cbor_item_t *cbor))(uint8_t *, size_t, uint8_t **, size_t *)
{
  uint64_t ptr = cbor_get_uint64(cbor);

  return *(tenkei_fun_ptr*)&ptr;
}
