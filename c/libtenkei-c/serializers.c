#include "cbor.h"

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
