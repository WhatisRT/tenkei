#include "cbor.h"
#include <vector>

struct tenkei_value {
  cbor_item_t *contents;
};

typedef void (*tenkei_fun_ptr_)(void *, uint8_t *, size_t, uint8_t **, size_t *);
typedef void (*tenkei_free_ptr)(uint8_t *, size_t);

typedef void *tenkei_ptr;

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

template <typename T> cbor_item_t *serialize(const T x);
template <typename T> T deserialize(cbor_item_t *x);


namespace specialized_serializers
{
  template <typename T> struct serializer {
    static cbor_item_t *serialize_impl(const T);

    static T deserialize_impl(cbor_item_t *);
  };

  template <> struct serializer<bool> {
    static cbor_item_t *serialize_impl(const bool i)
    {
      return cbor_build_uint8(i);
    }

    static bool deserialize_impl(cbor_item_t *cbor)
    {
      return (bool)cbor_get_int(cbor);
    }
  };

  template <> struct serializer<int32_t> {
    static cbor_item_t *serialize_impl(const int32_t i)
    {
      return cbor_build_uint32(i);
    }

    static int32_t deserialize_impl(cbor_item_t *cbor)
    {
      return (int32_t)cbor_get_int(cbor);
    }
  };

  template <> struct serializer<uint32_t> {
    static cbor_item_t *serialize_impl(const uint32_t i)
    {
      return cbor_build_uint32(i);
    }

    static uint32_t deserialize_impl(cbor_item_t *cbor)
    {
      return (uint32_t)cbor_get_int(cbor);
    }
  };

  // template <> cbor_item_t *serialize<tenkei_ptr>(const tenkei_ptr *i)
  // {
  //   return cbor_build_uint64((size_t) i);
  // }

  // template <> void *deserialize<tenkei_ptr>(cbor_item_t *cbor)
  // {
  //   return (void *)cbor_get_uint64(cbor);
  // }

  template <> struct serializer<tenkei_value> {
    static cbor_item_t *serialize_impl(const tenkei_value i)
    {
      return i.contents;
    }

    static tenkei_value deserialize_impl(cbor_item_t *i)
    {
      tenkei_value res = {i};
      return res;
    }
  };

  template <> struct serializer<tenkei_fun_ptr> {
    static cbor_item_t *serialize_impl(tenkei_fun_ptr f)
    {
      cbor_item_t *result = cbor_new_definite_array(3);

      cbor_array_push(result, cbor_build_uint64(tenkei_fun_ptr_to_ptr(f.fun)));
      cbor_array_push(result, cbor_build_uint64(tenkei_free_ptr_to_ptr(f.free_fun)));
      cbor_array_push(result, cbor_build_uint64((uint64_t)f.data));

      return result;
    }

    static tenkei_fun_ptr deserialize_impl(cbor_item_t *cbor)
    {
      cbor_item_t **res_list = cbor_array_handle(cbor);

      struct tenkei_fun_ptr result = {
        ptr_to_tenkei_fun_ptr(cbor_get_int(res_list[0])),
        ptr_to_tenkei_free_ptr(cbor_get_int(res_list[1])),
        (void *)cbor_get_int(res_list[2])
      };

      return result;
    }
  };

  template <typename T> struct serializer<std::vector<T>> {
    static cbor_item_t *serialize_impl(const std::vector<T> l)
    {
      cbor_item_t *result = cbor_new_definite_array(l.size());

      for(int i = 0; i < l.size(); i++)
        cbor_array_push(result, serialize<T>(l[i]));

      return result;
    }

    static std::vector<T> deserialize_impl(cbor_item_t *cbor)
    {
      cbor_item_t **res_list = cbor_array_handle(cbor);
      size_t res_len = cbor_array_size(cbor);

      std::vector<T> result(res_len);

      for(int i = 0; i < res_len; i++)
        result[i] = deserialize<T>(res_list[i]);

      return result;
    }
  };
};

template <typename T> cbor_item_t *serialize(const T x)
{
  return specialized_serializers::serializer<T>::serialize_impl(x);
}

template <typename T> T deserialize(cbor_item_t *x)
{
  return specialized_serializers::serializer<T>::deserialize_impl(x);
}

