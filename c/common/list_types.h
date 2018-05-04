#ifndef __list_types__
#define __list_types__

struct list_int32_t {
  int32_t *start;
  unsigned int length;
};

struct list_uint32_t {
  uint32_t *start;
  unsigned int length;
};

struct list_tenkei_ptr {
  void * *start;
  unsigned int length;
};

struct list_tenkei_value {
  struct tenkei_value *start;
  unsigned int length;
};

#endif
