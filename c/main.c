#include "cbor.h"
#include <stdio.h>
#include "ffi_wrappers.c"
#include "test_library_tenkei.c"

void print_list(const struct list_int32_t l)
{
  printf("[");
  for(int i = 0; i < l.length-1; i++)
    printf("%d, ", l.start[i]);

  printf("%d]\n", l.start[l.length-1]);

  fflush(stdout);
}

void print_list_char(const struct list_char l)
{
  for(int i = 0; i < l.length-1; i++)
    printf("%c", l.start[i]);

  printf("\n");
  fflush(stdout);
}

struct list_tenkei_ptr list_int32_t_to_list_tenkei_ptr(const struct list_int32_t l)
{
  void **list_ = malloc(sizeof(void *) * l.length);

  for(int i = 0; i < l.length; i++)
    list_[i] = &l.start[i];

  struct list_tenkei_ptr res = {list_, l.length};

  return res;
}

struct list_int32_t list_tenkei_ptr_to_list_int32_t(const struct list_tenkei_ptr l)
{
  int32_t *list_ = malloc(sizeof(int32_t) * l.length);

  for(int i = 0; i < l.length; i++)
    list_[i] = (*(int32_t*)(l.start[i]));

  struct list_int32_t res = {list_, l.length};

  return res;
}

int main(int argc, char *argv[])
{
  hs_init(&argc, &argv);

  int list_[] = {1,2,3,4,5,6,7,8,9,10};
  struct list_int32_t list = {list_, 10};

  struct list_int32_t res = modify_array(list);

  print_list(res);

  char list2_[] = "this IS a Test string";
  struct list_char list2 = {list2_, 22};

  struct list_char res2 = invert_string_case(list2);

  print_list_char(res2);

  printf("%d\n", exponentiate(2,5));

  void *ptr_list = &list;

  struct list_int32_t *res3 = identity(ptr_list);
  print_list(*res3);

  int i1 = 1;
  int i2 = 2;

  printf("%d\n", *(int *)choose_left((void*)&i1, (void*)&i2));

  struct list_tenkei_ptr list3 = list_int32_t_to_list_tenkei_ptr(list);

  struct list_tenkei_ptr res4 = reverse_list(list3);

  struct list_int32_t res5 = list_tenkei_ptr_to_list_int32_t(res4);

  print_list(res5);

  fflush(stdout);

  hs_exit();
}
