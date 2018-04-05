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

  fflush(stdout);

  hs_exit();
}
