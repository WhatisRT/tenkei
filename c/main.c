#include "cbor.h"
#include <stdio.h>
#include "ffi_wrappers.c"
#include "test_library_tenkei.c"

void print_list(const struct list_int l)
{
  printf("[");
  for(int i = 0; i < l.length-1; i++)
    printf("%d, ", l.start[i]);

  printf("%d]\n", l.start[l.length-1]);

  fflush(stdout);
}

int main(int argc, char *argv[])
{
  hs_init(&argc, &argv);

  int list_[] = {1,2,3,4,5,6,7,8,9,10};
  struct list_int list = {list_, 10};

  struct list_int res = modify_array(list);

  print_list(res);

  hs_exit();
}
