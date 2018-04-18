#include <stddef.h>

void hs_init(int *, char ***);
void hs_exit(void);

void __attribute__((constructor)) tenkei_haskell_init(void)
{
	hs_init(NULL, NULL);
}

void __attribute__((destructor)) tenkei_haskell_free(void)
{
	hs_exit();
}
