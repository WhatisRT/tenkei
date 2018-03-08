#include "tenkei_rust.h"
#include <stdio.h>
#include <stdint.h>

int main(void)
{
	uint8_t input = 3;
	uint8_t *output;
	size_t output_len;
	triple(&input, 1, &output, &output_len);
	for(int i = 0; i < output_len; i++)
	{
		printf("%02x\n", output[i]);
	}
	tenkei_free(output, output_len);
	return 0;
}
