#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void quintuple(
	uint8_t *input,
	size_t input_len,
	uint8_t **output,
	size_t *output_len
);

void tenkei_free(
	uint8_t *buffer,
	size_t buffer_len
);

int main() {
	uint8_t *result;
	size_t result_len;
	quintuple("\x03", 1, &result, &result_len);
	for(int i = 0; i < result_len; i++)
	{
		printf("%s%02x", i != 0 ? " " : "", result[i]);
	}
	printf("\n");
	tenkei_free(result, result_len);
	return 0;
}
