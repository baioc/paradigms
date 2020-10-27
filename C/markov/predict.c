#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "language.h"

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

#define OUTPUT_WORDS 15

int main(int argc, char const *argv[])
{
	if (argc < 2) {
		printf("Usage: %s train_files.txt ...\n", argv[0]);
		return -1;
	}

	struct language model;
	language_init(&model);

	// train the model with each of the given file's contents
	for (int i = 1; i < argc; ++i) {
		FILE *fp = fopen(argv[i], "r");
		if (fp == NULL) return i;
		language_train(&model, fp);
		fclose(fp);
	}

	char *output[OUTPUT_WORDS] = { 0 };
	for (int i = 0; i < ARRAY_SIZE(output); ++i)
		output[i] = malloc(MAX_WORD_LEN+1);

	// user input as seed
	char input[256];
	printf("> ");
	while (fgets(input, sizeof(input), stdin) != NULL) {
		// generate and print some words
		language_generate(&model, input, (char **)output, ARRAY_SIZE(output), MAX_WORD_LEN+1);
		for (int i = 0; i < ARRAY_SIZE(output); ++i) {
			printf("%s ", output[i]);
		}
		printf("\n> ");
	}

	for (int i = 0; i < ARRAY_SIZE(output); ++i)
		free(output[i]);

	language_destroy(&model);
}
