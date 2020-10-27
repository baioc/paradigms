#include <stdio.h>
#include <assert.h>
#include <float.h>
#include <pthread.h>

#include "language.h"

typedef struct {
	const char* name;
	const char* data_filename;
	struct language model;
} lang_t;

void *train(void *arg)
{
	lang_t *lang = (lang_t *)arg;
	language_init(&lang->model);
	FILE *fp = fopen(lang->data_filename, "r");
	assert(fp != NULL);
	language_train(&lang->model, fp);
	fclose(fp);
	return NULL;
}

int main()
{
	lang_t langs[] = {
		{ .name = "English", .data_filename = "res/multilang/ep-00-01-17.en.txt" },
		{ .name = "Spanish", .data_filename = "res/multilang/ep-00-01-17.es.txt" },
		{ .name = "French",  .data_filename = "res/multilang/ep-00-01-17.fr.txt" },
		{ .name = "Italian", .data_filename = "res/multilang/ep-00-01-17.it.txt" },
		{ .name = "Swedish", .data_filename = "res/multilang/ep-00-01-17.sv.txt" },
	};
	#define LANGS (sizeof(langs) / sizeof(lang_t))

	// learn each language's patterns
	pthread_t threads[LANGS];
	for (int i = 0; i < LANGS; ++i) {
		pthread_create(&threads[i], NULL, train, &langs[i]);
	}
	for (int i = 0; i < LANGS; ++i) {
		pthread_join(threads[i], NULL);
	}

	// read user input (until EOF)
	char buffer[1024];
	printf("> ");
	while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
		// find which language the input is in
		int choice = -1;
		double max = -FLT_MAX;
		for (int i = 0; i < LANGS; ++i) {
			const double prob = language_logp(&langs[i].model, buffer);
		#ifdef DEBUG
			printf("%s (logp): %lf\n", langs[i].name, prob);
		#endif
			if (max < prob) {
				max = prob;
				choice = i;
			}
		}
		printf("** %s **\n", langs[choice].name);
		printf("\n> ");
	}

	for (int i = 0; i < LANGS; ++i)
		language_destroy(&langs[i].model);
}
