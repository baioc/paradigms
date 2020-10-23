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
		{ .name = "English", .data_filename = "ep-00-01-17.en.txt" },
		{ .name = "Spanish", .data_filename = "ep-00-01-17.es.txt" },
		{ .name = "French",  .data_filename = "ep-00-01-17.fr.txt" },
		{ .name = "Italian", .data_filename = "ep-00-01-17.it.txt" },
		{ .name = "Swedish", .data_filename = "ep-00-01-17.sv.txt" },
	};
	#define LANGS (sizeof(langs) / sizeof(lang_t))

	pthread_t threads[LANGS];
	for (int i = 0; i < LANGS; ++i) {
		pthread_create(&threads[i], NULL, train, &langs[i]);
	}
	for (int i = 0; i < LANGS; ++i) {
		pthread_join(threads[i], NULL);
	}

	char buffer[1024];
	while (printf("\n> "), (fgets(buffer, sizeof(buffer), stdin) != NULL)) {
		int choice = -1;
		double max = -FLT_MAX;
		for (int i = 0; i < LANGS; ++i) {
			const double prob = language_logp(&langs[i].model, buffer);
			printf("%s (logp): %lf\n", langs[i].name, prob);
			if (max < prob) {
				max = prob;
				choice = i;
			}
		}
		printf("** %s **\n", langs[choice].name);
	}

	for (int i = 0; i < LANGS; ++i)
		language_destroy(&langs[i].model);
}
