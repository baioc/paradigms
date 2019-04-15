#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


// Le o conteudo do arquivo filename e retorna um vetor E o tamanho dele
// Se filename for da forma "gen:%d", gera um vetor aleatorio com %d elementos
//
// +-------> retorno da funcao, ponteiro para vetor malloc()ado e preenchido
// |
// |         tamanho do vetor (usado <-----+
// |         como 2o retorno)              |
// v                                       v
double* load_vector(const char* filename, int* out_size);

// Gera um vetor de tamanho size com valores aleatorios em [0, max).
double* generate_vector(int size, int max);

// Avalia o resultado no vetor c. Assume-se que todos os ponteiros (a, b, e c)
// tenham tamanho size.
static void test_vectorial_sum(const double* a, const double* b, const double* c, int size);

// Avalia se o prod_escalar eh o produto escalar dos vetores a e b. Assume-se
// que ambos a e b sejam vetores de tamanho size.
static void test_dot_product(const double* a, const double* b, int size, double prod_escalar);


struct vector_tuple {
	double *first;
	double *second;
	double *third;
	int size;
};

// Sums the first two vectors in a vector_tuple and fills the third with result.
void *threaded_vectorial_sum(void *vtuple)
{
	const double *a = ((struct vector_tuple *)vtuple)->first;
	const double *b = ((struct vector_tuple *)vtuple)->second;
	double *c = ((struct vector_tuple *)vtuple)->third;
	const int size = ((struct vector_tuple *)vtuple)->size;

	for (int i = 0; i < size; ++i)
		*(c++) = *(a++) + *(b++);

	pthread_exit(NULL);
}

// Returns the dot product between two vectors as a malloc()ed double *.
void *threaded_dot_product(void *vtuple)
{
	const double *a = ((struct vector_tuple *)vtuple)->first;
	const double *b = ((struct vector_tuple *)vtuple)->second;
	const int size = ((struct vector_tuple *)vtuple)->size;

	double *result = malloc(sizeof(double));
	for (int i = 0; i < size; ++i)
		*result += *(a++) * *(b++);

	pthread_exit(result);
}


int main(int argc, const char* argv[])
{
	// temos argumentos suficientes?
	if (argc < 4) {
		printf("Uso: %s n_threads a_file b_file\n"
		       "    n_threads    numero de threads a serem usadas na computacao\n"
		       "    *_file       caminho de arquivo ou uma expressao com a forma gen:N,\n"
		       "                 representando um vetor aleatorio de tamanho N\n",
		argv[0]);
		return 1;
	}

	srand(time(NULL));

	// quantas threads?
	int n_threads = atoi(argv[1]);
	if (n_threads <= 0) {
		printf("Numero de threads deve ser > 0\n");
		return 1;
	}

	// le numeros de arquivos para vetores alocados com malloc
	int a_size;
	double *a = load_vector(argv[2], &a_size);
	if (!a) {
		// load_vector nao conseguiu abrir o arquivo
		printf("Erro ao ler arquivo %s\n", argv[2]);
		return 1;
	}

	int b_size;
	double *b = load_vector(argv[3], &b_size);
	if (!b) {
		printf("Erro ao ler arquivo %s\n", argv[3]);
		return 1;
	}

	// garante que entradas sao compativeis
	if (a_size != b_size) {
		printf("Vetores a e b tem tamanhos diferentes! (%d != %d)\n", a_size, b_size);
		return 1;
	}

	// cria vetor do resultado
	double *c = malloc(a_size * sizeof(double));
	assert(c != NULL);


	{ // ex2: vectorial sum
		// make threads
		if (n_threads > a_size) {
			n_threads = a_size;
			printf("Numero de threads muito grande, limitando em %d.\n", a_size);
		}
		pthread_t thread[n_threads];

		// task division
		struct vector_tuple sub_vector[n_threads];
		const int batch_size = a_size / n_threads;
		const int last = n_threads - 1;
		const int remainders = a_size % n_threads;
		for (int i = last; i >= 0; --i) {
			const int offset = i * batch_size;
			sub_vector[i].first = a + offset;
			sub_vector[i].second = b + offset;
			sub_vector[i].third = c + offset;
			sub_vector[i].size = (i == last)
									? batch_size + remainders
			                        : batch_size;
			pthread_create(&thread[i], NULL, threaded_vectorial_sum, &sub_vector[i]);
		}

		// join threads
		for(int i = 0; i < n_threads; ++i)
			pthread_join(thread[i], NULL);

		//    +---------------------------------+
		// ** | IMPORTANTE: avalia o resultado! | **
		//    +---------------------------------+
		test_vectorial_sum(a, b, c, a_size);
	}
	printf("\n");

	{ // ex3: dot product
		// make threads
		if (n_threads > a_size) {
			n_threads = a_size;
			printf("Numero de threads muito grande, limitando em %d.\n", a_size);
		}
		pthread_t thread[n_threads];

		// task division
		struct vector_tuple sub_vector[n_threads];
		const int batch_size = a_size / n_threads;
		const int last = n_threads - 1;
		const int remainders = a_size % n_threads;
		for (int i = last; i >= 0; --i) {
			const int offset = i * batch_size;
			sub_vector[i].first = a + offset;
			sub_vector[i].second = b + offset;
			sub_vector[i].size = (i == last)
									? batch_size + remainders
			                        : batch_size;
			pthread_create(&thread[i], NULL, threaded_dot_product, &sub_vector[i]);
		}

		// join threads
		double result = 0;
		for(int i = 0; i < n_threads; ++i) {
			double *partial;
			pthread_join(thread[i], (void **)&partial);
			result += *partial;
			free(partial);
		}

		//    +---------------------------------+
		// ** | IMPORTANTE: avalia o resultado! | **
		//    +---------------------------------+
		test_dot_product(a, b, a_size, result);
	}


	// IMPORTANTE: libera memoria
	free(c);
	free(b);
	free(a);

	return 0;
}


double* generate_vector(int size, int max)
{
	double* vector = malloc(size * sizeof(double));

	for (int i = 0; i < size; ++i)
		vector[i] = rand() % max;

	return vector;
}

double* load_vector(const char* filename, int* out_size)
{
	int size = 0;
	double* vector;

	if (strncmp(filename, "gen:", 4) == 0) {
		if (sscanf(filename + 4, "%d", &size) <= 0)
			return NULL;
		vector = generate_vector(size, 10000);

	} else {
		FILE* f = fopen(filename, "r");
		if (!f)
			return NULL;

		int capacity = 10;
		vector = malloc(capacity * sizeof(double));
		double value = 0;
		//                 +-----> "long float" <=> double
		//                 v
		while (fscanf(f, "%lf", &value) > 0) {
			if (size >= capacity) {
				capacity *= 2;
				vector = realloc(vector, capacity * sizeof(double));
			}
			vector[size++] = value;
		}

		fclose(f);
	}

	*out_size = size;
	return vector;
}

static void test_vectorial_sum(const double* a, const double* b, const double* c, int size)
{
	printf("c = (");
	for (int i = 0; i < size; ++i)
		printf("%s%g", i ? " " : "", c[i]);
	printf(")\n");

	unsigned int diffs = 0;
	for (int i = 0; i < size; ++i)
		diffs += (*(a++) + *(b++) != *(c++)) ? 1 : 0;

	if (diffs)
		printf("Ops! Vetor c difere de a + b em %d posições\n", diffs);
	else
		printf("OK!\n");
}

static void test_dot_product(const double* a, const double* b, int size, double prod_escalar)
{
	double expected = 0;
	for (int i = 0; i < size; ++i)
		expected += *(a++) * *(b++);

	if (expected != prod_escalar) {
		printf("Ops! recebi %g, mas esperava %g como produto escalar\n",
		       prod_escalar, expected);
	} else {
		printf("OK! a.b = %f\n", prod_escalar);
	}
}
