#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <stdio.h>


// Le o conteudo do arquivo filename e retorna um vetor E o tamanho dele
// Se filename for da forma "gen:%d", gera um vetor aleatório com %d elementos
//
// +-------> retorno da função, ponteiro para vetor malloc()ado e preenchido
// |
// |         tamanho do vetor (usado <-----+
// |         como 2o retorno)              |
// v                                       v
double* load_vector(const char* filename, int* out_size);

// Avalia se o prod_escalar eh o produto escalar dos vetores a e b. Assume-se
// que ambos a e b sejam vetores de tamanho size.
void avaliar(double* a, double* b, int size, double prod_escalar);


struct vector_pair {
    double *first;
    double *second;
    int size;
};

void *dot_product(void *vtuple)
{
    const double *a = ((struct vector_pair *)vtuple)->first;
    const double *b = ((struct vector_pair *)vtuple)->second;
    const int size = ((struct vector_pair *)vtuple)->size;

    double *result = malloc(sizeof(double));
    for (int i = 0; i < size; ++i)
        *result += a[i] * b[i];

    pthread_exit(result);
}

int main(int argc, char* argv[])
{
    srand(time(NULL));

    // Temos argumentos suficientes?
    if (argc < 4) {
        printf("Uso: %s n_threads a_file b_file\n"
               "    n_threads    numero de threads a serem usadas na computacao\n"
               "    *_file       caminho de arquivo ou uma expressao com a forma gen:N,\n"
               "                 representando um vetor aleatorio de tamanho N\n",
               argv[0]);
        return 1;
    }

    // Quantas threads?
    int n_threads = atoi(argv[1]);
    if (n_threads <= 0) {
        printf("Numero de threads deve ser > 0\n");
        return 1;
    }

    // Le numeros de arquivos para vetores alocados com malloc
    int a_size;
    double* a = load_vector(argv[2], &a_size);
    if (!a) {
        //load_vector nao conseguiu abrir o arquivo
        printf("Erro ao ler arquivo %s\n", argv[2]);
        return 1;
    }

    int b_size;
    double* b = load_vector(argv[3], &b_size);
    if (!b) {
        printf("Erro ao ler arquivo %s\n", argv[3]);
        return 1;
    }

    // Garante que entradas sao compativeis
    if (a_size != b_size) {
        printf("Vetores a e b tem tamanhos diferentes! (%d != %d)\n", a_size, b_size);
        return 1;
    }


    // make threads
    if (n_threads > a_size) {
        n_threads = a_size;
        printf("Numero de threads muito grande, limitando em %d.\n", a_size);
    }

    pthread_t thread[n_threads];
    struct vector_pair sub_vector[n_threads];

    const int batch_size = a_size / n_threads;

    const int last = n_threads - 1;
    { // ultima thread comeca antes e faz mais trabalho na divisao nao inteira
        const int remainders = a_size % n_threads;
        const int offset = last * batch_size;
        sub_vector[last].first = a + offset;
        sub_vector[last].second = b + offset;
        sub_vector[last].size = batch_size + remainders;
        pthread_create(&thread[last], NULL, dot_product, &sub_vector[last]);
    }

    for (int i = 0; i < last; ++i) {
        const int offset = i * batch_size;
        sub_vector[i].first = a + offset;
        sub_vector[i].second = b + offset;
        sub_vector[i].size = batch_size;
        pthread_create(&thread[i], NULL, dot_product, &sub_vector[i]);
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
    avaliar(a, b, a_size, result);

    // Libera memoria
    free(b);
    free(a);

    return 0;
}
