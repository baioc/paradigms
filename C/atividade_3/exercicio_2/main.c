#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <stdio.h>
#include <assert.h>


// Lê o conteúdo do arquivo filename e retorna um vetor E o tamanho dele
// Se filename for da forma "gen:%d", gera um vetor aleatório com %d elementos
//
// +-------> retorno da função, ponteiro para vetor malloc()ado e preenchido
// |
// |         tamanho do vetor (usado <-----+
// |         como 2o retorno)              |
// v                                       v
double* load_vector(const char* filename, int* out_size);

// Avalia o resultado no vetor c. Assume-se que todos os ponteiros (a, b, e c)
// tenham tamanho size. Se silent for diferente de zero, não irá imprimir
// resultado na tela
void avaliar(double* a, double* b, double* c, int size);


struct vector_triple {
    double *first;
    double *second;
    double *third;
    int size;
};

void *vectorial_sum(void *vtuple)
{
    double *a = ((struct vector_triple *)vtuple)->first;
    double *b = ((struct vector_triple *)vtuple)->second;
    double *c = ((struct vector_triple *)vtuple)->third;
    const int size = ((struct vector_triple *)vtuple)->size;

    for (int i = 0; i < size; ++i)
        c[i] = a[i] + b[i];

    pthread_exit(NULL);
}

int main(int argc, char* argv[])
{
    if (argc < 4) {
        printf("Uso: %s n_threads a_file b_file\n"
               "    n_threads    numero de threads a serem usadas na computacao\n"
               "    *_file       caminho de arquivo ou uma expressao com a forma gen:N,\n"
               "                 representando um vetor aleatorio de tamanho N\n",
               argv[0]);
        return 1;
    }

    srand(time(NULL));

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


    // make threads
    if (n_threads > a_size) {
        n_threads = a_size;
        printf("Numero de threads muito grande, limitando em %d.\n", a_size);
    }

    pthread_t thread[n_threads];
    struct vector_triple sub_vector[n_threads];

    const int batch_size = a_size / n_threads;

    const int last = n_threads - 1;
    { // ultima thread comeca antes e faz mais trabalho na divisao nao inteira
        const int remainders = a_size % n_threads;
        const int offset = last * batch_size;
        sub_vector[last].first = a + offset;
        sub_vector[last].second = b + offset;
        sub_vector[last].third = c + offset;
        sub_vector[last].size = batch_size + remainders;
        pthread_create(&thread[last], NULL, vectorial_sum, &sub_vector[last]);
    }

    for (int i = 0; i < last; ++i) {
        const int offset = i * batch_size;
        sub_vector[i].first = a + offset;
        sub_vector[i].second = b + offset;
        sub_vector[i].third = c + offset;
        sub_vector[i].size = batch_size;
        pthread_create(&thread[i], NULL, vectorial_sum, &sub_vector[i]);
    }

    // join threads
    for(int i = 0; i < n_threads; ++i)
        pthread_join(thread[i], NULL);

    //    +---------------------------------+
    // ** | IMPORTANTE: avalia o resultado! | **
    //    +---------------------------------+
    avaliar(a, b, c, a_size);

    // IMPORTANTE: libera memoria
    free(c);
    free(b);
    free(a);

    return 0;
}
