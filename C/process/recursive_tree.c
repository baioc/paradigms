#include <unistd.h>     // fork, sleep
#include <sys/wait.h>
#include <sys/types.h>  // pid_t
#include <stdlib.h>     // exit codes
#include <stdio.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

//                          (principal)
//                               |
//              +----------------+--------------+
//              |                               |
//           filho_1                         filho_2
//              |                               |
//    +---------+-----------+          +--------+--------+
//    |         |           |          |        |        |
// neto_1_1  neto_1_2  neto_1_3     neto_2_1 neto_2_2 neto_2_3

// ~~~ printfs  ~~~
//      principal (ao finalizar): "Processo principal %d finalizado\n"
// filhos e netos (ao finalizar): "Processo %d finalizado\n"
//    filhos e netos (ao inciar): "Processo %d, filho de %d\n"

// Obs:
// - netos devem esperar 5 segundos antes de imprmir a mensagem de finalizado (e terminar)
// - pais devem esperar pelos seu descendentes diretos antes de terminar

static int branch(int *spawns, int depth)
{
    if (depth > 0) {
        int children = *spawns;
        for (int i = 0; i < children; ++i) {
            pid_t pid = fork();

            if (pid == 0) {
                printf("Processo %d, filho de %d\n", getpid(), getppid());
                fflush(stdout);
                int status = branch(spawns + 1, depth - 1);
                printf("Processo %d finalizado\n", getpid());
                fflush(stdout);
                _exit(status);
            }
        }

        // wait for all children
        while (wait(NULL) > 0);
    }
	else if (depth == 0) { // last generation only
        sleep(5);
    }
	else {
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

int main(int argc, char** argv)
{
    int generations[] = {2, 3};
    int status = branch(generations, ARRAY_SIZE(generations));
    printf("Processo principal %d finalizado\n", getpid());
    return status;
}
