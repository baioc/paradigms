#include <unistd.h>     // fork
#include <sys/wait.h>
#include <sys/types.h>  // pid_t
#include <stdio.h>

//       (pai)
//         |
//    +----+----+
//    |         |
// filho_1   filho_2

int main(int argc, char** argv)
{
    pid_t pid;
    for (int i = 0; i < 2; i++) {
        if ((pid = fork()) > 0) {
            printf("Processo pai criou %d\n", pid);
            fflush(stdout);
            continue;

        } else if (pid == 0) {
            printf("Processo filho %d criado\n", getpid());
            fflush(stdout);
            return pid;

        } else {
            printf("Erro %d ao criar processo.\n", pid);
            fflush(stdout);
            return pid;
        }
    }

    while (wait(NULL) > 0); // wait until there are no children left

    printf("Processo pai finalizado!\n");
    return 0;
}
