#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

#include <mpi.h>

int main(int argc, char* argv[])
{
    int msg;
    MPI_Comm parent, child;
    int rank, size;
    char hostname[512];
    pid_t pid;
    int i;
    char *cmds[2];
    char *argv0[] = { "foo", NULL };
    char *argv1[] = { "bar", NULL };
    char **spawn_argv[2];
    int maxprocs[] = { 1, 1 };
    MPI_Info info[] = { MPI_INFO_NULL, MPI_INFO_NULL };

    cmds[1] = cmds[0] = argv[0];
    spawn_argv[0] = argv0;
    spawn_argv[1] = argv1;

    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_get_parent(&parent);
    /* If we get COMM_NULL back, then we're the parent */
    if (MPI_COMM_NULL == parent) {
        pid = getpid();
        printf("Parent [pid %ld] about to spawn!\n", (long)pid);
        MPI_Comm_spawn_multiple(2, cmds, spawn_argv, maxprocs, 
                                info, 0, MPI_COMM_WORLD,
                                &child, MPI_ERRCODES_IGNORE);
        printf("Parent done with spawn\n");
        if (0 == rank) {
            msg = 38;
            printf("Parent sending message to children\n");
            MPI_Send(&msg, 1, MPI_INT, 0, 1, child);
            MPI_Send(&msg, 1, MPI_INT, 1, 1, child);
        }
        MPI_Comm_disconnect(&child);
        printf("Parent disconnected\n");
    } 
    /* Otherwise, we're the child */
    else {
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        MPI_Comm_size(MPI_COMM_WORLD, &size);
        gethostname(hostname, 512);
        pid = getpid();
        printf("Hello from the child %d of %d on host %s pid %ld: argv[1] = %s\n", rank, size, hostname, (long)pid, argv[1]);
        if (0 == rank) {
            MPI_Recv(&msg, 1, MPI_INT, 0, 1, parent, MPI_STATUS_IGNORE);
            printf("Child %d received msg: %d\n", rank, msg);
        }
        MPI_Comm_disconnect(&parent);
        printf("Child %d disconnected\n", rank);
    }

    MPI_Finalize();
    return 0;
}
