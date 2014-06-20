#define _GNU_SOURCE
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>

#include <mpi.h>

#define NUM_CHILDREN 5

int main(int argc, char* argv[])
{
    int msg;
    MPI_Comm parent, children[NUM_CHILDREN];
    int rank, size, i;
    char hostname[512];
    pid_t pid;
    char *child_argv[2] = { "", NULL };

    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_get_parent(&parent);
    /* If we get COMM_NULL back, then we're the parent */
    if (MPI_COMM_NULL == parent) {
        pid = getpid();

        /* First, spawn all the children.  Give them an argv
           identifying which child they are */
        for (i = 0; i < NUM_CHILDREN; ++i) {
            printf("Parent [pid %ld] about to spawn child #%d\n", 
                   (long)pid, i);
            asprintf(&(child_argv[0]), "%d", i);
            MPI_Comm_spawn(argv[0], child_argv, 1, MPI_INFO_NULL, 
                           0, MPI_COMM_WORLD, &children[i],
                           MPI_ERRCODES_IGNORE);
            printf("Parent done with spawn of child %d\n", i);
        }

        /* Now send each of the children a message */
        if (0 == rank) {
            for (i = 0; i < NUM_CHILDREN; ++i) {
                printf("Parent sending message to child %d\n", i);
                MPI_Send(&i, 1, MPI_INT, 0, 1, children[i]);
            }
        }

        /* Now disconnect from each of the children */
        for (i = 0; i < NUM_CHILDREN; ++i) {
            printf("Parent disconnecting from child %d\n", i);
            MPI_Comm_disconnect(&children[i]);
            printf("Parent disconnected from child %d\n", i);
        }
    } 
    /* Otherwise, we're the child */
    else {
        gethostname(hostname, 512);
        if (argc == 1) {
            printf("ERROR: child did not receive exepcted argv!\n");
            i = -1;
        } else {
            i = atoi(argv[1]);
        }
        pid = getpid();
        printf("Hello from the child %d on host %s pid %ld\n", i, hostname, (long)pid);
        if (0 == rank) {
            MPI_Recv(&msg, 1, MPI_INT, 0, 1, parent, MPI_STATUS_IGNORE);
            printf("Child %d received msg: %d\n", i, msg);
            if (i != msg) {
                printf("ERROR: Child %d got wrong message (got %d, expected %d)\n",
                       i, msg, i);
            }
        }
        MPI_Comm_disconnect(&parent);
        printf("Child %d disconnected\n", i);
    }

    MPI_Finalize();
    return 0;
}
