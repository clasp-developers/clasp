/*
 * Simple ring test program
 */

#include <stdio.h>
#include <mpi.h>

#ifdef MANUAL
#include "vt_user.h"
#endif

#define NRING 100
#define TAG   4711

int main(int argc, char *argv[])
{
    int rank, size, next, prev, message;

#ifdef MANUAL
    VT_USER_START("main");
#endif

    /* Start up MPI */

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
 
    /* Calculate the rank of the next process in the ring.  Use the
       modulus operator so that the last process "wraps around" to
       rank zero. */

    next = (rank + 1) % size;
    prev = (rank + size - 1) % size;

    /* If we are the "master" process (i.e., MPI_COMM_WORLD rank 0),
       put the number of times to go around the ring in the
       message. */

    if (0 == rank) {
        message = NRING;

        printf("Process 0 sending %d to %d, tag %d (%d processes in ring)\n", 
               message, next, TAG, size);
        MPI_Send(&message, 1, MPI_INT, next, TAG, MPI_COMM_WORLD); 
        printf("Process 0 sent to %d\n", next);
    }

    /* Pass the message around the ring.  The exit mechanism works as
       follows: the message (a positive integer) is passed around the
       ring.  Each time it passes rank 0, it is decremented.  When
       each processes receives a message containing a 0 value, it
       passes the message on to the next process and then quits.  By
       passing the 0 message first, every process gets the 0 message
       and can quit normally. */

    while (1) {
#ifdef MANUAL
        VT_USER_START("ring_loop");
#endif
        MPI_Recv(&message, 1, MPI_INT, prev, TAG, MPI_COMM_WORLD, 
                 MPI_STATUS_IGNORE);

        if (0 == rank) {
            --message;
            printf("Process 0 decremented value: %d\n", message);
        }

        MPI_Send(&message, 1, MPI_INT, next, TAG, MPI_COMM_WORLD);
        if (0 == message) {
            printf("Process %d exiting\n", rank);
            break;
        }
#ifdef MANUAL
        VT_USER_END("ring_loop");
#endif
    }

    /* The last process does one extra send to process 0, which needs
       to be received before the program can exit */

    if (0 == rank) {
        MPI_Recv(&message, 1, MPI_INT, prev, TAG, MPI_COMM_WORLD,
                 MPI_STATUS_IGNORE);
    }
    
    /* All done */

    MPI_Finalize();

#ifdef MANUAL
    VT_USER_END("main");
#endif

    return 0;
}
