/* -*- C -*-
 *
 * Copyright (c) 2008 Los Alamos National Security, LLC.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
#include <stdio.h>
#include <stdbool.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>

#include <mpi.h>

int main(int argc, char* argv[])
{
    int msg;
    int rank, size, my_twin;
    int ppn, my_node;
    struct timeval tv;
    unsigned long my_timestamp[2];
    long *timestamps;
    int i, maxrank;
    unsigned long maxsec, maxusec, minutes, seconds;
    unsigned long start_sec, start_usec;
    float fsecs;
    int nnodes;
    bool odd_nnodes;
    bool recvit;
    char *ppnstr;
    
    if (argc < 3) {
        fprintf(stderr, "start times must be provided\n");
        return 1;
    }

    ppnstr = getenv("OMPI_COMM_WORLD_LOCAL_SIZE");
    ppn = strtol(ppnstr, NULL, 10);
    start_sec = strtol(argv[1], NULL, 10);
    start_usec = strtol(argv[2], NULL, 10);
    
    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    /* this program requires that the size be an integer multiple of ppn */
    if (0 != (size % ppn)) {
        if (0 == rank) {
            fprintf(stderr, "The number of procs must be an integer multiple of the ppn\n"
                    "Given: num_procs %d ppn %d\n", size, ppn);
            MPI_Abort(MPI_COMM_WORLD, 1);
        } else {
            goto cleanup;
        }
    }
    
    /* see how many nodes we have */
    nnodes = size / ppn;
    
    odd_nnodes = false;
    if (0 != (nnodes % 2)) {
        /* we have an odd # of nodes */
        odd_nnodes = true;
    }
    
    /* compute the rank of the rank with which I am to exchange a message.
     * Per requirements, this proc must be on another node. To accomplish
     * this with max efficiency, we take advantage of knowing that the ppn
     * on every node will be the same. We therefore pair up the nodes, and
     * pair up the procs on each node, so that only one connection is setup
     * for each proc. We also want to ensure that the node pairs are
     * "neighboring" - i.e., that they hopefully share a switch so that the
     * hop count of sending the messages is minimized.
     */
    
    /* first, determine if my node is odd or even */
    my_node = rank / ppn;
    
     if (0 != (my_node % 2)) {
        /* compute my twin's rank - as I am an odd numbered node, my
         * twin will be on the node below me. Thus, its rank will be
         * my rank - ppn
         */
        my_twin = rank - ppn;
        /* if I am an odd numbered node, then I will receive first */
        MPI_Recv(&msg, 1, MPI_INT, my_twin, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        /* receive the return message so that we meet the stated requirement
         * that -every- proc send a message
         */
        MPI_Send(&msg, 1, MPI_INT, my_twin, 1, MPI_COMM_WORLD);
    } else {
        /* compute my twin's rank - as I am an even numbered node, my
         * twin will be on the node above me. Thus, its rank will be
         * my rank + ppn
         */
        my_twin = rank + ppn;
        /* if we have an odd number of nodes, then the last node will be
         * even and will have no one above them. In this case, we wrap around
         * and ask that node=0 take the additional connections
         */
        recvit = true;
        if (my_twin >= size) {
            my_twin = my_twin - size;
            recvit = false;
        }
        /* I am an even numbered node, so I send first */
        MPI_Send(&msg, 1, MPI_INT, my_twin, 1, MPI_COMM_WORLD);
        /* now receive the reply so my twin also meets the requirement - but only
         * if we don't have an odd number of nodes. If we have an odd number of
         * nodes, then the node=0 procs will already have met their requirement
         */
        if (recvit) {
            MPI_Recv(&msg, 1, MPI_INT, my_twin, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        }
    }
    
    /* if we have an odd number of nodes and I am on node=0, then I have
     * to take the extra recv
     */
    if (odd_nnodes && 0 == my_node) {
        my_twin = size - ppn + rank;
        MPI_Recv(&msg, 1, MPI_INT, my_twin, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    }
    
    /* get a completion time stamp */
    gettimeofday(&tv, NULL);
    my_timestamp[0] = tv.tv_sec;
    my_timestamp[1] = tv.tv_usec;
    
    /* THIS COMPLETES THE OFFICIAL TIMING POINT */

    /* Gather to get all the timestamps to rank 0 */
    timestamps = NULL;
    if (0 == rank) {
        timestamps = malloc(2 * size * sizeof(unsigned long));
        if (NULL == timestamps) {
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }
    MPI_Gather(&my_timestamp, 2, MPI_LONG,
               timestamps, 2, MPI_LONG, 0, MPI_COMM_WORLD);
    if (0 == rank) {
        /* The "timestamps" array will now have everyone's timestamp
         (i.e., rank 0's timestamp will be in pos 0 & 1,, rank 1's timestamp
         will be in 2 & 3, ...etc. */
        /* find the maximum timestamp */
        maxsec = start_sec;
        maxusec = start_usec;
        maxrank = -1;
        for (i=0; i < 2*size; i+=2) {
            if (timestamps[i] < maxsec) {
                continue;
            }
            if (timestamps[i] == maxsec &&
                timestamps[i+1] < maxusec) {
                continue;
            }
            maxsec = timestamps[i];
            maxusec = timestamps[i+1];
            maxrank = i/2;
        }
        free(timestamps);
        /* subtract starting time to get time in microsecs for test */
        maxsec = maxsec - start_sec;
        if (maxusec >= start_usec) {
            maxusec = maxusec - start_usec;
        } else {
            maxsec--;
            maxusec = 1000000 - start_usec + maxusec;
        }
        /* pretty-print the result */
        seconds = maxsec + (maxusec / 1000000l);
        minutes = seconds / 60l;
        seconds = seconds % 60l;
        if (0 == minutes && 0 == seconds) {
            fsecs = ((float)(maxsec)*1000000.0 + (float)maxusec) / 1000.0;
            fprintf(stderr, "Time test was completed in %8.2f millisecs\nSlowest rank: %d\n",
                    fsecs, maxrank);
        } else {
            fprintf(stderr, "Time test was completed in %3lu:%02lu min:sec\nSlowest rank: %d\n",
                    minutes, seconds, maxrank);
        }
    }
    
cleanup:
    /* this completes the test */
    MPI_Finalize();
    
    return 0;
}
