/*
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>

#include "mpi.h"

int main(int argc, char *argv[])
{ 
    MPI_Status     status;               /* MPI status                          */
    int            mpierr;               /* MPI function return code            */
    int            rank;                 /* Process rank within MPI_COMM_WORLD  */
    int            size;
    int            dest, src;
    int            tag0=41;              /* MPI message tag                     */
    
    int            inject;
    int            report;
    int            iterations;
    int            n_bytes; 
    unsigned char* send_buff;
    unsigned char* recv_buff;
    char*          tmp;
    
    int            i, j, count;
    
    float fraction, randval;
    struct timeval tp;
    
    if (1 < argc) {
        if (0 == strncmp(argv[1], "-h", 2) ||
            0 == strncmp(argv[1], "--h", 3)) {
            printf("Usage: mpirun --options-- ./sendrecv_blaster <options> where options are:\n"
                   "\tpattern=[self | pair | ring] where\n"
                   "\t\tself => sendrecv with self\n"
                   "\t\tpair => sendrecv with a complementary partner [0 <-> N-1, 1 <-> N-2...]\n"
                   "\t\tring [default] => sendrecv around a ring [0 recvs from N-1 and sends to 1]\n"
                   "\tsize=[value < 0 => max message size in kbytes, value > 0 => max message size in Mbytes (default=1MByte)]\n"
                   "\tinject=[value = #iterations before injecting MPI_Sendrecv to self (default: never)]\n"
                   "\treport=[value = #iterations/reporting point (default: 1000)\n"
                   "\titerations=[value = #iterations before stopping (default: 1000000)\n");
            return 0;
        }
    }
    
    mpierr = MPI_Init(&argc, &argv);
    if (mpierr != MPI_SUCCESS)
    {
        fprintf(stderr, "MPI Error %d (MPI_Init)\n",mpierr);
        fflush(stderr);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
    
    MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
    
    mpierr = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (mpierr != MPI_SUCCESS || rank < 0)
    {
        fprintf(stderr, "MPI Error %d (MPI_Comm_rank)\n",mpierr);
        fflush(stderr);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
    
    mpierr = MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (mpierr != MPI_SUCCESS || size < 0)
    {
        fprintf(stderr, "MPI Error %d (MPI_Comm_size)\n",mpierr);
        fflush(stderr);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    /* setup defaults in lieu of args */
    n_bytes = 1024*1024;
    inject = -1;
    report = 1000;
    iterations = 1000000;
    /* do a ring */
    src = rank - 1;
    if (src < 0) {
        src = size - 1;
    }
    dest = rank + 1;
    if (dest > size-1) {
        dest = 0;
    }
    
    for (i=1; i < argc; i++) {
        fprintf(stderr, "got %s\n", argv[i]);
        if (0 == strncmp(argv[i], "pattern", strlen("pattern"))) {
            tmp = strchr(argv[i], '=');
            tmp++;
            if (0 == strcmp(tmp, "self")) {
                /* just do it with myself */
                src = rank;
                dest = rank;
            } else if (0 == strcmp(tmp, "pair")) {
                /* do it pair-wise */
                src = (size-1) - rank;
                dest = src;
            } else {
                /* do a ring */
                src = rank - 1;
                if (src < 0) {
                    src = size - 1;
                }
                dest = rank + 1;
                if (dest > size-1) {
                    dest = 0;
                }
            }
        } else if (0 == strncmp(argv[i], "size", strlen("size"))) {
            tmp = strchr(argv[i], '=');
            tmp++;
            n_bytes = atoi(tmp);
            if (n_bytes < 0) {
                n_bytes = -1 * n_bytes * 1024;
            } else {
                n_bytes = n_bytes * 1024*1024;
            }
        } else if (0 == strncmp(argv[i], "inject", strlen("inject"))) {
            tmp = strchr(argv[i], '=');
            tmp++;
            inject = atoi(tmp);
        } else if (0 == strncmp(argv[i], "report", strlen("report"))) {
            tmp = strchr(argv[i], '=');
            tmp++;
            report = atoi(tmp);
        } else if (0 == strncmp(argv[i], "iter", strlen("iter"))) {
            tmp = strchr(argv[i], '=');
            tmp++;
            iterations = atoi(tmp);
        }
    }

    send_buff = (unsigned char *) valloc(n_bytes);
    recv_buff = (unsigned char *) valloc(n_bytes);
    
    /* seed the random number generator */
    gettimeofday (&tp, NULL);
    srand (tp.tv_usec);

    for ( i=0; i<n_bytes; i++ )
    {
        send_buff[i] = i%128;
    }
    
    fprintf(stderr, "Rank %d: recving from src %d sending to dest %d with max buff size %dKbytes\n",
            rank, src, dest, n_bytes/1024);

    i=0;
    while (i < iterations)
    {
        randval = rand();
        fraction = randval/RAND_MAX;
        count = fraction * n_bytes;
        mpierr = MPI_Sendrecv(send_buff, count, MPI_CHAR, dest, tag0,
                              recv_buff, n_bytes, MPI_CHAR, src, tag0, MPI_COMM_WORLD, &status);
        if (mpierr != MPI_SUCCESS)
        {
            fprintf(stderr,"MPI Error %d (MPI_Sendrecv) [%d,%d] at iteration %d\n",mpierr,src,dest,i);
            fflush(stderr);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
        i++;
        if (0 == (i % report)) {
            fprintf(stderr, "Rank %d has completed %dk iterations\n", rank, i/1000);
        }
        if (0 < inject && 0 == (i % inject)) {
            mpierr = MPI_Sendrecv(send_buff, count, MPI_CHAR, rank, tag0,
                                  recv_buff, n_bytes, MPI_CHAR, rank, tag0, MPI_COMM_WORLD, &status);
            if (mpierr != MPI_SUCCESS)
            {
                fprintf(stderr,"MPI Error %d (MPI_Sendrecv) [%d,%d] at iteration %d\n",mpierr,rank,rank,i);
                fflush(stderr);
                MPI_Abort(MPI_COMM_WORLD, -1);
            } else {
                fprintf(stderr, "Rank %d has completed MPI_Sendrecv with myself\n", rank);
            }
        }
    }
    
    fprintf(stderr, "Rank %d completed test\n", rank);
    MPI_Finalize();
}
