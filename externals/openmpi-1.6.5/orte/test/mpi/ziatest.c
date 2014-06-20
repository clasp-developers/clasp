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
    int nppn;
    struct timeval tv;
    char *cmd;

    /* check for proper usage */
    if (2 < argc) {
        printf("usage: ziatest <#procs/node>\n");
        exit(1);
    }
    
    nppn = strtol(argv[1], NULL, 10);
    
    /* THIS BEGINS THE OFFICIAL TIMING POINT */

    /* get a starting time stamp */
    gettimeofday(&tv, NULL);
    
    /* form the command */
    asprintf(&cmd, "mpirun -npernode %d ./ziaprobe %d %d", nppn, tv.tv_sec, tv.tv_usec);
    
    /* execute it */
    system(cmd);
    
    /* done */
    free(cmd);
    return 0;
}
