/* -*- C -*-
 *
 * $HEADER$
 *
 * A program that just spins - provides mechanism for testing user-driven
 * abnormal program termination
 */

#include <stdio.h>
#include <unistd.h>

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"

int main(int argc, char* argv[])
{

    int i, rc, j=0;
    double pi;
    pid_t pid;

    if (0 > (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "spin: couldn't init orte - error code %d\n", rc);
        return rc;
    }
    pid = getpid();

    printf("spin: Name %s Pid %ld\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)pid);
    
    i = 0;
    while (0 == j) {
        i++;
        pi = i / 3.14159256;
        if (i > 100) i = 0;
    }
    
    return 0;
}
