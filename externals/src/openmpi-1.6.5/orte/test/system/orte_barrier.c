/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of applications
 */

#include <stdio.h>

#include "orte/runtime/runtime.h"
#include "orte/mca/grpcomm/grpcomm.h"

int main(int argc, char* argv[])
{
    if (ORTE_SUCCESS != orte_init(&argc, &argv, ORTE_PROC_NON_MPI)) {
        fprintf(stderr, "Failed orte_init\n");
        exit(1);
    }
    
orte_grpcomm.barrier();
orte_grpcomm.barrier();

    if (ORTE_SUCCESS != orte_finalize()) {
        fprintf(stderr, "Failed orte_finalize\n");
        exit(1);
    }
    return 0;
}
