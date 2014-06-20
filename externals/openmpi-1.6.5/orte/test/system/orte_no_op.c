/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of applications
 */

#include <stdio.h>
#include "orte/constants.h"
#include "orte/runtime/runtime.h"

int main(int argc, char* argv[])
{
    if (ORTE_SUCCESS != orte_init(&argc, &argv, ORTE_PROC_NON_MPI)) {
        fprintf(stderr, "Failed orte_init\n");
        exit(1);
    }
    
    if (ORTE_SUCCESS != orte_finalize()) {
        fprintf(stderr, "Failed orte_finalize\n");
        exit(1);
    }
    return 0;
}
