/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>

#include "orte/runtime/runtime.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#define MY_TAG 12345

int main(int argc, char* argv[])
{
    int rc;
    char hostname[512];
    pid_t pid;
    struct iovec msg;

    gethostname(hostname, 512);
    pid = getpid();
    
    printf("CHILD starting: Node %s Pid %ld\n", hostname, (long)pid);
    
    if (0 > (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "orte_nodename: couldn't init orte - error code %d\n", rc);
        return rc;
    }

    printf("CHILD %s waiting for message\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    
    /* wait for message from our parent */
    if (0 > orte_rml.recv(ORTE_NAME_WILDCARD, &msg, 1, MY_TAG, ORTE_RML_ALLOC)) {
        printf("error at line %d\n", __LINE__);
    }
    
    printf("CHILD %s got message and is exiting\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    
    orte_finalize();
    return 0;
}
