#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "orte/constants.h"

#include "orte/util/proc_info.h"
#include "orte/runtime/runtime.h"

int main( int argc, char **argv ) 
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "couldn't init orte - error code %d\n", rc);
        return rc;
    }
    sleep(1);
    orte_finalize();
    
    return 0;
}
