#include <stdio.h>
#include <signal.h>
#include <math.h>

#include "orte/runtime/orte_globals.h"

#include "orte/runtime/runtime.h"

#define MAX_COUNT 3
#define ORTE_IOF_BASE_MSG_MAX   2048


int
main(int argc, char *argv[]){
    int count;
    int msgsize;
    unsigned char msg[ORTE_IOF_BASE_MSG_MAX];
    int i, j, rc;
    double maxpower;
    unsigned char chr;
    bool readstdin;
    
    /*
     * Init
     */
    orte_init(&argc, &argv, ORTE_PROC_NON_MPI);

    if (argc >= 2) {
        count = atoi(argv[1]);
        if (count < 0) {
            count = INT_MAX-1;
        }
    } else {
        count = MAX_COUNT;
    }
    
    if (argc == 3) {
        /* read from stdin */
        readstdin = true;
    } else {
        readstdin = false;
    }
    
    if (0 == ORTE_PROC_MY_NAME->vpid && readstdin) {
        while (0 != (msgsize = read(0, msg, ORTE_IOF_BASE_MSG_MAX))) {
            if (msgsize > 0) {
                msg[msgsize] = '\n';
                 write(1, msg, msgsize);
            }
        }
    }
    
    for (j=1; j < count+1; j++) {
        
#if 0
        maxpower = (double)(j%7);
#endif

        chr = (j % 26) + 65;
        memset(msg, chr, ORTE_IOF_BASE_MSG_MAX);
        msgsize = 10;
        msg[msgsize-1] = '\n';
        
        write(1, msg, msgsize);
        
    }

    orte_finalize();

    return 0;
}
