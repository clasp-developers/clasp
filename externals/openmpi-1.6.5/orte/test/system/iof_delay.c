#include <stdio.h>
#include <signal.h>
#include <math.h>


#include "orte/runtime/runtime.h"

#define MAX_COUNT 300
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
    

    i = 1;
    for (j=1; j < count+1; j++) {
        
#if 0
        maxpower = (double)(j%7);
#endif

        chr = (j % 26) + 65;
        memset(msg, chr, ORTE_IOF_BASE_MSG_MAX);
        msgsize = 10;
        msg[msgsize-1] = '\n';
        
        if (i == 1) {
            i = 2;
        } else {
            i = 1;
        }

        write(i, msg, msgsize);
        
        sleep(3);
        
    }

    orte_finalize();

    return 0;
}
