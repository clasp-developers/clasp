#include <stdio.h>
#include <signal.h>


#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/rml.h"

#include "orte/runtime/runtime.h"

#define MY_TAG 12345
#define MAX_COUNT 3

#            define false 0
#            define true 1

int
main(int argc, char *argv[]){
    int counter = 0;
    char * my_name = NULL;
    char * my_right_peer = NULL;
    char * my_left_peer  = NULL;
    orte_process_name_t right_peer_orte_name;
    orte_process_name_t left_peer_orte_name;
    int num_peers = 0;
    struct iovec msg;

    /*
     * Init
     */
    orte_init(&argc, &argv, ORTE_PROC_NON_MPI);

    num_peers = orte_process_info.num_procs;

    /*
     * Construct Peer name in a ring
     */
    right_peer_orte_name.jobid  = ORTE_PROC_MY_NAME->jobid;
    right_peer_orte_name.vpid   = ORTE_PROC_MY_NAME->vpid + 1;
    if( right_peer_orte_name.vpid >= num_peers ) {
        right_peer_orte_name.vpid = 0;
    }

    left_peer_orte_name.jobid  = ORTE_PROC_MY_NAME->jobid;
    left_peer_orte_name.vpid   = ORTE_PROC_MY_NAME->vpid - 1;
    if( ORTE_PROC_MY_NAME->vpid == 0 ) {
        left_peer_orte_name.vpid = num_peers - 1;
    }

    printf("My name is: %s -- PID %d\tMy Left Peer is %s\tMy Right Peer is %s\n",
           ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), getpid(),
           ORTE_NAME_PRINT(&left_peer_orte_name),
           ORTE_NAME_PRINT(&right_peer_orte_name));

    /*
     * Rank 0 starts the ring...
     */
    if( ORTE_PROC_MY_NAME->vpid == 0) {
        /* update value */
        counter = 1;

        /* Send to right */
        msg.iov_base = (void *) &counter;
        msg.iov_len  = sizeof(counter);

        printf("%s Send Counter (%d) to peer %s\n",
               ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), 
               counter, ORTE_NAME_PRINT(&right_peer_orte_name));
        
        if( 0 > orte_rml.send(&right_peer_orte_name,
                                             &msg,
                                             1,
                                             MY_TAG,
                                             0) ) {
            printf("error... %d\n", __LINE__);;
        }
    }


    while (counter <= MAX_COUNT ) {
        int *cnt;

        /* Receive from left */
        printf("%s Waiting to Recv Counter from peer %s\n", 
               ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(&left_peer_orte_name));
        
        msg.iov_base = NULL;
        msg.iov_len  = 0;

        if( 0 > orte_rml.recv(&left_peer_orte_name,
                              &msg,
                              1,
                              MY_TAG,
                              ORTE_RML_ALLOC) ) {
            printf("error A... %d\n", __LINE__);
        }
        
        cnt = (int *) msg.iov_base;
        counter = *cnt;

        /* Update */
        printf("%s Recv %d ... Send %d\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), counter, counter + 1);
        if( ORTE_PROC_MY_NAME->vpid == 0 ) {
            sleep(2);
        }
        if(ORTE_PROC_MY_NAME->vpid == 0) {
            counter++;
        }
        
        if(counter > MAX_COUNT && right_peer_orte_name.vpid == 0) {
            break;
        }

        /* Send to right */
        msg.iov_base = (void *) &counter;
        msg.iov_len  = sizeof(counter);

        printf("%s Send Counter (%d) to peer (%s)\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),  counter,
               ORTE_NAME_PRINT(&right_peer_orte_name));
        
        if( 0 > orte_rml.send(&right_peer_orte_name,
                                             &msg,
                                             1,
                                             MY_TAG,
                                             0) ) {
            printf("error B... %d\n", __LINE__);;
        }
    }

    orte_finalize();

    return 0;
}
