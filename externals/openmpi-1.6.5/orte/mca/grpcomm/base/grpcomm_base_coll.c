/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2008 Los Alamos National Security, LLC. 
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/types.h"


#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/util/output.h"

#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/grpcomm/base/base.h"

/***************  TUNED COLLECTIVES FOR GRPCOMM MODULES  **************/

/****    AVAILABLE ALGORITHMS    ****/
static int twoproc(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                   orte_jobid_t jobid, orte_vpid_t *vpids);
static int bruck(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                 orte_jobid_t jobid, orte_vpid_t np, orte_vpid_t *vpids);
static int recursivedoubling(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                             orte_jobid_t jobid, orte_vpid_t np, orte_vpid_t *vpids);

/****    LOCAL VARIABLES USED IN COLLECTIVES    ****/
static int num_recvd;
static opal_buffer_t bucket;

/* Receive and process collective messages */
static void process_coll_msg(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;

    /* transfer the data to the collecting bucket */
    opal_dss.copy_payload(&bucket, mev->buffer);
    
    /* cleanup */
    OBJ_RELEASE(mev);
    
    /* increment the number recvd */
    num_recvd++;
}

void orte_grpcomm_base_coll_recv(int status, orte_process_name_t* sender,
                                 opal_buffer_t* buffer, orte_rml_tag_t tag,
                                 void* cbdata)
{
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:coll:receive got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));
    
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_coll_msg);
    
    return;
}

/*
 * Switchyard for selecting the collective algorithm to use
 */
int orte_grpcomm_base_allgather(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                                orte_jobid_t jobid, orte_vpid_t np, orte_vpid_t *vpids)
{
    bool has_one;
    orte_vpid_t n;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:coll:allgather called with %d entries np %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         num_entries, (int)np));
    
    /* if we only have one proc participating, just copy the data across and return */
    if (1 == np) {
        opal_dss.pack(recvbuf, &num_entries, 1, OPAL_INT32);
        return opal_dss.copy_payload(recvbuf, sendbuf);
    }
    
    if (2 == np) {
        /* only two procs in collective */
        return twoproc(sendbuf, recvbuf, num_entries, jobid, vpids);
    }
    
    /* if we have power of 2 participants, use recursive doubling - otherwise,
     * use bruck algorithm
     */
    has_one = false;
    n = np;
    for ( ; n > 0; n >>= 1) {
        if (n & 0x1) {
            if (has_one) {
                return bruck(sendbuf, recvbuf, num_entries, jobid, np, vpids);
            }
            has_one = true;
        }
    }
    
    /* must be power of two! */
    return recursivedoubling(sendbuf, recvbuf, num_entries, jobid, np, vpids);
}


/*
 * The Two-Proc Algorithm
 *
 * One sends to zero, zero waits to recv from one
 * Zero adds its data to message, sends result back to one
 */
static int twoproc(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                   orte_jobid_t jobid, orte_vpid_t *vpids)
{
    orte_process_name_t peer;
    int32_t num_remote, cnt;
    int rc;
    opal_buffer_t buf;
    
    peer.jobid = jobid;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:coll:two-proc algo employed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (vpids[0] == ORTE_PROC_MY_NAME->vpid) {
        /* I send first */
        peer.vpid = vpids[1];
        /* setup a temp buffer so I can inform the other side as to the
         * number of entries in my buffer
         */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &num_entries, 1, OPAL_INT32);
        opal_dss.copy_payload(&buf, sendbuf);
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:coll:two-proc sending to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&peer)));
        
        if (0 > (rc = orte_rml.send_buffer(&peer, &buf, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
        
        /* wait for reply */
        num_recvd = 0;
        OBJ_CONSTRUCT(&bucket, opal_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                          ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                                          ORTE_RML_NON_PERSISTENT,
                                                          orte_grpcomm_base_coll_recv,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        
        ORTE_PROGRESSED_WAIT(false, num_recvd, 1);
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:coll:two-proc got my return message",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
    } else {
        /* if I am not the start, then I recv first */
        num_recvd = 0;
        OBJ_CONSTRUCT(&bucket, opal_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                          ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                                          ORTE_RML_NON_PERSISTENT,
                                                          orte_grpcomm_base_coll_recv,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        
        ORTE_PROGRESSED_WAIT(false, num_recvd, 1);
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:coll:two-proc got my starting message",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        /* send my data back */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &num_entries, 1, OPAL_INT32);
        opal_dss.copy_payload(&buf, sendbuf);
        peer.vpid = vpids[0];
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:coll:two-proc sending to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&peer)));
        if (0 > (rc = orte_rml.send_buffer(&peer, &buf, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
    }
    
    /* extract the number of entries in the remote buffer */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&bucket, &num_remote, &cnt, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* output of a collective begins with the total number of entries */
    num_remote += num_entries;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(recvbuf, &num_remote, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* xfer my data */
    opal_dss.copy_payload(recvbuf, sendbuf);
    /* xfer the recvd data */
    opal_dss.copy_payload(recvbuf, &bucket);
    
    /* cleanup */
    OBJ_DESTRUCT(&bucket);
    
    return ORTE_SUCCESS;
}


/* For a complete description of this algorithm, please look at
 * ompi/mca/coll/tuned/coll_tuned_allgather.c
 */
static int bruck(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                 orte_jobid_t jobid, orte_vpid_t np, orte_vpid_t *vpids)
{
    orte_vpid_t rank, distance, nv;
    orte_process_name_t peer;
    int32_t num_remote, total_entries, cnt;
    opal_buffer_t collection, buf;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:coll:bruck algo employed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* initialize */
    total_entries = num_entries;
    
    /* start by seeding the collection with our own data */
    OBJ_CONSTRUCT(&collection, opal_buffer_t);
    opal_dss.copy_payload(&collection, sendbuf);

    /* collective is constrained to take place within the specified jobid */
    peer.jobid = jobid;
    
    /* Communication step:
     At every step i, rank r:
     - doubles the distance
     - sends message containing all data collected so far to rank r - distance
     - receives message containing all data collected so far from rank (r + distance)
     */
    /* find my position in the group of participants. This
     * value is the "rank" we will use in the algo
     */
    rank = ORTE_VPID_INVALID;
    for (nv=0; nv < np; nv++) {
        if (vpids[nv] == ORTE_PROC_MY_NAME->vpid) {
            rank = nv;
            break;
        }
    }

    /* check for bozo case */
    if (ORTE_VPID_INVALID == rank) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    for (distance = 1; distance < np; distance <<= 1) {

        /* first send my current contents */
        nv = (rank - distance + np) % np;
        peer.vpid = vpids[nv];
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &total_entries, 1, OPAL_INT32);
        opal_dss.copy_payload(&buf, &collection);
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:coll:bruck sending to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&peer)));
        if (0 > (rc = orte_rml.send_buffer(&peer, &buf, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
        
        /* now setup to recv from my other partner */
        num_recvd = 0;
        nv = (rank + distance) % np;
        peer.vpid = vpids[nv];
        OBJ_CONSTRUCT(&bucket, opal_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(&peer,
                                                          ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                                          ORTE_RML_NON_PERSISTENT,
                                                          orte_grpcomm_base_coll_recv,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* and wait for it to get here */
        ORTE_PROGRESSED_WAIT(false, num_recvd, 1);
        
        /* extract the number of entries in the remote buffer */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&bucket, &num_remote, &cnt, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* add it to our running total */
        total_entries += num_remote;
        
        /* transfer the data to our collection */
        opal_dss.copy_payload(&collection, &bucket);
        
        /* cleanup */
        OBJ_DESTRUCT(&bucket);
    }
    
    /* output of a collective begins with the total number of entries */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(recvbuf, &total_entries, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* transfer the collected data */
    opal_dss.copy_payload(recvbuf, &collection);
    
    /* cleanup */
    OBJ_DESTRUCT(&collection);
    
    return ORTE_SUCCESS;
}

/* For a complete description of this algorithm, please look at
 * ompi/mca/coll/tuned/coll_tuned_allgather.c
 */
static int recursivedoubling(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                             orte_jobid_t jobid, orte_vpid_t np, orte_vpid_t *vpids)
{
    orte_vpid_t rank, distance, nv;
    int32_t num_remote, total_entries, cnt;
    opal_buffer_t collection, buf;
    orte_process_name_t peer;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:coll:recdub algo employed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* initialize */
    total_entries = num_entries;
    
    /* start by seeding the collection with our own data */
    OBJ_CONSTRUCT(&collection, opal_buffer_t);
    opal_dss.copy_payload(&collection, sendbuf);
    
    /* collective is constrained to take place within the specified jobid */
    peer.jobid = jobid;
    
    /* Communication step:
     At every step i, rank r:
     - exchanges message containing all data collected so far with rank peer = (r ^ 2^i).
     */
    /* find my position in the group of participants. This
     * value is the "rank" we will use in the algo
     */
    rank = ORTE_VPID_INVALID;
    for (nv=0; nv < np; nv++) {
        if (vpids[nv] == ORTE_PROC_MY_NAME->vpid) {
            rank = nv;
            break;
        }
    }
    
    /* check for bozo case */
    if (ORTE_VPID_INVALID == rank) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    for (distance = 0x1; distance < np; distance<<=1) {
        
        /* first send my current contents */
        nv = rank ^ distance;
        peer.vpid = vpids[nv];
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &total_entries, 1, OPAL_INT32);
        opal_dss.copy_payload(&buf, &collection);
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:coll:recdub sending to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&peer)));
        if (0 > (rc = orte_rml.send_buffer(&peer, &buf, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
        
        /* now setup to recv from my other partner */
        num_recvd = 0;
        OBJ_CONSTRUCT(&bucket, opal_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(&peer,
                                                          ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                                          ORTE_RML_NON_PERSISTENT,
                                                          orte_grpcomm_base_coll_recv,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* and wait for it to get here */
        ORTE_PROGRESSED_WAIT(false, num_recvd, 1);
        
        /* extract the number of entries in the remote buffer */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&bucket, &num_remote, &cnt, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* add it to our running total */
        total_entries += num_remote;
        
        /* transfer the data to our collection */
        opal_dss.copy_payload(&collection, &bucket);
        
        /* cleanup */
        OBJ_DESTRUCT(&bucket);
    }
    
    /* output of a collective begins with the total number of entries */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(recvbuf, &total_entries, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* transfer the collected data */
    opal_dss.copy_payload(recvbuf, &collection);
    
    /* cleanup */
    OBJ_DESTRUCT(&collection);
    
    return ORTE_SUCCESS;
}
