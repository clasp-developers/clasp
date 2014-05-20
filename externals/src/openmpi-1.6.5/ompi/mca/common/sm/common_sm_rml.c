/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/types.h"
#include "opal/dss/dss.h"

#include "orte/mca/rml/rml.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "ompi/constants.h"
#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/common/sm/common_sm_rml.h"

/* only for debug purposes only */
#include <assert.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * this routine assumes that sorted_procs is in the following state:
 *     o all the local procs at the beginning.
 *     o procs[0] is the lowest named process.
 */
int
mca_common_sm_rml_info_bcast(opal_shmem_ds_t *out_ds_buf,
                             ompi_proc_t **procs,
                             size_t num_local_procs,
                             int tag,
                             bool proc0,
                             char *msg_id_str)
{
    int rc = OMPI_SUCCESS, tmprc;
    char *msg_id_str_to_tx = NULL;
    opal_buffer_t *buffer = NULL;

    if (NULL == (buffer = OBJ_NEW(opal_buffer_t))) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    /* figure out if i am the root proc in the group.  if i am, bcast the
     * message the rest of the local procs. */
    if (proc0) {
        size_t p;
        /* pack the data that we are going to send. first the queueing id, then
         * the shmem_ds buf. note that msg_id_str is used only for verifying
         * "expected" common sm usage.  see "RML Messaging and Our Assumptions"
         * note in common_sm.c for more details. */
        tmprc = opal_dss.pack(buffer, &msg_id_str, 1, OPAL_STRING);
        if (OPAL_SUCCESS != tmprc) {
            ORTE_ERROR_LOG(ORTE_ERR_PACK_FAILURE);
            rc = OMPI_ERR_PACK_FAILURE;
            goto out;
        }
        tmprc = opal_dss.pack(buffer, out_ds_buf,
                              (int32_t)sizeof(opal_shmem_ds_t),
                              OPAL_BYTE);
        if (OPAL_SUCCESS != tmprc) {
            ORTE_ERROR_LOG(ORTE_ERR_PACK_FAILURE);
            rc = OMPI_ERR_PACK_FAILURE;
            goto out;
        }
        opal_progress_event_users_increment();
        /* first num_local_procs items should be local procs */
        for (p = 1; p < num_local_procs; ++p) {
            /* a potential future optimization: use non-blocking routines */
            tmprc = orte_rml.send_buffer(&(procs[p]->proc_name), buffer, tag,
                                         0);
            if (0 > tmprc) {
                ORTE_ERROR_LOG(tmprc);
                opal_progress_event_users_decrement();
                rc = OMPI_ERROR;
                goto out;
            }
        }
        opal_progress_event_users_decrement();
    }
    /* i am NOT the root proc */
    else {
        int32_t num_vals;
        /* bump up the libevent polling frequency while we're in this RML recv,
         * just to ensure we're checking libevent frequently. */
        opal_progress_event_users_increment();
        tmprc = orte_rml.recv_buffer(&(procs[0]->proc_name), buffer, tag, 0);
        opal_progress_event_users_decrement();
        if (0 > tmprc) {
            ORTE_ERROR_LOG(tmprc);
            rc = OMPI_ERROR;
            goto out;
        }
        /* unpack the buffer */
        num_vals = 1;
        tmprc = opal_dss.unpack(buffer, &msg_id_str_to_tx, &num_vals,
                                OPAL_STRING);
        if (0 > tmprc) {
            ORTE_ERROR_LOG(ORTE_ERR_UNPACK_FAILURE);
            rc = OMPI_ERROR;
            goto out;
        }
        num_vals = (int32_t)sizeof(opal_shmem_ds_t);
        tmprc = opal_dss.unpack(buffer, out_ds_buf, &num_vals, OPAL_BYTE);
        if (0 > tmprc) {
            ORTE_ERROR_LOG(ORTE_ERR_UNPACK_FAILURE);
            rc = OMPI_ERROR;
            goto out;
        }
        /* the message better be for me.  if not, freak out because this
         * probably means that common sm is being used in a new way that lies
         * outside of our current scope of assumptions. see "RML Messaging and
         * Our Assumptions" note in common_sm.c */
        if (0 != strcmp(msg_id_str_to_tx, msg_id_str)) {
            orte_show_help("help-mpi-common-sm.txt", "unexpected message id",
                           true, orte_process_info.nodename,
                           msg_id_str, msg_id_str_to_tx);
            rc = OMPI_ERROR;
            /* here for extra debug info only */
            assert(0);
            goto out;
        }
    }

out:
    if (NULL != msg_id_str_to_tx) {
        free(msg_id_str_to_tx);
        msg_id_str_to_tx = NULL;
    }
    OBJ_RELEASE(buffer);
    return rc;
}
