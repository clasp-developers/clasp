/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */


#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/grpcomm/grpcomm.h"

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"

#include "iof_hnp.h"

/*
 * Callback when non-blocking RML send completes.
 */
static void send_cb(int status, orte_process_name_t *peer,
                          opal_buffer_t *buf, orte_rml_tag_t tag,
                          void *cbdata)
{
    /* nothing to do here - just release buffer and return */
    OBJ_RELEASE(buf);
}

void orte_iof_hnp_send_data_to_endpoint(orte_process_name_t *host,
                                        orte_process_name_t *target,
                                        orte_iof_tag_t tag,
                                        unsigned char *data, int numbytes)
{
    opal_buffer_t *buf;
    int rc;
    
    buf = OBJ_NEW(opal_buffer_t);
    
    /* pack the tag - we do this first so that flow control messages can
     * consist solely of the tag
     */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &tag, 1, ORTE_IOF_TAG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return;
    }
    /* pack the name of the target - this is either the intended
     * recipient (if the tag is stdin and we are sending to a daemon),
     * or the source (if we are sending to anyone else)
     */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, target, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return;
    }
    
    /* if data is NULL, then we are done */
    if (NULL != data) {
        /* pack the data - if numbytes is zero, we will pack zero bytes */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, data, numbytes, OPAL_BYTE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buf);
            return;
        }
    }
    
    /* if the target is wildcard, then this needs to go to everyone - xcast it */
    if (ORTE_PROC_MY_NAME->jobid == host->jobid &&
        ORTE_VPID_WILDCARD == host->vpid) {
        /* xcast this to everyone - the local daemons will know how to handle it */
        orte_grpcomm.xcast(ORTE_PROC_MY_NAME->jobid, buf, ORTE_RML_TAG_IOF_PROXY);
        OBJ_RELEASE(buf);
        return;
    }
    
    /* send the buffer to the host - this is either a daemon or
     * a tool that requested IOF
     */
    orte_rml.send_buffer_nb(host, buf, ORTE_RML_TAG_IOF_PROXY,
                            0, send_cb, NULL);
}
