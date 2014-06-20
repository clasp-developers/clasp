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
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"

#include "iof_tool.h"


static int tool_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd);

static int tool_pull(const orte_process_name_t* src_name,
                      orte_iof_tag_t src_tag,
                      int fd);

static int tool_close(const orte_process_name_t* peer,
                       orte_iof_tag_t source_tag);

static int tool_ft_event(int state);

orte_iof_base_module_t orte_iof_tool_module = {
    tool_push,
    tool_pull,
    tool_close,
    tool_ft_event
};


/**
 * Push data from the specified file descriptor
 * to the indicated SINK set of peers.
 */

static int tool_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd)
{
    /* at this time, we do not allow tools to push data into the
     * stdin of a job. This is due to potential confusion over which
     * stdin is being read/used, and the impossibility of resolving
     * potential interleaving of the data
     */
    
    return ORTE_ERR_NOT_SUPPORTED;
}


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

/**
 * Pull data from the specified set of SOURCE peers and
 * dump to the indicated file descriptor.
 */

static int tool_pull(const orte_process_name_t* src_name,
                     orte_iof_tag_t src_tag,
                     int fd)
{
    /* if we are a tool, then we need to request the HNP to please
     * forward the data from the specified process to us. Note that
     * the HNP will return an error if the specified stream of any
     * intended recipient is not open. By default, stdout/err/diag
     * are all left open. However, the user can also direct us to
     * close any or all of those streams, so the success of this call
     * will depend upon how the user executed the application
     */
    
    opal_buffer_t *buf;
    orte_iof_tag_t tag;
    orte_process_name_t hnp;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s pulling output for proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(src_name)));

    buf = OBJ_NEW(opal_buffer_t);
    
    /* setup the tag to pull from HNP */
    tag = src_tag | ORTE_IOF_PULL;
    
    /* pack the tag - we do this first so that flow control messages can
     * consist solely of the tag
     */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &tag, 1, ORTE_IOF_TAG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }
    /* pack the name of the source */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, src_name, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }
    
    /* send the buffer to the correct HNP */
    ORTE_HNP_NAME_FROM_JOB(&hnp, src_name->jobid);
    orte_rml.send_buffer_nb(&hnp, buf, ORTE_RML_TAG_IOF_HNP,
                            0, send_cb, NULL);
    
    return ORTE_SUCCESS;
}


static int tool_close(const orte_process_name_t* src_name,
                      orte_iof_tag_t src_tag)
{
    /* if we are a tool, then we need to request the HNP to stop
     * forwarding data from this process/stream
     */
    
    opal_buffer_t *buf;
    orte_iof_tag_t tag;
    orte_process_name_t hnp;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s closing output for proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(src_name)));
    
    buf = OBJ_NEW(opal_buffer_t);
    
    /* setup the tag to stop the copy */
    tag = src_tag | ORTE_IOF_CLOSE;
    
    /* pack the tag - we do this first so that flow control messages can
     * consist solely of the tag
     */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &tag, 1, ORTE_IOF_TAG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }
    /* pack the name of the source */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, src_name, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }
    
    /* flag that the close is incomplete */
    mca_iof_tool_component.closed = false;

    /* send the buffer to the correct HNP */
    ORTE_HNP_NAME_FROM_JOB(&hnp, src_name->jobid);
    orte_rml.send_buffer_nb(&hnp, buf, ORTE_RML_TAG_IOF_HNP,
                            0, send_cb, NULL);
    
    /* wait right here until the close is confirmed */
    ORTE_PROGRESSED_WAIT(mca_iof_tool_component.closed, 0, 1);
    
    return ORTE_SUCCESS;
}


/*
 * FT event
 */

static int tool_ft_event(int state)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}
