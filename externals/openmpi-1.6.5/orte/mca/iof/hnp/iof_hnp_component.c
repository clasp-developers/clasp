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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/event/event.h"

#include "orte/util/proc_info.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/iof/base/base.h"
#include "iof_hnp.h"

/*
 * Local functions
 */
static int orte_iof_hnp_open(void);
static int orte_iof_hnp_close(void);
static int orte_iof_hnp_query(mca_base_module_t **module, int *priority);
static void
orte_iof_hnp_exception_handler(const orte_process_name_t* peer, orte_rml_exception_t reason);

/*
 * Local variables
 */
static bool initialized = false;

/*
 * Public string showing the iof hnp component version number
 */
const char *mca_iof_hnp_component_version_string =
    "Open MPI hnp iof MCA component version " ORTE_VERSION;

orte_iof_hnp_component_t mca_iof_hnp_component = {
    {
        /* First, the mca_base_component_t struct containing meta
         information about the component itself */
        
        {
            ORTE_IOF_BASE_VERSION_2_0_0,
            
            "hnp", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            
            /* Component open, close, and query functions */
            orte_iof_hnp_open,
            orte_iof_hnp_close,
            orte_iof_hnp_query 
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        
    }
};

/**
  * component open/close/init function
  */
static int orte_iof_hnp_open(void)
{
    /* Nothing to do */
    return ORTE_SUCCESS;
}


static int orte_iof_hnp_close(void)
{
    opal_list_item_t* item;

    if (initialized) {
        OPAL_THREAD_LOCK(&mca_iof_hnp_component.lock);
        /* if the stdin event is active, delete it */
        if (NULL != mca_iof_hnp_component.stdinev) {
            OBJ_RELEASE(mca_iof_hnp_component.stdinev);
        }
        /* cleanout all registered sinks */
        while ((item = opal_list_remove_first(&mca_iof_hnp_component.sinks)) != NULL) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&mca_iof_hnp_component.sinks);
        /* cleanout all pending proc objects holding receive events */
        while ((item = opal_list_remove_first(&mca_iof_hnp_component.procs)) != NULL) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&mca_iof_hnp_component.procs);
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_IOF_HNP);
        /* release and cleanup the lock */
        OPAL_THREAD_UNLOCK(&mca_iof_hnp_component.lock);
        OBJ_DESTRUCT(&mca_iof_hnp_component.lock);
    }

    return ORTE_SUCCESS;
}

/**
 * Module query
 */

static int orte_iof_hnp_query(mca_base_module_t **module, int *priority)
{
    int rc;
    
    /* set default */
    *module = NULL;
    *priority = -1;

    /* if we are not the HNP, then don't use this module */
    if (!ORTE_PROC_IS_HNP) {
        return ORTE_ERROR;
    }
        
    /* post non-blocking recv to catch forwarded IO from
     * the orteds
     */    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_IOF_HNP,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_iof_hnp_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
        
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml.add_exception_handler(orte_iof_hnp_exception_handler))) {
        ORTE_ERROR_LOG(rc);
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_IOF_HNP);
        return rc;
    }
    
    OBJ_CONSTRUCT(&mca_iof_hnp_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_iof_hnp_component.sinks, opal_list_t);
    OBJ_CONSTRUCT(&mca_iof_hnp_component.procs, opal_list_t);
    mca_iof_hnp_component.stdinev = NULL;
    
    /* we must be selected */
    *priority = 100;
    *module = (mca_base_module_t *) &orte_iof_hnp_module;
    initialized = true;
    
    return ORTE_SUCCESS;
}



/**
 * Callback when peer is disconnected
 */

static void
orte_iof_hnp_exception_handler(const orte_process_name_t* peer, orte_rml_exception_t reason)
{
#if 0
    orte_iof_base_endpoint_t *endpoint;
    opal_output_verbose(1, orte_iof_base.iof_output, 
                "iof svc exception handler! %s\n",
                ORTE_NAME_PRINT((orte_process_name_t*)peer));

    /* If we detect an exception on the RML connection to a peer,
       delete all of its subscriptions and publications.  Note that
       exceptions can be detected during a normal RML shutdown; they
       are recoverable events (no need to abort). */
    orte_iof_hnp_sub_delete_all(peer);
    orte_iof_hnp_pub_delete_all(peer);
    opal_output_verbose(1, orte_iof_base.iof_output, "deleted all pubs and subs\n");

    /* Find any streams on any endpoints for this peer and close them */
    while (NULL != 
           (endpoint = orte_iof_base_endpoint_match(peer, ORTE_NS_CMP_ALL,
                                                    ORTE_IOF_ANY))) {
        orte_iof_base_endpoint_closed(endpoint);

        /* Delete the endpoint that we just matched */
        orte_iof_base_endpoint_delete(peer, ORTE_NS_CMP_ALL, ORTE_IOF_ANY);
    }
#endif
    opal_output_verbose(1, orte_iof_base.iof_output, "done with exception handler\n");
}
