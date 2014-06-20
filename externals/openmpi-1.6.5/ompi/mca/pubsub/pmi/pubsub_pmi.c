/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include <pmi.h>
#if WANT_CRAY_PMI2_EXT
#include <pmi2.h>
#endif

#include "ompi/info/info.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "ompi/mca/pubsub/base/base.h"
#include "pubsub_pmi.h"

/*
 * Init the module
 */
static int init(void)
{    
    return OMPI_SUCCESS;
}

/*
 * publish the port_name for the specified service_name.
 */
static int publish ( char *service_name, ompi_info_t *info, char *port_name )
{
    int rc;

#if WANT_CRAY_PMI2_EXT
    if (PMI_SUCCESS != (rc = PMI2_Nameserv_publish(service_name, NULL, port_name))) {
        ORTE_PMI_ERROR(rc, "PMI2_Nameserv_publish");
        return OMPI_ERROR;
    }
#else
    if (PMI_SUCCESS != (rc = PMI_Publish_name(service_name, port_name))) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Publish_name");
        return OMPI_ERROR;
    }
#endif
    return OMPI_SUCCESS;
}

static char* lookup ( char *service_name, ompi_info_t *info )
{
    char *port=NULL;
    int rc;

#if WANT_CRAY_PMI2_EXT
    port = (char*)malloc(1024*sizeof(char));  /* arbitrary size */
    if (PMI_SUCCESS != (rc = PMI2_Nameserv_lookup(service_name, NULL, port, 1024))) {
        ORTE_PMI_ERROR(rc, "PMI2_Nameserv_lookup");
        free(port);
        return OMPI_ERROR;
    }
#else
    if (PMI_SUCCESS != (rc = PMI_Lookup_name(service_name, port))) {
        ORTE_PMI_ERROR(rc, "PMI_Lookup_name");
        return NULL;
    }
#endif
    return port;
}

/*
 * delete the entry */
static int unpublish ( char *service_name, ompi_info_t *info )
{
    int rc;

#if WANT_CRAY_PMI2_EXT
    if (PMI_SUCCESS != (rc = PMI2_Nameserv_unpublish(service_name, NULL))) {
        ORTE_PMI_ERROR(rc, "PMI2_Nameserv_unpublish");
        return OMPI_ERROR;
    }
#else
    if (PMI_SUCCESS != (rc = PMI_Unpublish_name(service_name))) {
        ORTE_PMI_ERROR(rc, "PMI_Unpublish_name");
        return OMPI_ERROR;
    }
#endif
    return OMPI_SUCCESS;;
}


/*
 * finalize the module
 */
static int finalize(void)
{
    return OMPI_SUCCESS;
}

/*
 * instantiate the module
 */
ompi_pubsub_base_module_t ompi_pubsub_pmi_module = {
    init,
    publish,
    unpublish,
    lookup,
    finalize
};
