/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include "common_portals.h"
#include "ompi/constants.h"
#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "ompi/runtime/ompi_module_exchange.h"


static mca_base_component_t portals_component = {
    MCA_BASE_VERSION_2_0_0,
    "common",
    MCA_BASE_VERSION_2_0_0,
    "portals",
    MCA_BASE_VERSION_2_0_0,
    NULL,
    NULL
};

char *
ompi_common_portals_nodeid(void)
{
    char *ret;
    asprintf(&ret, "CNL:");
    return ret;
}


int
ompi_common_portals_register_mca(void)
{
    return OMPI_SUCCESS;
}


int
ompi_common_portals_initialize(ptl_handle_ni_t *ni_handle, bool *accel)
{
    int ret, max_interfaces;
    ptl_process_id_t ptl_process_id;
    ptl_interface_t ni_iface = PTL_IFACE_DEFAULT;
    *accel = false;
    
    /*
     * If we use the YOD launcher we can use the default interface
     * otherwise we need to use the SeaStar Bridged interface (for CNL/APRUN)
     */
    ni_iface = IFACE_FROM_BRIDGE_AND_NALID(PTL_BRIDGE_UK,PTL_IFACE_SS);

    /*
     * Initialize Portals interface
     */
    ret = PtlInit(&max_interfaces);
    if (PTL_OK != ret) {
        opal_output(0, "PtlInit failed, returning %d\n", ret);
        return OMPI_ERR_NOT_AVAILABLE;
    }
    
    /*
     * Initialize a network device
     */
    ret = PtlNIInit(ni_iface,          /* interface to initialize */
                    PTL_PID_ANY,       /* let library assign our pid */
                    NULL,              /* no desired limits */
                    NULL,              /* actual limits */
                    ni_handle          /* our interface handle */
                    );
    if (PTL_OK != ret && PTL_IFACE_DUP != ret) {
        opal_output(0, "PtlNIInit failed, returning %d (%s : %d)\n",
                    ret, __FILE__, __LINE__);
        return OMPI_ERROR;
    }
    
    ret = PtlGetId(*ni_handle ,&ptl_process_id);
    if(PTL_OK != ret) { 
        opal_output(0, "PtlGetId failed, returning %d\n", ret);
        return OMPI_ERROR;
    }
   
    /* publish my nid/pid info */
    ret = ompi_modex_send(&portals_component,
                          &ptl_process_id, sizeof(ptl_process_id_t));
    
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    return OMPI_SUCCESS;
    
}


int
ompi_common_portals_ni_initialize(ptl_handle_ni_t *ni_handle, bool *accel)
{
    return OMPI_SUCCESS;
}


int
ompi_common_portals_get_procs(size_t nprocs,
                              struct ompi_proc_t **procs,
                              ptl_process_id_t *portals_procs)
{
    size_t i, size;
    int ret;
    ptl_process_id_t *ptl_process_id;
    
    for (i = 0 ; i < nprocs ; ++i) {
        ret = ompi_modex_recv(&portals_component,
                              procs[i], (void**) &ptl_process_id, &size);
        if (OMPI_SUCCESS != ret) {
            opal_output(0, "ompi_modex_recv failed: %d", ret);
            return ret;
        } else if (sizeof(ptl_process_id_t) != size) {
            opal_output(0, "ompi_modex_recv returned size %d, expected %d", 
                        (int) size, (int) sizeof(ptl_process_id_t));
            return OMPI_ERROR;
        }
        
        portals_procs[i] = *ptl_process_id;
    }
    
    return OMPI_SUCCESS;
}


int
ompi_common_portals_ni_finalize(void)
{
    return OMPI_SUCCESS;
}


int
ompi_common_portals_finalize(void)
{
    return OMPI_SUCCESS;
}
