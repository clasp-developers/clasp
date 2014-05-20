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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#if defined(HAVE_CNOS_MPI_OS_H)
#  include "cnos_mpi_os.h"
#elif defined(HAVE_CATAMOUNT_CNOS_MPI_OS_H)
#  include "catamount/cnos_mpi_os.h"
#endif

#include "ompi/constants.h"
#include "ompi/proc/proc.h"
#include "opal/mca/base/base.h"
#include "opal/util/output.h"

static bool use_accelerated;

char *
ompi_common_portals_nodeid(void)
{
    char *ret;
    asprintf(&ret, "%5d", cnos_get_rank());
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

    /*
     * Initialize Portals interface
     */
    ret = PtlInit(&max_interfaces);
    if (PTL_OK != ret) {
        opal_output(0, "%5d: PtlInit failed, returning %d\n", 
                    cnos_get_rank(), ret);
        return OMPI_ERR_NOT_AVAILABLE;
    }

    return OMPI_SUCCESS;
}


int
ompi_common_portals_ni_initialize(ptl_handle_ni_t *ni_handle, bool *accel)
{
    ptl_interface_t ni_iface = PTL_IFACE_DEFAULT;
    int max_interfaces;
    int launcher;
    int ret;
    int tmp = 0;

#if defined(CRAY_ACCEL)
    mca_base_param_reg_int_name("mca",
                                "use_accelerated_portals",
                                "Use Accelerated Portals",
                                false,
                                false,
                                0,
                                &tmp);
#endif

    *accel = use_accelerated = (tmp == 0) ? false : true;

    launcher = cnos_launcher();

    /*
     * If we use the YOD launcher we can use the default interface
     * otherwise we need to use the SeaStar Bridged interface (for CNL/APRUN)
     */
    if( launcher != CNOS_LAUNCHER_YOD ) {
        ni_iface = IFACE_FROM_BRIDGE_AND_NALID(PTL_BRIDGE_UK,PTL_IFACE_SS);
    }
#if defined(CRAY_ACCEL)
    else if (use_accelerated == true) {
        ni_iface = CRAY_ACCEL;
    }
#endif

    /*
     * Initialize Portals interface
     */
    ret = PtlInit(&max_interfaces);
    if (PTL_OK != ret) {
        opal_output(0, "%5d: PtlInit failed, returning %d\n", 
                    cnos_get_rank(), ret);
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
        opal_output(0, "%5d: PtlNIInit failed, returning %d (%s : %d)\n", 
                    cnos_get_rank(), ret, __FILE__, __LINE__);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int
ompi_common_portals_get_procs(size_t nprocs,
                              struct ompi_proc_t **procs,
                              ptl_process_id_t *portals_procs)
{
    int nptl_procs = 0;
    cnos_nidpid_map_t *map;
    int pid_space_offset = 0;
    int i;

    /*
     * assumption that the vpid of the process name is the rank in the
     * nidpid map.  THis will not be true if someone changes the sds
     * component...
     */
    nptl_procs = cnos_get_nidpid_map(&map);
    if (nptl_procs <= 0) {
        opal_output(0, "%5d: cnos_get_nidpid_map() returned %d", 
                    cnos_get_rank(), nptl_procs);
        return OMPI_ERR_FATAL;
    }

#if defined(CRAY_ACCEL)
    pid_space_offset = (use_accelerated == true) ? ACCEL_PTLS_PID_SPACE_OFFSET : 0;
#endif

    for (i = 0 ; i < nprocs ; ++i) {
        size_t idx = (size_t) procs[i]->proc_name.vpid;
        if (idx >= nptl_procs) return OMPI_ERR_NOT_FOUND;

	portals_procs[i].nid = map[idx].nid;
	portals_procs[i].pid = map[idx].pid + pid_space_offset;
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
