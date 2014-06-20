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

#ifndef OMPI_MCA_COMMON_PORTALS_H
#define OMPI_MCA_COMMON_PORTALS_H

#if OMPI_PORTALS_UTCP

#include <portals3.h>
#include <stdio.h>
#include <p3nal_utcp.h>
#include <p3rt/p3rt.h>
#include <p3api/debug.h>

#define OMPI_BTL_PORTALS_SEND_TABLE_ID 0
#define OMPI_BTL_PORTALS_RDMA_TABLE_ID 1

#define OMPI_MTL_PORTALS_SEND_TABLE_ID 2
#define OMPI_MTL_PORTALS_READ_TABLE_ID 3
#define OMPI_MTL_PORTALS_ACK_TABLE_ID  4

#elif (OMPI_PORTALS_CRAYXT3 || OMPI_PORTALS_CRAYXT3_MODEX)
#include <portals/portals3.h>
#define PTL_EQ_HANDLER_NONE NULL
/* Cray's definition, differs from the spec */
#define PTL_NO_ACK_REQ PTL_NOACK_REQ

#define OMPI_BTL_PORTALS_SEND_TABLE_ID 30
#define OMPI_BTL_PORTALS_RDMA_TABLE_ID 31

#define OMPI_MTL_PORTALS_SEND_TABLE_ID 32
#define OMPI_MTL_PORTALS_READ_TABLE_ID 33
#define OMPI_MTL_PORTALS_ACK_TABLE_ID  34

#else

#error "Unknown Portals library configuration"

#endif

#include "ompi/proc/proc.h"


/**
 * Simple identifier for identifying node/process
 *
 * Get a string representing a simple way to identify the node/rank of
 * the current process.  Currently returns the rank in the job on the
 * XT-3 or the hostname/pid on the reference implementation.
 *
 * \note Caller is responsible for calling free() on the returned
 * string.
 */
char* ompi_common_portals_nodeid(void);


/**
 * Register MCA parameters for Portals code
 *
 * Register MCA parameters for Portals common code.  This should be
 * called during component open so that parameters are available to
 * omp_info and the like.  This call will not intiailize the Portals
 * interface or cause any communication.
 *
 * @retval OMPI_SUCCESS
 */
int ompi_common_portals_register_mca(void);


/**
 * Initialize compatibility code
 *
 * Initialize Portals compatibility code.  A best effort is made to
 * initialize Portals (with PtlInit() and PtlNIInit(), although this
 * may not be possible if use of the modex is required to setup the
 * network (as is the case with the utcp reference implementation).
 *
 * @param ni_handle (OUT) network interface handle
 * @param bool (OUT) true if using accelerated Portals, false otherwise
 *
 * @retval OMPI_SUCCESS Portals successfully initialized
 * @retval OMPI_ERR_NOT_AVAILABLE Portals could not be initialized
 */
int ompi_common_portals_initialize(ptl_handle_ni_t *ni_handle, bool *accel);

/**
 * Initialize network interface
 *
 * Initialize the portals network interface.  The initializization may
 * actually have happened in ompi_common_portals_initialize(), but
 * this will return the network interface handle.  This function may
 * require some information shared by the modex, so should only be
 * called after the modex data is available.
 *
 * @param ni_handle (OUT) network interface handle
 * @param bool (OUT) true if using accelerated Portals, false otherwise
 *
 * @retval OMPI_SUCCESS Portals network interface successfully initialized
 * @retval OMPI_ERROR Something bad happened
 */
int ompi_common_portals_ni_initialize(ptl_handle_ni_t *ni_handle, bool *accel);


/**
 * Get process_id_t array for proc list
 *
 * Get ptl_process_id_t array for proc list
 *
 * @param nprocs (IN) Number of procs in proc list
 * @param procs (IN) List of OMPI procs
 * @param portals_procs (OUT) array of ptl_process_id_t
 *                            structures associated with OMPI procs
 *
 * @retval OMPI_SUCCESS All went well
 * @retval OMPI_ERROR   All went poorly
 */
int ompi_common_portals_get_procs(size_t nprocs,
                                  struct ompi_proc_t **procs,
                                  ptl_process_id_t *portals_procs);


/**
 * Shut down Portals network interface
 *
 * Shut down Portals network devince , including calling PtlNIFini()
 * if appropriate.  The common code will reference count so that it is
 * safe for each component that calls
 * ompi_component_portals_ni_initialize() to call
 * ompi_common_portals_ni_finalize()
 */
int ompi_common_portals_ni_finalize(void);


/**
 * Shut down Portals
 *
 * Shut down Portals, including calling PtlFini() if appropriate.  The
 * common code will reference count so that it is safe for each
 * component that calls ompi_component_portals_initialize() to call
 * ompi_common_portals_finalize()
 */
int ompi_common_portals_finalize(void);


int ompi_common_portals_error_ptl_to_ompi(int ptl_error);


#endif /* OMPI_MCA_COMMON_PORTALS_H */
