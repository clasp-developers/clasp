/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
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
/**
 *  @file 
 */

#ifndef MCA_PML_CRCPW_H
#define MCA_PML_CRCPW_H

#include "ompi_config.h"

#include "ompi/class/ompi_free_list.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"

BEGIN_C_DECLS

    /**
     * CRCPW PML module
     */
    struct mca_pml_crcpw_component_t {
        mca_pml_base_component_t super; 
        int verbose;
        int priority;
        int output_handle;
        bool pml_crcp_wrapped;
    };
    typedef struct mca_pml_crcpw_component_t mca_pml_crcpw_component_t;
    OMPI_MODULE_DECLSPEC extern mca_pml_crcpw_component_t mca_pml_crcpw_component;

    struct mca_pml_crcpw_module_t {
        mca_pml_base_module_t super;
        mca_pml_base_component_t wrapped_pml_component;
        mca_pml_base_module_t    wrapped_pml_module;
    };
    typedef struct mca_pml_crcpw_module_t mca_pml_crcpw_module_t;
    extern mca_pml_crcpw_module_t mca_pml_crcpw_module;

    /* Free list of PML states */
    OMPI_MODULE_DECLSPEC extern ompi_free_list_t pml_state_list;
    OMPI_MODULE_DECLSPEC extern bool pml_crcpw_is_finalized;

    /*
     * PML module functions.
     */
    int mca_pml_crcpw_component_open(void);
    int mca_pml_crcpw_component_close(void);

    mca_pml_base_module_t* mca_pml_crcpw_component_init(int *priority, 
                                                        bool enable_progress_threads,
                                                        bool enable_mpi_threads
                                                        );
    
    int mca_pml_crcpw_component_finalize(void);

    /*
     * PML interface functions.
     */
    int mca_pml_crcpw_enable( bool enable );

    int mca_pml_crcpw_add_comm( struct ompi_communicator_t* comm );
    int mca_pml_crcpw_del_comm( struct ompi_communicator_t* comm );

    int mca_pml_crcpw_add_procs( struct ompi_proc_t **procs, size_t nprocs );
    int mca_pml_crcpw_del_procs( struct ompi_proc_t **procs, size_t nprocs );

    int mca_pml_crcpw_progress(void);
    
    int mca_pml_crcpw_iprobe(int dst, int tag, struct ompi_communicator_t* comm, int *matched, ompi_status_public_t* status );

    int mca_pml_crcpw_probe( int dst, int tag, struct ompi_communicator_t* comm, ompi_status_public_t* status );
    
    int mca_pml_crcpw_isend_init( void *buf, size_t count, ompi_datatype_t *datatype, int dst, int tag, 
                                  mca_pml_base_send_mode_t mode, struct ompi_communicator_t* comm, struct ompi_request_t **request );
    
    int mca_pml_crcpw_isend( void *buf, size_t count, ompi_datatype_t *datatype, int dst, int tag,
                             mca_pml_base_send_mode_t mode, struct ompi_communicator_t* comm, struct ompi_request_t **request );
    
    int mca_pml_crcpw_send(  void *buf, size_t count, ompi_datatype_t *datatype, int dst, int tag,
                             mca_pml_base_send_mode_t mode, struct ompi_communicator_t* comm );
    
    int mca_pml_crcpw_irecv_init( void *buf, size_t count, ompi_datatype_t *datatype, int src, int tag,
                                  struct ompi_communicator_t* comm,  struct ompi_request_t **request);
    
    int mca_pml_crcpw_irecv( void *buf, size_t count, ompi_datatype_t *datatype, int src, int tag,
                             struct ompi_communicator_t* comm, struct ompi_request_t **request );
    
    int mca_pml_crcpw_recv(  void *buf, size_t count, ompi_datatype_t *datatype, int src, int tag,
                             struct ompi_communicator_t* comm,  ompi_status_public_t* status);
    
    int mca_pml_crcpw_dump( struct ompi_communicator_t* comm, int verbose );
    
    int mca_pml_crcpw_start( size_t count, ompi_request_t** requests );
    
    int mca_pml_crcpw_ft_event(int state);
    
END_C_DECLS

#endif /* MCA_PML_CRCPW_H */
