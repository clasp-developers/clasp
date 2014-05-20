/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_SELF_H
#define MCA_BTL_SELF_H

#include "ompi_config.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /* HAVE_STDLIB_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#include "opal/event/event.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h" 

BEGIN_C_DECLS

/**
 * Shared Memory (SELF) BTL module.
 */
struct mca_btl_self_component_t {
    mca_btl_base_component_2_0_0_t super;  /**< base BTL component */
    int free_list_num;                     /**< initial size of free lists */
    int free_list_max;                     /**< maximum size of free lists */
    int free_list_inc;                     /**< number of elements to alloc when growing free lists */
    opal_mutex_t self_lock;
    ompi_free_list_t self_frags_eager;     /**< free list of self first */
    ompi_free_list_t self_frags_send;      /**< free list of self second */
    ompi_free_list_t self_frags_rdma;      /**< free list of self second */
};
typedef struct mca_btl_self_component_t mca_btl_self_component_t;
OMPI_MODULE_DECLSPEC extern mca_btl_self_component_t mca_btl_self_component;

/**
 * Register shared memory module parameters with the MCA framework
 */
int mca_btl_self_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
int mca_btl_self_component_close(void);

/**
 * SELF module initialization.
 * 
 * @param num_btls (OUT)                  Number of BTLs returned in BTL array.
 * @param enable_progress_threads (IN)    Flag indicating whether BTL is allowed to have progress threads
 * @param enable_mpi_threads (IN)         Flag indicating whether BTL must support multilple simultaneous invocations from different threads
 *
 */
mca_btl_base_module_t** mca_btl_self_component_init(
    int *num_btls, 
    bool enable_progress_threads,
    bool enable_mpi_threads
);

extern mca_btl_base_module_t mca_btl_self;


/**
 * Cleanup any resources held by the BTL.
 * 
 * @param btl  BTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

int mca_btl_self_finalize(
    struct mca_btl_base_module_t* btl
);


/**
 * PML->BTL notification of change in the process list.
 * PML->BTL Notification that a receive fragment has been matched.
 * Called for message that is send from process with the virtual
 * address of the shared memory segment being different than that of
 * the receiver.
 * 
 * @param btl (IN)
 * @param proc (IN)  
 * @param peer (OUT)
 * @return     OMPI_SUCCESS or error status on failure.
 * 
 */

int mca_btl_self_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers,
    struct opal_bitmap_t* reachability
);


/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)     BTL instance
 * @param proc (IN)    Peer process
 * @param peer (IN)    Peer addressing information.
 * @return             Status indicating if cleanup was successful
 *
 */
int mca_btl_self_del_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t **peers
);


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
mca_btl_base_descriptor_t* mca_btl_self_alloc(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    uint8_t order,
    size_t size,
    uint32_t flags
);

/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param segment (IN)  Allocated segment.
 */
int mca_btl_self_free(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_descriptor_t* segment
);
                                                                                                                   
/**
 * Pack data
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
struct mca_btl_base_descriptor_t* mca_btl_self_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags
);
                                                                                                            
/**
 * Prepare data for RDMA
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
struct mca_btl_base_descriptor_t* mca_btl_self_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags
);
                                                                                                            
/**
 * Initiate a send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
int mca_btl_self_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    mca_btl_base_tag_t tag
);

/**
 * Initiate a put to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
                                                                                                            
int mca_btl_self_rdma(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor
);

/**
 * Fault Tolerance Event Notification Function
 * @param state Checkpoint Stae
 * @return OMPI_SUCCESS or failure status
 */
int mca_btl_self_ft_event(int state);

END_C_DECLS

#endif

