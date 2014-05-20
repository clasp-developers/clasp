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
 * Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_MX_H
#define MCA_PTL_MX_H

#include "ompi_config.h"
#include <sys/types.h>
#include <string.h>

/* Open MPI includes */
#include "ompi/class/ompi_free_list.h"
#include "opal/event/event.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mpool/mpool.h" 

#include <myriexpress.h>

#ifdef HAVE_MX_EXTENSIONS_H
#include <mx_extensions.h>
#endif  /* HAVE_MX_EXTENSIONS_H */

BEGIN_C_DECLS

/**
 * The mask used for receive and for the PUT protocol
 */
#define BTL_MX_RECV_MASK 0x00000000000000ffULL
#define BTL_MX_PUT_MASK  0xffffffffffffffffULL

/**
 * MX BTL component.
 */

struct mca_btl_mx_component_t {
    mca_btl_base_component_2_0_0_t          super;  /**< base BTL component */ 
    
    int32_t                                 mx_num_btls;
    int32_t                                 mx_max_btls;
    /**< number of hcas available to the MX component */

    struct mca_btl_mx_module_t**            mx_btls;
    /**< array of available BTL modules */

    int32_t                                 mx_free_list_num;
    /**< initial size of free lists */

    int32_t                                 mx_free_list_max;
    /**< maximum size of free lists */

    int32_t                                 mx_max_posted_recv;
    /**< number of posted receives on each NIC */

    int32_t                                 mx_free_list_inc;
    /**< number of elements to alloc when growing free lists */

    int32_t                                 mx_support_sharedmem;
    /**< true if we want to activate the MX support for shared memory */
    int32_t                                 mx_support_self;
    /**< true if we want to activate the MX support for self communications */
    int32_t                                 mx_bonding;
    /**< true if MX is in charge of doing the device bonding */
    int32_t                                 mx_use_unexpected;
    /**< true if Open MPI is allowed to register an unexpected handler with the MX library */

    opal_list_t mx_procs;  /**< list of mx proc structures */

    int32_t                                 mx_filter;
    int32_t                                 mx_timeout;
    int32_t                                 mx_connection_retries;

    ompi_free_list_t mx_send_eager_frags;      /**< free list of mx eager send fragments */
    ompi_free_list_t mx_send_user_frags;       /**< free list of mx user send fragments */

    opal_mutex_t     mx_lock;                  /**< lock for accessing module state */

#if MX_HAVE_MAPPER_STATE
    char* mx_if_include;                       /**< include the following NICs */
    char* mx_if_exclude;                       /**< Exclude the following NICs. These
                                                 *   values are based on the last 6
                                                 *   digits in hexadecimal of the MAC
                                                 *   address of the mapper.
                                                 */
#endif  /* MX_HAVE_MAPPER_STATE */
}; 
typedef struct mca_btl_mx_component_t mca_btl_mx_component_t;

OMPI_MODULE_DECLSPEC extern mca_btl_mx_component_t mca_btl_mx_component;

/**
 * BTL Module Interface.
 * Each BTL correspond to a high level vision of a network interface. The 
 * current version of the MX BTL is not able to handle stripping of the
 * messages by itself. Therefore, it rely on the PML layer for that.
 */
struct mca_btl_mx_module_t {
    mca_btl_base_module_t   super;                   /**< base BTL interface */
    mx_endpoint_t           mx_endpoint;             /**< local MX endpoint */
    mx_endpoint_addr_t      mx_endpoint_addr;        /**< local MX endpoint address */
    uint32_t                mx_unique_network_id;    /**< unique identifier for this BTL,
                                                      *   based on the MAC address of the
                                                      *   mapper used to route messages.
                                                      */
    opal_list_t             mx_peers;                /**<  list of peers */

    int32_t                 mx_posted_request;       /**< number of posted MX request */
    opal_mutex_t            mx_lock;                 /**< lock for accessing module state */
}; 
typedef struct mca_btl_mx_module_t mca_btl_mx_module_t;
extern mca_btl_mx_module_t mca_btl_mx_module;

/**
 * MX component initialization.
 * 
 * @param num_btl_modules (OUT)           Number of BTLs returned in BTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether BTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether BTL uses threads (TRUE)
 */
extern mca_btl_base_module_t** mca_btl_mx_component_init(
    int *num_btl_modules, 
    bool allow_multi_user_threads,
    bool have_hidden_threads
);


/**
 * MX component progress.
 */
extern int mca_btl_mx_component_progress(void);



/**
 * Cleanup any resources held by the BTL.
 * 
 * @param btl  BTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_btl_mx_finalize(
    struct mca_btl_base_module_t* btl
);


/**
 * PML->BTL notification of change in the process list.
 * 
 * @param btl (IN)
 * @param nprocs (IN)     Number of processes
 * @param procs (IN)      Set of processes
 * @param peers (OUT)     Set of (optional) peer addressing info.
 * @param peers (IN/OUT)  Set of processes that are reachable via this BTL.
 * @return     OMPI_SUCCESS or error status on failure.
 * 
 */

extern int mca_btl_mx_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers,
    opal_bitmap_t* reachable
);

/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)     BTL instance
 * @param nproc (IN)   Number of processes.
 * @param procs (IN)   Set of processes.
 * @param peers (IN)   Set of peer data structures.
 * @return             Status indicating if cleanup was successful
 *
 */

extern int mca_btl_mx_del_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers
);


/**
 * Initiate an asynchronous send.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */

extern int mca_btl_mx_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* descriptor, 
    mca_btl_base_tag_t tag
);


/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param btl (IN)     BTL module
 * @return             Status indicating if registration was successful
 *
 */

extern int mca_btl_mx_register(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_tag_t tag, 
    mca_btl_base_module_recv_cb_fn_t cbfunc, 
    void* cbdata); 
    
/**
 * Allocate a descriptor with a segment of the requested size.
 * Note that the BTL layer may choose to return a smaller size
 * if it cannot support the request.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

mca_btl_base_descriptor_t* mca_btl_mx_alloc( struct mca_btl_base_module_t* btl,
                                             struct mca_btl_base_endpoint_t* endpoint,
                                             uint8_t order,
                                             size_t size,
                                             uint32_t flags);


/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param descriptor (IN)  Allocated descriptor.
 */

int mca_btl_mx_free( struct mca_btl_base_module_t* btl, 
                     mca_btl_base_descriptor_t* des );
    

/**
 * Prepare a descriptor for send/rdma using the supplied
 * convertor. If the convertor references data that is contigous,
 * the descriptor may simply point to the user buffer. Otherwise,
 * this routine is responsible for allocating buffer space and
 * packing if required.
 *
 * @param btl (IN)          BTL module
 * @param endpoint (IN)     BTL peer addressing
 * @param convertor (IN)    Data type convertor
 * @param reserve (IN)      Additional bytes requested by upper layer to precede user data
 * @param size (IN/OUT)     Number of bytes to prepare (IN), number of bytes actually prepared (OUT) 
 */
mca_btl_base_descriptor_t*
mca_btl_mx_prepare_src( struct mca_btl_base_module_t* btl,
                        struct mca_btl_base_endpoint_t* peer,
                        struct mca_mpool_base_registration_t*,
                        struct opal_convertor_t* convertor,
                        uint8_t order,
                        size_t reserve,
                        size_t* size,
                        uint32_t flags);

mca_btl_base_descriptor_t*
mca_btl_mx_prepare_dst( struct mca_btl_base_module_t* btl, 
                        struct mca_btl_base_endpoint_t* peer,
                        struct mca_mpool_base_registration_t*,
                        struct opal_convertor_t* convertor,
                        uint8_t order,
                        size_t reserve,
                        size_t* size,
                        uint32_t flags);

/**
 * Fault Tolerance Event Notification Function
 * @param state Checkpoint Stae
 * @return OMPI_SUCCESS or failure status
 */
int mca_btl_mx_ft_event(int state);

END_C_DECLS

#endif
