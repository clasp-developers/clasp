/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
#ifndef MCA_BTL_SCTP_H
#define MCA_BTL_SCTP_H

#include "ompi_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

/* Open MPI includes */
#include "opal/event/event.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/mca/btl/btl.h"
#include "opal/class/opal_hash_table.h"

/* For the assocID -> proc mapping, do we use opal_hash_table (0)
 * or our homemade array (1)?
 */
#define MCA_BTL_SCTP_DONT_USE_HASH 1

#if 1
/* if you do not want to see these debug messages */
#define SCTP_BTL_ERROR(args)
#else
#define SCTP_BTL_ERROR(args) BTL_ERROR(args)
#endif

#define MCA_BTL_SCTP_STATISTICS 0
BEGIN_C_DECLS


/**
 * SCTP BTL component.
 */

struct mca_btl_sctp_component_t {
    mca_btl_base_component_2_0_0_t super;   /**< base BTL component */ 
    uint32_t sctp_num_btls;                  /**< number of hcas available to the SCTP component */
    struct mca_btl_sctp_module_t **sctp_btls; /**< array of available BTL modules */
    struct mca_btl_sctp_proc_t* sctp_local;   /**< local proc struct */
    int sctp_free_list_num;                  /**< initial size of free lists */
    int sctp_free_list_max;                  /**< maximum size of free lists */
    int sctp_free_list_inc;                  /**< number of elements to alloc when growing free lists */
    int sctp_endpoint_cache;                 /**< amount of cache on each endpoint */
    opal_hash_table_t sctp_procs;            /**< hash table of sctp proc structures */
#if MCA_BTL_SCTP_DONT_USE_HASH
#else    
    opal_hash_table_t sctp_assocID_hash;     /**< hash table of procs keyed on assocIDs */
#endif
    opal_list_t sctp_events;                 /**< list of pending sctp events */
    opal_mutex_t sctp_lock;                  /**< lock for accessing module state */
    opal_event_t sctp_recv_event;            /**< recv event for listen socket */
    int sctp_listen_sd;                      /**< listen socket for incoming connection requests */
    unsigned short sctp_listen_port;         /**< listen port */
    char*  sctp_if_include;                  /**< comma seperated list of interface to include */
    char*  sctp_if_exclude;                  /**< comma seperated list of interface to exclude */
    int    sctp_sndbuf;                      /**< socket sndbuf size */
    int    sctp_rcvbuf;                      /**< socket rcvbuf size */
    int    sctp_use_nodelay;                 /**< SCTP_NODELAY value */
    int sctp_if_11;                          /**< Are we going 1 to 1?  1 to many is default. */

    /* free list of fragment descriptors */
    ompi_free_list_t sctp_frag_eager;
    ompi_free_list_t sctp_frag_max;
    ompi_free_list_t sctp_frag_user;
}; 
typedef struct mca_btl_sctp_component_t mca_btl_sctp_component_t;

OMPI_MODULE_DECLSPEC extern mca_btl_sctp_component_t mca_btl_sctp_component;

/**
 * BTL Module Interface
 */
struct mca_btl_sctp_module_t {
    mca_btl_base_module_t  super;  /**< base BTL interface */
    int                sctp_ifindex; /**< PTL interface index */
    struct sockaddr_in sctp_ifaddr;  /**< PTL interface address */
    struct sockaddr_in sctp_ifmask;  /**< PTL interface netmask */
    opal_list_t        sctp_endpoints;
#if MCA_BTL_SCTP_STATISTICS
    size_t sctp_bytes_sent;
    size_t sctp_bytes_recv;
    size_t sctp_send_handler;
#endif
}; 
typedef struct mca_btl_sctp_module_t mca_btl_sctp_module_t;
extern mca_btl_sctp_module_t mca_btl_sctp_module;

#if defined(__WINDOWS__)
#define CLOSE_THE_SOCKET(socket)   closesocket(socket)
#else
#define CLOSE_THE_SOCKET(socket)   close(socket)
#endif  /* defined(__WINDOWS__) */

/**
 * SCTP component initialization.
 * 
 * @param num_btl_modules (OUT)           Number of BTLs returned in BTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether BTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether BTL uses threads (TRUE)
 */
extern mca_btl_base_module_t** mca_btl_sctp_component_init(
    int *num_btl_modules, 
    bool allow_multi_user_threads,
    bool have_hidden_threads
);



/**
 * SCTP component control.
 */
int mca_btl_sctp_component_control(
    int param, 
    void* value, 
    size_t size
);


/**
 * SCTP component progress.
 */
extern int mca_btl_sctp_component_progress(void);



/**
 * Cleanup any resources held by the BTL.
 * 
 * @param btl  BTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_btl_sctp_finalize(
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

extern int mca_btl_sctp_add_procs(
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

extern int mca_btl_sctp_del_procs(
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

extern int mca_btl_sctp_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* descriptor,
    mca_btl_base_tag_t tag
);


/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

extern int mca_btl_sctp_put(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* decriptor
);


/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

extern int mca_btl_sctp_get(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* decriptor
);

/**
 * Allocate a descriptor with a segment of the requested size.
 * Note that the BTL layer may choose to return a smaller size
 * if it cannot support the request.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

extern mca_btl_base_descriptor_t* mca_btl_sctp_alloc(
    struct mca_btl_base_module_t* btl,
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

extern int mca_btl_sctp_free(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_descriptor_t* des); 
    

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

mca_btl_base_descriptor_t* mca_btl_sctp_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* peer,
    struct mca_mpool_base_registration_t*,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags
);

extern mca_btl_base_descriptor_t* mca_btl_sctp_prepare_dst( 
    struct mca_btl_base_module_t* btl, 
    struct mca_btl_base_endpoint_t* peer,
    struct mca_mpool_base_registration_t*,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags); 


END_C_DECLS
#endif
