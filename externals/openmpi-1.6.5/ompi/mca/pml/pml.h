/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 * 
 * P2P Management Layer (PML)
 *
 * An MCA component type that provides the P2P interface functionality
 * required by the MPI layer. The PML is a relatively thin layer that
 * primarily provides for the fragmentation and scheduling of messages
 * over multiple transports (instances of the Byte Transfer Layer
 * (BTL) MCA component type) as depicted below:
 *
 *   ------------------------------------
 *   |                MPI               |
 *   ------------------------------------
 *   |                PML               |
 *   ------------------------------------
 *   | BTL (TCP) | BTL (SM) | BTL (...) |
 *   ------------------------------------
 *
 * A single PML component is selected by the MCA framework during
 * library initialization. Initially, all available PMLs are loaded
 * (potentially as shared libraries) and their component open and init
 * functions called.  The MCA framework selects the component
 * returning the highest priority and closes/unloads any other PML
 * components that may have been opened.
 *
 * After all of the MCA components are initialized, the MPI/RTE will
 * make downcalls into the PML to provide the initial list of
 * processes (ompi_proc_t instances), and notification of changes
 * (add/delete).
 * 
 * The PML module must select the set of BTL components that are to be
 * used to reach a given destination. These should be cached on a PML
 * specific data structure that is hung off the ompi_proc_t.
 *
 * The PML should then apply a scheduling algorithm (round-robin,
 * weighted distribution, etc), to schedule the delivery of messages
 * over the available BTLs.
 *
 */
                                                                                         
#ifndef MCA_PML_H
#define MCA_PML_H

#include "ompi_config.h"
#include "opal/mca/mca.h"
#include "mpi.h" /* needed for MPI_ANY_TAG */

BEGIN_C_DECLS

/*
 * PML component types
 */

typedef uint64_t mca_pml_sequence_t;

/** 
 * Base PML endpoint structure
 *
 * Base PML structure for caching endpoint information on a proc.  A
 * pointer to an mca_pml_endpoint_t is maintained on each ompi_proc_t,
 * in the proc_pml field, to provide per-process cache information.
 * The data is opaque to the active PML -- no other subsystem will
 * attempt to access the information in the cache.
 *
 * The PML is responsible for allocation and deallocation of the
 * endpoint data during pml_add_procs and pml_del_procs.
 */
struct mca_pml_endpoint_t;
struct ompi_proc_t;

typedef enum {
    MCA_PML_BASE_SEND_SYNCHRONOUS,
    MCA_PML_BASE_SEND_COMPLETE,
    MCA_PML_BASE_SEND_BUFFERED,
    MCA_PML_BASE_SEND_READY,
    MCA_PML_BASE_SEND_STANDARD,
    MCA_PML_BASE_SEND_SIZE
} mca_pml_base_send_mode_t;


#define OMPI_ANY_TAG    MPI_ANY_TAG
#define OMPI_ANY_SOURCE MPI_ANY_SOURCE
#define OMPI_PROC_NULL  MPI_PROC_NULL

/**
 * MCA->PML Called by MCA framework to initialize the component.
 * 
 * @param priority (OUT) Relative priority or ranking used by MCA to
 * selected a component.
 *
 * @param enable_progress_threads (IN) Whether this component is
 * allowed to run a hidden/progress thread or not.
 *
 * @param enable_mpi_threads (IN) Whether support for multiple MPI
 * threads is enabled or not (i.e., MPI_THREAD_MULTIPLE), which
 * indicates whether multiple threads may invoke this component
 * simultaneously or not.
 */
typedef struct mca_pml_base_module_1_0_0_t * (*mca_pml_base_component_init_fn_t)(
    int *priority, 
    bool enable_progress_threads,
    bool enable_mpi_threads);

typedef int (*mca_pml_base_component_finalize_fn_t)(void);

/**
 * PML component version and interface functions.
 */

struct mca_pml_base_component_2_0_0_t {
   mca_base_component_t pmlm_version;
   mca_base_component_data_t pmlm_data;
   mca_pml_base_component_init_fn_t pmlm_init;
   mca_pml_base_component_finalize_fn_t pmlm_finalize;
};
typedef struct mca_pml_base_component_2_0_0_t mca_pml_base_component_2_0_0_t;
typedef mca_pml_base_component_2_0_0_t mca_pml_base_component_t;


/**
 * MCA management functions.
 */


/**
 * Downcall from MPI/RTE layer when new processes are created.
 *
 * @param  procs   Array of new processes
 * @param  nprocs  Size of process array
 * @return         OMPI_SUCCESS or failure status.
 *
 * Provides a notification to the PML that new processes have been
 * created, and provides the PML the opportunity to cache data
 * (e.g. list of BTLs to use) on the ompi_proc_t data structure.
 */
typedef int (*mca_pml_base_module_add_procs_fn_t)(struct ompi_proc_t **procs, size_t nprocs);


/**
 * Downcall from MPI/RTE layer when processes are terminated.
 *
 * @param  procs   Array of processes
 * @param  nprocs  Size of process array
 * @return         OMPI_SUCCESS or failure status.
 *
 * Provides a notification to the PML that processes have 
 * gone away, and provides the PML the opportunity to cleanup
 * any data cached on the ompi_proc_t data structure.
 */
typedef int (*mca_pml_base_module_del_procs_fn_t)(struct ompi_proc_t **procs, size_t nprocs);

/**
 * Downcall from MCA layer to enable the PML/BTLs.
 *
 * @param   enable  Enable/Disable PML forwarding
 * @return          OMPI_SUCCESS or failure status.
*/
typedef int (*mca_pml_base_module_enable_fn_t)(
    bool enable
);


/**
 * For non-threaded case, provides MCA the opportunity to
 * progress outstanding requests on all btls.
 *
 *  * @return        Count of "completions", a metric of 
 *                   how many items where completed in the call 
 *                   to progress.
*/
typedef int (*mca_pml_base_module_progress_fn_t)(void);

/**
 * MPI Interface Functions
 */


/**
 * Downcall from MPI layer when a new communicator is created.
 *
 * @param comm  Communicator
 * @return      OMPI_SUCCESS or failure status.
 *
 * Provides the PML the opportunity to initialize/cache a data structure
 * on the communicator.
 */
typedef int (*mca_pml_base_module_add_comm_fn_t)(struct ompi_communicator_t* comm);


/**
 * Downcall from MPI layer when a communicator is destroyed.
 *
 * @param comm  Communicator
 * @return      OMPI_SUCCESS or failure status.
 *
 * Provides the PML the opportunity to cleanup any datastructures
 * associated with the communicator.
 */
typedef int (*mca_pml_base_module_del_comm_fn_t)(struct ompi_communicator_t* comm);

/**
 *  Initialize a persistent receive request. 
 *
 *  @param buf (IN)         User buffer.
 *  @param count (IN)       Number of elements of the specified datatype.
 *  @param datatype (IN)    User defined datatype.
 *  @param src (IN)         Source rank w/in communicator.
 *  @param tag (IN)         User defined tag.
 *  @param comm (IN)        Communicator.
 *  @param request (OUT)    Request handle.
 *  @return                 OMPI_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_module_irecv_init_fn_t)(
    void *buf,                           
    size_t count,                         
    struct ompi_datatype_t *datatype,              
    int src,
    int tag,                                
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request           
);

/**
 *  Post a receive request. 
 *
 *  @param buf (IN)         User buffer.
 *  @param count (IN)       Number of elements of the specified datatype.
 *  @param datatype (IN)    User defined datatype.
 *  @param src (IN)         Source rank w/in communicator.
 *  @param tag (IN)         User defined tag.
 *  @param comm (IN)        Communicator.
 *  @param request (OUT)    Request handle.
 *  @return                 OMPI_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_module_irecv_fn_t)(
    void *buf,
    size_t count,
    struct ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

/**
 *  Post a receive and wait for completion. 
 *
 *  @param buf (IN)         User buffer
 *  @param count (IN)       Number of elements of the specified datatype
 *  @param datatype (IN)    User defined datatype
 *  @param src (IN)         Source rank w/in communicator
 *  @param tag (IN)         User defined tag
 *  @param comm (IN)        Communicator
 *  @param status (OUT)     Completion status
 *  @return                 OMPI_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_module_recv_fn_t)(
    void *buf,
    size_t count,
    struct ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    ompi_status_public_t* status
);

/**
 *  Initialize a persistent send request. 
 *
 *  @param buf (IN)         User buffer.
 *  @param count (IN)       Number of elements of the specified datatype.
 *  @param datatype (IN)    User defined datatype.
 *  @param dst (IN)         Peer rank w/in communicator.
 *  @param tag (IN)         User defined tag.
 *  @param mode (IN)        Send mode (STANDARD,BUFFERED,SYNCHRONOUS,READY)
 *  @param comm (IN)        Communicator.
 *  @param request (OUT)    Request handle.
 *  @return                 OMPI_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_module_isend_init_fn_t)(
    void *buf,
    size_t count,
    struct ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);


/**
 *  Post a send request. 
 *
 *  @param buf (IN)         User buffer.
 *  @param count (IN)       Number of elements of the specified datatype.
 *  @param datatype (IN)    User defined datatype.
 *  @param dst (IN)         Peer rank w/in communicator.
 *  @param tag (IN)         User defined tag.
 *  @param mode (IN)        Send mode (STANDARD,BUFFERED,SYNCHRONOUS,READY)
 *  @param comm (IN)        Communicator.
 *  @param request (OUT)    Request handle.
 *  @return                 OMPI_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_module_isend_fn_t)(
    void *buf,
    size_t count,
    struct ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);


/**
 *  Post a send request and wait for completion.
 *
 *  @param buf (IN)         User buffer.
 *  @param count (IN)       Number of elements of the specified datatype.
 *  @param datatype (IN)    User defined datatype.
 *  @param dst (IN)         Peer rank w/in communicator.
 *  @param tag (IN)         User defined tag.
 *  @param mode (IN)        Send mode (STANDARD,BUFFERED,SYNCHRONOUS,READY)
 *  @param comm (IN)        Communicator.
 *  @return                 OMPI_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_module_send_fn_t)(
    void *buf,
    size_t count,
    struct ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm
);

/**
 * Initiate one or more persistent requests.
 *
 * @param count    Number of requests
 * @param request  Array of persistent requests
 * @return         OMPI_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_module_start_fn_t)(
    size_t count,
    struct ompi_request_t** requests
);

/**
 * Probe to poll for pending recv.
 *
 * @param src (IN)        Source rank w/in communicator.
 * @param tag (IN)        User defined tag.
 * @param comm (IN)       Communicator.
 * @param matched (OUT)   Flag indicating if matching recv exists.
 * @param status (OUT)    Completion statuses.
 * @return                OMPI_SUCCESS or failure status.
 *
 */
typedef int (*mca_pml_base_module_iprobe_fn_t)(
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    int *matched,
    ompi_status_public_t *status
);

/**
 * Blocking probe to wait for pending recv.
 *
 * @param src (IN)        Source rank w/in communicator.
 * @param tag (IN)        User defined tag.
 * @param comm (IN)       Communicator.
 * @param status (OUT)    Completion statuses.
 * @return                OMPI_SUCCESS or failure status.
 *
 */
typedef int (*mca_pml_base_module_probe_fn_t)(
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    ompi_status_public_t *status
);

/**
 * Cancel pending operation.
 * 
 * @param request (IN)    Request
 * @return                OMPI_SUCCESS or failure status.
 *
 */
typedef int (*mca_pml_base_module_cancel_fn_t)(
    struct ompi_request_t* request
);


/**
 * Has a request been cancelled?
 * 
 * @param request (IN)    Request
 * @return                OMPI_SUCCESS or failure status.
 *
 */
typedef int (*mca_pml_base_module_cancelled_fn_t)(
    struct ompi_request_t* request,
    int *flag
);

/**
 * Release resources held by a persistent mode request.
 *
 * @param request (IN)    Request
 * @return                OMPI_SUCCESS or failure status.
 *
 */
typedef int (*mca_pml_base_module_free_fn_t)(
    struct ompi_request_t** request
);


/**
 * A special NULL request handle.
 *
 * @param request (OUT)   Request
 * @return                OMPI_SUCCESS or failure status.
 *
 */
typedef int (*mca_pml_base_module_null_fn_t)(
    struct ompi_request_t** request
);

/**
 * Diagnostics function.
 *
 * @param request (IN)    Communicator
 * @param verbose (IN)    Verbosity level (passed to BTL)
 * @return                OMPI_SUCCESS or failure status.
 *
 */
typedef int (*mca_pml_base_module_dump_fn_t)(
    struct ompi_communicator_t* comm,
    int verbose
);

/**
 * Fault Tolerance Awareness function
 * @param status     Checkpoint status
 * @return           OMPI_SUCCESS or failure status
 */
typedef int (*mca_pml_base_module_ft_event_fn_t) (int status);



/**
 *  PML instance.
 */

struct mca_pml_base_module_1_0_0_t {

    /* downcalls from MCA to PML */
    mca_pml_base_module_add_procs_fn_t    pml_add_procs;
    mca_pml_base_module_del_procs_fn_t    pml_del_procs;
    mca_pml_base_module_enable_fn_t       pml_enable;
    mca_pml_base_module_progress_fn_t     pml_progress;

    /* downcalls from MPI to PML */
    mca_pml_base_module_add_comm_fn_t     pml_add_comm;
    mca_pml_base_module_del_comm_fn_t     pml_del_comm;
    mca_pml_base_module_irecv_init_fn_t   pml_irecv_init;
    mca_pml_base_module_irecv_fn_t        pml_irecv;
    mca_pml_base_module_recv_fn_t         pml_recv;
    mca_pml_base_module_isend_init_fn_t   pml_isend_init;
    mca_pml_base_module_isend_fn_t        pml_isend;
    mca_pml_base_module_send_fn_t         pml_send;
    mca_pml_base_module_iprobe_fn_t       pml_iprobe;
    mca_pml_base_module_probe_fn_t        pml_probe;
    mca_pml_base_module_start_fn_t        pml_start;

    /* diagnostics */
    mca_pml_base_module_dump_fn_t         pml_dump;

    /* FT Event */
    mca_pml_base_module_ft_event_fn_t     pml_ft_event;

    /* maximum constant sizes */
    uint32_t                              pml_max_contextid;
    int                                   pml_max_tag;
};
typedef struct mca_pml_base_module_1_0_0_t mca_pml_base_module_1_0_0_t;
typedef mca_pml_base_module_1_0_0_t mca_pml_base_module_t;

/*
 * Macro for use in components that are of type pml
 */
#define MCA_PML_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "pml", 2, 0, 0

    /*
     * macro for doing direct call / call through struct
     */
#if MCA_pml_DIRECT_CALL

#include MCA_pml_DIRECT_CALL_HEADER

#define MCA_PML_CALL_STAMP(a, b) mca_pml_ ## a ## _ ## b
#define MCA_PML_CALL_EXPANDER(a, b) MCA_PML_CALL_STAMP(a,b)
#define MCA_PML_CALL(a) MCA_PML_CALL_EXPANDER(MCA_pml_DIRECT_CALL_COMPONENT, a)

#else
#define MCA_PML_CALL(a) mca_pml.pml_ ## a
#endif

OMPI_DECLSPEC extern mca_pml_base_module_t mca_pml;


END_C_DECLS
#endif /* MCA_PML_H */
