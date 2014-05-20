/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * Byte Transfer Layer (BTL)
 *
 *
 * BTL Initialization:
 *
 * During library initialization, all available BTL components are
 * loaded and opened via their mca_base_open_component_fn_t
 * function. The BTL open function should register any mca parameters
 * used to tune/adjust the behaviour of the BTL (mca_base_param_register_int(),
 * mca_base_param_register_string()). Note that the open function may fail
 * if the resources (e.g. shared libraries, etc) required by the network
 * transport are not available.
 *
 * The mca_btl_base_component_init_fn_t() is then called for each of the
 * components that are succesfully opened. The component init function may
 * return either:
 *
 * (1) a NULL list of BTL modules if the transport is not available,
 * (2) a list containing a one or more single BTL modules, where the BTL provides
 *     a layer of abstraction over one or more physical devices (e.g. NICs),
 *
 * During module initialization, the module should post any addressing
 * information required by its peers. An example would be the TCP
 * listen port opened by the TCP module for incoming connection
 * requests. This information is published to peers via the
 * ompi_modex_send() interface. Note that peer information is not
 * guaranteed to be available via ompi_modex_recv() during the
 * module's init function. However, it will be available during
 * BTL selection (mca_btl_base_add_proc_fn_t()).
 *
 * BTL Selection:
 *
 * The upper layer builds an ordered list of the available BTL modules sorted
 * by their exclusivity ranking. This is a relative ranking that is used
 * to determine the set of BTLs that may be used to reach a given destination.
 * During startup the BTL modules are queried via their
 * mca_btl_base_add_proc_fn_t() to determine if they are able to reach
 * a given destination.  The BTL module with the highest ranking that
 * returns success is selected. Subsequent BTL modules are selected only
 * if they have the same exclusivity ranking.
 *
 * An example of how this might be used:
 *
 * BTL         Exclusivity   Comments
 * --------    -----------   ------------------
 * LO              100       Selected exclusively for local process
 * SM               50       Selected exclusively for other processes on host
 * IB                0       Selected based on network reachability
 * IB                0       Selected based on network reachability
 * TCP               0       Selected based on network reachability
 * TCP               0       Selected based on network reachability
 *
 * When mca_btl_base_add_proc_fn_t() is called on a  BTL module, the BTL 
 * will populate an OUT variable with mca_btl_base_endpoint_t pointers. 
 * Each pointer is treated as an opaque handle by the upper layer and is
 * returned to the BTL on subsequent data transfer calls to the
 * corresponding destination process.  The actual contents of the
 * data structure are defined on a per BTL basis, and may be used to
 * cache addressing or connection information, such as a TCP socket
 * or IB queue pair.
 *
 * Progress:
 *
 * By default, the library provides for polling based progress of outstanding
 * requests. The BTL component exports an interface function (btl_progress)
 * that is called in a polling mode by the PML during calls into the MPI
 * library. Note that the btl_progress() function is called on the BTL component
 * rather than each BTL module. This implies that the BTL author is responsible
 * for iterating over the pending operations in each of the BTL modules associated
 * with the component.
 *
 * On platforms where threading support is provided, the library provides the
 * option of building with asynchronous threaded progress. In this case, the BTL
 * author is responsible for providing a thread to progress pending operations.
 * A thread is associated with the BTL component/module such that transport specific
 * functionality/APIs may be used to block the thread until a pending operation
 * completes. This thread MUST NOT poll for completion as this would oversubscribe
 * the CPU.
 *
 * Note that in the threaded case the PML may choose to use a hybrid approach,
 * such that polling is implemented from the user thread for a fixed number of
 * cycles before relying on the background thread(s) to complete requests. If
 * possible the BTL should support the use of both modes concurrently.
 *
 */

#ifndef MCA_BTL_H
#define MCA_BTL_H

#include "ompi_config.h"
#include "opal/mca/mca.h"
#include "opal/class/opal_bitmap.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/prefetch.h" /* For OPAL_LIKELY */
#include "ompi/mca/mpool/mpool.h"
#include "ompi/types.h"
#include "opal/types.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

BEGIN_C_DECLS

/*
 * BTL types
 */

struct mca_btl_base_module_t;
struct mca_btl_base_endpoint_t;
struct mca_btl_base_descriptor_t;
struct mca_mpool_base_resources_t;
struct ompi_proc_t; 


/* send/recv operations require tag matching */
typedef uint8_t mca_btl_base_tag_t;

#define MCA_BTL_NO_ORDER       255

/*
 * Communication specific defines. There are a number of active message ID
 * that can be shred between all frameworks that need to communicate (i.e.
 * use the PML or the BTL directly). These ID are exchanged between the
 * processes, therefore they need to be identical everywhere. The simplest
 * approach is to have them defined as constants, and give each framework a
 * small number. Here is the rule that defines these ID (they are 8 bits):
 * - the first 3 bits are used to code the framework (i.e. PML, OSC, COLL)
 * - the remaining 5 bytes are used internally by the framework, and divided
 *   based on the components requirements. Therefore, the way the PML and
 * the OSC frameworks use these defines will be different. For more
 * information about how these framework ID are defined, take a look in the
 * header file associated with the framework.
 */
#define MCA_BTL_AM_FRAMEWORK_MASK   0xD0
#define MCA_BTL_TAG_BTL             0x20
#define MCA_BTL_TAG_PML             0x40
#define MCA_BTL_TAG_OSC_RDMA        0x60
#define MCA_BTL_TAG_USR             0x80
#define MCA_BTL_TAG_MAX             255 /* 1 + highest allowed tag num */

/*
 * Reserved tags for specific BTLs. As multiple BTLs can be active
 * simultaneously, their tags should not collide.
 */
#define MCA_BTL_TAG_IB                (MCA_BTL_TAG_BTL + 0)
#define MCA_BTL_TAG_UDAPL             (MCA_BTL_TAG_BTL + 1)

/* prefered protocol */
#define MCA_BTL_FLAGS_SEND            0x0001
#define MCA_BTL_FLAGS_PUT             0x0002
#define MCA_BTL_FLAGS_GET             0x0004
#define MCA_BTL_FLAGS_RDMA (MCA_BTL_FLAGS_GET|MCA_BTL_FLAGS_PUT)

/* btl can send directly from user buffer w/out registration */
#define MCA_BTL_FLAGS_SEND_INPLACE    0x0008

/* btl transport reliability flags - currently used only by the DR PML */
#define MCA_BTL_FLAGS_NEED_ACK        0x0010
#define MCA_BTL_FLAGS_NEED_CSUM       0x0020

/** RDMA put/get calls must have a matching prepare_{src,dst} call
    on the target with the same base (and possibly bound). */
#define MCA_BTL_FLAGS_RDMA_MATCHED    0x0040

/* btl needs local rdma completion */
#define MCA_BTL_FLAGS_RDMA_COMPLETION 0x0080

 /* btl can do heterogeneous rdma operations on byte buffers */
#define MCA_BTL_FLAGS_HETEROGENEOUS_RDMA 0x0100

/* btl can support failover if enabled */
#define MCA_BTL_FLAGS_FAILOVER_SUPPORT 0x0200

/* Default exclusivity levels */
#define MCA_BTL_EXCLUSIVITY_HIGH     (64*1024) /* internal loopback */
#define MCA_BTL_EXCLUSIVITY_DEFAULT  1024      /* GM/IB/etc. */
#define MCA_BTL_EXCLUSIVITY_LOW      0         /* TCP used as a last resort */

/* error callback flags */
#define MCA_BTL_ERROR_FLAGS_FATAL 0x1
#define MCA_BTL_ERROR_FLAGS_NONFATAL 0x2

/**
 * Asynchronous callback function on completion of an operation.
 * Completion Semantics: The descriptor can be reused or returned to the 
 *  BTL via mca_btl_base_module_free_fn_t. The operation has been queued to
 *  the network device or will otherwise make asynchronous progress without 
 *  subsequent calls to btl_progress.
 *
 * @param[IN] module      the BTL module
 * @param[IN] endpoint    the BTL endpoint
 * @param[IN] descriptor  the BTL descriptor
 *
 */
typedef void (*mca_btl_base_completion_fn_t)(
    struct mca_btl_base_module_t* module,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    int status);

/**
 * Describes a region/segment of memory that is addressable 
 * by an BTL.
 */

struct mca_btl_base_segment_t {
    /** Address of the memory */
    ompi_ptr_t seg_addr;        
     /** Length in bytes */
    uint32_t   seg_len;           
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    /** Heterogeneous padding */
    uint8_t    seg_padding[4];     
#endif
    /** Memory segment key required by some RDMA networks */
    union {
        uint32_t key32[2];
        uint64_t key64;
        uint8_t  key8[8];
    } seg_key;     
};
typedef struct mca_btl_base_segment_t mca_btl_base_segment_t;

/**
 * A descriptor that holds the parameters to a send/put/get
 * operation along w/ a callback routine that is called on
 * completion of the request.
 * Note: from the initiator of a PUT operation des_src is the local memory 
 *       and des_dst is the remote memory 
 *       from the initiator of a GET operations des_dst is the local memory 
 *       and des_src is the remote memory 
 *       from the initiator of a SEND operation des_src is the local memory
 *       and des_dst is not used
 */

struct mca_btl_base_descriptor_t {
    ompi_free_list_item_t super;  
    mca_btl_base_segment_t *des_src;    /**< source segments */
    size_t des_src_cnt;                 /**< number of source segments */
    mca_btl_base_segment_t *des_dst;    /**< destination segments */
    size_t des_dst_cnt;                 /**< number of destination segments */
    mca_btl_base_completion_fn_t des_cbfunc;  /**< local callback function */ 
    void* des_cbdata;                         /**< opaque callback data */
    void* des_context;                        /**< more opaque callback data */
    uint32_t des_flags;                       /**< hints to BTL */
    /** order value, this is only 
        valid in the local completion callback 
        and may be used in subsequent calls to 
        btl_alloc, btl_prepare_src/dst to request 
        a descriptor that will be ordered w.r.t. 
        this descriptor
    */
    uint8_t order;                            
};
typedef struct mca_btl_base_descriptor_t mca_btl_base_descriptor_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_btl_base_descriptor_t);

#define MCA_BTL_DES_FLAGS_PRIORITY          0x0001
/* Allow the BTL to dispose the descriptor once the callback
 * associated was triggered.
 */
#define MCA_BTL_DES_FLAGS_BTL_OWNERSHIP     0x0002
/* Allow the BTL to avoid calling the descriptor callback
 * if the send succeded in the btl_send (i.e in the fast path).
 */
#define MCA_BTL_DES_SEND_ALWAYS_CALLBACK    0x0004

/**
 * Maximum number of allowed segments in src/dst fields of a descriptor.
 */
#define MCA_BTL_DES_MAX_SEGMENTS 16

/* 
 *  BTL base header, stores the tag at a minimum 
 */ 
struct mca_btl_base_header_t{ 
    mca_btl_base_tag_t tag; 
}; 
typedef struct mca_btl_base_header_t mca_btl_base_header_t; 

#define MCA_BTL_BASE_HEADER_HTON(hdr)
#define MCA_BTL_BASE_HEADER_NTOH(hdr)

/*
 *  BTL component interface functions and datatype.
 */

/**
 * MCA->BTL Initializes the BTL component and creates specific BTL
 * module(s).
 *
 * @param num_btls (OUT) Returns the number of btl modules created, or 0
 *                       if the transport is not available.
 *
 * @param enable_progress_threads (IN) Whether this component is
 * allowed to run a hidden/progress thread or not.
 *
 * @param enable_mpi_threads (IN) Whether support for multiple MPI
 * threads is enabled or not (i.e., MPI_THREAD_MULTIPLE), which
 * indicates whether multiple threads may invoke this component
 * simultaneously or not.
 *
 * @return Array of pointers to BTL modules, or NULL if the transport  
 *         is not available.
 *
 * During component initialization, the BTL component should discover
 * the physical devices that are available for the given transport,
 * and create a BTL module to represent each device. Any addressing 
 * information required by peers to reach the device should be published 
 * during this function via the ompi_modex_send() interface. 
 *
 */

typedef struct mca_btl_base_module_t** (*mca_btl_base_component_init_fn_t)(
    int *num_btls, 
    bool enable_progress_threads,
    bool enable_mpi_threads
);

/**
 * MCA->BTL Called to progress outstanding requests for
 * non-threaded polling environments.
 *
 * @return           Count of "completions", a metric of 
 *                   how many items where completed in the call 
 *                   to progress.
 */

typedef int (*mca_btl_base_component_progress_fn_t)(void);


/**
 * Callback function that is called asynchronously on receipt
 * of data by the transport layer. 
 * Note that the the mca_btl_base_descriptor_t is only valid within the 
 * completion function, this implies that all data payload in the 
 * mca_btl_base_descriptor_t must be copied out within this callback or 
 * forfeited back to the BTL.
 * 
 * @param[IN] btl        BTL module
 * @param[IN] tag        The active message receive callback tag value 
 * @param[IN] descriptor The BTL descriptor (contains the receive payload) 
 * @param[IN] cbdata     Opaque callback data
 */

typedef void (*mca_btl_base_module_recv_cb_fn_t)(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_tag_t tag,
    mca_btl_base_descriptor_t* descriptor,
    void* cbdata
);

typedef struct mca_btl_active_message_callback_t {
    mca_btl_base_module_recv_cb_fn_t cbfunc;
    void* cbdata;
} mca_btl_active_message_callback_t;

OMPI_DECLSPEC extern
mca_btl_active_message_callback_t mca_btl_base_active_message_trigger[MCA_BTL_TAG_MAX];

/**
 *  BTL component descriptor. Contains component version information
 *  and component open/close/init functions.
 */

struct mca_btl_base_component_2_0_0_t {
  mca_base_component_t btl_version;
  mca_base_component_data_t btl_data;
  mca_btl_base_component_init_fn_t btl_init;
  mca_btl_base_component_progress_fn_t btl_progress;
};
typedef struct mca_btl_base_component_2_0_0_t mca_btl_base_component_2_0_0_t;
typedef struct mca_btl_base_component_2_0_0_t mca_btl_base_component_t;

/*  add the 1_0_0_t typedef for source compatibility 
 *  we can do this safely because 1_0_0 components are the same as 
 *  1_0_1 components, the difference is in the btl module. 
 *  Fortunately the only difference in the module is an additional interface
 *  function added to 1_0_1. We can therefore safely treat an older module just
 *  just like the new one so long as we check the component version 
 *  prior to invoking the new interface function.
 */
typedef struct mca_btl_base_component_2_0_0_t mca_btl_base_component_1_0_1_t;
typedef struct mca_btl_base_component_2_0_0_t mca_btl_base_component_1_0_0_t;



/*
 * BTL module interface functions and datatype.
 */

/**
 * MCA->BTL Clean up any resources held by BTL module 
 * before the module is unloaded.
 *  
 * @param btl (IN)   BTL module.
 * @return           OMPI_SUCCESS or error status on failure.
 *
 * Prior to unloading a BTL module, the MCA framework will call 
 * the BTL finalize method of the module. Any resources held by 
 * the BTL should be released and if required the memory corresponding
 * to the BTL module freed.
 * 
 */
typedef int (*mca_btl_base_module_finalize_fn_t)(
    struct mca_btl_base_module_t* btl
);
                                                                                                         
/**
 * BML->BTL notification of change in the process list. 
 *
 * @param btl (IN)            BTL module
 * @param nprocs (IN)         Number of processes
 * @param procs (IN)          Array of processes
 * @param endpoint (OUT)      Array of mca_btl_base_endpoint_t structures by BTL.
 * @param reachable (OUT)     Bitmask indicating set of peer processes that are reachable by this BTL.
 * @return                    OMPI_SUCCESS or error status on failure.
 *
 * The mca_btl_base_module_add_procs_fn_t() is called by the BML to 
 * determine the set of BTLs that should be used to reach each process.
 * Any addressing information exported by the peer via the ompi_modex_send()
 * function should be available during this call via the corresponding 
 * ompi_modex_recv() function. The BTL may utilize this information to 
 * determine reachability of each peer process. 
 *
 * For each process that is reachable by the BTL, the bit corresponding to the index 
 * into the proc array (nprocs) should be set in the reachable bitmask. The BTL 
 * will return an array of pointers to a data structure defined
 * by the BTL that is then returned to the BTL on subsequent calls to the BTL data
 * transfer functions (e.g btl_send). This may be used by the BTL to cache any addressing 
 * or connection information (e.g. TCP socket, IB queue pair).
 */
typedef int (*mca_btl_base_module_add_procs_fn_t)(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs,
    struct ompi_proc_t** procs, 
    struct mca_btl_base_endpoint_t** endpoints,
    struct opal_bitmap_t* reachable
);

/**
 * Notification of change to the process list.
 *
 * @param btl (IN)     BTL module
 * @param nprocs (IN)  Number of processes
 * @param proc (IN)    Set of processes
 * @param peer (IN)    Set of peer addressing information.
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the BML notifies the BTL of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */
typedef int (*mca_btl_base_module_del_procs_fn_t)(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs,
    struct ompi_proc_t** procs, 
    struct mca_btl_base_endpoint_t** peer
);

/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param[IN] btl      BTL module
 * @param[IN] tag      tag value of this callback 
 *                     (specified on subsequent send operations)
 * @param[IN] cbfunc   The callback function
 * @param[IN] cbdata   Opaque callback data 
 * 
 * @return OMPI_SUCCESS The callback was registered successfully
 * @return OMPI_ERROR   The callback was NOT registered successfully
 *
 */
typedef int (*mca_btl_base_module_register_fn_t)(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_tag_t tag,
    mca_btl_base_module_recv_cb_fn_t cbfunc,
    void* cbdata
);


/**
 * Callback function that is called asynchronously on receipt
 * of an error from the transport layer 
 *
 * @param[IN] btl     BTL module
 * @param[IN] flags   type of error 
 * @param[IN] errproc process that had an error
 * @param[IN] btlinfo descriptive string from the BTL
 */

typedef void (*mca_btl_base_module_error_cb_fn_t)(
        struct mca_btl_base_module_t* btl,
        int32_t flags,
        struct ompi_proc_t* errproc,
        char* btlinfo
);


/**
 * Register a callback function that is called on receipt
 * of an error.
 *
 * @param[IN] btl       BTL module
 * @param[IN] cbfunc    The callback function
 *
 * @return OMPI_SUCCESS The callback was registered successfully
 * @return OMPI_ERROR   The callback was NOT registered successfully
 *
 */
typedef int (*mca_btl_base_module_register_error_fn_t)(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_module_error_cb_fn_t cbfunc
);


/**
 * Allocate a descriptor with a segment of the requested size. 
 * Note that the BTL layer may choose to return a smaller size
 * if it cannot support the request. The order tag value ensures that
 * operations on the descriptor that is allocated will be 
 * ordered w.r.t. a previous operation on a particular descriptor. 
 * Ordering is only guaranteed if the previous descriptor had its 
 * local completion callback function called and the order tag of 
 * that descriptor is only valid upon the local completion callback function.
 * 
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 * @param order (IN)    The ordering tag (may be MCA_BTL_NO_ORDER)
 */

typedef mca_btl_base_descriptor_t* (*mca_btl_base_module_alloc_fn_t)(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    uint8_t order,
    size_t size,
    uint32_t flags
);

/**
 * Return a descriptor allocated from this BTL via alloc/prepare.
 * A descriptor can only be deallocated after its local completion 
 * callback function has called for all send/put/get operations.
 * 
 * @param btl (IN)      BTL module
 * @param segment (IN)  Descriptor allocated from the BTL
 */
typedef int (*mca_btl_base_module_free_fn_t)(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_descriptor_t* descriptor
);


/**
 * Prepare a descriptor for send/put/get using the supplied
 * convertor. If the convertor references data that is contiguous,
 * the descriptor may simply point to the user buffer. Otherwise,
 * this routine is responsible for allocating buffer space and
 * packing if required.
 *
 * The descriptor returned can be used in multiple concurrent operations 
 * (send/put/get) unless the BTL has the MCA_BTL_FLAGS_RDMA_MATCHED flag set 
 * in which case a corresponding prepare call must accompany the put/get call
 * in addition, the address and length that is put/get must match the address 
 * and length which is prepared.
 *
 * The order tag value ensures that operations on the 
 * descriptor that is prepared will be ordered w.r.t. a previous
 * operation on a particular descriptor. Ordering is only guaranteed if 
 * the previous descriptor had its local completion callback function 
 * called and the order tag of that descriptor is only valid upon the local 
 * completion callback function.
 *
 * @param btl (IN)          BTL module
 * @param endpoint (IN)     BTL peer addressing
 * @param registration (IN) Memory registration
 * @param convertor (IN)    Data type convertor
 * @param order (IN)        The ordering tag (may be MCA_BTL_NO_ORDER)
 * @param reserve (IN)      Additional bytes requested by upper layer to precede user data
 * @param size (IN/OUT)     Number of bytes to prepare (IN),
 *                          number of bytes actually prepared (OUT)
 *
 */
typedef struct mca_btl_base_descriptor_t* (*mca_btl_base_module_prepare_fn_t)(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags
);

/**
 * Initiate an asynchronous send.
 * Completion Semantics: the descriptor has been queued for a send operation
 *                       the BTL now controls the descriptor until local 
 *                       completion callback is made on the descriptor
 *                       
 * All BTLs allow multiple concurrent asynchronous send operations on a descriptor
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 * 
 * @retval OMPI_SUCCESS    The descriptor was successfully queued for a send 
 * @retval OMPI_ERROR      The descriptor was NOT successfully queued for a send 
 * @retval OMPI_ERR_UNREACH The endpoint is not reachable 
 */
typedef int (*mca_btl_base_module_send_fn_t)(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    mca_btl_base_tag_t tag
);

/**
 * Initiate an immediate blocking send. 
 * Completion Semantics: the BTL will make a best effort 
 *  to send the header and "size" bytes from the datatype using the convertor. 
 *  The header is guaranteed to be delivered entirely in the first segment. 
 *  Should the BTL be unable to deliver the data due to resource constraints 
 *  the BTL will return a descriptor (via the OUT param) 
 *  of size "payload_size + header_size".
 *
 * @param btl (IN)             BTL module
 * @param endpoint (IN)        BTL addressing information
 * @param convertor (IN)       Data type convertor
 * @param header (IN)          Pointer to header.
 * @param header_size (IN)     Size of header.
 * @param payload_size (IN)    Size of payload (from convertor).
 * @param order (IN)           The ordering tag (may be MCA_BTL_NO_ORDER)
 * @param flags (IN)           Flags.
 * @param tag (IN)             The tag value used to notify the peer.
 * @param descriptor (OUT)     The descriptor to be returned unable to be sent immediately

 * @retval OMPI_SUCCESS           The send was successfully queued  
 * @retval OMPI_ERROR             The send failed 
 * @retval OMPI_ERR_UNREACH       The endpoint is not reachable 
 * @retval OMPI_ERR_RESOURCE_BUSY The BTL is busy a descriptor will be returned 
 *                                (via the OUT param) if descriptors are available 

 */

typedef int (*mca_btl_base_module_sendi_fn_t)(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct opal_convertor_t* convertor,
    void* header,
    size_t header_size,
    size_t payload_size,
    uint8_t order,
    uint32_t flags,
    mca_btl_base_tag_t tag,
    mca_btl_base_descriptor_t** descriptor
 );

/**
 * Initiate an asynchronous put. 
 * Completion Semantics: the descriptor has been queued for a put operation
 *                       the BTL now controls the descriptor until local 
 *                       completion callback is made on the descriptor
 *
 * BTLs that do not have the MCA_BTL_FLAGS_RDMA_MATCHED flag set 
 *  allow multiple concurrent put operations on the same descriptor. 
 * BTLs that do have the MCA_BTL_FLAGS_RDMA_MATCHED  flag set require 
 *  a corresponding prepare_src/dst call for each put operation and 
 *  therefore prohibit multiple concurrent put operations.
 * 
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 * 
 * @retval OMPI_SUCCESS    The descriptor was successfully queued for a put
 * @retval OMPI_ERROR      The descriptor was NOT successfully queued for a put
 */

typedef int (*mca_btl_base_module_put_fn_t)(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor
);

/**
 * Initiate an asynchronous get.
 *
 * Completion Semantics: the descriptor has been queued for a get operation
 *                       the BTL now controls the descriptor until local 
 *                       completion callback is made on the descriptor
 *
 * BTLs that do not have the MCA_BTL_FLAGS_RDMA_MATCHED flag set 
 *  allow multiple concurrent get operations on the same descriptor. 
 * BTLs that do have the MCA_BTL_FLAGS_RDMA_MATCHED  flag set require 
 *  a corresponding prepare_src/dst call for each get operation and 
 *  therefore prohibit multiple concurrent get operations.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 * 
 * @retval OMPI_SUCCESS    The descriptor was successfully queued for a get
 * @retval OMPI_ERROR      The descriptor was NOT successfully queued for a get
 *
 */

typedef int (*mca_btl_base_module_get_fn_t)(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor
);


/**
 * Diagnostic dump of btl state.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL endpoint
 * @param verbose (IN)     Verbosity level
 */

typedef void (*mca_btl_base_module_dump_fn_t)(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    int verbose
);

/**
 * Fault Tolerance Event Notification Function
 * @param state Checkpoint Status
 * @return OMPI_SUCCESS or failure status
 */
typedef int (*mca_btl_base_module_ft_event_fn_t)(int state);

/**
 * BTL module interface functions and attributes.
 */
struct mca_btl_base_module_t {

    /* BTL common attributes */
    mca_btl_base_component_t* btl_component; /**< pointer back to the BTL component structure */
    size_t      btl_eager_limit;      /**< maximum size of first fragment -- eager send */
    size_t      btl_rndv_eager_limit;    /**< the size of a data sent in a first fragment of rendezvous protocol */
    size_t      btl_max_send_size;    /**< maximum send fragment size supported by the BTL */
    size_t      btl_rdma_pipeline_send_length; /**< amount of bytes that should be send by pipeline protocol */
    size_t      btl_rdma_pipeline_frag_size; /**< maximum rdma fragment size supported by the BTL */
    size_t      btl_min_rdma_pipeline_size; /**< minimum packet size for pipeline protocol  */
    uint32_t    btl_exclusivity;      /**< indicates this BTL should be used exclusively */
    uint32_t    btl_latency;          /**< relative ranking of latency used to prioritize btls */
    uint32_t    btl_bandwidth;        /**< bandwidth (Mbytes/sec) supported by each endpoint */
    uint32_t    btl_flags;            /**< flags (put/get...) */

    /* BTL function table */
    mca_btl_base_module_add_procs_fn_t      btl_add_procs;
    mca_btl_base_module_del_procs_fn_t      btl_del_procs;
    mca_btl_base_module_register_fn_t       btl_register;
    mca_btl_base_module_finalize_fn_t       btl_finalize;

    mca_btl_base_module_alloc_fn_t          btl_alloc;
    mca_btl_base_module_free_fn_t           btl_free;
    mca_btl_base_module_prepare_fn_t        btl_prepare_src;
    mca_btl_base_module_prepare_fn_t        btl_prepare_dst;
    mca_btl_base_module_send_fn_t           btl_send;
    mca_btl_base_module_sendi_fn_t          btl_sendi;
    mca_btl_base_module_put_fn_t            btl_put;
    mca_btl_base_module_get_fn_t            btl_get;
    mca_btl_base_module_dump_fn_t           btl_dump; 
   
    /** the mpool associated with this btl (optional) */ 
    mca_mpool_base_module_t*             btl_mpool; 
    /** register a default error handler */ 
    mca_btl_base_module_register_error_fn_t btl_register_error;
    /** fault tolerant even notification */
    mca_btl_base_module_ft_event_fn_t btl_ft_event;
};
typedef struct mca_btl_base_module_t mca_btl_base_module_t;

/*
 * Macro for use in modules that are of type btl v2.0.1
 */
#define MCA_BTL_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "btl", 2, 0, 0

END_C_DECLS

#endif /* OMPI_MCA_BTL_H */
