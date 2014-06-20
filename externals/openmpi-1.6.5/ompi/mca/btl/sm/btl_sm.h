
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
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.
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
#ifndef MCA_BTL_SM_H
#define MCA_BTL_SM_H

#include "ompi_config.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif  /* HAVE_STDINT_H */
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif  /* HAVE_SCHED_H */
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif  /* HAVE_SYS_IOCTL_H */
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif  /* HAVE_SYS_MMAN_H */
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#if OMPI_BTL_SM_HAVE_KNEM
#include "knem_io.h"
#endif  /* OMPI_BTL_SM_HAVE_KNEM */

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif  /* HAVE_SYS_SOCKET_H */
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif  /* HAVE_NETINET_IN_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include "opal/class/opal_free_list.h"
#include "ompi/class/ompi_free_list.h"
#include "opal/event/event.h"
#include "opal/threads/threads.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"

#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/common/sm/common_sm.h"

BEGIN_C_DECLS

/*
 * Shared Memory FIFOs
 *
 * The FIFO is implemented as a circular queue with head and tail pointers
 * (integer indices).  For efficient wraparound indexing, the size of the
 * queue is constrained to be a power of two and we "&" indices with a "mask".
 *
 * More than one process can write to the FIFO head.  Therefore, there is a head
 * lock.  One cannot write until the head slot is empty, indicated by the special
 * queue entry SM_FIFO_FREE.
 *
 * Only the receiver can read the FIFO tail.  Therefore, the tail lock is
 * required only in multithreaded applications.  If a tail read returns the
 * SM_FIFO_FREE value, that means the FIFO is empty.  Once a non-FREE value
 * has been read, the queue slot is *not* automatically reset to SM_FIFO_FREE.
 * Rather, read tail slots are reset "lazily" (see "lazy_free" and "num_to_clear")
 * to reduce the number of memory barriers and improve performance.
 *
 * Since the FIFO lives in shared memory that is mapped differently into
 * each address space, the "queue" pointer is relative (each process must
 * add its own offset) and the queue_recv pointer is meaningful only in the
 * receiver's address space.
 *
 * Since multiple processes access different parts of the FIFO structure in
 * different ways, we introduce padding to keep different parts on different
 * cachelines.
 */

#define SM_FIFO_FREE  (void *) (-2)
/* We can't use opal_cache_line_size here because we need a
   compile-time constant for padding the struct.  We can't really have
   a compile-time constant that is portable, either (e.g., compile on
   one machine and run on another).  So just use a big enough cache
   line that should hopefully be good in most places. */
#define SM_CACHE_LINE_PAD 128

struct sm_fifo_t {
    /* This queue pointer is used only by the heads. */
    volatile void **queue;           
    char pad0[SM_CACHE_LINE_PAD - sizeof(void **)];
    /* This lock is used by the heads. */
    opal_atomic_lock_t head_lock;    
    char pad1[SM_CACHE_LINE_PAD - sizeof(opal_atomic_lock_t)];
    /* This index is used by the head holding the head lock. */
    volatile int head;               
    char pad2[SM_CACHE_LINE_PAD - sizeof(int)];
    /* This mask is used "read only" by all processes. */
    unsigned int mask;               
    char pad3[SM_CACHE_LINE_PAD - sizeof(int)];
    /* The following are used only by the tail. */
    volatile void **queue_recv;
    opal_atomic_lock_t tail_lock;
    volatile int tail;
    int num_to_clear;
    int lazy_free;                   
    char pad4[SM_CACHE_LINE_PAD - sizeof(void **) -
              sizeof(opal_atomic_lock_t) -
              sizeof(int) * 3];
};
typedef struct sm_fifo_t sm_fifo_t;

/*
 * Shared Memory resource managment
 */

#if OPAL_ENABLE_PROGRESS_THREADS == 1
#define DATA (char)0
#define DONE (char)1
#endif

typedef struct mca_btl_sm_mem_node_t {
    mca_mpool_base_module_t* sm_mpool; /**< shared memory pool */
} mca_btl_sm_mem_node_t;

/**
 * Shared Memory (SM) BTL module.
 */
struct mca_btl_sm_component_t {
    mca_btl_base_component_2_0_0_t super;  /**< base BTL component */
    int sm_free_list_num;              /**< initial size of free lists */
    int sm_free_list_max;              /**< maximum size of free lists */
    int sm_free_list_inc;              /**< number of elements to alloc when growing free lists */
    int32_t sm_max_procs;              /**< upper limit on the number of processes using the shared memory pool */
    int sm_extra_procs;                /**< number of extra procs to allow */
    char* sm_mpool_name;               /**< name of shared memory pool module */
    mca_mpool_base_module_t **sm_mpools; /**< shared memory pools (one for each memory node */
    mca_mpool_base_module_t *sm_mpool; /**< mpool on local node */
    void* sm_mpool_base;               /**< base address of shared memory pool */
    size_t eager_limit;                /**< first fragment size */
    size_t max_frag_size;              /**< maximum (second and beyone) fragment size */
    opal_mutex_t sm_lock;
    mca_common_sm_module_t *sm_seg;   /**< description of shared memory segment */
    volatile sm_fifo_t **shm_fifo;     /**< pointer to fifo 2D array in shared memory */
    char **shm_bases;                  /**< pointer to base pointers in shared memory */
    uint16_t *shm_mem_nodes;           /**< pointer to mem noded in shared memory */
    sm_fifo_t **fifo;                  /**< cached copy of the pointer to the 2D
                                          fifo array.  The address in the shared
                                          memory segment sm_ctl_header is a relative,
                                          but this one, in process private memory, is
                                          a real virtual address */
    uint16_t *mem_nodes;               /**< cached copy of mem nodes of each local rank */
    size_t fifo_size;                  /**< number of FIFO queue entries */
    size_t fifo_lazy_free;             /**< number of reads before lazy fifo free is triggered */
    int nfifos;                        /**< number of FIFOs per receiver */
    int32_t num_smp_procs;             /**< current number of smp procs on this host */
    int32_t my_smp_rank;               /**< My SMP process rank.  Used for accessing
                                        *   SMP specfic data structures. */
    ompi_free_list_t sm_frags_eager;   /**< free list of sm first */
    ompi_free_list_t sm_frags_max;     /**< free list of sm second */
    ompi_free_list_t sm_frags_user;
    ompi_free_list_t sm_first_frags_to_progress;  /**< list of first
                                                    fragments that are
                                                    awaiting resources */
    struct mca_btl_base_endpoint_t **sm_peers;

    opal_free_list_t pending_send_fl;
    int num_outstanding_frags;         /**< number of fragments sent but not yet returned to free list */
    int num_pending_sends;             /**< total number on all of my pending-send queues */
    int mem_node;
    int num_mem_nodes;
    
#if OPAL_ENABLE_PROGRESS_THREADS == 1
    char sm_fifo_path[PATH_MAX];   /**< path to fifo used to signal this process */
    int  sm_fifo_fd;               /**< file descriptor corresponding to opened fifo */
    opal_thread_t sm_fifo_thread;
#endif
    struct mca_btl_sm_t      **sm_btls;
    struct mca_btl_sm_frag_t **table;
    size_t sm_num_btls;
    size_t sm_max_btls;

#if OMPI_BTL_SM_HAVE_KNEM
    /* Knem capabilities info */
    struct knem_cmd_info knem_info;
#endif

    /** MCA: should we be using knem or not?  neg=try but continue if
        not available, 0=don't try, 1=try and fail if not available */
    int use_knem;

    /** MCA: minimal message size (bytes) to offload on DMA engine
        when using knem */
    uint32_t knem_dma_min;

    /** MCA: how many simultaneous ongoing knem operations to
        support */
    int knem_max_simultaneous;

    /** If we want DMA and DMA is supported, this will be loaded with
        KNEM_FLAG_DMA.  Otherwise, it'll be 0. */
    int knem_dma_flag;
};
typedef struct mca_btl_sm_component_t mca_btl_sm_component_t;
OMPI_MODULE_DECLSPEC extern mca_btl_sm_component_t mca_btl_sm_component;

/**
 * SM BTL Interface
 */
struct mca_btl_sm_t {
    mca_btl_base_module_t  super;       /**< base BTL interface */
    bool btl_inited;  /**< flag indicating if btl has been inited */
    mca_btl_base_module_error_cb_fn_t error_cb;

#if OMPI_BTL_SM_HAVE_KNEM

    /* File descriptor for knem */
    int knem_fd;

    /* Array of knem status items for non-blocking knem requests */
    knem_status_t *knem_status_array;

    /* Array of fragments currently being moved by knem non-blocking
       operations */
    struct mca_btl_sm_frag_t **knem_frag_array;

    /* First free/available location in knem_status_array */
    int knem_status_first_avail;

    /* First currently-being used location in the knem_status_array */
    int knem_status_first_used;

    /* Number of status items currently in use */
    int knem_status_num_used;
#endif
};
typedef struct mca_btl_sm_t mca_btl_sm_t;
OMPI_MODULE_DECLSPEC extern mca_btl_sm_t mca_btl_sm;





struct btl_sm_pending_send_item_t
{
    opal_free_list_item_t super;
    void *data;
};
typedef struct btl_sm_pending_send_item_t btl_sm_pending_send_item_t;

/***
 * FIFO support for sm BTL.
 */

/***
 * One or more FIFO components may be a pointer that must be
 * accessed by multiple processes.  Since the shared region may
 * be mmapped differently into each process's address space,
 * these pointers will be relative to some base address.  Here,
 * we define macros to translate between relative addresses and
 * virtual addresses.
 */
#define VIRTUAL2RELATIVE(VADDR ) ((long)(VADDR)  - (long)mca_btl_sm_component.shm_bases[mca_btl_sm_component.my_smp_rank])
#define RELATIVE2VIRTUAL(OFFSET) ((long)(OFFSET) + (long)mca_btl_sm_component.shm_bases[mca_btl_sm_component.my_smp_rank])

static inline int sm_fifo_init(int fifo_size, mca_mpool_base_module_t *mpool,
                               sm_fifo_t *fifo, int lazy_free)
{
    int i, qsize;

    /* figure out the queue size (a power of two that is at least 1) */
    qsize = 1;
    while ( qsize < fifo_size )
        qsize <<= 1;

    /* allocate the queue in the receiver's address space */
    fifo->queue_recv = (volatile void **)mpool->mpool_alloc(
            mpool, sizeof(void *) * qsize, opal_cache_line_size, 0, NULL);
    if(NULL == fifo->queue_recv) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* initialize the queue */
    for ( i = 0; i < qsize; i++ )
        fifo->queue_recv[i] = SM_FIFO_FREE;

    /* shift queue address to be relative */
    fifo->queue = (volatile void **) VIRTUAL2RELATIVE(fifo->queue_recv);

    /* initialize the locks */
    opal_atomic_init(&(fifo->head_lock), OPAL_ATOMIC_UNLOCKED);
    opal_atomic_init(&(fifo->tail_lock), OPAL_ATOMIC_UNLOCKED);
    opal_atomic_unlock(&(fifo->head_lock));  /* should be unnecessary */
    opal_atomic_unlock(&(fifo->tail_lock));  /* should be unnecessary */

    /* other initializations */
    fifo->head = 0;
    fifo->mask = qsize - 1;
    fifo->tail = 0;
    fifo->num_to_clear = 0;
    fifo->lazy_free = lazy_free;

    return OMPI_SUCCESS;
}


static inline int sm_fifo_write(void *value, sm_fifo_t *fifo)
{
    volatile void **q = (volatile void **) RELATIVE2VIRTUAL(fifo->queue);

    /* if there is no free slot to write, report exhausted resource */
    opal_atomic_rmb();
    if ( SM_FIFO_FREE != q[fifo->head] )
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* otherwise, write to the slot and advance the head index */
    q[fifo->head] = value;
    opal_atomic_wmb(); 
    fifo->head = (fifo->head + 1) & fifo->mask;
    return OMPI_SUCCESS;
}


static inline void *sm_fifo_read(sm_fifo_t *fifo)
{
    void *value;

    /* read the next queue entry */
    value = (void *) fifo->queue_recv[fifo->tail];

    opal_atomic_rmb();

    /* if you read a non-empty slot, advance the tail pointer */
    if ( SM_FIFO_FREE != value ) {

        fifo->tail = ( fifo->tail + 1 ) & fifo->mask;
        fifo->num_to_clear += 1;

        /* check if it's time to free slots, which we do lazily */
        if ( fifo->num_to_clear >= fifo->lazy_free ) {
            int i = (fifo->tail - fifo->num_to_clear ) & fifo->mask;

            while ( fifo->num_to_clear > 0 ) {
                fifo->queue_recv[i] = SM_FIFO_FREE;
                i = (i+1) & fifo->mask;
                fifo->num_to_clear -= 1;
            }
            opal_atomic_wmb();
        }
    }

    return value;
}

/**
 * Register shared memory module parameters with the MCA framework
 */
extern int mca_btl_sm_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_btl_sm_component_close(void);

/**
 * SM module initialization.
 *
 * @param num_btls (OUT)                  Number of BTLs returned in BTL array.
 * @param enable_progress_threads (IN)    Flag indicating whether BTL is allowed to have progress threads
 * @param enable_mpi_threads (IN)         Flag indicating whether BTL must support multilple simultaneous invocations from different threads
 *
 */
extern mca_btl_base_module_t** mca_btl_sm_component_init(
    int *num_btls,
    bool enable_progress_threads,
    bool enable_mpi_threads
);

/**
 * shared memory component progress.
 */
extern int mca_btl_sm_component_progress(void);



/**
 * Register a callback function that is called on error..
 *
 * @param btl (IN)     BTL module
 * @return             Status indicating if cleanup was successful
 */

int mca_btl_sm_register_error_cb(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_module_error_cb_fn_t cbfunc
);

/**
 * Cleanup any resources held by the BTL.
 *
 * @param btl  BTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_btl_sm_finalize(
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

extern int mca_btl_sm_add_procs(
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
extern int mca_btl_sm_del_procs(
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
extern mca_btl_base_descriptor_t* mca_btl_sm_alloc(
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
extern int mca_btl_sm_free(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_descriptor_t* segment
);


/**
 * Pack data
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
struct mca_btl_base_descriptor_t* mca_btl_sm_prepare_src(
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
 * Initiate an inlined send to the peer or return a descriptor.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
extern int mca_btl_sm_sendi( struct mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* endpoint,
                             struct opal_convertor_t* convertor,
                             void* header,
                             size_t header_size,
                             size_t payload_size,
                             uint8_t order,
                             uint32_t flags,
                             mca_btl_base_tag_t tag,
                             mca_btl_base_descriptor_t** descriptor );

/**
 * Initiate a send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
extern int mca_btl_sm_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    mca_btl_base_tag_t tag
);

#if OMPI_BTL_SM_HAVE_KNEM
/*
 * Synchronous knem get
 */
extern int mca_btl_sm_get_sync(
		struct mca_btl_base_module_t* btl,
		struct mca_btl_base_endpoint_t* endpoint,
		struct mca_btl_base_descriptor_t* des );
/*
 * Asynchronous knem get
 */
extern int mca_btl_sm_get_async(
		struct mca_btl_base_module_t* btl,
		struct mca_btl_base_endpoint_t* endpoint,
		struct mca_btl_base_descriptor_t* des );

extern struct mca_btl_base_descriptor_t* mca_btl_sm_prepare_dst(
		struct mca_btl_base_module_t* btl,
		struct mca_btl_base_endpoint_t* endpoint,
		struct mca_mpool_base_registration_t* registration,
		struct opal_convertor_t* convertor,
		uint8_t order,
		size_t reserve,
		size_t* size,
		uint32_t flags);
#endif

/**
 * Fault Tolerance Event Notification Function
 * @param state Checkpoint Stae
 * @return OMPI_SUCCESS or failure status
 */
int mca_btl_sm_ft_event(int state);

#if OPAL_ENABLE_PROGRESS_THREADS == 1
void mca_btl_sm_component_event_thread(opal_object_t*);
#endif

#if OPAL_ENABLE_PROGRESS_THREADS == 1
#define MCA_BTL_SM_SIGNAL_PEER(peer) \
{ \
    unsigned char cmd = DATA; \
    if(write(peer->fifo_fd, &cmd, sizeof(cmd)) != sizeof(cmd)) { \
        opal_output(0, "mca_btl_sm_send: write fifo failed: errno=%d\n", errno); \
    } \
}
#else
#define MCA_BTL_SM_SIGNAL_PEER(peer)
#endif

END_C_DECLS

#endif

