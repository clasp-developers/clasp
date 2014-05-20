/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oracle and/or all affliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_UD_H
#define MCA_BTL_UD_H

/* Number of QP's to stripe sends over - keep this as power of 2 */
/* AWF - This is intentionally NOT an MCA parameter so that I can do fast
   modular arithmetic with it. */
#define MCA_BTL_UD_NUM_QP 4

#include "ompi_config.h"
#include <sys/types.h>
#include <infiniband/verbs.h>

/* Open MPI includes */
#include "opal/class/opal_hash_table.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mpool/mpool.h"

/* TODO - If I want this to go away, addr_t has to come over here */
#include "btl_ofud_endpoint.h"

BEGIN_C_DECLS


/**
 * UD Infiniband (IB) BTL component.
 */

struct mca_btl_ud_component_t {
    mca_btl_base_component_2_0_0_t super;  /**< base BTL component */

    uint32_t max_btls;  /**< Maximum number of BTL modules */
    uint32_t num_btls;  /**< Number of available/initialized BTL modules */

    char* if_include;
    char** if_include_list;
    char* if_exclude;
    char** if_exclude_list;
    char** if_list;

    struct mca_btl_ud_module_t* ud_btls;    /**< array of available BTLs */

    opal_list_t ud_procs;   /**< list of ib proc structures */
    opal_mutex_t ud_lock;   /**< lock for accessing component state */

    char* ud_mpool_name;    /**< name of memory pool */

    int32_t sd_num;         /**< max send descriptors to post per BTL */

    int32_t rd_num;         /**< number of receive descriptors per BTL */
#if 0
    int32_t rd_num_init;    /**< initial recv descriptors to post per BTL */
    int32_t rd_num_max;
    int32_t rd_num_inc;
#endif

    uint32_t ib_pkey_ix;
    uint32_t ib_qkey;
    uint32_t ib_service_level;
    uint32_t ib_src_path_bits;

}; typedef struct mca_btl_ud_component_t mca_btl_ud_component_t;

OMPI_MODULE_DECLSPEC extern mca_btl_ud_component_t mca_btl_ofud_component;

typedef mca_btl_base_recv_reg_t mca_btl_ud_recv_reg_t;


/**
 * Profiling variables
 */

#if OPAL_ENABLE_DEBUG
#define MCA_BTL_UD_ENABLE_PROFILE 0
#else
#define MCA_BTL_UD_ENABLE_PROFILE 0
#endif

#if MCA_BTL_UD_ENABLE_PROFILE

#define MCA_BTL_UD_PROFILE_VAR(var) \
    opal_timer_t avg_ ## var; \
    opal_timer_t cnt_ ## var; \
    opal_timer_t tmp_ ## var

struct mca_btl_ud_profile_t {
    MCA_BTL_UD_PROFILE_VAR(post_send);
    MCA_BTL_UD_PROFILE_VAR(ibv_post_send);
};

typedef struct mca_btl_ud_profile_t mca_btl_ud_profile_t;
extern mca_btl_ud_profile_t mca_btl_ud_profile;

#endif


/**
 * UD/IB BTL Interface
 */

struct mca_btl_ud_module_t {
    mca_btl_base_module_t super;

    uint8_t ib_port_num;
    struct ibv_device* ib_dev;
    struct ibv_context* ib_dev_context;
    struct ibv_pd* ib_pd;
    struct ibv_cq* ib_cq;

    struct mca_btl_ud_addr_t addr;  /**< local address information */

    ompi_free_list_t send_frags;    /**< send fragments & buffers */
    ompi_free_list_t user_frags;    /**< user data fragments */
    ompi_free_list_t recv_frags;    /**< receive fragments & buffers */

    opal_list_t pending_frags;      /**< list of pending send frags */

    opal_mutex_t ud_lock;           /**< lock for pending_frags */

    size_t ib_inline_max;           /**< max size of IB inline send */

    /*int32_t rd_posted;*/          /**< number of receives currently posted */

    int32_t sd_wqe;                 /**< available send WQ entries */
    /* No lock needed, these are incremented/decremented atomically */

    /*opal_hash_table_t* ep_lookup;*/
    /**< hash table for fast lookup of endpoint structures in recv path */
    /* lid:qpnum is key, value is mca_btl_ud_endpoint_t* */

    struct ibv_qp* ib_qp[MCA_BTL_UD_NUM_QP];
    uint32_t ib_qp_next;
    /**< Local QPs and stripe counters */
    /* No lock needed - counters only ever increase by 1 */
}; typedef struct mca_btl_ud_module_t mca_btl_ud_module_t;

struct mca_btl_ud_frag_t;
extern mca_btl_ud_module_t mca_btl_ofud_module;


/**
 * IB component initialization.
 *
 * @param num_btl_modules (OUT)
 *          Number of BTLs returned in BTL array.
 * @param allow_multi_user_threads (OUT)
 *          Flag indicating wether BTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)
 *          Flag indicating whether BTL uses threads (TRUE)
 *
 *  (1) read interface list from verbs and compare against component parameters
 *      then create a BTL instance for selected interfaces
 *  (2) publish BTL addressing info
 */

extern mca_btl_base_module_t** mca_btl_ud_component_init(
                                                  int *num_btl_modules,
                                                  bool allow_multi_user_threads,
                                                  bool have_hidden_threads);


/**
 * UD/IB component progress.
 */
extern int mca_btl_ud_component_progress(void);


/**
 * Cleanup any resources held by the BTL.
 *
 * @param btl  BTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_btl_ud_finalize(struct mca_btl_base_module_t* btl);


/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)
 * @param nprocs (IN)     Number of processes
 * @param procs (IN)      Set of processes
 * @param peers (OUT)     Set of (optional) peer addressing info.
 * @param peers (IN/OUT)  Set of processes that are reachable via this BTL.
 * @return                OMPI_SUCCESS or error status on failure.
 */

extern int mca_btl_ud_add_procs(struct mca_btl_base_module_t* btl,
                                size_t nprocs,
                                struct ompi_proc_t **procs,
                                struct mca_btl_base_endpoint_t** peers,
                                opal_bitmap_t* reachable);


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

extern int mca_btl_ud_del_procs(struct mca_btl_base_module_t* btl,
                                size_t nprocs,
                                struct ompi_proc_t **procs,
                                struct mca_btl_base_endpoint_t** peers);


/**
 * PML->BTL Initiate a send of the specified size.
 *
 * @param btl (IN)
 *          BTL instance
 * @param btl_base_peer (IN)
 *          BTL peer addressing
 * @param send_request (IN/OUT)
 *          Send request (allocated by PML via mca_btl_base_request_alloc_fn_t)
 * @param size (IN)
 *          Number of bytes PML is requesting BTL to deliver
 * @param flags (IN)
 *          Flags that should be passed to the peer via the message header.
 * @param request (OUT)
 *          OMPI_SUCCESS if the BTL was able to queue one or more fragments
 */

extern int mca_btl_ud_send(struct mca_btl_base_module_t* btl,
                           struct mca_btl_base_endpoint_t* btl_peer,
                           struct mca_btl_base_descriptor_t* descriptor,
                           mca_btl_base_tag_t tag);


/**
 * Allocate a descriptor.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Requested descriptor size.
 */

extern mca_btl_base_descriptor_t* mca_btl_ud_alloc(
                                              struct mca_btl_base_module_t* btl,
                                              struct mca_btl_base_endpoint_t* endpoint,
                                              uint8_t order,
                                              size_t size,
                                              uint32_t flags);


/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)         BTL module
 * @param descriptor (IN)  Allocated descriptor.
 */

extern int mca_btl_ud_free(struct mca_btl_base_module_t* btl,
                           mca_btl_base_descriptor_t* des);


/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */

mca_btl_base_descriptor_t* mca_btl_ud_prepare_src(
                                    struct mca_btl_base_module_t* btl,
                                    struct mca_btl_base_endpoint_t* peer,
                                    mca_mpool_base_registration_t* registration,
                                    struct opal_convertor_t* convertor,
                                    uint8_t order,
                                    size_t reserve,
                                    size_t* size,
                                    uint32_t flags);



int mca_btl_ud_module_init(mca_btl_ud_module_t* ud_btl);

/**
 * Fault Tolerance Event Notification Function
 * @param state Checkpoint State
 * @return OMPI_SUCCESS or failure status
 */

extern int mca_btl_ud_ft_event(int state);



/*
 * Profiling stuff
 */

#if MCA_BTL_UD_ENABLE_PROFILE

#define MCA_BTL_UD_START_TIME(var) \
    ((mca_btl_ud_profile.tmp_ ## var) = opal_timer_base_get_cycles())

#define MCA_BTL_UD_END_TIME(var)                                             \
do {                                                                         \
    mca_btl_ud_profile.avg_ ## var +=                                        \
        opal_timer_base_get_cycles() - mca_btl_ud_profile.tmp_ ## var;        \
    mca_btl_ud_profile.cnt_ ## var++;                                        \
} while(0)

#define MCA_BTL_UD_SHOW_TIME(var)                                            \
    OPAL_OUTPUT((0, "  " #var " avg %lu cnt %lu",                            \
        (mca_btl_ud_profile.avg_ ## var) / (mca_btl_ud_profile.cnt_ ## var), \
        mca_btl_ud_profile.cnt_ ## var));

#else
#define MCA_BTL_UD_START_TIME(var)
#define MCA_BTL_UD_END_TIME(var)
#define MCA_BTL_UD_SHOW_TIME(var)
#endif

END_C_DECLS
#endif
