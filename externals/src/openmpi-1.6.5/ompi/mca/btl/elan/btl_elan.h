/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_ELAN_H
#define MCA_BTL_ELAN_H

#include "ompi_config.h"

#include "opal/event/event.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/btl_base_error.h"

#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h" 
#include "btl_elan_endpoint.h" 
#include "btl_elan_frag.h"

#include "elan/elan.h"

BEGIN_C_DECLS

/**
 * ELAN BTL component.
 */

struct mca_btl_elan_component_t {
    mca_btl_base_component_2_0_0_t          super;  /**< base BTL component */ 

    size_t                                  queue_max_size;
    /**< maximum amount of data transfered using the queues */
	
    uint32_t                                elan_num_btls;
    /**< number of hcas available to the ELAN component */

    struct mca_btl_elan_module_t            **elan_btls;
    /**< array of available BTL modules */

    int                                     elan_free_list_num;
    /**< initial size of free lists */

    int                                     elan_free_list_max;
    /**< maximum size of free lists */

    int                                     elan_free_list_inc;
    /**< number of elements to alloc when growing free lists */

    int                                     elan_max_posted_recv;
    /**< number of pre-posted receives */

    /* free list of fragment descriptors */
    ompi_free_list_t                        elan_frag_eager;
    ompi_free_list_t                        elan_frag_max;
    ompi_free_list_t                        elan_frag_user;

    opal_list_t                             elan_procs;
    /**< list of elan proc structures */

    opal_mutex_t                            elan_lock;
    /**< lock for accessing module state */
	
    char* elanidmap_file;  /**< name of the ELANIDMAP file */
}; 
typedef struct mca_btl_elan_component_t mca_btl_elan_component_t;

OMPI_MODULE_DECLSPEC extern mca_btl_elan_component_t mca_btl_elan_component;

/**
 * BTL Module Interface
 */

struct mca_btl_elan_module_t {
    mca_btl_base_module_t  super;             /**< base BTL interface */
    int                    expect_tport_recv;
    int                    elan_vp;
    int                    elan_position;     /**< position of this elan interface */
    ELAN_BASE*             base;
    ELAN_TPORT*            tport;
    ELAN_QUEUE*            global_queue;      /**< The global queue */
    ELAN_QUEUE*            tport_queue;
    ELAN_QUEUE_RX*         rx_queue;          /**< The local receive queue */
    ELAN_QUEUE_TX*         tx_queue;          /**< The global send queue */
    opal_mutex_t           elan_lock;
    opal_list_t            send_list;         /**< list of posted sends */
    opal_list_t            rdma_list;         /**< list of posted receives */
    opal_list_t            recv_list;
}; 
typedef struct mca_btl_elan_module_t mca_btl_elan_module_t;
extern mca_btl_elan_module_t mca_btl_elan_module;

typedef struct mca_btl_elan_hdr_t {
    int tag;
    int length;
} mca_btl_elan_hdr_t;

/**
 * Register ELAN component parameters with the MCA framework
 */
extern int mca_btl_elan_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_btl_elan_component_close(void);

/**
 * ELAN component initialization.
 * 
 * @param num_btl_modules (OUT)           Number of BTLs returned in BTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether BTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether BTL uses threads (TRUE)
 */
extern mca_btl_base_module_t**
mca_btl_elan_component_init( int* num_btl_modules, 
                             bool allow_multi_user_threads,
                             bool have_hidden_threads );

/**
 * ELAN component progress.
 */
extern int mca_btl_elan_component_progress(void);

extern int mca_btl_elan_finalize( struct mca_btl_base_module_t* btl );

extern int mca_btl_elan_ft_event(int state);

END_C_DECLS

#endif  /* MCA_BTL_ELAN_H */

