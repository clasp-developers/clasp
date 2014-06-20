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
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/*
 * @file
 */
#ifndef OMPI_BTL_PORTALS_H
#define OMPI_BTL_PORTALS_H

#include "ompi_config.h"
#include "ompi/mca/common/portals/common_portals.h"

#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/class/ompi_free_list.h"

#include "btl_portals_endpoint.h"
#include "btl_portals_frag.h"

/*
 * Portals BTL component.
 */
struct mca_btl_portals_component_t {
    /* base BTL component */
    mca_btl_base_component_2_0_0_t super;

    /* output channel for debugging.  Value settings when using
     * output_verbose:
     *
     *  - 0 : critical user information
     *  - 10: general execution diagnostic information
     *  - 20: initialization / shutdown diagnostic information
     *  - 30: basic debugging information
     *  - 90: useful only to developers
     *  - 100: lots and lots of performance impacting output
     */
    int portals_output;

    /* initial size of free lists */
    int portals_free_list_init_num;
    /* max size of free lists */
    int portals_free_list_max_num;
    /* numer of elements to grow free lists */
    int portals_free_list_inc_num;

    /* number of eager fragments */
    int portals_free_list_eager_max_num;
    
    /* shall I use portals to send to thyself? */
    int portals_support_self;

    /* do I need a portals ACK? */
    int portals_need_ack;

};
typedef struct mca_btl_portals_component_t mca_btl_portals_component_t;


#define OMPI_BTL_PORTALS_EQ_SEND  0
#define OMPI_BTL_PORTALS_EQ_RECV  1
#define OMPI_BTL_PORTALS_EQ_SIZE  2

struct mca_btl_portals_module_t {
    /* base BTL module interface */
    mca_btl_base_module_t super;

    /* number of processes we're actively connected to.  Needed to
       know when to do activation / shutdown */
    int32_t portals_num_procs;

    /* fragment free lists */
    ompi_free_list_t portals_frag_eager;
    ompi_free_list_t portals_frag_max;
    ompi_free_list_t portals_frag_user;

    /* incoming send message receive memory descriptors */
    int portals_recv_mds_num;
    int portals_recv_mds_size;
    opal_list_t portals_recv_blocks;

    /* frag for receive callbacks */
    mca_btl_portals_frag_recv_t portals_recv_frag;

    /* event queues.  Keep sends on own eq, since we can't control
       space for the ack otherwise */
    int portals_eq_sizes[OMPI_BTL_PORTALS_EQ_SIZE];
    ptl_handle_eq_t portals_eq_handles[OMPI_BTL_PORTALS_EQ_SIZE];

    /* "reject" entry for recv match list */
    ptl_handle_me_t portals_recv_reject_me_h;

    /* number outstanding sends and local rdma */
    volatile int32_t portals_outstanding_ops;
    int32_t portals_max_outstanding_ops;

    /* sends queued until there's time to send */
    opal_list_t portals_queued_sends;

    /* key to use for next rdma operation */
    volatile int64_t portals_rdma_key;

    /* our portals network interface */
    ptl_handle_ni_t portals_ni_h;

    /* number of dropped messages */
    ptl_sr_value_t portals_sr_dropped;

    /* descriptors for send */
    ptl_md_t md_send;
};
typedef struct mca_btl_portals_module_t mca_btl_portals_module_t;

/*
 * Component functions (btl_portals_component.c)
 */
int mca_btl_portals_component_open(void);
int mca_btl_portals_component_close(void);


mca_btl_base_module_t** mca_btl_portals_component_init(int *num_btls, 
                                                       bool has_progress_threads,
                                                       bool has_mpi_threads);

int mca_btl_portals_component_progress(void);

/*
 * Compatibility functions (btl_portals_compat_{}.c)
 *
 * Not part of the BTL interface.  Need to be implemented for every
 * version of Portals
 */
int mca_btl_portals_init_compat(mca_btl_portals_component_t *comp);

/* 4th argument is a ptl_peers array, as that's what we'll get back
   from many of the access functions... */
int mca_btl_portals_add_procs_compat(mca_btl_portals_module_t* btl,
                                     size_t nprocs, struct ompi_proc_t **procs,
                                     ptl_process_id_t **ptl_peers);

/*
 * Module configuration functions (btl_portals.c)
 */
int mca_btl_portals_finalize(struct mca_btl_base_module_t* btl_base);

int mca_btl_portals_add_procs(struct mca_btl_base_module_t* btl_base,
                              size_t nprocs,
                              struct ompi_proc_t **procs,
                              struct mca_btl_base_endpoint_t** peers,
                              opal_bitmap_t* reachable);

int mca_btl_portals_del_procs(struct mca_btl_base_module_t* btl_base,
                              size_t nprocs,
                              struct ompi_proc_t **procs,
                              struct mca_btl_base_endpoint_t** peers);

mca_btl_base_descriptor_t* 
mca_btl_portals_alloc(struct mca_btl_base_module_t* btl_base, 
                      struct mca_btl_base_endpoint_t* endpoint,
                      uint8_t order,
                      size_t size,
                      uint32_t flags); 

int mca_btl_portals_free(struct mca_btl_base_module_t* btl_base, 
                         mca_btl_base_descriptor_t* des); 

mca_btl_base_descriptor_t* 
mca_btl_portals_prepare_src(struct mca_btl_base_module_t* btl_base,
                            struct mca_btl_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct opal_convertor_t* convertor,
                            uint8_t order,
                            size_t reserve,
                            size_t* size,
                            uint32_t flags);

mca_btl_base_descriptor_t* 
mca_btl_portals_prepare_dst(struct mca_btl_base_module_t* btl_base, 
                            struct mca_btl_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct opal_convertor_t* convertor,
                            uint8_t order,
                            size_t reserve,
                            size_t* size,
                            uint32_t flags); 

int mca_btl_portals_send(struct mca_btl_base_module_t* btl_base,
                         struct mca_btl_base_endpoint_t* btl_peer,
                         struct mca_btl_base_descriptor_t* descriptor, 
                         mca_btl_base_tag_t tag);


int mca_btl_portals_sendi(struct mca_btl_base_module_t* btl_base,
                          struct mca_btl_base_endpoint_t* endpoint,
                          struct opal_convertor_t* convertor,
                          void* header,
                          size_t header_size,
                          size_t payload_size,
                          uint8_t order,
                          uint32_t flags,
                          mca_btl_base_tag_t tag, 
                          mca_btl_base_descriptor_t** des);

int mca_btl_portals_put(struct mca_btl_base_module_t* btl_base,
                        struct mca_btl_base_endpoint_t* btl_peer,
                        struct mca_btl_base_descriptor_t* decriptor);


int mca_btl_portals_get(struct mca_btl_base_module_t* btl_base,
                        struct mca_btl_base_endpoint_t* btl_peer,
                        struct mca_btl_base_descriptor_t* decriptor);


/*
 * global structures
 */
OMPI_MODULE_DECLSPEC extern mca_btl_portals_component_t mca_btl_portals_component;
extern mca_btl_portals_module_t mca_btl_portals_module;

#endif
