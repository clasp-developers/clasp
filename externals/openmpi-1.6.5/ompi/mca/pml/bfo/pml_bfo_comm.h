/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PML_BFO_COMM_H
#define MCA_PML_BFO_COMM_H

#include "opal/threads/mutex.h"
#include "opal/class/opal_list.h"
#include "ompi/proc/proc.h"
BEGIN_C_DECLS


struct mca_pml_bfo_comm_proc_t {
    opal_object_t super;
    uint16_t expected_sequence;    /**< send message sequence number - receiver side */
    struct ompi_proc_t* ompi_proc;
#if OPAL_HAVE_THREAD_SUPPORT
    volatile int32_t send_sequence; /**< send side sequence number */
#else
    int32_t send_sequence; /**< send side sequence number */
#endif
    opal_list_t frags_cant_match;  /**< out-of-order fragment queues */
    opal_list_t specific_receives; /**< queues of unmatched specific receives */
    opal_list_t unexpected_frags;  /**< unexpected fragment queues */
};
typedef struct mca_pml_bfo_comm_proc_t mca_pml_bfo_comm_proc_t;


/**
 *  Cached on ompi_communicator_t to hold queues/state
 *  used by the PML<->PTL interface for matching logic. 
 */
struct mca_pml_comm_t {
    opal_object_t super;
#if OPAL_HAVE_THREAD_SUPPORT
    volatile uint32_t recv_sequence;  /**< recv request sequence number - receiver side */
#else
    uint32_t recv_sequence;  /**< recv request sequence number - receiver side */
#endif
    opal_mutex_t matching_lock;   /**< matching lock */
    opal_list_t wild_receives;    /**< queue of unmatched wild (source process not specified) receives */
    mca_pml_bfo_comm_proc_t* procs;
    size_t num_procs;
};
typedef struct mca_pml_comm_t mca_pml_bfo_comm_t;

OBJ_CLASS_DECLARATION(mca_pml_bfo_comm_t);


/**
 * Initialize an instance of mca_pml_bfo_comm_t based on the communicator size.
 *
 * @param  comm   Instance of mca_pml_bfo_comm_t
 * @param  size   Size of communicator 
 * @return        OMPI_SUCCESS or error status on failure.
 */

extern int mca_pml_bfo_comm_init_size(mca_pml_bfo_comm_t* comm, size_t size);

END_C_DECLS
#endif

