/* -*- Mode: C; c-basic-offset:4 ; -*- */
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
 * Copyright (c) 2007-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/event/event.h"
#include "mpi.h"
#include "ompi/runtime/params.h"
#include "ompi/mca/pml/pml.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "pml_bfo.h"
#include "pml_bfo_hdr.h"
#include "pml_bfo_sendreq.h"
#include "pml_bfo_recvreq.h"
#include "pml_bfo_rdmafrag.h"
#include "pml_bfo_recvfrag.h"
#include "ompi/mca/bml/base/base.h" 
#include "pml_bfo_component.h"
#include "ompi/mca/allocator/base/base.h"

OBJ_CLASS_INSTANCE( mca_pml_bfo_pckt_pending_t,
                    ompi_free_list_item_t,
                    NULL,
                    NULL );

static int mca_pml_bfo_component_open(void);
static int mca_pml_bfo_component_close(void);
static mca_pml_base_module_t*
mca_pml_bfo_component_init( int* priority, bool enable_progress_threads,
                            bool enable_mpi_threads );
static int mca_pml_bfo_component_fini(void);
int mca_pml_bfo_output = 0;

mca_pml_base_component_2_0_0_t mca_pml_bfo_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
      MCA_PML_BASE_VERSION_2_0_0,
    
      "bfo", /* MCA component name */
      OMPI_MAJOR_VERSION,  /* MCA component major version */
      OMPI_MINOR_VERSION,  /* MCA component minor version */
      OMPI_RELEASE_VERSION,  /* MCA component release version */
      mca_pml_bfo_component_open,  /* component open */
      mca_pml_bfo_component_close  /* component close */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    mca_pml_bfo_component_init,  /* component init */
    mca_pml_bfo_component_fini   /* component finalize */
    
};

void *mca_pml_bfo_seg_alloc( struct mca_mpool_base_module_t* mpool,
                             size_t* size,
                             mca_mpool_base_registration_t** registration);
 
void mca_pml_bfo_seg_free( struct mca_mpool_base_module_t* mpool,
                           void* segment );

static inline int mca_pml_bfo_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("pml","bfo",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

static int mca_pml_bfo_component_open(void)
{
    int value;
    mca_allocator_base_component_t* allocator_component;

    value = mca_pml_bfo_param_register_int("verbose", 0);
    mca_pml_bfo_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_pml_bfo_output, value);

    mca_pml_bfo.free_list_num =
        mca_pml_bfo_param_register_int("free_list_num", 4);
    mca_pml_bfo.free_list_max =
        mca_pml_bfo_param_register_int("free_list_max", -1);
    mca_pml_bfo.free_list_inc =
        mca_pml_bfo_param_register_int("free_list_inc", 64);
    mca_pml_bfo.priority =
#if PML_BFO
        mca_pml_bfo_param_register_int("priority", 5);
#else /* PML_BFO */
        mca_pml_bfo_param_register_int("priority", 20);
#endif /* PML_BFO */
    mca_pml_bfo.send_pipeline_depth =
        mca_pml_bfo_param_register_int("send_pipeline_depth", 3);
    mca_pml_bfo.recv_pipeline_depth =
        mca_pml_bfo_param_register_int("recv_pipeline_depth", 4);
    mca_pml_bfo.rdma_put_retries_limit =
        mca_pml_bfo_param_register_int("rdma_put_retries_limit", 5);
    mca_pml_bfo.max_rdma_per_request =
        mca_pml_bfo_param_register_int("max_rdma_per_request", 4);
    mca_pml_bfo.max_send_per_range =
        mca_pml_bfo_param_register_int("max_send_per_range", 4);

    mca_pml_bfo.unexpected_limit =
        mca_pml_bfo_param_register_int("unexpected_limit", 128);
 
    mca_base_param_reg_string(&mca_pml_bfo_component.pmlm_version,
                              "allocator",
                              "Name of allocator component for unexpected messages",
                              false, false,
                              "bucket",
                              &mca_pml_bfo.allocator_name);

    allocator_component = mca_allocator_component_lookup( mca_pml_bfo.allocator_name );
    if(NULL == allocator_component) {
        opal_output(0, "mca_pml_bfo_component_open: can't find allocator: %s\n", mca_pml_bfo.allocator_name);
        return OMPI_ERROR;
    }

    mca_pml_bfo.allocator = allocator_component->allocator_init(true,
                                                                mca_pml_bfo_seg_alloc,
                                                                mca_pml_bfo_seg_free, NULL);
    if(NULL == mca_pml_bfo.allocator) {
        opal_output(0, "mca_pml_bfo_component_open: unable to initialize allocator\n");
        return OMPI_ERROR;
    }

    mca_pml_bfo.enabled = false; 
    return mca_bml_base_open(); 
}


static int mca_pml_bfo_component_close(void)
{
    int rc;

    if (OMPI_SUCCESS != (rc = mca_bml_base_close())) {
         return rc;
    }
    if (NULL != mca_pml_bfo.allocator_name) {
        free(mca_pml_bfo.allocator_name);
    }

    return OMPI_SUCCESS;
}


static mca_pml_base_module_t*
mca_pml_bfo_component_init( int* priority, 
                            bool enable_progress_threads,
                            bool enable_mpi_threads )
{
    opal_output_verbose( 10, mca_pml_bfo_output,
                         "in bfo, my priority is %d\n", mca_pml_bfo.priority);

    if((*priority) > mca_pml_bfo.priority) { 
        *priority = mca_pml_bfo.priority;
        return NULL;
    }
    *priority = mca_pml_bfo.priority;

    if(OMPI_SUCCESS != mca_bml_base_init( enable_progress_threads, 
                                          enable_mpi_threads)) {
        return NULL;
    }

    /* Set this here (vs in component_open()) because
       ompi_mpi_leave_pinned* may have been set after MCA params were
       read (e.g., by the openib btl) */
    mca_pml_bfo.leave_pinned = (1 == ompi_mpi_leave_pinned);
    mca_pml_bfo.leave_pinned_pipeline = (int) ompi_mpi_leave_pinned_pipeline;

    return &mca_pml_bfo.super;
}

int mca_pml_bfo_component_fini(void)
{
    int rc;

    /* Shutdown BML */
    if(OMPI_SUCCESS != (rc = mca_bml.bml_finalize()))
        return rc;

    if(!mca_pml_bfo.enabled)
        return OMPI_SUCCESS; /* never selected.. return success.. */  
    mca_pml_bfo.enabled = false;  /* not anymore */

    OBJ_DESTRUCT(&mca_pml_bfo.rdma_pending);
    OBJ_DESTRUCT(&mca_pml_bfo.pckt_pending);
    OBJ_DESTRUCT(&mca_pml_bfo.recv_pending);
    OBJ_DESTRUCT(&mca_pml_bfo.send_pending);
    OBJ_DESTRUCT(&mca_pml_bfo.non_existing_communicator_pending);
    OBJ_DESTRUCT(&mca_pml_bfo.buffers);
    OBJ_DESTRUCT(&mca_pml_bfo.pending_pckts);
    OBJ_DESTRUCT(&mca_pml_bfo.recv_frags);
    OBJ_DESTRUCT(&mca_pml_bfo.rdma_frags);
    OBJ_DESTRUCT(&mca_pml_bfo.lock);

    if(OMPI_SUCCESS != (rc = mca_pml_bfo.allocator->alc_finalize(mca_pml_bfo.allocator))) {
        return rc;
    }

#if 0
    if (mca_pml_base_send_requests.fl_num_allocated !=
        mca_pml_base_send_requests.super.opal_list_length) {
        opal_output(0, "bfo send requests: %d allocated %d returned\n",
                    mca_pml_base_send_requests.fl_num_allocated,
                    mca_pml_base_send_requests.super.opal_list_length);
    }
    if (mca_pml_base_recv_requests.fl_num_allocated !=
        mca_pml_base_recv_requests.super.opal_list_length) {
        opal_output(0, "bfo recv requests: %d allocated %d returned\n",
                    mca_pml_base_recv_requests.fl_num_allocated,
                    mca_pml_base_recv_requests.super.opal_list_length);
    }
#endif

    return OMPI_SUCCESS;
}

void *mca_pml_bfo_seg_alloc( struct mca_mpool_base_module_t* mpool,
                             size_t* size,
                             mca_mpool_base_registration_t** registration) { 
    return malloc(*size);
}

void mca_pml_bfo_seg_free( struct mca_mpool_base_module_t* mpool,
                           void* segment ) { 
    free(segment);
}
