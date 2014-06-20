/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2009 University of Houston. All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "ompi_config.h"
#include "coll_hierarch.h"

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"

/*
 * Public string showing the coll ompi_hierarch component version number
 */
const char *mca_coll_hierarch_component_version_string =
  "OMPI/MPI hierarch collective MCA component version " OMPI_VERSION;

/*
 * Global variable
 */
int mca_coll_hierarch_priority_param=0;
int mca_coll_hierarch_verbose_param=0;
int mca_coll_hierarch_use_rdma_param=0;   
int mca_coll_hierarch_ignore_sm_param=0;   
int mca_coll_hierarch_detection_alg_param=2;
int mca_coll_hierarch_bcast_alg_param=COLL_HIERARCH_BASIC_BCAST_ALG;
int mca_coll_hierarch_segsize_param=32768;

/*
 * Local function
 */
static int hierarch_open(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const mca_coll_base_component_2_0_0_t mca_coll_hierarch_component = {

  /* First, the mca_component_t struct containing meta information
     about the component itself */

  {
    MCA_COLL_BASE_VERSION_2_0_0,

    /* Component name and version */
    "hierarch",
    OMPI_MAJOR_VERSION,
    OMPI_MINOR_VERSION,
    OMPI_RELEASE_VERSION,

    /* Component open and close functions */
    hierarch_open,
    NULL
  },
  {
      /* The component is checkpoint ready */
      MCA_BASE_METADATA_PARAM_CHECKPOINT
  },

  /* Initialization / querying functions */
  mca_coll_hierarch_init_query,
  mca_coll_hierarch_comm_query,
};


static int hierarch_open(void)
{
    

    /* Use a high priority, but allow other components to be higher */
    mca_base_param_reg_int(&mca_coll_hierarch_component.collm_version, 
			   "priority",
                           "Priority of the hierarchical coll component",
                           false, false, mca_coll_hierarch_priority_param,
                           &mca_coll_hierarch_priority_param);


    mca_base_param_reg_int(&mca_coll_hierarch_component.collm_version, 
			   "verbose",
                           "Turn verbose message of the hierarchical coll component on/off",
                           false, false, mca_coll_hierarch_verbose_param,
                           &mca_coll_hierarch_verbose_param);

    mca_base_param_reg_int(&mca_coll_hierarch_component.collm_version, 
			   "use_rdma",
                           "Switch from the send btl list used to detect hierarchies to "
			   "the rdma btl list",
                           false, false, mca_coll_hierarch_use_rdma_param,
                           &mca_coll_hierarch_use_rdma_param);

    mca_base_param_reg_int(&mca_coll_hierarch_component.collm_version, 
			   "ignore_sm",
                           "Ignore sm protocol when detecting hierarchies. "
			   "Required to enable the usage of protocol"
			   " specific collective operations",
                           false, false, mca_coll_hierarch_ignore_sm_param,
                           &mca_coll_hierarch_ignore_sm_param);

    mca_base_param_reg_int(&mca_coll_hierarch_component.collm_version,
                           "detection_alg",
                           "Used to specify the algorithm for detecting Hierarchy."
			   "Choose between all or two levels of hierarchy",
                           false, false, mca_coll_hierarch_detection_alg_param,
                           &mca_coll_hierarch_detection_alg_param);


    mca_base_param_reg_int(&mca_coll_hierarch_component.collm_version,
                           "bcast_alg",
                           "Used to specify the algorithm used for bcast operations.",
                           false, false, mca_coll_hierarch_bcast_alg_param,
                           &mca_coll_hierarch_bcast_alg_param);

    mca_base_param_reg_int(&mca_coll_hierarch_component.collm_version,
                           "segment_size",
                           "Used to specify the segment size for segmented algorithms.",
                           false, false, mca_coll_hierarch_segsize_param,
                           &mca_coll_hierarch_segsize_param);

    return OMPI_SUCCESS;
}

static void
mca_coll_hierarch_module_construct(mca_coll_hierarch_module_t *module)
{
    module->hier_lcomm    = MPI_COMM_NULL;
    module->hier_reqs     = NULL;
    module->hier_colorarr = NULL;
    module->hier_llr      = NULL;

    return;
}

static void
mca_coll_hierarch_module_destruct(mca_coll_hierarch_module_t *hierarch_module)
{
    int i, size;
    struct mca_coll_hierarch_llead_t *current=NULL;

    if ( MPI_COMM_NULL != hierarch_module->hier_lcomm ) {
	ompi_comm_free (&(hierarch_module->hier_lcomm) );
    }
    if ( NULL != hierarch_module->hier_reqs ) {
	free ( hierarch_module->hier_reqs );
    }

    size = opal_pointer_array_get_size ( &(hierarch_module->hier_llead));
    for ( i=0; i<size; i++) {
        current = (struct mca_coll_hierarch_llead_t *)opal_pointer_array_get_item ( 
                  &(hierarch_module->hier_llead), i ) ;

        if ( NULL == current ) {
            continue;
        }

        if ( NULL != current->lleaders ) {
            free ( current->lleaders );
        }
	if ( MPI_COMM_NULL != current->llcomm ){
	    ompi_comm_free ( &(current->llcomm));
	}
        free ( current );
    }
    opal_pointer_array_remove_all ( &(hierarch_module->hier_llead));
    OBJ_DESTRUCT (&(hierarch_module->hier_llead));

    if ( NULL != hierarch_module->hier_colorarr ) {
	free ( hierarch_module->hier_colorarr );
    }
    if ( NULL != hierarch_module->hier_llr ) {
	free ( hierarch_module->hier_llr);
    }

    return;
}


OBJ_CLASS_INSTANCE(mca_coll_hierarch_module_t,
                   mca_coll_base_module_t,
                   mca_coll_hierarch_module_construct,
                   mca_coll_hierarch_module_destruct);
