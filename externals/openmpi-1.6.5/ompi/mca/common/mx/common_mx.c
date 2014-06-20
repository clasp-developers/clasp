/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Myricom. All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "common_mx.h"

#include <errno.h>
#include "opal/memoryhooks/memory.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/runtime/params.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/fake/mpool_fake.h"


int mx__regcache_clean(void *ptr, size_t size);

static int ompi_common_mx_initialize_ref_cnt = 0;
static int ompi_common_mx_available = 0;
static mca_mpool_base_module_t *ompi_common_mx_fake_mpool = 0;

int
ompi_common_mx_initialize(void)
{
    mx_return_t mx_return;
    struct mca_mpool_base_resources_t mpool_resources;
    int index, value, ret = OMPI_SUCCESS;
    
    ompi_common_mx_initialize_ref_cnt++;
    
    if(ompi_common_mx_initialize_ref_cnt == 1) { 
        /* set the MX error handle to always return. This function is the
         * only MX function allowed to be called before mx_init in order
         * to make sure that if the MX is not up and running the MX
         * library does not exit the application.
         */
        mx_set_error_handler(MX_ERRORS_RETURN);
	
	/* If we have a memory manager available, and
	   mpi_leave_pinned == -1, then set mpi_leave_pinned to 1.

	   We have a memory manager if:
	   - we have both FREE and MUNMAP support
	   - we have MUNMAP support and the linux mallopt */
	value = opal_mem_hooks_support_level();
        mpool_resources.regcache_clean = mx__regcache_clean;
	if ((value & (OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT))
            == (OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT)) {
	  index = mca_base_param_find("mpi", NULL, "leave_pinned");
	  if (index >= 0)
            if ((mca_base_param_lookup_int(index, &value) == OPAL_SUCCESS) 
		&& (value == -1)) {
	      
	      ompi_mpi_leave_pinned = 1;
	      setenv("MX_RCACHE", "2", 1);
	      ompi_common_mx_fake_mpool = 
		mca_mpool_base_module_create("fake", NULL, &mpool_resources);
	      if (!ompi_common_mx_fake_mpool) {
		ompi_mpi_leave_pinned = 0;
		setenv("MX_RCACHE", "0", 1);
		opal_output(0, "Error creating fake mpool (error %s)\n",
			    strerror(errno));
	      }
	    }
	}
	
        /* initialize the mx library */
        mx_return = mx_init(); 
        
        if(MX_SUCCESS != mx_return) {
            ompi_common_mx_available = -1;
            if (ompi_common_mx_fake_mpool) {
                mca_mpool_base_module_destroy(ompi_common_mx_fake_mpool);
	        ompi_common_mx_fake_mpool = NULL;
            }
            opal_output(0,
                        "Error in mx_init (error %s)\n",
                        mx_strerror(mx_return));
            return OMPI_ERR_NOT_AVAILABLE;
        }
        ompi_common_mx_available = 1;
    } else if (ompi_common_mx_available < 0) {
        ret = OMPI_ERR_NOT_AVAILABLE;
    }

    return ret;
} 

int
ompi_common_mx_finalize(void)
{
    ompi_common_mx_initialize_ref_cnt--;
    if( 0 == ompi_common_mx_initialize_ref_cnt ) { 
        mx_return_t mx_return;

        if (ompi_common_mx_fake_mpool) {
	  mca_mpool_base_module_destroy(ompi_common_mx_fake_mpool);
	  ompi_common_mx_fake_mpool = NULL;
        }
        
        mx_return = mx_finalize(); 
        if(mx_return != MX_SUCCESS){ 
            opal_output(0, "Error in mx_finalize (error %s)\n", mx_strerror(mx_return));
            return OMPI_ERROR;
        } 
    }
    return OMPI_SUCCESS;
}
