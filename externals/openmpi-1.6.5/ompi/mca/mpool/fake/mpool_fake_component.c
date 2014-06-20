/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Myricom. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#define OPAL_DISABLE_ENABLE_MEM_DEBUG 1
#include "ompi_config.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "mpool_fake.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

/*
 * Local functions
 */
static int mca_mpool_fake_open(void);
static mca_mpool_base_module_t* mca_mpool_fake_init(
	struct mca_mpool_base_resources_t* resources);

mca_mpool_fake_component_t mca_mpool_fake_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
          MCA_MPOOL_BASE_VERSION_2_0_0,

          "fake", /* MCA component name */
          OMPI_MAJOR_VERSION,  /* MCA component major version */
          OMPI_MINOR_VERSION,  /* MCA component minor version */
          OMPI_RELEASE_VERSION,  /* MCA component release version */
          mca_mpool_fake_open,  /* component open  */
          NULL
      },
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },

      mca_mpool_fake_init
    }
};

/**
  * component open/close/init function
  */
static int mca_mpool_fake_open(void)
{
    return OMPI_SUCCESS;
}

static mca_mpool_base_module_t* mca_mpool_fake_init(
     struct mca_mpool_base_resources_t* resources)
{
    mca_mpool_fake_module_t* mpool_module;

    mpool_module = (mca_mpool_fake_module_t*)
      malloc(sizeof(mca_mpool_fake_module_t));

    mpool_module->resources = *resources;

    mca_mpool_fake_module_init(mpool_module);

    return &mpool_module->super;
}
