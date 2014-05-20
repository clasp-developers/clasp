/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.
 *                         All rights reserved.
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

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/shmem/shmem.h"
#include "shmem_mmap.h"

/**
 * public string showing the shmem ompi_mmap component version number
 */
const char *opal_shmem_mmap_component_version_string =
    "OPAL mmap shmem MCA component version " OPAL_VERSION;

int opal_shmem_mmap_relocate_backing_file = 0;
char *opal_shmem_mmap_backing_file_base_dir = NULL;
bool opal_shmem_mmap_nfs_warning = true;

/**
 * local functions
 */
static int mmap_register(void);
static int mmap_open(void);
static int mmap_close(void);
static int mmap_query(mca_base_module_t **module, int *priority);
static int mmap_runtime_query(mca_base_module_t **module,
                              int *priority,
                              const char *hint);

/**
 * instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
opal_shmem_mmap_component_t mca_shmem_mmap_component = {
    /* ////////////////////////////////////////////////////////////////////// */
    /* super */
    /* ////////////////////////////////////////////////////////////////////// */
    {
        /**
         * common MCA component data
         */
        {
            OPAL_SHMEM_BASE_VERSION_2_0_0,

            /* component name and version */
            "mmap",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,

            /* component open */
            mmap_open,
            /* component close */
            mmap_close,
            /* component query */
            mmap_query,
            /* component register */
            mmap_register,
        },
        /* MCA v2.0.0 component meta data */
        {
            /* the component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        mmap_runtime_query,
    },
    /* ////////////////////////////////////////////////////////////////////// */
    /* mmap component-specific information */
    /* see: shmem_mmap.h for more information */
    /* ////////////////////////////////////////////////////////////////////// */
    /* (default) priority - set high to make mmap the default */
    50
};

/* ////////////////////////////////////////////////////////////////////////// */
static int
mmap_runtime_query(mca_base_module_t **module,
                   int *priority,
                   const char *hint)
{
    /* no run-time query needed for mmap, so this is easy */
    *priority = mca_shmem_mmap_component.priority;
    *module = (mca_base_module_t *)&opal_shmem_mmap_module.super;
    return OPAL_SUCCESS;
}

static int mmap_register(void)
{
    int value;

    /*
     * Do we want the "warning: your mmap file is on NFS!" message?  Per a
     * thread on the OMPI devel list
     * (http://www.open-mpi.org/community/lists/devel/2011/12/10054.php),
     * on some systems, it doesn't seem to matter.  But per older threads,
     * it definitely does matter on some systems.  Perhaps newer kernels
     * are smarter about this kind of stuff...?  Regardless, we should
     * provide the ability to turn off this message for systems where the
     * effect doesn't matter.
     */
    mca_base_param_reg_int(&mca_shmem_mmap_component.super.base_version,
                           "enable_nfs_warning", 
                           "Enable the warning emitted when Open MPI detects that its shared memory backing file is located on a network filesystem (1 = enabled, 0 = disabled).",
                           false, false,
                           (int) true, &value);

    opal_shmem_mmap_nfs_warning = OPAL_INT_TO_BOOL(value);

    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
mmap_open(void)
{
    mca_base_param_reg_int(
        &mca_shmem_mmap_component.super.base_version,
        "priority", "Priority of the mmap shmem component", false, false,
        mca_shmem_mmap_component.priority, &mca_shmem_mmap_component.priority
    );

    mca_base_param_reg_int(
        &mca_shmem_mmap_component.super.base_version,
        "relocate_backing_file",
        "Whether to change the default placement of backing files or not "
        "(Negative = try to relocate backing files to an area rooted at "
        "the path specified by "
        "shmem_mmap_opal_shmem_mmap_backing_file_base_dir, but continue "
        "with the default path if the relocation fails, 0 = do not relocate, "
        "Positive = same as the negative option, but will fail if the "
        "relocation fails.", false, false, 0,
        &opal_shmem_mmap_relocate_backing_file
    );

    mca_base_param_reg_string(
        &mca_shmem_mmap_component.super.base_version,
        "backing_file_base_dir",
        "Specifies where backing files will be created when "
        "shmem_mmap_relocate_backing_file is in use.", false, false, "/dev/shm",
        &opal_shmem_mmap_backing_file_base_dir
    );

    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
mmap_query(mca_base_module_t **module, int *priority)
{
    *priority = mca_shmem_mmap_component.priority;
    *module = (mca_base_module_t *)&opal_shmem_mmap_module.super;
    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
mmap_close(void)
{
    if (NULL != opal_shmem_mmap_backing_file_base_dir) {
        free(opal_shmem_mmap_backing_file_base_dir);
    }
    return OPAL_SUCCESS;
}

