/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* ASSUMING local process homogeneity with respect to all utilized shared memory
 * facilities. that is, if one local process deems a particular shared memory
 * facility acceptable, then ALL local processes should be able to utilize that
 * facility. as it stands, this is an important point because one process
 * dictates to all other local processes which common sm component will be
 * selected based on its own, local run-time test.
 */

/* RML Messaging in common sm and Our Assumptions
 * o MPI_Init is single threaded
 * o this routine will not be called after MPI_Init.
 *
 * if these assumptions ever change, then we may need to add some support code
 * that queues  up RML messages that have arrived, but have not yet been
 * consumed by the thread who is looking to complete its component
 * initialization.
 */

#include "ompi_config.h"

#include "opal/align.h"
#include "opal/util/argv.h"
#if OPAL_ENABLE_FT_CR == 1
#include "opal/runtime/opal_cr.h"
#endif

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "ompi/constants.h"
#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/mpool/sm/mpool_sm.h"

#include "common_sm_rml.h"

OBJ_CLASS_INSTANCE(
    mca_common_sm_module_t,
    opal_list_item_t,
    NULL,
    NULL
);

/* shared memory information used for initialization and setup. */
static opal_shmem_ds_t shmem_ds;

/* ////////////////////////////////////////////////////////////////////////// */
/* static utility functions */
/* ////////////////////////////////////////////////////////////////////////// */

/* ////////////////////////////////////////////////////////////////////////// */
static mca_common_sm_module_t *
attach_and_init(size_t size_ctl_structure,
                size_t data_seg_alignment,
                bool first_call)
{
    mca_common_sm_module_t *map = NULL;
    mca_common_sm_seg_header_t *seg = NULL;
    unsigned char *addr = NULL;

    /* map the file and initialize segment state */
    if (NULL == (seg = (mca_common_sm_seg_header_t *)
                       opal_shmem_segment_attach(&shmem_ds))) {
        return NULL;
    }
    opal_atomic_rmb();

    /* set up the map object */
    if (NULL == (map = OBJ_NEW(mca_common_sm_module_t))) {
        ORTE_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    /* copy information: from ====> to */
    opal_shmem_ds_copy(&shmem_ds, &map->shmem_ds);

    /* the first entry in the file is the control structure. the first
     * entry in the control structure is an mca_common_sm_seg_header_t
     * element
     */
    map->module_seg = seg;

    addr = ((unsigned char *)seg) + size_ctl_structure;
    /* if we have a data segment (i.e., if 0 != data_seg_alignment),
     * then make it the first aligned address after the control
     * structure.  IF THIS HAPPENS, THIS IS A PROGRAMMING ERROR IN
     * OPEN MPI!
     */
    if (0 != data_seg_alignment) {
        addr = OPAL_ALIGN_PTR(addr, data_seg_alignment, unsigned char *);
        /* is addr past end of the shared memory segment? */
        if ((unsigned char *)seg + shmem_ds.seg_size < addr) {
            orte_show_help("help-mpi-common-sm.txt", "mmap too small", 1,
                           orte_process_info.nodename,
                           (unsigned long)shmem_ds.seg_size,
                           (unsigned long)size_ctl_structure,
                           (unsigned long)data_seg_alignment);
            return NULL;
        }
    }

    map->module_data_addr = addr;
    map->module_seg_addr = (unsigned char *)seg;

    /* map object successfully initialized - we can safely increment
     * seg_num_procs_attached_and_inited. this value is used by
     * opal_shmem_unlink.
     */
    if (first_call) {
        /* make sure that the first call to this function initializes
         * seg_num_procs_inited to zero */
        map->module_seg->seg_num_procs_inited = 0;
        opal_atomic_wmb();
    }
    (void)opal_atomic_add_size_t(&map->module_seg->seg_num_procs_inited, 1);
    opal_atomic_wmb();

    return map;
}

/* ////////////////////////////////////////////////////////////////////////// */
mca_common_sm_module_t *
mca_common_sm_init(ompi_proc_t **procs,
                   size_t num_procs,
                   size_t size,
                   char *file_name,
                   size_t size_ctl_structure,
                   size_t data_seg_alignment)
{
    /* indicates whether or not i'm the lowest named process */
    bool lowest_local_proc = false;
    mca_common_sm_module_t *map = NULL;
    ompi_proc_t *temp_proc = NULL;
    bool found_lowest = false;
    size_t num_local_procs = 0, p = 0;

    /* o reorder procs array to have all the local procs at the beginning.
     * o look for the local proc with the lowest name.
     * o determine the number of local procs.
     * o ensure that procs[0] is the lowest named process.
     */
    for (p = 0; p < num_procs; ++p) {
        if (OPAL_PROC_ON_LOCAL_NODE(procs[p]->proc_flags)) {
            /* if we don't have a lowest, save the first one */
            if (!found_lowest) {
                procs[0] = procs[p];
                found_lowest = true;
            }
            else {
                /* save this proc */
                procs[num_local_procs] = procs[p];
                /* if we have a new lowest, swap it with position 0
                 * so that procs[0] is always the lowest named proc
                 */
                if (OPAL_VALUE2_GREATER == orte_util_compare_name_fields(
                                               ORTE_NS_CMP_ALL,
                                               &(procs[p]->proc_name),
                                               &(procs[0]->proc_name))) {
                    temp_proc = procs[0];
                    procs[0] = procs[p];
                    procs[num_local_procs] = temp_proc;
                }
            }
            /* regardless of the comparisons above, we found
             * another proc on the local node, so increment
             */
            ++num_local_procs;
        }
    }

    /* if there is less than 2 local processes, there's nothing to do. */
    if (num_local_procs < 2) {
        return NULL;
    }

    /* determine whether or not i am the lowest local process */
    lowest_local_proc = (0 == orte_util_compare_name_fields(
                                  ORTE_NS_CMP_ALL,
                                  ORTE_PROC_MY_NAME,
                                  &(procs[0]->proc_name)));

    /* figure out if i am the lowest rank in the group.
     * if so, i will create the shared memory backing store
     */
    if (lowest_local_proc) {
        if (OPAL_SUCCESS == opal_shmem_segment_create(&shmem_ds, file_name,
                                                      size)) {
            map = attach_and_init(size_ctl_structure, data_seg_alignment, true);
            if (NULL != map) {
                size_t mem_offset = map->module_data_addr -
                                        (unsigned char *)map->module_seg;
                map->module_seg->seg_offset = mem_offset;
                map->module_seg->seg_size = size - mem_offset;
                opal_atomic_init(&map->module_seg->seg_lock,
                                 OPAL_ATOMIC_UNLOCKED);
                map->module_seg->seg_inited = 0;
            }
            else {
                /* fail!
                 * only invalidate the shmem_ds.  doing so will let the rest
                 * of the local processes know that the lowest local rank
                 * failed to properly initialize the shared memory segment, so
                 * they should try to carry on without shared memory support
                 */
                 OPAL_SHMEM_DS_INVALIDATE(&shmem_ds);
            }
        }
    }

    /* send shmem info to the rest of the local procs. */
    if (OMPI_SUCCESS !=
        mca_common_sm_rml_info_bcast(&shmem_ds, procs, num_local_procs,
                                     OMPI_RML_TAG_SM_BACK_FILE_CREATED,
                                     lowest_local_proc, file_name)) {
        goto out;
    }

    /* are we dealing with a valid shmem_ds?  that is, did the lowest
     * process successfully initialize the shared memory segment?
     */
    if (OPAL_SHMEM_DS_IS_VALID(&shmem_ds)) {
        if (!lowest_local_proc) {
            map = attach_and_init(size_ctl_structure, data_seg_alignment,
                                  false);
        }
        else {
            /* wait until every other participating process has attached to the
             * shared memory segment.
             */
            while (num_local_procs > map->module_seg->seg_num_procs_inited) {
                opal_atomic_rmb();
            }
            opal_shmem_unlink(&shmem_ds);
        }
    }

out:
    return map;
}

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * this routine is the same as mca_common_sm_mmap_init() except that
 * it takes an (ompi_group_t *) parameter to specify the peers rather
 * than an array of procs.  unlike mca_common_sm_mmap_init(), the
 * group must contain *only* local peers, or this function will return
 * NULL and not create any shared memory segment.
 */
mca_common_sm_module_t *
mca_common_sm_init_group(ompi_group_t *group,
                         size_t size,
                         char *file_name,
                         size_t size_ctl_structure,
                         size_t data_seg_alignment)
{
    mca_common_sm_module_t *ret = NULL;
    ompi_proc_t **procs = NULL;
    size_t i;
    size_t group_size;
    ompi_proc_t *proc;

    /* if there is less than 2 procs, there's nothing to do */
    if ((group_size = ompi_group_size(group)) < 2) {
        goto out;
    }
    else if (NULL == (procs = (ompi_proc_t **)
                              malloc(sizeof(ompi_proc_t *) * group_size))) {
        ORTE_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        goto out;
    }
    /* make sure that all the procs in the group are local */
    for (i = 0; i < group_size; ++i) {
        proc = ompi_group_peer_lookup(group, i);
        if (!OPAL_PROC_ON_LOCAL_NODE(proc->proc_flags)) {
            goto out;
        }
        procs[i] = proc;
    }
    /* let mca_common_sm_init take care of the rest ... */
    ret = mca_common_sm_init(procs, group_size, size, file_name,
                             size_ctl_structure, data_seg_alignment);
out:
    if (NULL != procs) {
        free(procs);
    }
    return ret;
}

/* ////////////////////////////////////////////////////////////////////////// */
/**
 *  allocate memory from a previously allocated shared memory
 *  block.
 *
 *  @param size size of request, in bytes (IN)
 *
 *  @retval addr virtual address
 */
void *
mca_common_sm_seg_alloc(struct mca_mpool_base_module_t *mpool,
                        size_t *size,
                        mca_mpool_base_registration_t **registration)
{
    mca_mpool_sm_module_t *sm_module = (mca_mpool_sm_module_t *)mpool;
    mca_common_sm_seg_header_t* seg = sm_module->sm_common_module->module_seg;
    void *addr;

    opal_atomic_lock(&seg->seg_lock);
    if (seg->seg_offset + *size > seg->seg_size) {
        addr = NULL;
    }
    else {
        size_t fixup;

        /* add base address to segment offset */
        addr = sm_module->sm_common_module->module_data_addr + seg->seg_offset;
        seg->seg_offset += *size;

        /* fix up seg_offset so next allocation is aligned on a
         * sizeof(long) boundry.  Do it here so that we don't have to
         * check before checking remaining size in buffer
         */
        if ((fixup = (seg->seg_offset & (sizeof(long) - 1))) > 0) {
            seg->seg_offset += sizeof(long) - fixup;
        }
    }
    if (NULL != registration) {
        *registration = NULL;
    }
    opal_atomic_unlock(&seg->seg_lock);
    return addr;
}

/* ////////////////////////////////////////////////////////////////////////// */
int
mca_common_sm_fini(mca_common_sm_module_t *mca_common_sm_module)
{
    int rc = OMPI_SUCCESS;

    if (NULL != mca_common_sm_module->module_seg) {
        if (OPAL_SUCCESS !=
            opal_shmem_segment_detach(&mca_common_sm_module->shmem_ds)) {
            rc = OMPI_ERROR;
        }
    }
    return rc;
}

