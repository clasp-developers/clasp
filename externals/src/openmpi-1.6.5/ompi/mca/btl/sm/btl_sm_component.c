/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif  /* HAVE_SYS_MMAN_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>  /* for mkfifo */
#endif  /* HAVE_SYS_STAT_H */

#include "ompi/constants.h"
#include "opal/event/event.h"
#include "opal/util/output.h"
#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/common/sm/common_sm.h"
#include "ompi/mca/btl/base/btl_base_error.h"

#if OPAL_ENABLE_FT_CR    == 1
#include "opal/runtime/opal_cr.h"
#endif

#include "btl_sm.h"
#include "btl_sm_frag.h"
#include "btl_sm_fifo.h"

/*
 * Shared Memory (SM) component instance.
 */
mca_btl_sm_component_t mca_btl_sm_component = {
    {  /* super is being filled in */
        /* First, the mca_base_component_t struct containing meta information
          about the component itself */
        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "sm", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_sm_component_open,  /* component open */
            mca_btl_sm_component_close  /* component close */
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        mca_btl_sm_component_init,
        mca_btl_sm_component_progress,
    }  /* end super */
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_btl_sm_param_register_string(
    const char* param_name,
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("btl","sm",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_btl_sm_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("btl","sm",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_sm_component_open(void)
{
    int i;

    /* Register an MCA param to indicate whether we have knem support
       or not */
    mca_base_param_reg_int(&mca_btl_sm_component.super.btl_version,
                           "have_knem_support", "Whether this component supports the knem Linux kernel module or not",
                           false, true, OMPI_BTL_SM_HAVE_KNEM, NULL);

    if (OMPI_BTL_SM_HAVE_KNEM) {
        i = -1;
    } else {
        i = 0;
    }
    mca_base_param_reg_int(&mca_btl_sm_component.super.btl_version,
                           "use_knem",
                           "Whether knem support is desired or not "
                           "(negative = try to enable knem support, but continue even if it is not available, 0 = do not enable knem support, positive = try to enable knem support and fail if it is not available)",
                           false, false, i, &i);
    if (OMPI_BTL_SM_HAVE_KNEM) {
        mca_btl_sm_component.use_knem = i;
    } else {
        if (i > 0) {
            orte_show_help("help-mpi-btl-sm.txt",
                           "knem requested but not supported", true,
                           orte_process_info.nodename);
            return OMPI_ERROR;
        }
        mca_btl_sm_component.use_knem = 0;
    }
    /* Currently disabling DMA mode by default; it's not clear that
       this is useful in all applications and architectures. */
    mca_base_param_reg_int(&mca_btl_sm_component.super.btl_version,
                           "knem_dma_min",
                           "Minimum message size (in bytes) to use the knem DMA mode; ignored if knem does not support DMA mode (0 = do not use the knem DMA mode)",
                           false, false, 0, &i);
    mca_btl_sm_component.knem_dma_min = (uint32_t) i;

    mca_base_param_reg_int(&mca_btl_sm_component.super.btl_version,
                           "knem_max_simultaneous",
                           "Max number of simultaneous ongoing knem operations to support (0 = do everything synchronously, which probably gives the best large message latency; >0 means to do all operations asynchronously, which supports better overlap for simultaneous large message sends)",
                           false, false, 0,
                           &mca_btl_sm_component.knem_max_simultaneous);

    mca_btl_sm_component.sm_max_btls = 1;
    /* register SM component parameters */
    mca_btl_sm_component.sm_free_list_num =
        mca_btl_sm_param_register_int("free_list_num", 8);
    mca_btl_sm_component.sm_free_list_max =
        mca_btl_sm_param_register_int("free_list_max", -1);
    mca_btl_sm_component.sm_free_list_inc =
        mca_btl_sm_param_register_int("free_list_inc", 64);
    mca_btl_sm_component.sm_max_procs =
        mca_btl_sm_param_register_int("max_procs", -1);
    mca_btl_sm_component.sm_mpool_name =
        mca_btl_sm_param_register_string("mpool", "sm");
    mca_btl_sm_component.fifo_size =
        mca_btl_sm_param_register_int("fifo_size", 4096);
    mca_btl_sm_component.nfifos =
        mca_btl_sm_param_register_int("num_fifos", 1);
    /* make sure the number of fifos is a power of 2 */
    {
        int i = 1;
        while ( i < mca_btl_sm_component.nfifos )
            i <<= 1;
        mca_btl_sm_component.nfifos = i;
    }
    mca_btl_sm_component.fifo_lazy_free =
        mca_btl_sm_param_register_int("fifo_lazy_free", 120);

    /* make sure that queue size and lazy free parameter are compatible */
    if (mca_btl_sm_component.fifo_lazy_free >= (mca_btl_sm_component.fifo_size >> 1) )
        mca_btl_sm_component.fifo_lazy_free  = (mca_btl_sm_component.fifo_size >> 1);
    if (mca_btl_sm_component.fifo_lazy_free <= 0)
        mca_btl_sm_component.fifo_lazy_free  = 1;

    /* default number of extra procs to allow for future growth */
    mca_btl_sm_component.sm_extra_procs =
        mca_btl_sm_param_register_int("sm_extra_procs", 0);

    mca_btl_sm.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH-1;
    mca_btl_sm.super.btl_eager_limit = 4*1024;
    mca_btl_sm.super.btl_rndv_eager_limit = 4*1024;
    mca_btl_sm.super.btl_max_send_size = 32*1024;
    mca_btl_sm.super.btl_rdma_pipeline_send_length = 64*1024;
    mca_btl_sm.super.btl_rdma_pipeline_frag_size = 64*1024;
    mca_btl_sm.super.btl_min_rdma_pipeline_size = 64*1024;
    mca_btl_sm.super.btl_flags = MCA_BTL_FLAGS_SEND;
#if OMPI_BTL_SM_HAVE_KNEM
    if (mca_btl_sm_component.use_knem) {
        mca_btl_sm.super.btl_flags |= MCA_BTL_FLAGS_GET;
    }
#endif
    mca_btl_sm.super.btl_bandwidth = 9000;  /* Mbs */
    mca_btl_sm.super.btl_latency   = 1;     /* Microsecs */

    mca_btl_base_param_register(&mca_btl_sm_component.super.btl_version,
                                &mca_btl_sm.super);
    mca_btl_sm_component.max_frag_size = mca_btl_sm.super.btl_max_send_size;
    mca_btl_sm_component.eager_limit = mca_btl_sm.super.btl_eager_limit;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_frags_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_frags_max, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_frags_user, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.pending_send_fl, opal_free_list_t);
    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_sm_component_close(void)
{
    int return_value = OMPI_SUCCESS;

#if OMPI_BTL_SM_HAVE_KNEM
    if (NULL != mca_btl_sm.knem_frag_array) {
        free(mca_btl_sm.knem_frag_array);
        mca_btl_sm.knem_frag_array = NULL;
    }
    if (NULL != mca_btl_sm.knem_status_array) {
        munmap(mca_btl_sm.knem_status_array, 
               mca_btl_sm_component.knem_max_simultaneous);
        mca_btl_sm.knem_status_array = NULL;
    }
    if (-1 != mca_btl_sm.knem_fd) {
        close(mca_btl_sm.knem_fd);
        mca_btl_sm.knem_fd = -1;
    }
#endif

    OBJ_DESTRUCT(&mca_btl_sm_component.sm_lock);
    /**
     * We don't have to destroy the fragment lists. They are allocated
     * directly into the mmapped file, they will auto-magically dissapear
     * when the file get unmapped.
     */
    /*OBJ_DESTRUCT(&mca_btl_sm_component.sm_frags_eager);*/
    /*OBJ_DESTRUCT(&mca_btl_sm_component.sm_frags_max);*/

    /* unmap the shared memory control structure */
    if(mca_btl_sm_component.sm_seg != NULL) {
        return_value = mca_common_sm_fini( mca_btl_sm_component.sm_seg );
        if( OMPI_SUCCESS != return_value ) {
            return_value=OMPI_ERROR;
            opal_output(0," mca_common_sm_fini failed\n");
            goto CLEANUP;
        }

        /* unlink file, so that it will be deleted when all references
         * to it are gone - no error checking, since we want all procs
         * to call this, so that in an abnormal termination scenario,
         * this file will still get cleaned up */
#if OPAL_ENABLE_FT_CR    == 1
        /* Only unlink the file if we are *not* restarting
         * If we are restarting the file will be unlinked at a later time.
         */
        if(OPAL_CR_STATUS_RESTART_PRE  != opal_cr_checkpointing_state &&
           OPAL_CR_STATUS_RESTART_POST != opal_cr_checkpointing_state ) {
            unlink(mca_btl_sm_component.sm_seg->shmem_ds.seg_name);
        }
#else
        unlink(mca_btl_sm_component.sm_seg->shmem_ds.seg_name);
#endif
        OBJ_RELEASE(mca_btl_sm_component.sm_seg);
    }

#if OPAL_ENABLE_PROGRESS_THREADS == 1
    /* close/cleanup fifo create for event notification */
    if(mca_btl_sm_component.sm_fifo_fd > 0) {
        /* write a done message down the pipe */
        unsigned char cmd = DONE;
        if( write(mca_btl_sm_component.sm_fifo_fd,&cmd,sizeof(cmd)) !=
                sizeof(cmd)){
            opal_output(0, "mca_btl_sm_component_close: write fifo failed: errno=%d\n",
                    errno);
        }
        opal_thread_join(&mca_btl_sm_component.sm_fifo_thread, NULL);
        close(mca_btl_sm_component.sm_fifo_fd);
        unlink(mca_btl_sm_component.sm_fifo_path);
    }
#endif

    if (NULL != mca_btl_sm_component.sm_mpool_name) {
        free(mca_btl_sm_component.sm_mpool_name);
    }

CLEANUP:

    /* return */
    return return_value;
}

/*
 *  SM component initialization
 */
mca_btl_base_module_t** mca_btl_sm_component_init(
    int *num_btls,
    bool enable_progress_threads,
    bool enable_mpi_threads)
{
    mca_btl_base_module_t **btls = NULL;
#if OMPI_BTL_SM_HAVE_KNEM
    int rc;
#endif

    *num_btls = 0;

    /* lookup/create shared memory pool only when used */
    mca_btl_sm_component.sm_mpool = NULL;
    mca_btl_sm_component.sm_mpool_base = NULL;

#if OPAL_ENABLE_PROGRESS_THREADS == 1
    /* create a named pipe to receive events  */
    sprintf( mca_btl_sm_component.sm_fifo_path,
             "%s"OPAL_PATH_SEP"sm_fifo.%lu", orte_process_info.job_session_dir,
             (unsigned long)ORTE_PROC_MY_NAME->vpid );
    if(mkfifo(mca_btl_sm_component.sm_fifo_path, 0660) < 0) {
        opal_output(0, "mca_btl_sm_component_init: mkfifo failed with errno=%d\n",errno);
        return NULL;
    }
    mca_btl_sm_component.sm_fifo_fd = open(mca_btl_sm_component.sm_fifo_path, O_RDWR);
    if(mca_btl_sm_component.sm_fifo_fd < 0) {
        opal_output(0, "mca_btl_sm_component_init: open(%s) failed with errno=%d\n",
                    mca_btl_sm_component.sm_fifo_path, errno);
        return NULL;
    }

    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_fifo_thread, opal_thread_t);
    mca_btl_sm_component.sm_fifo_thread.t_run = (opal_thread_fn_t) mca_btl_sm_component_event_thread;
    opal_thread_start(&mca_btl_sm_component.sm_fifo_thread);
#endif

    mca_btl_sm_component.sm_btls = (mca_btl_sm_t **) malloc( mca_btl_sm_component.sm_max_btls * sizeof (mca_btl_sm_t *));
    if (NULL == mca_btl_sm_component.sm_btls) {
        return NULL;
    }

    /* allocate the Shared Memory BTL */
    *num_btls = 1;
    btls = (mca_btl_base_module_t**)malloc(sizeof(mca_btl_base_module_t*));
    if (NULL == btls) {
        return NULL;
    }

    /* get pointer to the btls */
    btls[0] = (mca_btl_base_module_t*)(&(mca_btl_sm));
    mca_btl_sm_component.sm_btls[0] = (mca_btl_sm_t*)(&(mca_btl_sm));

    /* initialize some BTL data */
    /* start with no SM procs */
    mca_btl_sm_component.num_smp_procs = 0;
    mca_btl_sm_component.my_smp_rank   = -1;  /* not defined */
    mca_btl_sm_component.sm_num_btls   = 1;
    /* set flag indicating btl not inited */
    mca_btl_sm.btl_inited = false;

#if OMPI_BTL_SM_HAVE_KNEM
    /* Set knem_status_num_used outside the check for use_knem so that
       we can only have to check one thing (knem_status_num_used) in
       the progress loop. */
    mca_btl_sm.knem_fd = -1;
    mca_btl_sm.knem_status_array = NULL;
    mca_btl_sm.knem_frag_array = NULL;
    mca_btl_sm.knem_status_num_used = 0;
    mca_btl_sm.knem_status_first_avail = 0;
    mca_btl_sm.knem_status_first_used = 0;

    if (0 != mca_btl_sm_component.use_knem) {
        /* Open the knem device.  Try to print a helpful message if we
           fail to open it. */
        mca_btl_sm.knem_fd = open("/dev/knem", O_RDWR);
        if (mca_btl_sm.knem_fd < 0) {
            if (EACCES == errno) {
                struct stat sbuf;
                if (0 != stat("/dev/knem", &sbuf)) {
                    sbuf.st_mode = 0;
                }
                orte_show_help("help-mpi-btl-sm.txt", "knem permission denied",
                               true, orte_process_info.nodename, sbuf.st_mode);
            } else {
                orte_show_help("help-mpi-btl-sm.txt", "knem fail open",
                               true, orte_process_info.nodename, errno,
                               strerror(errno));
            }
            goto no_knem;
        }

        /* Check that the ABI if the kernel module running is the same
           as what we were compiled against */
        rc = ioctl(mca_btl_sm.knem_fd, KNEM_CMD_GET_INFO, 
                   &mca_btl_sm_component.knem_info);
        if (rc < 0) {
            orte_show_help("help-mpi-btl-sm.txt", "knem get ABI fail",
                           true, orte_process_info.nodename, errno,
                           strerror(errno));
            goto no_knem;
        }
        if (KNEM_ABI_VERSION != mca_btl_sm_component.knem_info.abi) {
            orte_show_help("help-mpi-btl-sm.txt", "knem ABI mismatch",
                           true, orte_process_info.nodename, KNEM_ABI_VERSION,
                           mca_btl_sm_component.knem_info.abi);
            goto no_knem;
        }

        /* If we want DMA mode and DMA mode is supported, then set
           knem_dma_flag to KNEM_FLAG_DMA. */
        mca_btl_sm_component.knem_dma_flag = 0;
        if (mca_btl_sm_component.knem_dma_min > 0 && 
            (mca_btl_sm_component.knem_info.features & KNEM_FEATURE_DMA)) {
            mca_btl_sm_component.knem_dma_flag = KNEM_FLAG_DMA;
        }

        /* Get the array of statuses from knem if max_simultaneous > 0 */
        if (mca_btl_sm_component.knem_max_simultaneous > 0) {
            mca_btl_sm.knem_status_array = mmap(NULL, 
                                                mca_btl_sm_component.knem_max_simultaneous,
                                                (PROT_READ | PROT_WRITE), 
                                                MAP_SHARED, mca_btl_sm.knem_fd, 
                                                KNEM_STATUS_ARRAY_FILE_OFFSET);
            if (MAP_FAILED == mca_btl_sm.knem_status_array) {
                orte_show_help("help-mpi-btl-sm.txt", "knem mmap fail",
                               true, orte_process_info.nodename, errno,
                               strerror(errno));
                goto no_knem;
            }

            /* The first available status index is 0.  Make an empty frag
               array. */
            mca_btl_sm.knem_frag_array = (mca_btl_sm_frag_t **)
                malloc(sizeof(mca_btl_sm_frag_t *) *
                       mca_btl_sm_component.knem_max_simultaneous);
            if (NULL == mca_btl_sm.knem_frag_array) {
                orte_show_help("help-mpi-btl-sm.txt", "knem init fail",
                               true, orte_process_info.nodename, "malloc",
                               errno, strerror(errno));
                goto no_knem;
            }
        }
    }
    /* Set the BTL get function pointer if we're supporting KNEM;
       choose between synchronous and asynchronous. */
    if (mca_btl_sm_component.knem_max_simultaneous > 0) {
        mca_btl_sm.super.btl_get = mca_btl_sm_get_async;
    } else {
        mca_btl_sm.super.btl_get = mca_btl_sm_get_sync;
    }
#endif

    return btls;

#if OMPI_BTL_SM_HAVE_KNEM
 no_knem:
    mca_btl_sm.super.btl_flags &= ~MCA_BTL_FLAGS_GET;

    if (NULL != mca_btl_sm.knem_frag_array) {
        free(mca_btl_sm.knem_frag_array);
        mca_btl_sm.knem_frag_array = NULL;
    }
    if (NULL != mca_btl_sm.knem_status_array) {
        munmap(mca_btl_sm.knem_status_array, 
               mca_btl_sm_component.knem_max_simultaneous);
        mca_btl_sm.knem_status_array = NULL;
    }
    if (-1 != mca_btl_sm.knem_fd) {
        close(mca_btl_sm.knem_fd);
        mca_btl_sm.knem_fd = -1;
    }
    
    /* If "use_knem" is positive, then it's an error if knem support
       is not available -- deactivate the sm btl. */
    if (mca_btl_sm_component.use_knem > 0) {
        return NULL;
    }

    /* Otherwise, use_knem was 0 (and we didn't get here) or use_knem
       was <0, in which case the fact that knem is not available is
       not an error. */
    return btls;
#endif
}


/*
 *  SM component progress.
 */

#if OPAL_ENABLE_PROGRESS_THREADS == 1
void mca_btl_sm_component_event_thread(opal_object_t* thread)
{
    while(1) {
        unsigned char cmd;
        if(read(mca_btl_sm_component.sm_fifo_fd, &cmd, sizeof(cmd)) != sizeof(cmd)) {
            /* error condition */
            return;
        }
        if( DONE == cmd ){
            /* return when done message received */
            return;
        }
        mca_btl_sm_component_progress();
    }
}
#endif

void btl_sm_process_pending_sends(struct mca_btl_base_endpoint_t *ep) 
{ 
    btl_sm_pending_send_item_t *si; 
    int rc; 

    while ( 0 < opal_list_get_size(&ep->pending_sends) ) {
        /* Note that we access the size of ep->pending_sends unlocked
           as it doesn't really matter if the result is wrong as 
           opal_list_remove_first is called with a lock and we handle it
           not finding an item to process */
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        si = (btl_sm_pending_send_item_t*)opal_list_remove_first(&ep->pending_sends); 
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);

        if(NULL == si) return; /* Another thread got in before us. Thats ok. */
    
        OPAL_THREAD_ADD32(&mca_btl_sm_component.num_pending_sends, -1);

        MCA_BTL_SM_FIFO_WRITE(ep, ep->my_smp_rank, ep->peer_smp_rank, si->data,
                          true, false, rc);

        OPAL_FREE_LIST_RETURN(&mca_btl_sm_component.pending_send_fl, (opal_list_item_t*)si);

        if ( OMPI_SUCCESS != rc )
            return;
    }
} 

int mca_btl_sm_component_progress(void)
{
    /* local variables */
    mca_btl_sm_frag_t *frag;
    mca_btl_sm_frag_t Frag;
    sm_fifo_t *fifo = NULL;
    mca_btl_sm_hdr_t *hdr;
    int my_smp_rank = mca_btl_sm_component.my_smp_rank;
    int peer_smp_rank, j, rc = 0, nevents = 0;

    /* first, deal with any pending sends */
    /* This check should be fast since we only need to check one variable. */
    if ( 0 < mca_btl_sm_component.num_pending_sends ) {

        /* perform a loop to find the endpoints that have pending sends */
        /* This can take a while longer if there are many endpoints to check. */
        for ( peer_smp_rank = 0; peer_smp_rank < mca_btl_sm_component.num_smp_procs; peer_smp_rank++) {
            struct mca_btl_base_endpoint_t* endpoint;
            if ( peer_smp_rank == my_smp_rank )
                continue;
            endpoint = mca_btl_sm_component.sm_peers[peer_smp_rank];
            if ( 0 < opal_list_get_size(&endpoint->pending_sends) )
                btl_sm_process_pending_sends(endpoint);
        }
    }

    /* poll each fifo */
    for(j = 0; j < FIFO_MAP_NUM(mca_btl_sm_component.num_smp_procs); j++) {
        fifo = &(mca_btl_sm_component.fifo[my_smp_rank][j]);
      recheck_peer:
        /* aquire thread lock */
        if(opal_using_threads()) {
            opal_atomic_lock(&(fifo->tail_lock));
        }

        hdr = (mca_btl_sm_hdr_t *)sm_fifo_read(fifo);

        /* release thread lock */
        if(opal_using_threads()) {
            opal_atomic_unlock(&(fifo->tail_lock));
        }

        if(SM_FIFO_FREE == hdr) {
            continue;
        }

        nevents++;
        /* dispatch fragment by type */
        switch(((uintptr_t)hdr) & MCA_BTL_SM_FRAG_TYPE_MASK) {
            case MCA_BTL_SM_FRAG_SEND:
            {
                mca_btl_active_message_callback_t* reg;
                /* change the address from address relative to the shared
                 * memory address, to a true virtual address */
                hdr = (mca_btl_sm_hdr_t *) RELATIVE2VIRTUAL(hdr);
                peer_smp_rank = hdr->my_smp_rank;
#if OPAL_ENABLE_DEBUG
                if ( FIFO_MAP(peer_smp_rank) != j ) {
                    opal_output(0, "mca_btl_sm_component_progress: "
                                "rank %d got %d on FIFO %d, but this sender should send to FIFO %d\n",
                                my_smp_rank, peer_smp_rank, j, FIFO_MAP(peer_smp_rank));
                }
#endif
                /* recv upcall */
                reg = mca_btl_base_active_message_trigger + hdr->tag;
                Frag.segment.seg_addr.pval = ((char*)hdr) +
                    sizeof(mca_btl_sm_hdr_t);
                Frag.segment.seg_len = hdr->len;
                Frag.base.des_dst_cnt = 1;
                Frag.base.des_dst = &(Frag.segment);
                reg->cbfunc(&mca_btl_sm.super, hdr->tag, &(Frag.base),
                            reg->cbdata);
                /* return the fragment */
                MCA_BTL_SM_FIFO_WRITE(
                        mca_btl_sm_component.sm_peers[peer_smp_rank],
                        my_smp_rank, peer_smp_rank, hdr->frag, false, true, rc);
                break;
            }
        case MCA_BTL_SM_FRAG_ACK:
            {
                int status = (uintptr_t)hdr & MCA_BTL_SM_FRAG_STATUS_MASK;
                int btl_ownership;
                struct mca_btl_base_endpoint_t* endpoint;

                frag = (mca_btl_sm_frag_t *)((char*)((uintptr_t)hdr &
                                                     (~(MCA_BTL_SM_FRAG_TYPE_MASK |
                                                        MCA_BTL_SM_FRAG_STATUS_MASK))));

                endpoint = frag->endpoint;
                btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                if( MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags ) {
                    /* completion callback */
                    frag->base.des_cbfunc(&mca_btl_sm.super, frag->endpoint,
                                          &frag->base, status?OMPI_ERROR:OMPI_SUCCESS);
                }
                if( btl_ownership ) {
                    MCA_BTL_SM_FRAG_RETURN(frag);
                }
                OPAL_THREAD_ADD32(&mca_btl_sm_component.num_outstanding_frags, -1);
                if ( 0 < opal_list_get_size(&endpoint->pending_sends) ) {
                    btl_sm_process_pending_sends(endpoint);
                }
                goto recheck_peer;
            }
            default:
                /* unknown */
                /*
                 * This code path should presumably never be called.
                 * It's unclear if it should exist or, if so, how it should be written.
                 * If we want to return it to the sending process,
                 * we have to figure out who the sender is.
                 * It seems we need to subtract the mask bits.
                 * Then, hopefully this is an sm header that has an smp_rank field.
                 * Presumably that means the received header was relative.
                 * Or, maybe this code should just be removed.
                 */
                opal_output(0, "mca_btl_sm_component_progress read an unknown type of header");
                hdr = (mca_btl_sm_hdr_t *) RELATIVE2VIRTUAL(hdr);
                peer_smp_rank = hdr->my_smp_rank;
                hdr = (mca_btl_sm_hdr_t*)((uintptr_t)hdr->frag |
                        MCA_BTL_SM_FRAG_STATUS_MASK);
                MCA_BTL_SM_FIFO_WRITE(
                        mca_btl_sm_component.sm_peers[peer_smp_rank],
                        my_smp_rank, peer_smp_rank, hdr, false, true, rc);
                break;
        }
    }

#if OMPI_BTL_SM_HAVE_KNEM
    /* The sm btl is currently hard-wired for a single module.  So
       we're not breaking anything here by checking that one module
       for knem specifics.

       Since knem completes requests in order, we can loop around the
       circular status buffer until:
           - we find a KNEM_STATUS_PENDING, or
           - knem_status_num_used == 0

       Note that knem_status_num_used will never be >0 if
       component.use_knem<0, so we'll never enter the while loop if
       knem is not being used.  It will also never be >0 if
       max_simultaneous == 0 (because they will all complete
       synchronously in _get). However, in order to save a jump
       before the return we should test the use_knem here.
    */
    if( 0 == mca_btl_sm_component.use_knem ) {
        return nevents;
    }
    while (mca_btl_sm.knem_status_num_used > 0 &&
           KNEM_STATUS_PENDING != 
           mca_btl_sm.knem_status_array[mca_btl_sm.knem_status_first_used]) {
        if (KNEM_STATUS_SUCCESS == 
            mca_btl_sm.knem_status_array[mca_btl_sm.knem_status_first_used]) {
            int btl_ownership;

            /* Handle the completed fragment */
            frag = 
                mca_btl_sm.knem_frag_array[mca_btl_sm.knem_status_first_used];
            btl_ownership = (frag->base.des_flags & 
                             MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
            if (0 != (MCA_BTL_DES_SEND_ALWAYS_CALLBACK & 
                      frag->base.des_flags)) {
                frag->base.des_cbfunc(&mca_btl_sm.super, 
                                      frag->endpoint, &frag->base, 
                                      OMPI_SUCCESS);
            }
            if (btl_ownership) {
                MCA_BTL_SM_FRAG_RETURN(frag);
            }

            /* Bump counters, loop around the circular buffer if
               necessary */
            ++nevents;
            --mca_btl_sm.knem_status_num_used;
            ++mca_btl_sm.knem_status_first_used;
            if (mca_btl_sm.knem_status_first_used >= 
                mca_btl_sm_component.knem_max_simultaneous) {
                mca_btl_sm.knem_status_first_used = 0;
            }
        } else {
            /* JMS knem fail */
            break;
        }
    }
#endif
    return nevents;
}
