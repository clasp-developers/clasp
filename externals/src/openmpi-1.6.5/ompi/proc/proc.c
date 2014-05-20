/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "ompi/constants.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/threads/mutex.h"
#include "opal/dss/dss.h"
#include "opal/util/arch.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "ompi/proc/proc.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/ompi_module_exchange.h"

static opal_list_t  ompi_proc_list;
static opal_mutex_t ompi_proc_lock;
ompi_proc_t* ompi_proc_local_proc = NULL;

static void ompi_proc_construct(ompi_proc_t* proc);
static void ompi_proc_destruct(ompi_proc_t* proc);

OBJ_CLASS_INSTANCE(
    ompi_proc_t,
    opal_list_item_t,
    ompi_proc_construct,
    ompi_proc_destruct
);


void ompi_proc_construct(ompi_proc_t* proc)
{
    proc->proc_bml = NULL;
    proc->proc_pml = NULL;

    /* By default all processors are supposedly having the same architecture as me. Thus,
     * by default we run in a homogeneous environment. Later, when the RTE can tell us
     * the arch of the remote nodes, we will have to set the convertors to the correct
     * architecture.
     */
    proc->proc_arch = opal_local_arch;
    proc->proc_convertor = ompi_mpi_local_convertor;
    OBJ_RETAIN( ompi_mpi_local_convertor );

    proc->proc_flags = 0;

    /* initialize this pointer to NULL */
    proc->proc_hostname = NULL;
}


void ompi_proc_destruct(ompi_proc_t* proc)
{
    /* As all the convertors are created with OBJ_NEW we can just call OBJ_RELEASE. All, except
     * the local convertor, will get destroyed at some point here. If the reference count is correct
     * the local convertor (who has the reference count increased in the datatype) will not get
     * destroyed here. It will be destroyed later when the ompi_datatype_finalize is called.
     */
    OBJ_RELEASE( proc->proc_convertor );
    /* DO NOT FREE THE HOSTNAME FIELD AS THIS POINTS
     * TO AN AREA ALLOCATED/FREE'D ELSEWHERE
     */
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    opal_list_remove_item(&ompi_proc_list, (opal_list_item_t*)proc);
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
}


int ompi_proc_init(void)
{
    orte_vpid_t i;
    int ret;

    OBJ_CONSTRUCT(&ompi_proc_list, opal_list_t);
    OBJ_CONSTRUCT(&ompi_proc_lock, opal_mutex_t);

    /* create proc structures and find self */
    for( i = 0; i < orte_process_info.num_procs; i++ ) {
        ompi_proc_t *proc = OBJ_NEW(ompi_proc_t);
        opal_list_append(&ompi_proc_list, (opal_list_item_t*)proc);

        proc->proc_name.jobid = ORTE_PROC_MY_NAME->jobid;
        proc->proc_name.vpid = i;
        if (i == ORTE_PROC_MY_NAME->vpid) {
            ompi_proc_local_proc = proc;
            proc->proc_flags = OPAL_PROC_ALL_LOCAL;
            proc->proc_hostname = orte_process_info.nodename;
            proc->proc_arch = opal_local_arch;
            /* add our arch to the modex */
            if (OMPI_SUCCESS != (ret = ompi_modex_send_key_value("OMPI_ARCH", &proc->proc_arch, OPAL_UINT32))) {
                return ret;
            }
        }
    }

    return OMPI_SUCCESS;
}


/* in some cases, all MPI procs are required to do a modex so they
 * can (at the least) exchange their architecture. Since we cannot
 * know in advance if this was required, we provide a separate function
 * to set the arch (instead of doing it inside of ompi_proc_init) that
 * can be called after the modex completes in ompi_mpi_init. Thus, we
 * know that - regardless of how the arch is known, whether via modex
 * or dropped in from a local daemon - the arch can be set correctly
 * at this time
 */
int ompi_proc_set_arch(void)
{
    ompi_proc_t *proc = NULL;
    opal_list_item_t *item = NULL;
    int ret;
    
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    
    for( item  = opal_list_get_first(&ompi_proc_list);
        item != opal_list_get_end(&ompi_proc_list);
        item  = opal_list_get_next(item)) {
        proc = (ompi_proc_t*)item;
        
        if (proc->proc_name.vpid != ORTE_PROC_MY_NAME->vpid) {
            ret = ompi_modex_recv_key_value("OMPI_ARCH", proc, (void*)&(proc->proc_arch), OPAL_UINT32);
            if (OMPI_SUCCESS == ret) {
                /* if arch is different than mine, create a new convertor for this proc */
                if (proc->proc_arch != opal_local_arch) {
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                    OBJ_RELEASE(proc->proc_convertor);
                    proc->proc_convertor = opal_convertor_create(proc->proc_arch, 0);
#else
                    orte_show_help("help-mpi-runtime",
                                   "heterogeneous-support-unavailable",
                                   true, orte_process_info.nodename, 
                                   proc->proc_hostname == NULL ? "<hostname unavailable>" :
                                   proc->proc_hostname);
                    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
                    return OMPI_ERR_NOT_SUPPORTED;
#endif
                }
            } else if (OMPI_ERR_NOT_IMPLEMENTED == ret) {
                proc->proc_arch = opal_local_arch;
            } else {
                OPAL_THREAD_UNLOCK(&ompi_proc_lock);
                return ret;
            }
            /* get the locality information */
            proc->proc_flags = orte_ess.proc_get_locality(&proc->proc_name);
            /* get the name of the node it is on */
            proc->proc_hostname = orte_ess.proc_get_hostname(&proc->proc_name);
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
    return OMPI_SUCCESS;
}


int ompi_proc_finalize (void)
{
    opal_list_item_t *item;

    /* remove all items from list and destroy them. Since we cannot know
     * the reference count of the procs for certain, it is possible that
     * a single OBJ_RELEASE won't drive the count to zero, and hence will
     * not release the memory. Accordingly, we cycle through the list here,
     * calling release on each item.
     *
     * This will cycle until it forces the reference count of each item
     * to zero, thus causing the destructor to run - which will remove
     * the item from the list!
     *
     * We cannot do this under the thread lock as the destructor will
     * call it when removing the item from the list. However, this function
     * is ONLY called from MPI_Finalize, and all threads are prohibited from
     * calling an MPI function once ANY thread has called MPI_Finalize. Of
     * course, multiple threads are allowed to call MPI_Finalize, so this
     * function may get called multiple times by various threads. We believe
     * it is thread safe to do so...though it may not -appear- to be so
     * without walking through the entire list/destructor sequence.
     */
    while (opal_list_get_end(&ompi_proc_list) != (item = opal_list_get_first(&ompi_proc_list))) {
        OBJ_RELEASE(item);
    }
    /* now destruct the list and thread lock */
    OBJ_DESTRUCT(&ompi_proc_list);
    OBJ_DESTRUCT(&ompi_proc_lock);

    return OMPI_SUCCESS;
}

ompi_proc_t** ompi_proc_world(size_t *size)
{
    ompi_proc_t **procs;
    ompi_proc_t *proc;
    size_t count = 0;
    orte_ns_cmp_bitmask_t mask;
    orte_process_name_t my_name;

    /* check bozo case */
    if (NULL == ompi_proc_local_proc) {
        return NULL;
    }
    mask = ORTE_NS_CMP_JOBID;
    my_name = ompi_proc_local_proc->proc_name;

    /* First count how many match this jobid */
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    for (proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list);
         proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
         proc =  (ompi_proc_t*)opal_list_get_next(proc)) {
        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &proc->proc_name, &my_name)) {
            ++count;
        }
    }

    /* allocate an array */
    procs = (ompi_proc_t**) malloc(count * sizeof(ompi_proc_t*));
    if (NULL == procs) {
        OPAL_THREAD_UNLOCK(&ompi_proc_lock);
        return NULL;
    }

    /* now save only the procs that match this jobid */
    count = 0;
    for (proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list);
         proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
         proc =  (ompi_proc_t*)opal_list_get_next(proc)) {
        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &proc->proc_name, &my_name)) {
            /* DO NOT RETAIN THIS OBJECT - the reference count on this
             * object will be adjusted by external callers. The intent
             * here is to allow the reference count to drop to zero if
             * the app no longer desires to communicate with this proc.
             * For example, the proc may call comm_disconnect on all
             * communicators involving this proc. In such cases, we want
             * the proc object to be removed from the list. By not incrementing
             * the reference count here, we allow this to occur.
             *
             * We don't implement that yet, but we are still safe for now as
             * the OBJ_NEW in ompi_proc_init owns the initial reference
             * count which cannot be released until ompi_proc_finalize is
             * called.
             */
            procs[count++] = proc;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);

    *size = count;
    return procs;
}


ompi_proc_t** ompi_proc_all(size_t* size)
{
    ompi_proc_t **procs =
        (ompi_proc_t**) malloc(opal_list_get_size(&ompi_proc_list) * sizeof(ompi_proc_t*));
    ompi_proc_t *proc;
    size_t count = 0;

    if (NULL == procs) {
        return NULL;
    }

    OPAL_THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list);
        proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)opal_list_get_next(proc)) {
        /* We know this isn't consistent with the behavior in ompi_proc_world,
         * but we are leaving the RETAIN for now because the code using this function
         * assumes that the results need to be released when done. It will
         * be cleaned up later as the "fix" will impact other places in
         * the code
         */
        OBJ_RETAIN(proc);
        procs[count++] = proc;
    }
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
    *size = count;
    return procs;
}


ompi_proc_t** ompi_proc_self(size_t* size)
{
    ompi_proc_t **procs = (ompi_proc_t**) malloc(sizeof(ompi_proc_t*));
    if (NULL == procs) {
        return NULL;
    }
    /* We know this isn't consistent with the behavior in ompi_proc_world,
     * but we are leaving the RETAIN for now because the code using this function
     * assumes that the results need to be released when done. It will
     * be cleaned up later as the "fix" will impact other places in
     * the code
     */
    OBJ_RETAIN(ompi_proc_local_proc);
    *procs = ompi_proc_local_proc;
    *size = 1;
    return procs;
}

ompi_proc_t * ompi_proc_find ( const orte_process_name_t * name )
{
    ompi_proc_t *proc, *rproc=NULL;
    orte_ns_cmp_bitmask_t mask;

    /* return the proc-struct which matches this jobid+process id */
    mask = ORTE_NS_CMP_JOBID | ORTE_NS_CMP_VPID;
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list);
        proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)opal_list_get_next(proc)) {
        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &proc->proc_name, name)) {
            rproc = proc;
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);

    return rproc;
}


int ompi_proc_refresh(void) {
    ompi_proc_t *proc = NULL;
    opal_list_item_t *item = NULL;
    orte_vpid_t i = 0;

    OPAL_THREAD_LOCK(&ompi_proc_lock);

    for( item  = opal_list_get_first(&ompi_proc_list), i = 0;
         item != opal_list_get_end(&ompi_proc_list);
         item  = opal_list_get_next(item), ++i ) {
        proc = (ompi_proc_t*)item;

        /* Does not change: proc->proc_name.vpid */
        proc->proc_name.jobid = ORTE_PROC_MY_NAME->jobid;

        /* Make sure to clear the local flag before we set it below */
        proc->proc_flags = 0;

        if (i == ORTE_PROC_MY_NAME->vpid) {
            ompi_proc_local_proc = proc;
            proc->proc_flags = OPAL_PROC_ALL_LOCAL;
            proc->proc_hostname = orte_process_info.nodename;
            proc->proc_arch = opal_local_arch;
        } else {
            proc->proc_flags = orte_ess.proc_get_locality(&proc->proc_name);
            proc->proc_hostname = orte_ess.proc_get_hostname(&proc->proc_name);
            /* if arch is different than mine, create a new convertor for this proc */
            if (proc->proc_arch != opal_local_arch) {
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                OBJ_RELEASE(proc->proc_convertor);
                proc->proc_convertor = opal_convertor_create(proc->proc_arch, 0);
#else
                orte_show_help("help-mpi-runtime",
                               "heterogeneous-support-unavailable",
                               true, orte_process_info.nodename, 
                               proc->proc_hostname == NULL ? "<hostname unavailable>" :
                               proc->proc_hostname);
                OPAL_THREAD_UNLOCK(&ompi_proc_lock);
                return OMPI_ERR_NOT_SUPPORTED;
#endif
            } 
        }
    }

    OPAL_THREAD_UNLOCK(&ompi_proc_lock);

    return OMPI_SUCCESS;   
}

int
ompi_proc_pack(ompi_proc_t **proclist, int proclistsize, opal_buffer_t* buf)
{
    int i, rc;
    
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    
    /* cycle through the provided array, packing the OMPI level
     * data for each proc. This data may or may not be included
     * in any subsequent modex operation, so we include it here
     * to ensure completion of a connect/accept handshake. See
     * the ompi/mca/dpm framework for an example of where and how
     * this info is used.
     *
     * Eventually, we will review the procedures that call this
     * function to see if duplication of communication can be
     * reduced. For now, just go ahead and pack the info so it
     * can be sent.
     */
    for (i=0; i<proclistsize; i++) {
        rc = opal_dss.pack(buf, &(proclist[i]->proc_name), 1, ORTE_NAME);
        if(rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&ompi_proc_lock);
            return rc;
        }
        rc = opal_dss.pack(buf, &(proclist[i]->proc_arch), 1, OPAL_UINT32);
        if(rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&ompi_proc_lock);
            return rc;
        }
        rc = opal_dss.pack(buf, &(proclist[i]->proc_hostname), 1, OPAL_STRING);
        if(rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&ompi_proc_lock);
            return rc;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
    return OMPI_SUCCESS;
}

static ompi_proc_t *
ompi_proc_find_and_add(const orte_process_name_t * name, bool* isnew)
{
    ompi_proc_t *proc, *rproc = NULL;
    orte_ns_cmp_bitmask_t mask;
    
    /* return the proc-struct which matches this jobid+process id */
    mask = ORTE_NS_CMP_JOBID | ORTE_NS_CMP_VPID;
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list);
        proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)opal_list_get_next(proc)) {
        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &proc->proc_name, name)) {
            rproc = proc;
            *isnew = false;
            break;
        }
    }
    
    /* if we didn't find this proc in the list, create a new
     * proc_t and append it to the list
     */
    if (NULL == rproc) {
        *isnew = true;
        rproc = OBJ_NEW(ompi_proc_t);
        if (NULL != rproc) {
            opal_list_append(&ompi_proc_list, (opal_list_item_t*)rproc);
            rproc->proc_name = *name;
        }
        /* caller had better fill in the rest of the proc, or there's
         going to be pain later... */
    }
    
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
    
    return rproc;
}


int
ompi_proc_unpack(opal_buffer_t* buf, 
                 int proclistsize, ompi_proc_t ***proclist,
                 int *newproclistsize, ompi_proc_t ***newproclist)
{
    int i;
    size_t newprocs_len = 0;
    ompi_proc_t **plist=NULL, **newprocs = NULL;
    
    /* do not free plist *ever*, since it is used in the remote group
        structure of a communicator */
    plist = (ompi_proc_t **) calloc (proclistsize, sizeof (ompi_proc_t *));
    if ( NULL == plist ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    /* free this on the way out */
    newprocs = (ompi_proc_t **) calloc (proclistsize, sizeof (ompi_proc_t *));
    if (NULL == newprocs) {
        free(plist);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    /* cycle through the array of provided procs and unpack
     * their info - as packed by ompi_proc_pack
     */
    for ( i=0; i<proclistsize; i++ ){
        orte_std_cntr_t count=1;
        orte_process_name_t new_name;
        uint32_t new_arch;
        char *new_hostname;
        bool isnew = false;
        int rc;
        
        rc = opal_dss.unpack(buf, &new_name, &count, ORTE_NAME);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            free(plist);
            free(newprocs);
            return rc;
        }
        rc = opal_dss.unpack(buf, &new_arch, &count, OPAL_UINT32);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            free(plist);
            free(newprocs);
            return rc;
        }
        rc = opal_dss.unpack(buf, &new_hostname, &count, OPAL_STRING);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            free(plist);
            free(newprocs);
            return rc;
        }
        
        /* see if this proc is already on our ompi_proc_list */
        plist[i] = ompi_proc_find_and_add(&new_name, &isnew);
        if (isnew) {
            /* if not, then it was added, so update the values
             * in the proc_t struct with the info that was passed
             * to us
             */
            newprocs[newprocs_len++] = plist[i];
            
            /* update all the values */
            plist[i]->proc_arch = new_arch;
            /* if arch is different than mine, create a new convertor for this proc */
            if (plist[i]->proc_arch != opal_local_arch) {
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                OBJ_RELEASE(plist[i]->proc_convertor);
                plist[i]->proc_convertor = opal_convertor_create(plist[i]->proc_arch, 0);
#else
                orte_show_help("help-mpi-runtime",
                               "heterogeneous-support-unavailable",
                               true, orte_process_info.nodename, 
                               new_hostname == NULL ? "<hostname unavailable>" :
                               new_hostname);
                free(plist);
                free(newprocs);
                return OMPI_ERR_NOT_SUPPORTED;
#endif
            }
            if (0 == strcmp(ompi_proc_local_proc->proc_hostname,new_hostname)) {
                plist[i]->proc_flags |= (OPAL_PROC_ON_NODE | OPAL_PROC_ON_CU | OPAL_PROC_ON_CLUSTER);
            }
            
            /* Save the hostname */
            plist[i]->proc_hostname = new_hostname;
            
            /* eventually, we will update the orte/mca/ess framework's data
             * to contain the info for the new proc. For now, we ignore
             * this step since the MPI layer already has all the info
             * it requires
             */
        }
    }
    
    if (NULL != newproclistsize) *newproclistsize = newprocs_len;
    if (NULL != newproclist) {
        *newproclist = newprocs;
    } else if (newprocs != NULL) {
        free(newprocs);
    }
    
    *proclist = plist;
    return OMPI_SUCCESS;
}
