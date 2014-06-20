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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

/* This component will only be compiled on Mac OSX, where we are
   guaranteed to have these headers */
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/sysctl.h>
#include <assert.h>
#include <time.h>
#include <string.h>

#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/pstat/pstat.h"
#include "opal/mca/pstat/base/base.h"

#include "pstat_darwin.h"

static int init(void);
static int query(pid_t pid, opal_pstats_t *stats);
static int fini(void);

/*
 * Darwin pstat module
 */
const opal_pstat_base_module_t opal_pstat_darwin_module = {
    init,
    query,
    fini
};

static int init(void)
{
    return OPAL_SUCCESS;
}

static int fini(void)
{
    return OPAL_SUCCESS;
}

/* Trivial helper function to convert system error codes to OPAL_ERR_*
 codes */
static int convert(int ret) 
{
    switch(ret) {
        case 0:
            return OPAL_SUCCESS;
        case ENOSYS:
            return OPAL_ERR_NOT_SUPPORTED;
        case EINVAL:
            return OPAL_ERR_BAD_PARAM;
        default:
            return OPAL_ERROR;
    }
}

/* Mac OSX does things a little differently than Linux
 * by providing process stats via an API. This means we
 * don't have to parse files that could change!
 */
static int query(pid_t pid, opal_pstats_t *stats)
{
    struct kinfo_proc *procs;
    int kprocinfo[] = { CTL_KERN, KERN_PROC, KERN_PROC_PID, 0, 0 };
    size_t length;
    size_t cnt;
    
    kprocinfo[3] = pid;
    
    /* Call sysctl with a NULL buffer to find out how much memory the
     * eventual data will consume
     */
    
    length = 0;
    if (0 != sysctl(kprocinfo, (sizeof(kprocinfo) / sizeof(*kprocinfo)) - 1,
                    NULL, &length, NULL, 0)) {
        /* something went wrong */
        return convert(errno);
    }
    
    /* Allocate an appropriately sized buffer based on the results
     * from the previous call.
     */
    
    if (NULL == (procs = malloc(length))) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    
    /* Call sysctl again with the new buffer to get the info */
    
    if (0 != sysctl(kprocinfo, (sizeof(kprocinfo) / sizeof(*kprocinfo)) - 1,
                    procs, &length,
                    NULL, 0)) {
        /* something went wrong */
        free(procs);
        return convert(errno);
    }
    
    /* figure out how many results we got */
    cnt = length / sizeof(struct kinfo_proc);
    if (1 < cnt) {
        /* if we got more than one, something is wrong */
        free(procs);
        return OPAL_ERROR;
    }
    
    stats->pid = pid;
    if (MAXCOMLEN < OPAL_PSTAT_MAX_STRING_LEN) {
        memcpy(stats->cmd, procs->kp_proc.p_comm, MAXCOMLEN);
    } else {
        /* leave the trailing NULL to end the string */
        memcpy(stats->cmd, procs->kp_proc.p_comm, OPAL_PSTAT_MAX_STRING_LEN-1);
    }
    /* we aren't getting anything useful back on state, so just leave it
     * as undefined
     * stats->state = procs->kp_proc.p_stat;
     */
    stats->time =  procs->kp_proc.p_cpticks / CLOCKS_PER_SEC;
    stats->priority = procs->kp_proc.p_priority;
    
    
    return OPAL_SUCCESS;
}
