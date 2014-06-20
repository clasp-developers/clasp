/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "opal/util/output.h"
#include "orte/constants.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#else
#ifdef HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif
#endif


#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/odls/odls_types.h"

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"

#include "iof_orted.h"


/* LOCAL FUNCTIONS */
static void stdin_write_handler(int fd, short event, void *cbdata);


/* API FUNCTIONS */
static int orted_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd);

static int orted_pull(const orte_process_name_t* src_name,
                      orte_iof_tag_t src_tag,
                      int fd);

static int orted_close(const orte_process_name_t* peer,
                       orte_iof_tag_t source_tag);

static int orted_ft_event(int state);

/* The API's in this module are solely used to support LOCAL
 * procs - i.e., procs that are co-located to the daemon. Output
 * from local procs is automatically sent to the HNP for output
 * and possible forwarding to other requestors. The HNP automatically
 * determines and wires up the stdin configuration, so we don't
 * have to do anything here.
 */

orte_iof_base_module_t orte_iof_orted_module = {
    orted_push,
    orted_pull,
    orted_close,
    orted_ft_event
};


/**
 * Push data from the specified file descriptor
 * to the HNP
 */

static int orted_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd)
{
    int flags;
    opal_list_item_t *item;
    orte_iof_proc_t *proct;
    orte_iof_sink_t *sink;
    char *outfile;
    int fdout;
    orte_odls_job_t *jobdat=NULL;
    int np, numdigs;

    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s iof:orted pushing fd %d for process %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         fd, ORTE_NAME_PRINT(dst_name)));
    
    /* set the file descriptor to non-blocking - do this before we setup
     * and activate the read event in case it fires right away
     */
    if((flags = fcntl(fd, F_GETFL, 0)) < 0) {
        opal_output(orte_iof_base.iof_output, "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", 
                    __FILE__, __LINE__, errno);
    } else {
        flags |= O_NONBLOCK;
        fcntl(fd, F_SETFL, flags);
    }

    /* do we already have this process in our list? */
    for (item = opal_list_get_first(&mca_iof_orted_component.procs);
         item != opal_list_get_end(&mca_iof_orted_component.procs);
         item = opal_list_get_next(item)) {
        proct = (orte_iof_proc_t*)item;
        if (proct->name.jobid == dst_name->jobid &&
            proct->name.vpid == dst_name->vpid) {
            /* found it */
            goto SETUP;
        }
    }
    /* if we get here, then we don't yet have this proc in our list */
    proct = OBJ_NEW(orte_iof_proc_t);
    proct->name.jobid = dst_name->jobid;
    proct->name.vpid = dst_name->vpid;
    opal_list_append(&mca_iof_orted_component.procs, &proct->super);
    /* see if we are to output to a file */
    if (NULL != orte_output_filename) {
        /* get the local jobdata for this proc */
        for (item = opal_list_get_first(&orte_local_jobdata);
             item != opal_list_get_end(&orte_local_jobdata);
             item = opal_list_get_next(item)) {
            jobdat = (orte_odls_job_t*)item;
            if (jobdat->jobid == proct->name.jobid) {
                break;
            }
        }
        if (NULL == jobdat) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        np = jobdat->num_procs / 10;
        /* determine the number of digits required for max vpid */
        numdigs = 1;
        while (np > 0) {
            numdigs++;
            np = np / 10;
        }
        /* construct the filename */
        asprintf(&outfile, "%s.%d.%0*lu", orte_output_filename,
                 (int)ORTE_LOCAL_JOBID(proct->name.jobid),
                 numdigs, (unsigned long)proct->name.vpid);
        /* create the file */
        fdout = open(outfile, O_CREAT|O_RDWR|O_TRUNC, 0644);
        free(outfile);
        if (fdout < 0) {
            /* couldn't be opened */
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
        /* define a sink to that file descriptor */
        ORTE_IOF_SINK_DEFINE(&sink, dst_name, fdout, ORTE_IOF_STDOUTALL,
                             orte_iof_base_write_handler,
                             &mca_iof_orted_component.sinks);
    }
    
SETUP:
    /* define a read event and activate it */
    if (src_tag & ORTE_IOF_STDOUT) {
        ORTE_IOF_READ_EVENT(&proct->revstdout, dst_name, fd, ORTE_IOF_STDOUT,
                            orte_iof_orted_read_handler, false);
    } else if (src_tag & ORTE_IOF_STDERR) {
        ORTE_IOF_READ_EVENT(&proct->revstderr, dst_name, fd, ORTE_IOF_STDERR,
                            orte_iof_orted_read_handler, false);
    } else if (src_tag & ORTE_IOF_STDDIAG) {
        ORTE_IOF_READ_EVENT(&proct->revstddiag, dst_name, fd, ORTE_IOF_STDDIAG,
                            orte_iof_orted_read_handler, false);
    }
    /* if -all- of the readevents for this proc have been defined, then
     * activate them. Otherwise, we can think that the proc is complete
     * because one of the readevents fires -prior- to all of them having
     * been defined!
     */
    if (NULL != proct->revstdout && NULL != proct->revstderr && NULL != proct->revstddiag) {
        proct->revstdout->active = true;
        opal_event_add(&(proct->revstdout->ev), 0);
        proct->revstderr->active = true;
        opal_event_add(&(proct->revstderr->ev), 0);
        proct->revstddiag->active = true;
        opal_event_add(&(proct->revstddiag->ev), 0);
    }
    return ORTE_SUCCESS;
}


/**
 * Pull for a daemon tells
 * us that any info we receive from the HNP that is targeted
 * for stdin of the specified process should be fed down the
 * indicated file descriptor. Thus, all we need to do here
 * is define a local endpoint so we know where to feed anything
 * that comes to us
 */

static int orted_pull(const orte_process_name_t* dst_name,
                      orte_iof_tag_t src_tag,
                      int fd)
{
    orte_iof_sink_t *sink;
    int flags;
    
    /* this is a local call - only stdin is supported */
    if (ORTE_IOF_STDIN != src_tag) {
        return ORTE_ERR_NOT_SUPPORTED;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s iof:orted pulling fd %d for process %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         fd, ORTE_NAME_PRINT(dst_name)));
    
    /* set the file descriptor to non-blocking - do this before we setup
     * the sink in case it fires right away
     */
    if((flags = fcntl(fd, F_GETFL, 0)) < 0) {
        opal_output(orte_iof_base.iof_output, "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", 
                    __FILE__, __LINE__, errno);
    } else {
        flags |= O_NONBLOCK;
        fcntl(fd, F_SETFL, flags);
    }

    ORTE_IOF_SINK_DEFINE(&sink, dst_name, fd, ORTE_IOF_STDIN,
                         stdin_write_handler,
                         &mca_iof_orted_component.sinks);
    
    return ORTE_SUCCESS;
}


/*
 * One of our local procs wants us to close the specifed
 * stream(s), thus terminating any potential io to/from it.
 * For the orted, this just means closing the local fd
 */
static int orted_close(const orte_process_name_t* peer,
                       orte_iof_tag_t source_tag)
{
    opal_list_item_t *item, *next_item;
    orte_iof_sink_t* sink;

    OPAL_THREAD_LOCK(&mca_iof_orted_component.lock);
    
    for(item = opal_list_get_first(&mca_iof_orted_component.sinks);
        item != opal_list_get_end(&mca_iof_orted_component.sinks);
        item = next_item ) {
        sink = (orte_iof_sink_t*)item;
        next_item = opal_list_get_next(item);
        
        if((sink->name.jobid == peer->jobid) &&
           (sink->name.vpid == peer->vpid) &&
           (source_tag & sink->tag)) {

            /* No need to delete the event or close the file
             * descriptor - the destructor will automatically
             * do it for us.
             */
            opal_list_remove_item(&mca_iof_orted_component.sinks, item);
            OBJ_RELEASE(item);
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&mca_iof_orted_component.lock);

    return ORTE_SUCCESS;
}


/*
 * FT event
 */

static int orted_ft_event(int state)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static void stdin_write_handler(int fd, short event, void *cbdata)
{
    orte_iof_sink_t *sink = (orte_iof_sink_t*)cbdata;
    orte_iof_write_event_t *wev = sink->wev;
    opal_list_item_t *item;
    orte_iof_write_output_t *output;
    int num_written;
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s orted:stdin:write:handler writing data to %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         wev->fd));
    
    /* lock us up to protect global operations */
    OPAL_THREAD_LOCK(&mca_iof_orted_component.lock);
    wev->pending = false;
    
    while (NULL != (item = opal_list_remove_first(&wev->outputs))) {
        output = (orte_iof_write_output_t*)item;
        if (0 == output->numbytes) {
            /* this indicates we are to close the fd - there is
             * nothing to write
             */
            OPAL_OUTPUT_VERBOSE((20, orte_iof_base.iof_output,
                                 "%s iof:orted closing fd %d on write event due to zero bytes output",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), wev->fd));
            OBJ_RELEASE(wev);
            sink->wev = NULL;
            goto DEPART;
        }
        num_written = write(wev->fd, output->data, output->numbytes);
        OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                             "%s orted:stdin:write:handler wrote %d bytes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             num_written));
        if (num_written < 0) {
            if (EAGAIN == errno || EINTR == errno) {
                /* push this item back on the front of the list */
                opal_list_prepend(&wev->outputs, item);
                /* leave the write event running so it will call us again
                 * when the fd is ready.
                 */
                wev->pending = true;
                opal_event_add(&wev->ev, 0);
                goto CHECK;
            }            
            /* otherwise, something bad happened so all we can do is declare an
             * error and abort
             */
            OBJ_RELEASE(output);
            OPAL_OUTPUT_VERBOSE((20, orte_iof_base.iof_output,
                                 "%s iof:orted closing fd %d on write event due to negative bytes written",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), wev->fd));
            OBJ_RELEASE(wev);
            sink->wev = NULL;
            /* tell the HNP to stop sending us stuff */
            if (!mca_iof_orted_component.xoff) {
                mca_iof_orted_component.xoff = true;
                orte_iof_orted_send_xonxoff(ORTE_IOF_XOFF);
            }
            goto DEPART;
        } else if (num_written < output->numbytes) {
            OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                                 "%s orted:stdin:write:handler incomplete write %d - adjusting data",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_written));
            /* incomplete write - adjust data to avoid duplicate output */
            memmove(output->data, &output->data[num_written], output->numbytes - num_written);
            /* push this item back on the front of the list */
            opal_list_prepend(&wev->outputs, item);
            /* leave the write event running so it will call us again
             * when the fd is ready. 
             */
            wev->pending = true;
            opal_event_add(&wev->ev, 0);
            goto CHECK;
        }
        OBJ_RELEASE(output);
    }
    
CHECK:
    if (mca_iof_orted_component.xoff) {
        /* if we have told the HNP to stop reading stdin, see if
         * the proc has absorbed enough to justify restart
         *
         * RHC: Note that when multiple procs want stdin, we
         * can get into a fight between a proc turnin stdin
         * back "on" and other procs turning it "off". There
         * is no clear way to resolve this as different procs
         * may take input at different rates.
         */
        if (opal_list_get_size(&wev->outputs) < ORTE_IOF_MAX_INPUT_BUFFERS) {
            /* restart the read */
            mca_iof_orted_component.xoff = false;
            orte_iof_orted_send_xonxoff(ORTE_IOF_XON);
        }
    }
    
DEPART:
    /* unlock and go */
    OPAL_THREAD_UNLOCK(&mca_iof_orted_component.lock);
}
