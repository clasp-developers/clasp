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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
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


#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/mca/odls/odls_types.h"

#include "orte/mca/iof/base/base.h"
#include "iof_hnp.h"

/* LOCAL FUNCTIONS */
static void stdin_write_handler(int fd, short event, void *cbdata);


/* API FUNCTIONS */
static int hnp_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd);

static int hnp_pull(const orte_process_name_t* src_name,
                orte_iof_tag_t src_tag,
                int fd);

static int hnp_close(const orte_process_name_t* peer,
                 orte_iof_tag_t source_tag);

static int hnp_ft_event(int state);

/* The API's in this module are solely used to support LOCAL
 * procs - i.e., procs that are co-located to the HNP. Remote
 * procs interact with the HNP's IOF via the HNP's receive function,
 * which operates independently and is in the iof_hnp_receive.c file
 */

orte_iof_base_module_t orte_iof_hnp_module = {
    hnp_push,
    hnp_pull,
    hnp_close,
    hnp_ft_event
};


/* Setup to read local data. If the tag is other than STDIN,
 * then this is output being pushed from one of my child processes
 * and I'll write the data out myself. If the tag is STDIN,
 * then I need to setup to read from my stdin, and send anything
 * I get to the specified dst_name. The dst_name in this case tells
 * us which procs are to get stdin - only two options are supported:
 *
 * (a) a specific name, usually vpid=0; or
 *
 * (b) all procs, specified by vpid=ORTE_VPID_WILDCARD
 *
 * The orte_plm_base_launch_apps function calls iof.push after
 * the procs are launched and tells us how to distribute stdin. This
 * ensures that the procs are started -before- we begin reading stdin
 * and attempting to send it to remote procs
 */
static int hnp_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd)
{
    orte_job_t *jdata;
    orte_proc_t *proc;
    orte_iof_sink_t *sink;
    orte_iof_proc_t *proct;
    opal_list_item_t *item;
    int flags;
    char *outfile;
    int fdout;
    orte_odls_job_t *jobdat=NULL;
    int np, numdigs;
    int rc;

    /* don't do this if the dst vpid is invalid or the fd is negative! */
    if (ORTE_VPID_INVALID == dst_name->vpid || fd < 0) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s iof:hnp pushing fd %d for process %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         fd, ORTE_NAME_PRINT(dst_name)));
    
    if (!(src_tag & ORTE_IOF_STDIN)) {
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
        for (item = opal_list_get_first(&mca_iof_hnp_component.procs);
             item != opal_list_get_end(&mca_iof_hnp_component.procs);
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
        opal_list_append(&mca_iof_hnp_component.procs, &proct->super);
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
                                 &mca_iof_hnp_component.sinks);
        }
        
    SETUP:
        /* define a read event and activate it */
        if (src_tag & ORTE_IOF_STDOUT) {
            ORTE_IOF_READ_EVENT(&proct->revstdout, dst_name, fd, ORTE_IOF_STDOUT,
                                orte_iof_hnp_read_local_handler, false);
        } else if (src_tag & ORTE_IOF_STDERR) {
            ORTE_IOF_READ_EVENT(&proct->revstderr, dst_name, fd, ORTE_IOF_STDERR,
                                orte_iof_hnp_read_local_handler, false);
        } else if (src_tag & ORTE_IOF_STDDIAG) {
            ORTE_IOF_READ_EVENT(&proct->revstddiag, dst_name, fd, ORTE_IOF_STDDIAG,
                                orte_iof_hnp_read_local_handler, false);
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

    /* if we are pushing stdin, this is happening only during launch - setup
     * a target for this destination if it is going somewhere other than me
     */
    if (ORTE_VPID_WILDCARD == dst_name->vpid) {
        /* if wildcard, define a sink with that info so it gets sent out */
        ORTE_IOF_SINK_DEFINE(&sink, dst_name, -1, ORTE_IOF_STDIN,
                             stdin_write_handler,
                             &mca_iof_hnp_component.sinks);
        sink->daemon.jobid = ORTE_PROC_MY_NAME->jobid;
        sink->daemon.vpid = ORTE_VPID_WILDCARD;
     } else {
        /* no - lookup the proc's daemon and set that into sink */
        if (NULL == (jdata = orte_get_job_data_object(dst_name->jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            return ORTE_ERR_BAD_PARAM;
        }
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, dst_name->vpid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        /* if it is me, then don't set this up - we'll get it on the pull */
        if (ORTE_PROC_MY_NAME->vpid != proc->node->daemon->name.vpid) {
            ORTE_IOF_SINK_DEFINE(&sink, dst_name, -1, ORTE_IOF_STDIN,
                                 stdin_write_handler,
                                 &mca_iof_hnp_component.sinks);
            sink->daemon.jobid = ORTE_PROC_MY_NAME->jobid;
            sink->daemon.vpid = proc->node->daemon->name.vpid;
        }
    }
    
    /* now setup the read - but check to only do this once */
    if (NULL == mca_iof_hnp_component.stdinev) {
        /* Since we are the HNP, we don't want to set nonblocking on our
         * stdio stream.  If we do so, we set the file descriptor to
         * non-blocking for everyone that has that file descriptor, which
         * includes everyone else in our shell pipeline chain.  (See
         * http://lists.freebsd.org/pipermail/freebsd-hackers/2005-January/009742.html).
         * This causes things like "mpirun -np 1 big_app | cat" to lose
         * output, because cat's stdout is then ALSO non-blocking and cat
         * isn't built to deal with that case (same with almost all other
         * unix text utils). 
         */
        if (0 != fd) {
            if((flags = fcntl(fd, F_GETFL, 0)) < 0) {
                opal_output(orte_iof_base.iof_output, "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", 
                            __FILE__, __LINE__, errno);
            } else {
                flags |= O_NONBLOCK;
                fcntl(fd, F_SETFL, flags);
            }            
        }
        if (isatty(fd)) {
            /* We should avoid trying to read from stdin if we
             * have a terminal, but are backgrounded.  Catch the
             * signals that are commonly used when we switch
             * between being backgrounded and not.  If the
             * filedescriptor is not a tty, don't worry about it
             * and always stay connected.
             */
            opal_signal_set(&mca_iof_hnp_component.stdinsig,
                            SIGCONT, orte_iof_hnp_stdin_cb,
                            NULL);
            
            /* setup a read event to read stdin, but don't activate it yet. The
             * dst_name indicates who should receive the stdin. If that recipient
             * doesn't do a corresponding pull, however, then the stdin will
             * be dropped upon receipt at the local daemon
             */
            ORTE_IOF_READ_EVENT(&mca_iof_hnp_component.stdinev,
                                dst_name, fd, ORTE_IOF_STDIN,
                                orte_iof_hnp_read_local_handler, false);
            
            /* check to see if we want the stdin read event to be
             * active - we will always at least define the event,
             * but may delay its activation
             */
            if (!(src_tag & ORTE_IOF_STDIN) || orte_iof_hnp_stdin_check(fd)) {
                mca_iof_hnp_component.stdinev->active = true;
                if (OPAL_SUCCESS != (rc = opal_event_add(&(mca_iof_hnp_component.stdinev->ev), 0))) {
                    ORTE_ERROR_LOG(rc);
                }
            }
        } else {
            /* if we are not looking at a tty, just setup a read event
             * and activate it
             */
            ORTE_IOF_READ_EVENT(&mca_iof_hnp_component.stdinev,
                                dst_name, fd, ORTE_IOF_STDIN,
                                orte_iof_hnp_read_local_handler, true);
        }
    }
    return ORTE_SUCCESS;
}


/*
 * Since we are the HNP, the only "pull" call comes from a local
 * process so we can record the file descriptor for its stdin.
 */

static int hnp_pull(const orte_process_name_t* dst_name,
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
                         "%s iof:hnp pulling fd %d for process %s",
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
                         &mca_iof_hnp_component.sinks);
    sink->daemon.jobid = ORTE_PROC_MY_NAME->jobid;
    sink->daemon.vpid = ORTE_PROC_MY_NAME->vpid;

    return ORTE_SUCCESS;
}

/*
 * One of our local procs wants us to close the specifed
 * stream(s), thus terminating any potential io to/from it.
 */
static int hnp_close(const orte_process_name_t* peer,
                     orte_iof_tag_t source_tag)
{
    opal_list_item_t *item, *next_item;
    orte_iof_sink_t* sink;
    
    for(item = opal_list_get_first(&mca_iof_hnp_component.sinks);
        item != opal_list_get_end(&mca_iof_hnp_component.sinks);
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
            opal_list_remove_item(&mca_iof_hnp_component.sinks, item);
            OBJ_RELEASE(item);
            break;
        }
    }
    return ORTE_SUCCESS;
}

int hnp_ft_event(int state) {
    /*
     * Replica doesn't need to do anything for a checkpoint
     */
    return ORTE_SUCCESS;
}


static void stdin_write_handler(int fd, short event, void *cbdata)
{
    orte_iof_sink_t *sink = (orte_iof_sink_t*)cbdata;
    orte_iof_write_event_t *wev = sink->wev;
    opal_list_item_t *item;
    orte_iof_write_output_t *output;
    int num_written;
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s hnp:stdin:write:handler writing data to %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         wev->fd));
    
    /* lock us up to protect global operations */
    OPAL_THREAD_LOCK(&mca_iof_hnp_component.lock);
    wev->pending = false;
    
    while (NULL != (item = opal_list_remove_first(&wev->outputs))) {
        output = (orte_iof_write_output_t*)item;
        /* if an abnormal termination has occurred, just dump
         * this data as we are aborting
         */
        if (orte_abnormal_term_ordered) {
            OBJ_RELEASE(output);
            continue;
        }
        if (0 == output->numbytes) {
            /* this indicates we are to close the fd - there is
             * nothing to write
             */
            OPAL_OUTPUT_VERBOSE((20, orte_iof_base.iof_output,
                                 "%s iof:hnp closing fd %d on write event due to zero bytes output",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), wev->fd));
            OBJ_RELEASE(wev);
            sink->wev = NULL;
            /* just leave - we don't want to restart the
             * read event!
             */
            goto DEPART;
        }
        num_written = write(wev->fd, output->data, output->numbytes);
        OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                             "%s hnp:stdin:write:handler wrote %d bytes",
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
                                 "%s iof:hnp closing fd %d on write event due to negative bytes written",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), wev->fd));
            OBJ_RELEASE(wev);
            sink->wev = NULL;
            goto DEPART;
        } else if (num_written < output->numbytes) {
            OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                                 "%s hnp:stdin:write:handler incomplete write %d - adjusting data",
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
    if (NULL != mca_iof_hnp_component.stdinev &&
        !orte_abnormal_term_ordered &&
        !mca_iof_hnp_component.stdinev->active) {
        OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                            "read event is off - checking if okay to restart"));
        /* if we have turned off the read event, check to
         * see if the output list has shrunk enough to
         * turn it back on
         *
         * RHC: Note that when multiple procs want stdin, we
         * can get into a fight between a proc turnin stdin
         * back "on" and other procs turning it "off". There
         * is no clear way to resolve this as different procs
         * may take input at different rates.
         */
        if (opal_list_get_size(&wev->outputs) < ORTE_IOF_MAX_INPUT_BUFFERS) {
            /* restart the read */
            OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                                 "restarting read event"));
            mca_iof_hnp_component.stdinev->active = true;
            opal_event_add(&(mca_iof_hnp_component.stdinev->ev), 0);
        }
    }

DEPART:
    /* unlock and go */
    OPAL_THREAD_UNLOCK(&mca_iof_hnp_component.lock);
}
