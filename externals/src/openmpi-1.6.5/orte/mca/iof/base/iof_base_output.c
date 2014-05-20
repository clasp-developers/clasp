/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
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

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#include <errno.h>

#include "opal/util/output.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/iof/base/base.h"

int orte_iof_base_write_output(orte_process_name_t *name, orte_iof_tag_t stream,
                                unsigned char *data, int numbytes,
                               orte_iof_write_event_t *channel)
{
    char starttag[ORTE_IOF_BASE_TAG_MAX], endtag[ORTE_IOF_BASE_TAG_MAX], *suffix;
    orte_iof_write_output_t *output;
    int i, j, k, starttaglen, endtaglen, num_buffered;
    bool endtagged;
    char qprint[10];

    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s write:output setting up to write %d bytes to %s for %s on fd %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numbytes,
                         (ORTE_IOF_STDIN & stream) ? "stdin" : ((ORTE_IOF_STDOUT & stream) ? "stdout" : ((ORTE_IOF_STDERR & stream) ? "stderr" : "stddiag")),
                         ORTE_NAME_PRINT(name), channel->fd));

    /* setup output object */
    output = OBJ_NEW(orte_iof_write_output_t);
    
    /* write output data to the corresponding tag */
    if (ORTE_IOF_STDIN & stream) {
        /* copy over the data to be written */
        if (0 < numbytes) {
            /* don't copy 0 bytes - we just need to pass
             * the zero bytes so the fd can be closed
             * after it writes everything out
             */
            memcpy(output->data, data, numbytes);
        }
        output->numbytes = numbytes;
        goto process;
    } else if (ORTE_IOF_STDOUT & stream) {
        /* write the bytes to stdout */
        suffix = "stdout";
    } else if (ORTE_IOF_STDERR & stream) {
        /* write the bytes to stderr */
        suffix = "stderr";
    } else if (ORTE_IOF_STDDIAG & stream) {
        /* write the bytes to stderr */
        suffix = "stddiag";
    } else {
        /* error - this should never happen */
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                             "%s stream %0x", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), stream));
        return ORTE_ERR_VALUE_OUT_OF_BOUNDS;
    }

    /* if this is to be xml tagged, create a tag with the correct syntax - we do not allow
     * timestamping of xml output
     */
    if (orte_xml_output) {
        snprintf(starttag, ORTE_IOF_BASE_TAG_MAX, "<%s rank=\"%s\">", suffix, ORTE_VPID_PRINT(name->vpid));
        snprintf(endtag, ORTE_IOF_BASE_TAG_MAX, "</%s>", suffix);
        goto construct;
    }
    
    /* if we are to timestamp output, start the tag with that */
    if (orte_timestamp_output) {
        time_t mytime;
        char *cptr;
        /* get the timestamp */
        time(&mytime);
        cptr = ctime(&mytime);
        cptr[strlen(cptr)-1] = '\0';  /* remove trailing newline */
        
        if (orte_tag_output) {
            /* if we want it tagged as well, use both */
            snprintf(starttag, ORTE_IOF_BASE_TAG_MAX, "%s[%s,%s]<%s>:",
                     cptr, ORTE_LOCAL_JOBID_PRINT(name->jobid),
                     ORTE_VPID_PRINT(name->vpid), suffix);
        } else {
            /* only use timestamp */
            snprintf(starttag, ORTE_IOF_BASE_TAG_MAX, "%s<%s>:", cptr, suffix);
        }
        /* no endtag for this option */
        memset(endtag, '\0', ORTE_IOF_BASE_TAG_MAX);
        goto construct;
    }
    
    if (orte_tag_output) {
        snprintf(starttag, ORTE_IOF_BASE_TAG_MAX, "[%s,%s]<%s>:",
                 ORTE_LOCAL_JOBID_PRINT(name->jobid),
                 ORTE_VPID_PRINT(name->vpid), suffix);
        /* no endtag for this option */
        memset(endtag, '\0', ORTE_IOF_BASE_TAG_MAX);
        goto construct;
    }
    
    /* if we get here, then the data is not to be tagged - just copy it
     * and move on to processing
     */
    if (0 < numbytes) {
        /* don't copy 0 bytes - we just need to pass
         * the zero bytes so the fd can be closed
         * after it writes everything out
         */
        memcpy(output->data, data, numbytes);
    }
    output->numbytes = numbytes;
    goto process;

construct:
    starttaglen = strlen(starttag);
    endtaglen = strlen(endtag);
    endtagged = false;
    /* start with the tag */
    for (j=0, k=0; j < starttaglen && k < ORTE_IOF_BASE_TAGGED_OUT_MAX; j++) {
        output->data[k++] = starttag[j];
    }        
    /* cycle through the data looking for <cr>
     * and replace those with the tag
     */
    for (i=0; i < numbytes && k < ORTE_IOF_BASE_TAGGED_OUT_MAX; i++) {
        if (orte_xml_output) {
            if ('&' == data[i]) {
                if (k+5 >= ORTE_IOF_BASE_TAGGED_OUT_MAX) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    goto process;
                }
                snprintf(qprint, 10, "&amp;");
                for (j=0; j < (int)strlen(qprint) && k < ORTE_IOF_BASE_TAGGED_OUT_MAX; j++) {
                    output->data[k++] = qprint[j];
                }
            } else if ('<' == data[i]) {
                if (k+4 >= ORTE_IOF_BASE_TAGGED_OUT_MAX) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    goto process;
                }
                snprintf(qprint, 10, "&lt;");
                for (j=0; j < (int)strlen(qprint) && k < ORTE_IOF_BASE_TAGGED_OUT_MAX; j++) {
                    output->data[k++] = qprint[j];
                }
            } else if ('>' == data[i]) {
                if (k+4 >= ORTE_IOF_BASE_TAGGED_OUT_MAX) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    goto process;
                }
                snprintf(qprint, 10, "&gt;");
                for (j=0; j < (int)strlen(qprint) && k < ORTE_IOF_BASE_TAGGED_OUT_MAX; j++) {
                    output->data[k++] = qprint[j];
                }
            } else if (data[i] < 32 || data[i] > 127) {
                /* this is a non-printable character, so escape it too */
                if (k+7 >= ORTE_IOF_BASE_TAGGED_OUT_MAX) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    goto process;
                }
                snprintf(qprint, 10, "&#%03d;", (int)data[i]);
                for (j=0; j < (int)strlen(qprint) && k < ORTE_IOF_BASE_TAGGED_OUT_MAX; j++) {
                    output->data[k++] = qprint[j];
                }
                /* if this was a \n, then we also need to break the line with the end tag */
                if ('\n' == data[i] && (k+endtaglen+1) < ORTE_IOF_BASE_TAGGED_OUT_MAX) {
                    /* we need to break the line with the end tag */
                    for (j=0; j < endtaglen && k < ORTE_IOF_BASE_TAGGED_OUT_MAX-1; j++) {
                        output->data[k++] = endtag[j];
                    }
                    /* move the <cr> over */
                    output->data[k++] = '\n';
                    /* if this isn't the end of the data buffer, add a new start tag */
                    if (i < numbytes-1 && (k+starttaglen) < ORTE_IOF_BASE_TAGGED_OUT_MAX) {
                        for (j=0; j < starttaglen && k < ORTE_IOF_BASE_TAGGED_OUT_MAX; j++) {
                            output->data[k++] = starttag[j];
                            endtagged = false;
                        }
                    } else {
                        endtagged = true;
                    }
                }
            } else {
                output->data[k++] = data[i];
            }
        } else {
            if ('\n' == data[i]) {
                /* we need to break the line with the end tag */
                for (j=0; j < endtaglen && k < ORTE_IOF_BASE_TAGGED_OUT_MAX-1; j++) {
                    output->data[k++] = endtag[j];
                }
                /* move the <cr> over */
                output->data[k++] = '\n';
                /* if this isn't the end of the data buffer, add a new start tag */
                if (i < numbytes-1) {
                    for (j=0; j < starttaglen && k < ORTE_IOF_BASE_TAGGED_OUT_MAX; j++) {
                        output->data[k++] = starttag[j];
                        endtagged = false;
                    }
                } else {
                    endtagged = true;
                }
            } else {
                output->data[k++] = data[i];
            }
        }
    }
    if (!endtagged) {
        /* need to add an endtag */
        for (j=0; j < endtaglen && k < ORTE_IOF_BASE_TAGGED_OUT_MAX-1; j++) {
            output->data[k++] = endtag[j];
        }
        output->data[k++] = '\n';
    }
    output->numbytes = k;
    
process:
    /* lock us up to protect global operations */
    OPAL_THREAD_LOCK(&orte_iof_base.iof_write_output_lock);
    
    /* add this data to the write list for this fd */
    opal_list_append(&channel->outputs, &output->super);

    /* record how big the buffer is */
    num_buffered = opal_list_get_size(&channel->outputs);
    
    /* is the write event issued? */
    if (!channel->pending) {
        /* issue it */
        OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                             "%s write:output adding write event",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        opal_event_add(&channel->ev, 0);
        channel->pending = true;
    }
    
    /* unlock and go */
    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_write_output_lock);
    
    return num_buffered;
}

void orte_iof_base_write_handler(int fd, short event, void *cbdata)
{
    orte_iof_sink_t *sink = (orte_iof_sink_t*)cbdata;
    orte_iof_write_event_t *wev = sink->wev;
    opal_list_item_t *item;
    orte_iof_write_output_t *output;
    int num_written;
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s write:handler writing data to %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         wev->fd));

    /* lock us up to protect global operations */
    OPAL_THREAD_LOCK(&orte_iof_base.iof_write_output_lock);
    
    while (NULL != (item = opal_list_remove_first(&wev->outputs))) {
        output = (orte_iof_write_output_t*)item;
        num_written = write(wev->fd, output->data, output->numbytes);
        if (num_written < 0) {
            if (EAGAIN == errno || EINTR == errno) {
                /* push this item back on the front of the list */
                opal_list_prepend(&wev->outputs, item);
                /* leave the write event running so it will call us again
                 * when the fd is ready.
                 */
                goto DEPART;
            }
            /* otherwise, something bad happened so all we can do is abort
             * this attempt
             */
            OBJ_RELEASE(output);
            goto ABORT;
        } else if (num_written < output->numbytes) {
            /* incomplete write - adjust data to avoid duplicate output */
            memmove(output->data, &output->data[num_written], output->numbytes - num_written);
            /* push this item back on the  front of the list */
            opal_list_prepend(&wev->outputs, item);
            /* leave the write event running so it will call us again
             * when the fd is ready
             */
            goto DEPART;
        }
        OBJ_RELEASE(output);
    }
ABORT:
    opal_event_del(&wev->ev);
    wev->pending = false;

DEPART:
    /* unlock and go */
    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_write_output_lock);
}
