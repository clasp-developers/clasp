/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/output.h"
#include "opal/util/basename.h"

#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public orte_base_component_t struct.
 */

#include "orte/mca/iof/base/static-components.h"

orte_iof_base_module_t orte_iof = {0};


#if ORTE_DISABLE_FULL_SUPPORT
/* have to include a bogus function here so that
 * the build system sees at least one function
 * in the library
 */
int orte_iof_base_open(void)
{
    return ORTE_SUCCESS;
}

#else

/* class instances */
static void orte_iof_base_proc_construct(orte_iof_proc_t* ptr)
{
    ptr->revstdout = NULL;
    ptr->revstderr = NULL;
    ptr->revstddiag = NULL;
}
static void orte_iof_base_proc_destruct(orte_iof_proc_t* ptr)
{
    if (NULL != ptr->revstdout) {
        OBJ_RELEASE(ptr->revstdout);
    }
    if (NULL != ptr->revstderr) {
        OBJ_RELEASE(ptr->revstderr);
    }
    if (NULL != ptr->revstddiag) {
        OBJ_RELEASE(ptr->revstddiag);
    }
}
OBJ_CLASS_INSTANCE(orte_iof_proc_t,
                   opal_list_item_t,
                   orte_iof_base_proc_construct,
                   orte_iof_base_proc_destruct);


static void orte_iof_base_sink_construct(orte_iof_sink_t* ptr)
{
    ptr->daemon.jobid = ORTE_JOBID_INVALID;
    ptr->daemon.vpid = ORTE_VPID_INVALID;
    ptr->wev = OBJ_NEW(orte_iof_write_event_t);
}
static void orte_iof_base_sink_destruct(orte_iof_sink_t* ptr)
{
    OPAL_OUTPUT_VERBOSE((20, orte_iof_base.iof_output,
                         "%s iof: closing sink for process %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&ptr->name)));
    if (NULL != ptr->wev) {
        OBJ_RELEASE(ptr->wev);
    }
}
OBJ_CLASS_INSTANCE(orte_iof_sink_t,
                   opal_list_item_t,
                   orte_iof_base_sink_construct,
                   orte_iof_base_sink_destruct);


static void orte_iof_base_read_event_construct(orte_iof_read_event_t* rev)
{
    memset(&rev->ev,0,sizeof(rev->ev));
}
static void orte_iof_base_read_event_destruct(orte_iof_read_event_t* rev)
{
    opal_event_del(&rev->ev);
    if (0 <= rev->ev.ev_fd) {
        OPAL_OUTPUT_VERBOSE((20, orte_iof_base.iof_output,
                             "%s iof: closing fd %d for process %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             rev->ev.ev_fd, ORTE_NAME_PRINT(&rev->name)));
        close(rev->ev.ev_fd);
    }
}
OBJ_CLASS_INSTANCE(orte_iof_read_event_t,
                   opal_object_t,
                   orte_iof_base_read_event_construct,
                   orte_iof_base_read_event_destruct);

static void orte_iof_base_write_event_construct(orte_iof_write_event_t* wev)
{
    wev->pending = false;
    wev->fd = -1;
    OBJ_CONSTRUCT(&wev->outputs, opal_list_t);
}
static void orte_iof_base_write_event_destruct(orte_iof_write_event_t* wev)
{
    if (wev->pending) {
        opal_event_del(&wev->ev);
    }
    if (ORTE_PROC_IS_HNP) {
        int xmlfd = fileno(orte_xml_fp);
        if (xmlfd == wev->fd) {
            /* don't close this one - will get it later */
            OBJ_DESTRUCT(&wev->outputs);
            return;
        }
    }
    
    if (2 < wev->fd) {
        OPAL_OUTPUT_VERBOSE((20, orte_iof_base.iof_output,
                             "%s iof: closing fd %d for write event",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), wev->fd));
        close(wev->fd);
    }
    OBJ_DESTRUCT(&wev->outputs);
}
OBJ_CLASS_INSTANCE(orte_iof_write_event_t,
                   opal_list_item_t,
                   orte_iof_base_write_event_construct,
                   orte_iof_base_write_event_destruct);

OBJ_CLASS_INSTANCE(orte_iof_write_output_t,
                   opal_list_item_t,
                   NULL, NULL);

/*
 * Global variables
 */

orte_iof_base_t orte_iof_base;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_iof_base_open(void)
{
    int rc, xmlfd;
    
    /* Initialize globals */
    OBJ_CONSTRUCT(&orte_iof_base.iof_components_opened, opal_list_t);
    OBJ_CONSTRUCT(&orte_iof_base.iof_write_output_lock, opal_mutex_t);

    /* did the user request we print output to files? */
    if (NULL != orte_output_filename) {
        /* we will setup the files themselves as needed in the iof
         * module. For now, let's see if the filename contains a
         * path, or just a name
         */
        char *path;
        path = opal_dirname(orte_output_filename);
        if (0 != strcmp(path, orte_output_filename)) {
            /* there is a path in this name - ensure that the directory
             * exists, and create it if not
             */
            if (ORTE_SUCCESS != (rc = opal_os_dirpath_create(path, S_IRWXU))) {
                return rc;
            }
        }
    }
    
    /* daemons do not need to do this as they do not write out stdout/err */
    if (!ORTE_PROC_IS_DAEMON) {
        if (orte_xml_output) {
            if (NULL != orte_xml_fp) {
                /* user wants all xml-formatted output sent to file */
                xmlfd = fileno(orte_xml_fp);
            } else {
                xmlfd = 1;
            }
            /* setup the stdout event */
            ORTE_IOF_SINK_DEFINE(&orte_iof_base.iof_write_stdout, ORTE_PROC_MY_NAME,
                                 xmlfd, ORTE_IOF_STDOUT, orte_iof_base_write_handler, NULL);
            /* don't create a stderr event - all output will go to
             * the stdout channel
             */
        } else {
            /* setup the stdout event */
            ORTE_IOF_SINK_DEFINE(&orte_iof_base.iof_write_stdout, ORTE_PROC_MY_NAME,
                                 1, ORTE_IOF_STDOUT, orte_iof_base_write_handler, NULL);
            /* setup the stderr event */
            ORTE_IOF_SINK_DEFINE(&orte_iof_base.iof_write_stderr, ORTE_PROC_MY_NAME,
                                 2, ORTE_IOF_STDERR, orte_iof_base_write_handler, NULL);
        }
        
        /* do NOT set these file descriptors to non-blocking. If we do so,
         * we set the file descriptor to non-blocking for everyone that has
         * that file descriptor, which includes everyone else in our shell
         * pipeline chain.  (See
         * http://lists.freebsd.org/pipermail/freebsd-hackers/2005-January/009742.html).
         * This causes things like "mpirun -np 1 big_app | cat" to lose
         * output, because cat's stdout is then ALSO non-blocking and cat
         * isn't built to deal with that case (same with almost all other
         * unix text utils). 
         */        
    }
    
    orte_iof_base.iof_output = opal_output_open(NULL);
    
    /* Open up all available components */
    if (ORTE_SUCCESS != 
        mca_base_components_open("iof", orte_iof_base.iof_output,
                                 mca_iof_base_static_components, 
                                 &orte_iof_base.iof_components_opened,
                                 true)) {
        return ORTE_ERROR;
    }
  
    /* All done */
    return ORTE_SUCCESS;
}
#endif
