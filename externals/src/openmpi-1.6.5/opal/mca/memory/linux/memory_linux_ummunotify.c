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
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#ifdef HAVE_STROPTS_H
#include <stropts.h>
#endif
#include <errno.h>
#include <string.h>
#include <sys/ioctl.h>

#include <linux/ummunotify.h>

#include "opal_stdint.h"
#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/memory/memory.h"
#include "opal/memoryhooks/memory.h"
#include "opal/memoryhooks/memory_internal.h"

#include "opal/mca/memory/linux/memory_linux.h"
#include "opal/mca/memory/linux/public.h"

#define DEV_UMMUNOTIFY "/dev/ummunotify"


/*
 * Local functions
 */
static int ummunotify_process(void);
static int ummunotify_register(void *start, size_t len, uint64_t cookie);
static int ummunotify_deregister(void *start, size_t len, uint64_t cookie);


/*
 * Local variables
 */
static bool initialized = false;


/*
 * Global variables (these need to be global variables rather than in
 * the component struct because they are accessed in the
 * opal_memory_changed() macro defined in public.h, and we don't want
 * to have to include the component structure definition in public.h).
 */
uint64_t opal_memory_linux_ummunotify_counter_last_value = 0;
volatile uint64_t *opal_memory_linux_ummunotify_counter = 
    &opal_memory_linux_ummunotify_counter_last_value;


int opal_memory_linux_ummunotify_open(void)
{
    uint64_t *p;

    /* Just to be safe... */
    opal_memory_linux_ummunotify_counter_last_value = 0;
    opal_memory_linux_ummunotify_counter =
        &opal_memory_linux_ummunotify_counter_last_value;

    /* Open the device.  Try to give a meaningful error message if
       we're unable to open it. */
    mca_memory_linux_component.ummunotify_fd = 
        open(DEV_UMMUNOTIFY, O_RDONLY | O_NONBLOCK);
    if (mca_memory_linux_component.ummunotify_fd < 0) {
        char hostname[HOST_NAME_MAX];
        gethostname(hostname, sizeof(hostname));

        if (EACCES == errno) {
            /* This will get a proper show_help when merged into the
               linux component */
            opal_show_help("help-opal-memory-linux.txt",
                           "ummunotify eaccess", true,
                           hostname, DEV_UMMUNOTIFY);
        } else if (ENOENT != errno) {
            /* Don't print an error if DEV_UMMUNOTIFY simply doesn't exist */
            opal_show_help("help-opal-memory-linux.txt",
                           "ummunotify open error", true,
                           hostname, DEV_UMMUNOTIFY, 
                           strerror(errno), errno);
        }
        return OPAL_ERR_NOT_SUPPORTED;
    }

    p = mmap(NULL, sizeof(*opal_memory_linux_ummunotify_counter), 
             PROT_READ, MAP_SHARED, 
             mca_memory_linux_component.ummunotify_fd, 0);
    if (MAP_FAILED == opal_memory_linux_ummunotify_counter) {
        close(mca_memory_linux_component.ummunotify_fd);
        mca_memory_linux_component.ummunotify_fd = -1;
        return OPAL_ERR_NOT_SUPPORTED;
    }
    opal_memory_linux_ummunotify_counter = p;

    /* If everything went well, tell OMPI that we have full support
       for the memory hooks and fill in the component function
       pointers */
    opal_mem_hooks_set_support(OPAL_MEMORY_FREE_SUPPORT | 
                               OPAL_MEMORY_CHUNK_SUPPORT |
                               OPAL_MEMORY_MUNMAP_SUPPORT);
    mca_memory_linux_component.super.memoryc_process = ummunotify_process;
    mca_memory_linux_component.super.memoryc_register = ummunotify_register;
    mca_memory_linux_component.super.memoryc_deregister = ummunotify_deregister;
    initialized = true;

    return OPAL_SUCCESS;
}


/*
 * Called during opal_finalize (usually during MPI_FINALIZE) to tear
 * down anything that can/should be torn down to disable this
 * component.  The application may continue for a while after
 * MPI_FINALIZE, so we should do as much as possible to disable
 * anything we enabled during ummunotify_open().
 */
int opal_memory_linux_ummunotify_close(void)
{
    if (initialized && mca_memory_linux_component.ummunotify_fd >= 0) {
        munmap((void*) opal_memory_linux_ummunotify_counter, 
               sizeof(*opal_memory_linux_ummunotify_counter));
        close(mca_memory_linux_component.ummunotify_fd);
        mca_memory_linux_component.ummunotify_fd = -1;
        opal_memory_linux_ummunotify_counter =
            &opal_memory_linux_ummunotify_counter_last_value;
        initialized = false;
    }

    return OPAL_SUCCESS;
}

/*
 * Called when opal_memory_changed() returns 1
 */
static int ummunotify_process(void)
{
    int n;
    unsigned int i;
    struct ummunotify_event events[128];

    /* Loop reading from the ummunot fd until there's nothing left to
       read.  If we get a LAST event, re-record the counter. */
    while (initialized) {
        n = read(mca_memory_linux_component.ummunotify_fd, 
                 &events, sizeof(events));
        if (n <= 0) {
            return (EAGAIN == errno) ? OPAL_SUCCESS : OPAL_ERR_IN_ERRNO;
        }

        for (i = 0; i < n / sizeof(events[0]); ++i) {
            switch (events[i].type) {
            case UMMUNOTIFY_EVENT_TYPE_INVAL:
                /* 0 => this callback did not come from malloc */
                OPAL_OUTPUT((-1, "ummunot: invalidate start %p, end %p", 
                             (void*) events[i].hint_start,
                             (void*) events[i].hint_end));
                opal_mem_hooks_release_hook((void *) (uintptr_t) events[i].hint_start,
                                            events[i].hint_end - events[i].hint_start,
                                            0);
                break;

            case UMMUNOTIFY_EVENT_TYPE_LAST:
                opal_memory_linux_ummunotify_counter_last_value = 
                    events[i].user_cookie_counter;
                /* Are there more events to read? */
                if (opal_memory_linux_ummunotify_counter_last_value ==
                    *opal_memory_linux_ummunotify_counter) {
                    OPAL_OUTPUT((-1, "ummunot: LAST; done"));
                    return OPAL_SUCCESS;
                }
                OPAL_OUTPUT((-1, "ummunot: LAST; but looping around"));
                break;
            }
        }
    }

    /* Will only get here if this component has not been
       initialized */
    return OPAL_SUCCESS;
}

static int ummunotify_register(void *start, size_t len, uint64_t cookie)
{
    struct ummunotify_register_ioctl r;
    r.reserved = 0;
    r.start = (unsigned long) start;
    r.end = (unsigned long) start + len;
    r.user_cookie = cookie;

    OPAL_OUTPUT((-1, "ummunot: register %p - %p", 
                 start, ((char*) start) + len));
    if (initialized && ioctl(mca_memory_linux_component.ummunotify_fd,
                             UMMUNOTIFY_REGISTER_REGION, &r)) {
        OPAL_OUTPUT((-1, "Error in ioctl register!"));
        return OPAL_ERR_IN_ERRNO;
    }
    
    return OPAL_SUCCESS;
}

static int ummunotify_deregister(void *start, size_t len, uint64_t cookie)
{
    OPAL_OUTPUT((-1, "ummunot: deregister %p - %p", 
                 start, ((char*) start) + len));
    if (initialized && ioctl(mca_memory_linux_component.ummunotify_fd, 
                             UMMUNOTIFY_UNREGISTER_REGION, &cookie)) {
        OPAL_OUTPUT((-1, "Error in ioctl unregister!"));
        return OPAL_ERR_IN_ERRNO;
    }

    return OPAL_SUCCESS;
}
