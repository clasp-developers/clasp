/*
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * Note: this file is a little fast-n-loose with OPAL_HAVE_THREADS --
 * it uses this value in run-time "if" conditionals (vs. compile-time
 * #if conditionals).  We also don't protect including <pthread.h>.
 * That's because this component currently only compiles on Linux and
 * Solaris, and both of these OS's have pthreads.  Using the run-time
 * conditionals gives us bettern compile-time checking, even of code
 * that isn't activated.
 *
 * Note, too, that the functionality in this file does *not* require
 * all the heavyweight OMPI thread infrastructure (e.g., from
 * --enable-mpi-threads or --enable-progress-threads).  All work that
 * is done in a separate progress thread is very carefully segregated
 * from that of the main thread, and communication back to the main
 * thread
 */

#include "orte_config.h"

#include <unistd.h>
#include <errno.h>

#include "orte/constants.h"

#include "notifier_command.h"


/*
 * Simple loop over reading from a fd
 */
int orte_notifier_command_read_fd(int fd, int len, void *buffer)
{
    int rc;
    char *b = buffer;

    while (len > 0) {
        rc = read(fd, b, len);
        if (rc < 0 && EAGAIN == errno) {
            continue;
        } else if (rc > 0) {
            len -= rc;
            b += rc;
        } else {
            return ORTE_ERROR;
        }
    }
    return ORTE_SUCCESS;
}


/*
 * Simple loop over writing to an fd
 */
int orte_notifier_command_write_fd(int fd, int len, void *buffer)
{
    int rc;
    char *b = buffer;

    while (len > 0) {
        rc = write(fd, b, len);
        if (rc < 0 && EAGAIN == errno) {
            continue;
        } else if (rc > 0) {
            len -= rc;
            b += rc;
        } else {
            return ORTE_ERROR;
        }
    }

    return ORTE_SUCCESS;
}
