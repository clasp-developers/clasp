/*
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009 Sandia National Laboratories. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>

#include "opal/util/fd.h"
#include "opal/constants.h"


/*
 * Simple loop over reading from a fd
 */
int opal_fd_read(int fd, int len, void *buffer)
{
    int rc;
    char *b = buffer;

    while (len > 0) {
        rc = read(fd, b, len);
        if (rc < 0 && (EAGAIN == errno || EINTR == errno)) {
            continue;
        } else if (rc > 0) {
            len -= rc;
            b += rc;
        } else if (0 == rc) {
            return OPAL_ERR_TIMEOUT;
        } else {
            return OPAL_ERR_IN_ERRNO;
        }
    }
    return OPAL_SUCCESS;
}


/*
 * Simple loop over writing to an fd
 */
int opal_fd_write(int fd, int len, const void *buffer)
{
    int rc;
    const char *b = buffer;

    while (len > 0) {
        rc = write(fd, b, len);
        if (rc < 0 && (EAGAIN == errno || EINTR == errno)) {
            continue;
        } else if (rc > 0) {
            len -= rc;
            b += rc;
        } else {
            return OPAL_ERR_IN_ERRNO;
        }
    }

    return OPAL_SUCCESS;
}


