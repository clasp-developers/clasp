/*
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009 Sandia National Laboratories. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/* @file */

#ifndef OPAL_UTIL_FD_H_
#define OPAL_UTIL_FD_H_

#include "opal_config.h"

BEGIN_C_DECLS

/**
 * Read a complete buffer from a file descriptor.
 *
 * @param fd File descriptor
 * @param len Number of bytes to read
 * @param buffer Pre-allocated buffer (large enough to hold len bytes)
 *
 * @returns OPAL_SUCCESS upon success.
 * @returns OPAL_ERR_TIMEOUT if the fd closes before reading the full amount.
 * @returns OPAL_ERR_IN_ERRNO otherwise.
 *
 * Loop over reading from the fd until len bytes are read or an error
 * occurs.  EAGAIN and EINTR are transparently handled.
 */
OPAL_DECLSPEC int opal_fd_read(int fd, int len, void *buffer);

/**
 * Write a complete buffer to a file descriptor.
 *
 * @param fd File descriptor
 * @param len Number of bytes to write
 * @param buffer Buffer to write from
 *
 * @returns OPAL_SUCCESS upon success.
 * @returns OPAL_ERR_IN_ERRNO otherwise.
 *
 * Loop over writing to the fd until len bytes are written or an error
 * occurs.  EAGAIN and EINTR are transparently handled.
 */
OPAL_DECLSPEC int opal_fd_write(int fd, int len, const void *buffer);

END_C_DECLS

#endif
