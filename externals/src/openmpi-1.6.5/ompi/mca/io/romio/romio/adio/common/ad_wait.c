/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 2004 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_AIO_H
#include <aio.h>
#endif
#ifdef HAVE_SYS_AIO_H
#include <sys/aio.h>
#endif

/* Workaround for incomplete set of definitions if __REDIRECT is not 
   defined and large file support is used in aio.h */
#if !defined(__REDIRECT) && defined(__USE_FILE_OFFSET64)
#define aiocb aiocb64
#endif

/* ADIOI_GEN_IOComplete
 *
 * This code handles two distinct cases.  If ROMIO_HAVE_WORKING_AIO is
 * not defined, then I/O was performed by a blocking call already.  In
 * that case all we need to do is optionally set the bytes in the
 * status structure and free the request.
 *
 * If ROMIO_HAVE_WORKING_AIO is defined, then we may need to wait for I/O
 * to complete.
 */
void ADIOI_GEN_IOComplete(ADIO_Request *request, ADIO_Status *status,
			  int *error_code)  
{
	return;
 
}
